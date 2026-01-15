use crate::{
    ebi_framework::{ebi_input::EbiInput, ebi_trait::FromEbiTraitObject},
    ebi_traits::{
        ebi_trait_finite_stochastic_language::EbiTraitFiniteStochasticLanguage,
        ebi_trait_queriable_stochastic_language::EbiTraitQueriableStochasticLanguage,
    },
    semantics::{
        labelled_petri_net_semantics::LPNMarking,
        semantics::Semantics,
        stochastic_nondeterministic_finite_automaton_semantics::{
            State as SnfaState, StochasticNondeterministicFiniteAutomaton as Snfa,
            Transition as SnfaTransition,
        },
    },
    stochastic_semantics::stochastic_semantics::StochasticSemantics,
    techniques::{
        bounded::Bounded, chi_square_stochastic_conformance::ChiSquareStochasticConformance,
        hellinger_stochastic_conformance::HellingerStochasticConformance, livelock_patch,
        sample::Sampler,
        unit_earth_movers_stochastic_conformance::UnitEarthMoversStochasticConformance,
    },
};
use anyhow::{Context, Ok, Result, anyhow};
use ebi_derive::EbiInputEnum;
use ebi_objects::{
    ActivityKey, HasActivityKey, TranslateActivityKey,
    ebi_arithmetic::{Fraction, Recip, ebi_number::Zero},
    ebi_objects::{
        finite_stochastic_language::FiniteStochasticLanguage,
        stochastic_labelled_petri_net::StochasticLabelledPetriNet,
    },
};
use rayon::prelude::*;
use rustc_hash::FxHashMap;
use std::{collections::HashMap, sync::Arc};

/// Supported distance metrics for Markovian abstraction comparison.
#[derive(Clone, Copy, Debug, EbiInputEnum)]
pub enum DistanceMetric {
    CSSC,
    HSC,
    UEMSC,
}

/// Default number of traces to sample when falling back to simulation for
/// unbounded Petri nets. (maybe optional parameter later)
const DEFAULT_SAMPLE_SIZE: usize = 10_000;

#[derive(Debug, Clone)]
/// Represents a k-th order Markovian abstraction of a stochastic language
pub struct MarkovianAbstraction {
    /// The order of the abstraction (k)
    pub k: usize,
    /// The mapping from subtraces to normalized frequencies
    /// Uses Arc<[String]> to reduce memory usage through shared ownership
    pub abstraction: FxHashMap<Arc<[String]>, Fraction>,
}

// function can convert markovianabstraction to FiniteStochasticLanguage
impl From<MarkovianAbstraction> for FiniteStochasticLanguage {
    fn from(ma: MarkovianAbstraction) -> Self {
        // 1. Create a new ActivityKey to handle the mapping from String -> Activity ID
        let mut activity_key = ActivityKey::new();
        let mut traces = HashMap::new();

        // 2. Iterate over the abstraction (subtraces -> fraction)
        for (subtrace, fraction) in ma.abstraction {
            // Convert Arc<[String]> to Vec<String> so it can be processed
            let trace_strings: Vec<String> = subtrace.to_vec();

            // Register the strings in the ActivityKey and get back the internal indices (Vec<Activity>)
            // Note: based on the provided file, process_trace handles string registration
            let encoded_trace = activity_key.process_trace(&trace_strings);

            // Insert into the new traces map
            traces.insert(encoded_trace, fraction);
        }

        // 3. Construct the FiniteStochasticLanguage
        // new_raw is used because we are populating the map manually and don't necessarily want to force re-normalization immediately
        FiniteStochasticLanguage::new_raw(traces, activity_key)
    }
}

pub trait StochasticMarkovianAbstraction {
    /// Compare `self` with another stochastic language using the selected
    /// distance metric over their k-th order Markovian abstractions and
    /// return a conformance score in the range [0,1] where 1 means perfect
    /// match.
    ///
    /// Implemented metrics:
    /// * `DistanceMetric::Uemsc` – returns `1 – uEMSC distance`
    /// * `DistanceMetric::ScaledL2` – returns `1 – Scaled L2 distance`
    /// * `DistanceMetric::JensenShannon` – returns `1 – Square-root Jensen–Shannon distance`
    /// * `DistanceMetric::Hellinger` – returns `1 – Hellinger distance`
    ///
    /// Any other variant (if added in the future) will yield an error until
    /// its computation is implemented.
    fn markovian_conformance(
        &self,
        slpn: StochasticLabelledPetriNet,
        k: usize,
        metric: DistanceMetric,
    ) -> Result<Fraction>;
}

impl StochasticMarkovianAbstraction for dyn EbiTraitFiniteStochasticLanguage {
    fn markovian_conformance(
        &self,
        slpn: StochasticLabelledPetriNet,
        k: usize,
        metric: DistanceMetric,
    ) -> Result<Fraction> {
        let delta = Fraction::from((1, 1000)); // default delta value

        // Check the value of k
        if k < 1 {
            return Err(anyhow::anyhow!("k must be at least 1"));
        }

        // Step 1: Compute abstraction for the first language
        let abstraction1 = compute_abstraction_for_log(self, k)
            .context("Computing abstraction for first language (finite log)")?;

        let fsl: FiniteStochasticLanguage = abstraction1.into();
        let language1: Box<dyn EbiTraitFiniteStochasticLanguage> = Box::new(fsl);

        // Step 2: Compute abstraction for the second language
        let mut shared_key = self.activity_key().clone();
        let model = slpn;
        let abstraction2 = abstraction_of_slpn(model, &mut shared_key, k, delta)
            .context("Computing abstraction for second language")?;
        let fsl2: FiniteStochasticLanguage = abstraction2.into();
        let language2: Box<dyn EbiTraitQueriableStochasticLanguage> = Box::new(fsl2);

        // Step 3: Compute the conformance between the abstractions depending on the metric
        match metric {
            DistanceMetric::CSSC => language1.chi_square_stochastic_conformance(language2),
            DistanceMetric::HSC => language1.hellinger_stochastic_conformance(language2),
            DistanceMetric::UEMSC => language1.unit_earth_movers_stochastic_conformance(language2),
        }
    }
}

/// Constructs the k-th order Markovian abstraction for an arbitrary
/// EbiTraitQueriableStochasticLanguage. Currently supports
/// StochasticLabelledPetriNet (bounded / unbounded) and
/// FiniteStochasticLanguage. It can be extended later for further models
/// by adding additional downcast_mut branches in one place.
fn abstraction_of_slpn(
    mut pn: StochasticLabelledPetriNet,
    activity_key: &mut ActivityKey,
    k: usize,
    delta: Fraction,
) -> Result<MarkovianAbstraction> {
    if !pn.bounded()? {
        log::warn!(
            "Model is unbounded; falling back to random sampling. If a livelock is also present this may not terminate."
        );
        let sampled: FiniteStochasticLanguage = pn
            .sample(DEFAULT_SAMPLE_SIZE)
            .context("Sampling unbounded Petri net")?;
        let mut sampled = sampled;
        sampled.translate_using_activity_key(activity_key);
        compute_abstraction_for_log(&sampled, k)
    } else {
        pn.translate_using_activity_key(activity_key);
        compute_abstraction_for_petri_net(&pn, k, delta)
    }
}

/// This implements the calculation of the k-th order Stochastic Markovian abstraction
/// for the log (finite stochastic language). First it computes the k-th order multiset
/// markovian abstraction by adding the special start '+' and end '-' markers and then
/// computing the k-trimmed subtraces. Afterwards, the k-th order stochastic markovian
/// abstraction gets computed by normalizing the multiset markovian abstraction.
pub fn compute_abstraction_for_log(
    log: &dyn EbiTraitFiniteStochasticLanguage,
    k: usize,
) -> Result<MarkovianAbstraction> {
    // Validate k
    if k < 1 {
        return Err(anyhow::anyhow!(
            "k must be at least 1 for Markovian abstraction"
        ));
    }

    // Initialize f_l^k which stores the expected number of occurrences of each subtrace
    let mut f_l_k: FxHashMap<Arc<[String]>, Fraction> = FxHashMap::default();

    let mut activity_key_clone = log.activity_key().clone();
    // For each trace in the log with its probability
    for (trace, probability) in log.iter_traces_probabilities() {
        // Create a Vec<String> from the Vec<Activity>
        let string_trace: Vec<String> = trace.iter().map(|activity| activity.to_string()).collect();

        // Compute M_σ^k for this trace (k-th order multiset Markovian abstraction)
        let m_sigma_k = compute_multiset_abstraction_for_trace_with_key(
            &string_trace,
            k,
            &mut activity_key_clone,
        );

        // Add contribution to f_l^k
        for (subtrace, occurrences) in m_sigma_k {
            let occurrences_as_fraction = Fraction::from((occurrences, 1));
            // Create an owned contribution using explicit reference operations
            let contribution = {
                let p_ref: &Fraction = probability;
                let o_ref: &Fraction = &occurrences_as_fraction;
                p_ref * o_ref // This returns an owned FractionEnum
            };

            // Update the expected occurrence count in f_l^k
            f_l_k
                .entry(subtrace)
                .and_modify(|current| {
                    *current += &contribution; // Add this trace's contribution to existing count
                })
                .or_insert(contribution); // Insert new count if subtrace not seen before
        }
    }

    // Calculate the total sum for normalization
    let mut total = Fraction::from((0, 1));
    for value in f_l_k.values() {
        total += value;
    }

    // Normalize f_l^k to get m_l^k
    let mut abstraction = FxHashMap::default();
    for (subtrace, count) in f_l_k {
        let count_ref: &Fraction = &count;
        let total_ref: &Fraction = &total;
        abstraction.insert(subtrace, count_ref / total_ref);
    }

    Ok(MarkovianAbstraction { k, abstraction })
}

/// Compute the k-th order Markovian abstraction for a Petri net.
pub fn compute_abstraction_for_petri_net(
    petri_net: &StochasticLabelledPetriNet,
    k: usize,
    delta: Fraction,
) -> Result<MarkovianAbstraction> {
    if k < 1 {
        return Err(anyhow::anyhow!(
            "k must be at least 1 for Markovian abstraction"
        ));
    }

    // 0.5 Patch bounded livelocks by adding timeout escapes
    let patched_net = livelock_patch::patch_livelocks(petri_net, delta)?;

    // 1 Build embedded SNFA
    let mut snfa_raw = build_embedded_snfa(&patched_net)?;

    // 1.1 Remove tau transitions
    snfa_raw.remove_tau_transitions();

    // 2 Patch it
    let snfa = patch_snfa(&snfa_raw);

    // 3 Build matrix and solve for x
    let n = snfa.states.len();
    // Build sparse A = (I - Delta)^T
    let delta = build_delta(&snfa);
    let mut a_sparse: Vec<FxHashMap<usize, Fraction>> = vec![FxHashMap::default(); n];

    for i in 0..n {
        // Identity contribution
        a_sparse[i].insert(i, Fraction::from((1, 1)));

        // Subtract row i of Delta into column i of A
        for (&j, p) in &delta[i] {
            // A[j,i] = I[j,i] - Delta[i,j]
            a_sparse[j]
                .entry(i)
                .and_modify(|v| *v -= p)
                .or_insert_with(|| -p.clone());
        }
    }
    let mut b = vec![Fraction::from((0, 1)); n];
    b[snfa.initial] = Fraction::from((1, 1));
    let mut a_ref = a_sparse;
    let x = if n < 100 {
        // small matrices -> simpler hash map solver avoids conversion overhead
        solve_sparse_linear_system(&mut a_ref, b)?
    } else {
        solve_sparse_linear_system_optimized(&mut a_ref, b)?
    };

    // 4 Compute phi for each state
    // Compute phi on ID space then translate back to Strings using the shared ActivityKey
    let mut key = patched_net.activity_key().clone();
    let phi_ids = compute_phi_ids(&snfa, k, &mut key);

    let key_read = key.clone();
    // helper to translate a trace of u32 IDs back to Strings
    let translate = move |ids: &Arc<[u32]>| -> Arc<[String]> {
        let mut vec: Vec<String> = Vec::with_capacity(ids.len());
        for id in ids.iter() {
            let act = key_read.get_activity_by_id(*id as usize);
            vec.push(key_read.deprocess_activity(&act).to_string());
        }
        Arc::from(vec)
    };

    // 5 Compute f_l^k
    let mut f_l_k: FxHashMap<Arc<[String]>, Fraction> = FxHashMap::default();
    for (q, map) in phi_ids.iter().enumerate() {
        for (gamma_ids, phi_val) in map {
            let gamma = translate(gamma_ids);
            let contribution = &x[q] * phi_val;
            f_l_k
                .entry(gamma)
                .and_modify(|v| *v = &*v + &contribution)
                .or_insert(contribution.clone());
        }
    }

    // 6 Normalize
    let mut total = Fraction::from((0, 1));
    for v in f_l_k.values() {
        total += v;
    }
    let mut abstraction = FxHashMap::default();
    for (gamma, val) in f_l_k {
        abstraction.insert(gamma, &val / &total);
    }

    Ok(MarkovianAbstraction { k, abstraction })
}

/// Compute the multiset of k-trimmed subtraces for a given trace
fn compute_multiset_abstraction_for_trace_with_key(
    trace: &[String],
    k: usize,
    key: &mut ActivityKey,
) -> FxHashMap<Arc<[String]>, usize> {
    // Convert labels to IDs and add start/end markers in ID space
    let mut augmented_ids: Vec<u32> = Vec::with_capacity(trace.len() + 2);
    {
        let act = key.process_activity("+");
        augmented_ids.push(key.get_id_from_activity(act) as u32);
    }
    for lbl in trace {
        let act = key.process_activity(lbl);
        augmented_ids.push(key.get_id_from_activity(act) as u32);
    }
    {
        let act = key.process_activity("-");
        augmented_ids.push(key.get_id_from_activity(act) as u32);
    }

    // Compute multiset over IDs
    let id_multiset = compute_multiset_k_trimmed_subtraces_iterative_ids(&augmented_ids, k);

    // Reconstruct String subtrace keys via the reverse map
    let mut result: FxHashMap<Arc<[String]>, usize> = FxHashMap::default();
    result.reserve(id_multiset.len());
    for (sub_ids, cnt) in id_multiset {
        let strings: Vec<String> = sub_ids
            .iter()
            .map(|&id| {
                let act = key.get_activity_by_id(id as usize);
                key.deprocess_activity(&act).to_string()
            })
            .collect();
        result.insert(Arc::from(strings), cnt);
    }
    result
}

/// Operates on a slice of `u32` activity IDs and returns a
/// multiset keyed by `Arc<[u32]>`. This avoids heap traffic except when a new
/// *unique* subtrace is inserted into the map.
fn compute_multiset_k_trimmed_subtraces_iterative_ids(
    trace: &[u32],
    k: usize,
) -> FxHashMap<Arc<[u32]>, usize> {
    let mut result: FxHashMap<Arc<[u32]>, usize> = FxHashMap::default();

    if trace.len() <= k {
        result.insert(Arc::from(trace.to_owned()), 1);
        return result;
    }

    let mut ring: Vec<u32> = Vec::with_capacity(k);
    ring.extend_from_slice(&trace[..k]);
    let mut head: usize = 0; // index of the oldest element

    // Reusable key construction
    let mut tmp: Vec<u32> = Vec::with_capacity(k);
    // Helper to build an Arc key representing the current window
    let make_key = |ring: &Vec<u32>, head: usize, tmp: &mut Vec<u32>| -> Arc<[u32]> {
        tmp.clear();
        tmp.extend_from_slice(&ring[head..]);
        tmp.extend_from_slice(&ring[..head]);
        Arc::from(tmp.clone().into_boxed_slice())
    };

    // Insert first window
    result.insert(make_key(&ring, head, &mut tmp), 1);

    // Slide through the trace
    for &next_id in &trace[k..] {
        ring[head] = next_id; // overwrite the oldest
        head = (head + 1) % k; // advance head
        let key = make_key(&ring, head, &mut tmp);
        *result.entry(key).or_insert(0) += 1;
    }

    result
}

// Embedded-SNFA construction
fn build_embedded_snfa(net: &StochasticLabelledPetriNet) -> Result<Snfa> {
    use std::collections::VecDeque;

    // Create a new SNFA without the default initial state
    let mut snfa = Snfa::new();
    snfa.states.clear();

    // Reachability exploration queue
    let mut marking2idx: FxHashMap<LPNMarking, usize> = FxHashMap::default();
    let mut queue: VecDeque<LPNMarking> = VecDeque::new();

    // Insert the initial state of the Petri net
    let initial_marking = net
        .get_initial_state()
        .context("SLPN has no initial state")?;
    marking2idx.insert(initial_marking.clone(), 0);
    snfa.states.push(SnfaState {
        transitions: vec![],
        p_final: Fraction::from((0, 1)),
    });
    queue.push_back(initial_marking);

    while let Some(state) = queue.pop_front() {
        let src_idx = *marking2idx.get(&state).unwrap();

        // Collect enabled transitions and the total enabled weight in this state
        let enabled_transitions = net.get_enabled_transitions(&state);
        if enabled_transitions.is_empty() {
            // Deadlock state -> make it final with probability 1
            snfa.states[src_idx].p_final = Fraction::from((1, 1));
            continue;
        }

        let weight_sum = net.get_total_weight_of_enabled_transitions(&state)?;

        for &t in &enabled_transitions {
            let w = net.get_transition_weight(&state, t).clone();
            let prob = &w / &weight_sum;

            // Fire transition (creates a successor state)
            let mut next_state = state.clone();
            net.execute_transition(&mut next_state, t)?;

            // Map / enqueue successor
            let tgt_idx = *marking2idx.entry(next_state.clone()).or_insert_with(|| {
                let idx = snfa.states.len();
                snfa.states.push(SnfaState {
                    transitions: vec![],
                    p_final: Fraction::from((0, 1)),
                });
                queue.push_back(next_state.clone());
                idx
            });

            // Transition label (empty string for tau transition)
            let label = if let Some(a) = net.get_transition_label(t) {
                a.to_string()
            } else {
                "".to_string()
            };

            snfa.states[src_idx].transitions.push(SnfaTransition {
                target: tgt_idx,
                label,
                probability: prob,
            });
        }
    }

    // The first discovered marking is the initial state of the SNFA
    snfa.initial = 0;
    Ok(snfa)
}

// Patch the SNFA by adding + and - transitions
fn patch_snfa(snfa: &Snfa) -> Snfa {
    // Create a new vector of states to avoid borrow conflicts
    let mut states = snfa.states.clone();

    let q_plus = states.len();
    let q_minus = states.len() + 1;

    // Redirect original finals to q_minus via '-'
    for i in 0..states.len() {
        let s = &mut states[i];
        if !s.p_final.is_zero() {
            // Copy the existing p_final value
            let final_prob = s.p_final.clone();
            // Reset p_final to zero
            s.p_final = Fraction::from((0, 1));
            // Add a "-" transition with the original final probability
            s.transitions.push(SnfaTransition {
                target: q_minus,
                label: "-".to_string(),
                probability: final_prob,
            });
        }
    }

    // q_plus with + transition to original initial state
    states.push(SnfaState {
        transitions: vec![SnfaTransition {
            target: snfa.initial,
            label: "+".to_string(),
            probability: Fraction::from((1, 1)),
        }],
        p_final: Fraction::from((0, 1)),
    });

    // q_minus absorbing final state
    states.push(SnfaState {
        transitions: vec![],
        p_final: Fraction::from((1, 1)),
    });

    // Return the patched automaton
    Snfa {
        states,
        initial: q_plus,
    }
}

// Build sparse transition matrix
fn build_delta(snfa: &Snfa) -> Vec<FxHashMap<usize, Fraction>> {
    let n = snfa.states.len();
    let mut delta = vec![FxHashMap::<usize, Fraction>::default(); n];

    for (i, state) in snfa.states.iter().enumerate() {
        for t in &state.transitions {
            delta[i]
                .entry(t.target)
                .and_modify(|v| *v += &t.probability)
                .or_insert(t.probability.clone());
        }
    }
    delta
}

/// Sparse exact Gaussian elimination where each row is a **sorted** Vec<(usize, Fraction)>.
/// We convert the incoming FxHashMap representation once and then run a cache friendly merge based
/// elimination (A := A - factor * pivot).
fn solve_sparse_linear_system_optimized(
    a_hash: &mut [FxHashMap<usize, Fraction>],
    mut b: Vec<Fraction>,
) -> Result<Vec<Fraction>> {
    // Convert the incoming FxHashMap rows into sorted vec rows
    fn to_vec_rows(a: &mut [FxHashMap<usize, Fraction>]) -> Vec<Vec<(usize, Fraction)>> {
        a.iter_mut()
            .map(|row| {
                let mut v: Vec<(usize, Fraction)> = row.drain().collect();
                v.sort_by_key(|(c, _)| *c);
                v
            })
            .collect()
    }

    // Binary search helper
    fn find_col(row: &[(usize, Fraction)], col: usize) -> Option<usize> {
        row.binary_search_by_key(&col, |(c, _)| *c).ok()
    }

    /// target = target - factor * pivot   (skips column i)
    fn saxpy_row(
        target: &mut Vec<(usize, Fraction)>,
        i: usize,
        pivot: &[(usize, Fraction)],
        factor: &Fraction,
    ) {
        let mut out = Vec::with_capacity(target.len() + pivot.len());
        let mut t = 0;
        let mut p = 0;
        while t < target.len() || p < pivot.len() {
            match (target.get(t), pivot.get(p)) {
                (Some(&(c_t, ref v_t)), Some(&(c_p, ref v_p))) if c_t == c_p => {
                    if c_t != i {
                        let mut new_val = v_t.clone();
                        new_val -= &(factor * v_p);
                        if !new_val.is_zero() {
                            out.push((c_t, new_val));
                        }
                    }
                    t += 1;
                    p += 1;
                }
                (Some(&(c_t, ref v_t)), Some(&(c_p, _))) if c_t < c_p => {
                    if c_t != i {
                        out.push((c_t, v_t.clone()));
                    }
                    t += 1;
                }
                (Some(&(c_t, ref v_t)), None) => {
                    if c_t != i {
                        out.push((c_t, v_t.clone()));
                    }
                    t += 1;
                }
                (None, Some(&(c_p, ref v_p))) | (Some(&(_, _)), Some(&(c_p, ref v_p))) => {
                    // c_p < c_t  OR target exhausted
                    if c_p != i {
                        let new_val = -(factor * v_p);
                        if !new_val.is_zero() {
                            out.push((c_p, new_val));
                        }
                    }
                    p += 1;
                }
                (None, None) => unreachable!(),
            }
        }
        out.shrink_to_fit();
        *target = out;
    }

    // Convert matrix once
    let mut a: Vec<Vec<(usize, Fraction)>> = to_vec_rows(a_hash);
    let n = b.len();

    for i in 0..n {
        // 1. Pivot search (find first row r >= i with non zero col i)
        let pivot = (i..n)
            .find(|&r| find_col(&a[r], i).map_or(false, |idx| !a[r][idx].1.is_zero()))
            .ok_or_else(|| anyhow::anyhow!("Matrix is singular"))?;

        if pivot != i {
            a.swap(i, pivot);
            b.swap(i, pivot);
        }

        // 2. Normalize pivot row so diagonal becomes 1
        let diag_idx = find_col(&a[i], i).expect("pivot exists");
        let inv = a[i][diag_idx].1.clone().recip();
        for &mut (_, ref mut v) in &mut a[i] {
            *v *= &inv;
        }
        b[i] *= &inv;

        // 3. Split mutable slice around pivot row
        let (left, rest) = a.split_at_mut(i);
        let (pivot_row, below) = rest.split_first_mut().expect("pivot row");
        let pivot_ref: &[(usize, Fraction)] = &pivot_row[..];
        let pivot_b = b[i].clone();

        // Thread local updates for RHS
        let mut updates: Vec<(usize, Fraction)> = Vec::with_capacity(left.len() + below.len());

        // rows above
        updates.extend(
            left.par_iter_mut()
                .enumerate()
                .filter_map(|(r, row)| {
                    find_col(row, i)
                        .map(|idx| {
                            let factor = row[idx].1.clone();
                            row[idx].1 = Fraction::zero(); // lazy zeroing avoids shift
                            if factor.is_zero() {
                                None
                            } else {
                                saxpy_row(row, i, pivot_ref, &factor);
                                Some((r, factor))
                            }
                        })
                        .flatten()
                })
                .collect::<Vec<_>>(),
        );

        // rows below
        updates.extend(
            below
                .par_iter_mut()
                .enumerate()
                .filter_map(|(off, row)| {
                    let r = i + 1 + off;
                    find_col(row, i)
                        .map(|idx| {
                            let factor = row[idx].1.clone();
                            row[idx].1 = Fraction::zero();
                            if factor.is_zero() {
                                None
                            } else {
                                saxpy_row(row, i, pivot_ref, &factor);
                                Some((r, factor))
                            }
                        })
                        .flatten()
                })
                .collect::<Vec<_>>(),
        );

        // Apply RHS updates sequentially
        for (r, factor) in updates {
            b[r] -= &factor * &pivot_b;
        }
    }

    Ok(b)
}

/// Naive sparse Gaussian elimination for Fraction matrices represented as Vec<FxHashMap<usize, Fraction>>
fn solve_sparse_linear_system(
    a: &mut [FxHashMap<usize, Fraction>],
    mut b: Vec<Fraction>,
) -> Result<Vec<Fraction>> {
    let n = b.len();
    // Forward elimination
    for i in 0..n {
        // Find pivot
        let mut pivot = i;
        while pivot < n && a[pivot].get(&i).map_or(true, |v| v.is_zero()) {
            pivot += 1;
        }
        if pivot == n {
            return Err(anyhow::anyhow!("Matrix is singular"));
        }
        if pivot != i {
            a.swap(i, pivot);
            b.swap(i, pivot);
        }
        // Normalize row i
        let inv = a[i].get(&i).unwrap().clone().recip();
        // scale row i
        let keys: Vec<usize> = a[i].keys().cloned().collect();
        for j in keys {
            if let Some(val) = a[i].get_mut(&j) {
                *val *= &inv;
            }
        }
        b[i] *= &inv;
        let pivot_b = b[i].clone();
        // Eliminate other rows
        for r in 0..n {
            if r == i {
                continue;
            }
            if let Some(factor_val) = a[r].get(&i).cloned() {
                if !factor_val.is_zero() {
                    // subtract factor * row_i from row_r
                    let keys: Vec<(usize, Fraction)> =
                        a[i].iter().map(|(k, v)| (*k, v.clone())).collect();
                    for (c, val_i) in keys {
                        let product = &factor_val * &val_i;
                        let entry = a[r].entry(c).or_insert_with(Fraction::zero);
                        *entry = &*entry - &product;
                        if entry.is_zero() {
                            a[r].remove(&c);
                        }
                    }
                    b[r] -= &factor_val * &pivot_b;
                }
            }
        }
    }
    Ok(b.to_vec())
}

/// Compute Phi maps using integer label IDs for efficiency.
/// Returns Vec indexed by start state q, each mapping Arc<[u32]> -> Fraction.
fn compute_phi_ids(
    snfa: &Snfa,
    k: usize,
    key: &mut ActivityKey,
) -> Vec<FxHashMap<Arc<[u32]>, Fraction>> {
    let n = snfa.states.len();
    // Compute numeric IDs for "+" and "-" for quick comparisons
    let plus_act = key.process_activity("+");
    let id_plus = key.get_id_from_activity(plus_act) as u32;
    let minus_act = key.process_activity("-");
    let id_minus = key.get_id_from_activity(minus_act) as u32;
    let mut phi: Vec<FxHashMap<Arc<[u32]>, Fraction>> = vec![FxHashMap::default(); n];

    // Memoisation cache mapping (state, remaining_len) -> suffix map
    // Suffix map: subtrace (starting at the first symbol that leaves the current state) -> probability
    type SuffixMap = FxHashMap<Arc<[u32]>, Fraction>;
    // Cache stores Arc<SuffixMap> so a hit clones only the pointer
    let mut cache: FxHashMap<(usize, usize), Arc<SuffixMap>> = FxHashMap::default();

    // Recursively collect every suffix of length <= remaining that can be produced
    // from state_idx, together with its probability relative to the current state.
    // The probabilities stored in the map do not include any prefix probability.
    // This allows us to multiply the prefix probability later and still reuse the suffixes.
    fn collect_suffixes(
        state_idx: usize,
        remaining: usize,
        snfa: &Snfa,
        key: &mut ActivityKey,
        cache: &mut FxHashMap<(usize, usize), Arc<SuffixMap>>,
    ) -> Arc<SuffixMap> {
        // Fast path -> already computed
        if let Some(m) = cache.get(&(state_idx, remaining)) {
            return m.clone();
        }

        let mut result: SuffixMap = FxHashMap::default();

        if remaining == 0 {
            // No more symbols allowed -> empty suffix with probability 1
            result.insert(
                Arc::<[u32]>::from(Vec::<u32>::new()),
                Fraction::from((1, 1)),
            );
        } else {
            for tr in &snfa.states[state_idx].transitions {
                let label = tr.label.clone();
                let prob = tr.probability.clone();

                if label == "-" {
                    // End marker -> stop exploring beyond this symbol
                    let id = key.process_activity(&label);
                    let arc = Arc::from(vec![key.get_id_from_activity(id) as u32]);
                    result
                        .entry(arc)
                        .and_modify(|v| *v += &prob)
                        .or_insert(prob);
                } else {
                    // Recurse to target with one fewer remaining symbol
                    let child_map = collect_suffixes(tr.target, remaining - 1, snfa, key, cache);
                    for (suf, w) in child_map.as_ref() {
                        let mut vec = Vec::with_capacity(1 + suf.len());
                        let id = key.process_activity(&label);
                        vec.push(key.get_id_from_activity(id) as u32);
                        vec.extend_from_slice(&suf[..]);
                        let arc = Arc::from(vec);
                        let weight = &prob * w;
                        result
                            .entry(arc)
                            .and_modify(|v| *v += &weight)
                            .or_insert(weight);
                    }
                }
            }
        }

        let arc_result = Arc::new(result);
        cache.insert((state_idx, remaining), arc_result.clone());
        arc_result
    }

    // Build phi for every potential start state
    for q in 0..n {
        let suffixes_arc = collect_suffixes(q, k, snfa, key, &mut cache);
        let suffixes = suffixes_arc.as_ref();

        for (trace_arc, prob_suffix) in suffixes.iter() {
            let trace_slice = trace_arc.as_ref();

            // Valid subtraces follow exactly these conditions
            let is_exact_k = trace_slice.len() == k;
            let is_short_with_end = !trace_slice.is_empty()
                && trace_slice.len() <= k
                && trace_slice.last().unwrap() == &id_minus
                && trace_slice.first().unwrap() == &id_plus;

            if is_exact_k || is_short_with_end {
                phi[q].insert(trace_arc.clone(), prob_suffix.clone());
            }
        }
    }

    phi
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::ebi_traits::ebi_trait_finite_stochastic_language::EbiTraitFiniteStochasticLanguage;
    use ebi_objects::{
        ActivityKeyTranslator, HasActivityKey, IntoRefTraceProbabilityIterator,
        TranslateActivityKey,
        ebi_objects::{
            event_log::EventLog, finite_stochastic_language::FiniteStochasticLanguage,
            stochastic_labelled_petri_net::StochasticLabelledPetriNet,
        },
    };
    use std::fs;

    #[test]
    fn test_compute_abstraction_for_example_log() {
        let file_content =
            fs::read_to_string("testfiles/simple_log_markovian_abstraction.xes").unwrap();
        let event_log = file_content.parse::<EventLog>().unwrap();
        let finite_lang: FiniteStochasticLanguage = Into::into(event_log);

        // Compute abstraction with k=2 for example log [<a,b>^{5}, <a,a,b,c>^{2}, <a,a,c,b>^{1}]
        let abstraction = compute_abstraction_for_log(&finite_lang, 2).unwrap();

        println!("\nComputed abstraction for example log with k=2:");

        assert_eq!(
            abstraction.abstraction.len(),
            8,
            "Should be exactly 8 entries"
        );

        // map for checking
        let mut check: std::collections::HashMap<String, Fraction> =
            std::collections::HashMap::default();
        for (subtrace, prob) in abstraction.abstraction.iter() {
            let key = subtrace
                .iter()
                .map(|s| s.as_str())
                .collect::<Vec<_>>()
                .join(",");
            println!("{:<12} : {}", key, prob);
            check.insert(key, prob.clone());
        }

        assert_eq!(check["+,ac0"], Fraction::from((4, 15)));
        assert_eq!(check["ac0,ac0"], Fraction::from((1, 10)));

        let pair1 = [Fraction::from((7, 30)), Fraction::from((1, 30))];
        let pair2 = [Fraction::from((1, 5)), Fraction::from((1, 15))];
        let pair3 = [Fraction::from((1, 15)), Fraction::from((1, 30))];

        // Helper to check that a key has one of two expected values and that the other value is on the other key
        fn assert_pair(
            check: &std::collections::HashMap<String, Fraction>,
            k1: &str,
            k2: &str,
            exp: [Fraction; 2],
        ) {
            let v1 = check.get(k1).expect("missing key");
            let v2 = check.get(k2).expect("missing key");
            assert!(
                (v1 == &exp[0] && v2 == &exp[1]) || (v1 == &exp[1] && v2 == &exp[0]),
                "Pair {{ {}, {} }} has unexpected values {{ {}, {} }}",
                k1,
                k2,
                v1,
                v2
            );
        }

        assert_pair(&check, "ac0,ac1", "ac0,ac2", pair1);
        assert_pair(&check, "ac1,-", "ac2,-", pair2);
        assert_pair(&check, "ac1,ac2", "ac2,ac1", pair3);
    }

    #[test]
    fn test_compute_abstraction_for_petri_net() {
        let file_content =
            fs::read_to_string("testfiles/simple_markovian_abstraction.slpn").unwrap();
        let petri_net = file_content.parse::<StochasticLabelledPetriNet>().unwrap();

        // Check that k < 1 is rejected (k = 0)
        let result = compute_abstraction_for_petri_net(&petri_net, 0, Fraction::from((1, 1000)));
        assert!(result.is_err(), "Should reject k < 1");

        // Compute abstraction with k=2
        let abstraction =
            compute_abstraction_for_petri_net(&petri_net, 2, Fraction::from((1, 1000))).unwrap();
        let language_of_model: FiniteStochasticLanguage = abstraction.clone().into();
        let language1: Box<dyn EbiTraitFiniteStochasticLanguage> = Box::new(language_of_model);

        // Check that probabilities sum to 1
        let mut total = Fraction::from((0, 1));
        for (_, probability1) in language1.iter_traces_probabilities() {
            total += probability1;
        }
        assert_eq!(
            total,
            Fraction::from((1, 1)),
            "Total probability should be 1"
        );

        // Expected internal traces and probabilities
        let fin2 = fs::read_to_string("testfiles/markovian.slang").unwrap();
        let expected_traces = fin2.parse::<FiniteStochasticLanguage>().unwrap();
        let mut activity_key1 = language1.activity_key().clone();
        let translator =
            ActivityKeyTranslator::new(&expected_traces.activity_key(), &mut activity_key1);

        for (trace1, probability1) in language1.iter_traces_probabilities() {
            for (trace2, probability2) in expected_traces.iter_traces_probabilities() {
                if trace1 == &translator.translate_trace(trace2) {
                    assert_eq!(
                        probability2, probability1,
                        "Probability mismatch for trace {:?}: expected {}, got {}",
                        trace1, probability2, probability1
                    );
                }
            }
        }
    }

    #[test]
    fn test_markovian_conformance_uemsc_log_vs_petri_net() {
        // Load the example log and convert to finite stochastic language
        let file_content =
            fs::read_to_string("testfiles/simple_log_markovian_abstraction.xes").unwrap();
        let event_log = file_content.parse::<EventLog>().unwrap();
        let mut finite_lang: FiniteStochasticLanguage = Into::into(event_log);

        // Load the Petri net model
        let file_content =
            fs::read_to_string("testfiles/simple_markovian_abstraction.slpn").unwrap();
        let mut petri_net = file_content.parse::<StochasticLabelledPetriNet>().unwrap();

        // Ensure both share a common ActivityKey to avoid nondeterministic mappings
        petri_net.translate_using_activity_key(finite_lang.activity_key_mut());

        // Compute the m^2-uEMSC distance (k = 2)
        let conformance = (&finite_lang as &dyn EbiTraitFiniteStochasticLanguage)
            .markovian_conformance(petri_net, 2, DistanceMetric::UEMSC)
            .unwrap();
        assert_eq!(conformance, Fraction::from((4, 5))); // Expect 4/5
    }
}
