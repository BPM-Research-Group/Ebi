use std::any::Any;
use std::collections::HashMap;
use rayon::prelude::*;

use std::sync::Arc;

use anyhow::{Context, Result};
use ebi_arithmetic::{Fraction, Recip, Sqrt, Zero};
use ebi_arithmetic::exact::MaybeExact;

use ebi_objects::{
    ActivityKey, FiniteStochasticLanguage, HasActivityKey, StochasticLabelledPetriNet,
    TranslateActivityKey,
};

use crate::{
    ebi_file_handlers::stochastic_nondeterministic_finite_automaton::{
        StochasticNondeterministicFiniteAutomaton as Snfa,
        State as SnfaState,
        Transition as SnfaTransition,
    },
    ebi_traits::{
        ebi_trait_finite_stochastic_language::EbiTraitFiniteStochasticLanguage,
        ebi_trait_queriable_stochastic_language::EbiTraitQueriableStochasticLanguage,
    },
    semantics::{
        labelled_petri_net_semantics::LPNMarking,
        semantics::Semantics,
    },
    stochastic_semantics::stochastic_semantics::StochasticSemantics,
    techniques::bounded::Bounded,
    techniques::livelock_patch,
    techniques::sample::Sampler,
};

/// Supported distance metrics for Markovian abstraction comparison.
/// The list can be extended later – variants that are not yet implemented
/// will return an `anyhow::Error`.
#[derive(Clone, Copy, Debug)]
pub enum DistanceMetric {
    Uemsc,
    ScaledL2,
    JensenShannon,
    Hellinger,
}

/// Default number of traces to sample when falling back to simulation for
/// unbounded Petri nets. (maybe optional parameter later)
const DEFAULT_SAMPLE_SIZE: usize = 10_000;

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
        language2: Box<dyn EbiTraitQueriableStochasticLanguage>,
        k: usize,
        metric: DistanceMetric,
        delta: Fraction,
    ) -> Result<Fraction>;
}

/// Represents a k-th order Markovian abstraction of a stochastic language
pub struct MarkovianAbstraction {
    /// The order of the abstraction (k)
    pub k: usize,
    /// The mapping from subtraces to normalized frequencies
    /// Uses Arc<[String]> to reduce memory usage through shared ownership
    pub abstraction: HashMap<Arc<[String]>, Fraction>,
}

impl StochasticMarkovianAbstraction for dyn EbiTraitFiniteStochasticLanguage {
    fn markovian_conformance(
        &self,
        language2: Box<dyn EbiTraitQueriableStochasticLanguage>,
        k: usize,
        metric: DistanceMetric,
        delta: Fraction,
    ) -> Result<Fraction> {
        // Validate k
        if k < 1 {
            return Err(anyhow::anyhow!("k must be at least 1"));
        }

        // Step 1: Compute abstraction for the first language
        let abstraction1 = compute_abstraction_for_log(self, k)
            .context("Computing abstraction for first language (finite log)")?;

        // Step 2: Compute abstraction for the second language
        let mut shared_key = self.activity_key().clone();
        let mut language2 = language2;
        let abstraction2 = abstraction_of(language2.as_mut(), &mut shared_key, k, delta)
            .context("Computing abstraction for second language")?;

        // Step 3: Compute the conformance between the abstractions depending on the metric
        match metric {
            DistanceMetric::Uemsc => {
                let uemsc = compute_uemsc_distance(&abstraction1, &abstraction2)?;
                let one = Fraction::from((1, 1));
                Ok(&one - &uemsc)
            }
            DistanceMetric::ScaledL2 => {
                let l2 = compute_scaled_l2_distance(&abstraction1, &abstraction2)?;
                let one = Fraction::from((1, 1));
                Ok(&one - &l2)
            }
            DistanceMetric::JensenShannon => {
                let js = compute_jensen_shannon_distance(&abstraction1, &abstraction2)?;
                let one = Fraction::from((1, 1));
                Ok(&one - &js)
            }
            DistanceMetric::Hellinger => {
                let h = compute_hellinger_distance(&abstraction1, &abstraction2)?;
                let one = Fraction::from((1, 1));
                Ok(&one - &h)
            }

        }
    }
}

/// Constructs the k-th order Markovian abstraction for an arbitrary
/// EbiTraitQueriableStochasticLanguage. Currently supports
/// StochasticLabelledPetriNet (bounded / unbounded) and
/// FiniteStochasticLanguage. It can be extended later for further models
/// by adding additional downcast_mut branches in one place.
fn abstraction_of(
    lang: &mut dyn EbiTraitQueriableStochasticLanguage,
    activity_key: &mut ActivityKey,
    k: usize,
    delta: Fraction,
) -> Result<MarkovianAbstraction> {
    // Case 1: Stochastic labelled Petri net
    if let Some(pn) = (lang as &mut dyn Any).downcast_mut::<StochasticLabelledPetriNet>() {
        if !pn.bounded()? {
            log::warn!("Model is unbounded; falling back to random sampling. If a livelock is also present this may not terminate.");
            let sampled: FiniteStochasticLanguage = pn
                .sample(DEFAULT_SAMPLE_SIZE)
                .context("Sampling unbounded Petri net")?;
            let mut sampled = sampled;
            sampled.translate_using_activity_key(activity_key);
            compute_abstraction_for_log(&sampled, k)
        } else {
            pn.translate_using_activity_key(activity_key);
            compute_abstraction_for_petri_net(pn, k, delta)
        }
    // Case 2: Finite stochastic language (log)
    } else if let Some(flog) = (lang as &mut dyn Any).downcast_mut::<FiniteStochasticLanguage>() {
        flog.translate_using_activity_key(activity_key);
        compute_abstraction_for_log(flog, k)
    } else {
        Err(anyhow::anyhow!("Unsupported type for stochastic Markovian abstraction"))
    }
}

/// Compute the uEMSC distance between two abstractions.
fn compute_uemsc_distance(
    abstraction1: &MarkovianAbstraction,
    abstraction2: &MarkovianAbstraction,
) -> Result<Fraction> {
    let mut positive_diff = Fraction::from((0, 1));
    let zero = Fraction::from((0, 1));

    for (gamma, p1) in &abstraction1.abstraction {
        let p2 = abstraction2
            .abstraction
            .get(gamma)
            .unwrap_or(&zero);

        if p1 > p2 {
            let diff = &*p1 - &*p2;
            positive_diff += diff;
        }
    }

    Ok(positive_diff)
}

/// Compute the Jensen–Shannon distance between two abstractions.
fn compute_jensen_shannon_distance(
    abstraction1: &MarkovianAbstraction,
    abstraction2: &MarkovianAbstraction,
) -> Result<Fraction> {

    // Helper: convert an exact or approximate Fraction to f64 quickly.
    // For Exact fractions we compute n / d in double precision which is 
    // good enough for a Jensen–Shannon distance that is approximated to 1 e-15.
    #[inline]
    fn frac_to_f64(fr: &Fraction) -> f64 {
        if fr.is_zero() {
            return 0.0;
        }

        // Try the fast path for approximate fractions first
        if let Ok(v) = fr.clone().approx() {
            return v;
        }

        // Exact fraction -> fall back to converting via string or direct rational conversion
        // This is safer for very large exact fractions that might overflow
        if let Ok(exact) = fr.exact_ref() {
            // Use f64 approximation of the rational
            let n_str = exact.numerator_ref().to_string();
            let d_str = exact.denominator_ref().to_string();
            if let (Ok(n), Ok(d)) = (n_str.parse::<f64>(), d_str.parse::<f64>()) {
                return n / d;
            }
        }
        0.0 // Should not happen, but keeps the compiler happy
    }

    #[inline]
    fn n_log_n(x: f64) -> f64 {
        if x <= 0.0 { 0.0 } else { x * x.log2() }
    }

    let mut h = 0.0;

    // Keys from abstraction1
    for (gamma, p1_frac) in &abstraction1.abstraction {
        let p1 = frac_to_f64(p1_frac);
        let p2 = abstraction2
            .abstraction
            .get(gamma)
            .map(|f| frac_to_f64(f))
            .unwrap_or(0.0);
        if p1 + p2 == 0.0 {
            continue;
        }
        let pq = p1 + p2;
        h += n_log_n(p1) + n_log_n(p2) - n_log_n(pq) + pq;
    }

    // Keys only in abstraction2
    for (gamma, p2_frac) in &abstraction2.abstraction {
        if abstraction1.abstraction.contains_key(gamma) {
            continue;
        }
        let p2 = frac_to_f64(p2_frac);
        if p2 == 0.0 {
            continue;
        }
        h += n_log_n(0.0) + n_log_n(p2) - n_log_n(p2) + p2; // pq == p2
    }

    h *= 0.5; // scale by 1/2

    // Identical abstractions or tiny difference
    if h <= 2.220_446_049_250_313e-16 {
        return Ok(Fraction::from((0, 1)));
    }

    let d = h.sqrt();
    const DEN: u64 = 1_000_000_000_000_000; // 1e15
    let num = (d * DEN as f64).round() as u64;
    Ok(Fraction::from((num, DEN)))
}

/// Compute the Hellinger distance between two abstractions.
fn compute_hellinger_distance(
    abstraction1: &MarkovianAbstraction,
    abstraction2: &MarkovianAbstraction,
) -> Result<Fraction> {
    let zero = Fraction::from((0, 1));
    let mut bc = Fraction::from((0, 1));

    for (gamma, p1) in &abstraction1.abstraction {
        let p2 = abstraction2.abstraction.get(gamma).unwrap_or(&zero);
        // sqrt(p1 * p2)
        let prod = p1 * p2;
        if !prod.is_zero() {
            bc += prod.approx_abs_sqrt(4);
        }
    }
    for (gamma, _) in &abstraction2.abstraction {
        if abstraction1.abstraction.contains_key(gamma) {
            continue;
        }
        // p1 = 0 for these keys -> contribution to BC is zero -> nothing to add
    }
    let one = Fraction::from((1, 1));
    let h_sq = &one - &bc;
    Ok(h_sq.approx_abs_sqrt(4))
}

/// Compute the scaled L2 distance between two abstractions.
fn compute_scaled_l2_distance(
    abstraction1: &MarkovianAbstraction,
    abstraction2: &MarkovianAbstraction,
) -> Result<Fraction> {
    let mut sum_squared = Fraction::from((0, 1));
    let zero = Fraction::from((0, 1));
    
    // Iterate over union of keys; first pass abstraction1
    for (gamma, p1) in &abstraction1.abstraction {
        let p2 = abstraction2.abstraction.get(gamma).unwrap_or(&zero);
        let diff = p1 - p2;
        sum_squared += &diff * &diff;
    }
    
    // Add keys only in abstraction2
    for (gamma, p2) in &abstraction2.abstraction {
        if !abstraction1.abstraction.contains_key(gamma) {
            sum_squared += p2 * p2;
        }
    }
    
    // Take square root and divide by sqrt(2) to get range [0,1]
    let l2_norm = sum_squared.approx_abs_sqrt(15);
    let sqrt_2 = Fraction::from((2, 1)).approx_abs_sqrt(15);
    Ok(&l2_norm / &sqrt_2)
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
        return Err(anyhow::anyhow!("k must be at least 1 for Markovian abstraction"));
    }

    // Initialize f_l^k which stores the expected number of occurrences of each subtrace
    let mut f_l_k: HashMap<Arc<[String]>, Fraction> = HashMap::default();

    let mut activity_key_clone = log.activity_key().clone();
    // For each trace in the log with its probability
    for (trace, probability) in log.iter_trace_probability() {
        // Create a Vec<String> from the Vec<Activity>
        let string_trace: Vec<String> = trace.iter()
            .map(|activity| activity.to_string())
            .collect();

        // Compute M_σ^k for this trace (k-th order multiset Markovian abstraction)
        let m_sigma_k = compute_multiset_abstraction_for_trace_with_key(&string_trace, k, &mut activity_key_clone);

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
            f_l_k.entry(subtrace)
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
    let mut abstraction = HashMap::default();
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
        return Err(anyhow::anyhow!("k must be at least 1 for Markovian abstraction"));
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
    let mut a_sparse: Vec<HashMap<usize, Fraction>> = vec![HashMap::default(); n];

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
    let x = if n < 100{
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
    let mut f_l_k: HashMap<Arc<[String]>, Fraction> = HashMap::default();
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
    let mut abstraction = HashMap::default();
    for (gamma, val) in f_l_k {
        abstraction.insert(gamma, &val / &total);
    }

    Ok(MarkovianAbstraction { k, abstraction })
}

/// Compute the multiset of k-trimmed subtraces for a given trace
fn compute_multiset_abstraction_for_trace_with_key(trace: &[String], k: usize, key: &mut ActivityKey) -> HashMap<Arc<[String]>, usize> {
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
    let mut result: HashMap<Arc<[String]>, usize> = HashMap::default();
    result.reserve(id_multiset.len());
    for (sub_ids, cnt) in id_multiset {
        let strings: Vec<String> = sub_ids.iter().map(|&id| {
            let act = key.get_activity_by_id(id as usize);
            key.deprocess_activity(&act).to_string()
        }).collect();
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
) -> HashMap<Arc<[u32]>, usize> {
    let mut result: HashMap<Arc<[u32]>, usize> = HashMap::default();

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
        ring[head] = next_id;           // overwrite the oldest
        head = (head + 1) % k;          // advance head
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
    let mut marking2idx: HashMap<LPNMarking, usize> = HashMap::default();
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
                probability: final_prob
            });
        }
    }

    // q_plus with + transition to original initial state
    states.push(SnfaState { 
        transitions: vec![SnfaTransition { 
            target: snfa.initial, 
            label: "+".to_string(), 
            probability: Fraction::from((1, 1)) 
        }], 
        p_final: Fraction::from((0, 1))
    });

    // q_minus absorbing final state
    states.push(SnfaState { 
        transitions: vec![], 
        p_final: Fraction::from((1, 1))
    });

    // Return the patched automaton
    Snfa {
        states,
        initial: q_plus,
    }
}

// Build sparse transition matrix
fn build_delta(snfa: &Snfa) -> Vec<HashMap<usize, Fraction>> {
    let n = snfa.states.len();
    let mut delta = vec![HashMap::<usize, Fraction>::default(); n];

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
/// We convert the incoming HashMap representation once and then run a cache friendly merge based
/// elimination (A := A - factor * pivot).
fn solve_sparse_linear_system_optimized(a_hash: &mut [HashMap<usize, Fraction>], mut b: Vec<Fraction>) -> Result<Vec<Fraction>> {
    // Convert the incoming HashMap rows into sorted vec rows
    fn to_vec_rows(a: &mut [HashMap<usize, Fraction>]) -> Vec<Vec<(usize, Fraction)>> {
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
    fn saxpy_row(target: &mut Vec<(usize, Fraction)>, i: usize, pivot: &[(usize, Fraction)], factor: &Fraction) {
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
            .find(|&r| {
                find_col(&a[r], i)
                    .map_or(false, |idx| !a[r][idx].1.is_zero())
            })
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
                    find_col(row, i).map(|idx| {
                        let factor = row[idx].1.clone();
                        row[idx].1 = Fraction::zero(); // lazy zeroing avoids shift
                        if factor.is_zero() {
                            None
                        } else {
                            saxpy_row(row, i, pivot_ref, &factor);
                            Some((r, factor))
                        }
                    }).flatten()
                })
                .collect::<Vec<_>>()
        );

        // rows below
        updates.extend(
            below.par_iter_mut()
                .enumerate()
                .filter_map(|(off, row)| {
                    let r = i + 1 + off;
                    find_col(row, i).map(|idx| {
                        let factor = row[idx].1.clone();
                        row[idx].1 = Fraction::zero();
                        if factor.is_zero() {
                            None
                        } else {
                            saxpy_row(row, i, pivot_ref, &factor);
                            Some((r, factor))
                        }
                    }).flatten()
                })
                .collect::<Vec<_>>()
        );

        // Apply RHS updates sequentially
        for (r, factor) in updates {
            b[r] -= &factor * &pivot_b;
        }
    }

    Ok(b)
}

/// Naive sparse Gaussian elimination for Fraction matrices represented as Vec<HashMap<usize, Fraction>>
fn solve_sparse_linear_system(a: &mut [HashMap<usize, Fraction>], mut b: Vec<Fraction>) -> Result<Vec<Fraction>> {
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
            if r == i { continue; }
            if let Some(factor_val) = a[r].get(&i).cloned() {
                if !factor_val.is_zero() {
                    // subtract factor * row_i from row_r
                    let keys: Vec<(usize, Fraction)> = a[i].iter().map(|(k,v)| (*k, v.clone())).collect();
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
fn compute_phi_ids(snfa: &Snfa, k: usize, key: &mut ActivityKey) -> Vec<HashMap<Arc<[u32]>, Fraction>> {
    let n = snfa.states.len();
    // Compute numeric IDs for "+" and "-" for quick comparisons
    let plus_act = key.process_activity("+");
    let id_plus = key.get_id_from_activity(plus_act) as u32;
    let minus_act = key.process_activity("-");
    let id_minus = key.get_id_from_activity(minus_act) as u32;
    let mut phi: Vec<HashMap<Arc<[u32]>, Fraction>> = vec![HashMap::default(); n];

    // Memoisation cache mapping (state, remaining_len) -> suffix map
    // Suffix map: subtrace (starting at the first symbol that leaves the current state) -> probability
    type SuffixMap = HashMap<Arc<[u32]>, Fraction>;
    // Cache stores Arc<SuffixMap> so a hit clones only the pointer
    let mut cache: HashMap<(usize, usize), Arc<SuffixMap>> = HashMap::default();

    // Recursively collect every suffix of length <= remaining that can be produced
    // from state_idx, together with its probability relative to the current state.
    // The probabilities stored in the map do not include any prefix probability.
    // This allows us to multiply the prefix probability later and still reuse the suffixes.
    fn collect_suffixes(
        state_idx: usize,
        remaining: usize,
        snfa: &Snfa,
        key: &mut ActivityKey,
        cache: &mut HashMap<(usize, usize), Arc<SuffixMap>>,
    ) -> Arc<SuffixMap> {
        // Fast path -> already computed
        if let Some(m) = cache.get(&(state_idx, remaining)) {
            return m.clone();
        }

        let mut result: SuffixMap = HashMap::default();

        if remaining == 0 {
            // No more symbols allowed -> empty suffix with probability 1
            result.insert(Arc::<[u32]>::from(Vec::<u32>::new()), Fraction::from((1, 1)));
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
    use std::fs;
    use ebi_objects::{EventLog, FiniteStochasticLanguage, HasActivityKey, StochasticLabelledPetriNet, TranslateActivityKey};
    use crate::ebi_traits::ebi_trait_finite_stochastic_language::EbiTraitFiniteStochasticLanguage;
    use super::*;

    #[test]
    fn test_compute_abstraction_for_example_log() {
        let file_content = fs::read_to_string("testfiles/simple_log_markovian_abstraction.xes").unwrap();
        let event_log = file_content.parse::<EventLog>().unwrap();
        let finite_lang: FiniteStochasticLanguage = Into::into(event_log);
        
        // Compute abstraction with k=2 for example log [<a,b>^{5}, <a,a,b,c>^{2}, <a,a,c,b>^{1}]
        let abstraction = compute_abstraction_for_log(&finite_lang, 2).unwrap();
        
        println!("\nComputed abstraction for example log with k=2:");

        assert_eq!(abstraction.abstraction.len(), 8, "Should be exactly 8 entries");
        
        // map for checking
        let mut check: std::collections::HashMap<String, Fraction> = std::collections::HashMap::default();
        for (subtrace, prob) in abstraction.abstraction.iter() {
            let key = subtrace.iter().map(|s| s.as_str()).collect::<Vec<_>>().join(",");
            println!("{:<12} : {}", key, prob);
            check.insert(key, prob.clone());
        }

        assert_eq!(check["+,ac0"], Fraction::from((4, 15)));
        assert_eq!(check["ac0,ac0"], Fraction::from((1, 10)));

        let pair1 = [Fraction::from((7, 30)), Fraction::from((1, 30))];
        let pair2 = [Fraction::from((1, 5)), Fraction::from((1, 15))];
        let pair3 = [Fraction::from((1, 15)), Fraction::from((1, 30))];

        // Helper to check that a key has one of two expected values and that the other value is on the other key
        fn assert_pair(check: &std::collections::HashMap<String, Fraction>, k1: &str, k2: &str, exp: [Fraction; 2]) {
            let v1 = check.get(k1).expect("missing key");
            let v2 = check.get(k2).expect("missing key");
            assert!( (v1 == &exp[0] && v2 == &exp[1]) || (v1 == &exp[1] && v2 == &exp[0]),
                "Pair {{ {}, {} }} has unexpected values {{ {}, {} }}", k1, k2, v1, v2);
        }

        assert_pair(&check, "ac0,ac1", "ac0,ac2", pair1);
        assert_pair(&check, "ac1,-",   "ac2,-",   pair2);
        assert_pair(&check, "ac1,ac2", "ac2,ac1", pair3);
    }
    
    #[test]
    fn test_compute_abstraction_for_petri_net() {
        // This test verifies the computation of Markovian abstractions
        // for a simple SLPN model with the following structure:
        // - Initial token in place 0
        // - Transition 'a' moves token to place 1
        // - From place 1, transition 'b' (weight 3/4) or 'c' (weight 1/4) to deadlock
        let file_content = fs::read_to_string("testfiles/simple_markovian_abstraction.slpn").unwrap();
        let petri_net = file_content.parse::<StochasticLabelledPetriNet>().unwrap();
        
        // Check that k < 1 is rejected (k = 0)
        let result = compute_abstraction_for_petri_net(&petri_net, 0, Fraction::from((1, 1000)));
        assert!(result.is_err(), "Should reject k < 1");
        
        // Compute abstraction with k=2
        let abstraction = compute_abstraction_for_petri_net(&petri_net, 2, Fraction::from((1, 1000))).unwrap();
        

        // Check that probabilities sum to 1
        let mut total = Fraction::from((0, 1));
        for probability in abstraction.abstraction.values() {
            total += probability;
        }
        assert_eq!(total, Fraction::from((1, 1)), "Total probability should be 1");

        // Expected internal traces and probabilities
        let expected_traces = [
            ("+ ac0", "1/3"),
            ("ac0 ac1", "1/4"),
            ("ac1 -", "1/4"),
            ("ac0 ac2", "1/12"),
            ("ac2 -", "1/12")
        ];

        // Verify expected traces have the correct probabilities
        for (trace_str, expected_prob) in expected_traces.iter() {
            let key: Vec<String> = trace_str.split_whitespace().map(|s| s.to_string()).collect();
            let arc_key = Arc::from(key.as_slice());
            if let Some(prob) = abstraction.abstraction.get(&arc_key) {
                println!("Found <{}> with probability {}", trace_str, prob);
                assert_eq!(prob.to_string(), *expected_prob,
                    "Probability mismatch for trace <{}>: expected {}, got {}",
                    trace_str, expected_prob, prob);
            } else {
                panic!("Expected trace <{}> not found in abstraction", trace_str);
            }
        }

        println!("Test passed: Abstraction matches expected internal trace probabilities!");
    }

    #[test]
    fn test_markovian_conformance_uemsc_log_vs_petri_net() {
        // Load the example log and convert to finite stochastic language
        let file_content = fs::read_to_string("testfiles/simple_log_markovian_abstraction.xes").unwrap();
        let event_log = file_content.parse::<EventLog>().unwrap();
        let mut finite_lang: FiniteStochasticLanguage = Into::into(event_log);

        // Load the Petri net model
        let file_content = fs::read_to_string("testfiles/simple_markovian_abstraction.slpn").unwrap();
        let mut petri_net = file_content.parse::<StochasticLabelledPetriNet>().unwrap();

        // Ensure both share a common ActivityKey to avoid nondeterministic mappings
        petri_net.translate_using_activity_key(finite_lang.activity_key_mut());

        // Compute the m^2-uEMSC distance (k = 2)
        let conformance = (&finite_lang as &dyn EbiTraitFiniteStochasticLanguage)
            .markovian_conformance(Box::new(petri_net), 2, DistanceMetric::Uemsc, Fraction::from((1, 1000)))
            .unwrap();

        println!("Computed m^2-uEMSC conformance: {}", conformance);
        assert_eq!(conformance, Fraction::from((4, 5))); // Expect 4/5
    }
}