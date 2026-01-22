use crate::{
    ebi_traits::ebi_trait_finite_stochastic_language::EbiTraitFiniteStochasticLanguage,
    semantics::{labelled_petri_net_semantics::LPNMarking, semantics::Semantics},
    stochastic_semantics::stochastic_semantics::StochasticSemantics,
    techniques::tau_removal::TauRemoval,
};
use anyhow::{Ok, Result, anyhow};
use ebi_objects::{
    Activity, ActivityKey, HasActivityKey, StochasticNondeterministicFiniteAutomaton,
    ebi_arithmetic::{Fraction, One, Recip, ebi_number::Zero},
    ebi_objects::{
        finite_stochastic_language::FiniteStochasticLanguage,
        stochastic_labelled_petri_net::StochasticLabelledPetriNet,
    },
};
use itertools::Itertools;
use rayon::prelude::*;
use rustc_hash::FxHashMap;
use std::{cmp::max, collections::VecDeque, fmt::Display};

#[derive(Debug, Clone)]
/// Represents a k-th order Markovian abstraction of a stochastic language
pub struct MarkovianAbstraction {
    pub activity_key: ActivityKey,

    /// The order of the abstraction (k)
    pub order: usize,

    /// The mapping from subtraces to normalised frequencies
    pub abstraction: FxHashMap<Vec<Activity>, Fraction>,

    pub start_activity: Activity,
    pub end_activity: Activity,
}

impl MarkovianAbstraction {
    pub fn create_start_end(activity_key: &mut ActivityKey) -> (Activity, Activity) {
        let max_len = activity_key
            .activity2name
            .iter()
            .map(|name| name.len())
            .max()
            .unwrap_or(0);
        let start_activity = activity_key.process_activity(&"+".repeat(max_len + 1));
        let end_activity = activity_key.process_activity(&"-".repeat(max_len + 1));
        (start_activity, end_activity)
    }

    pub fn harmonise_start_end(&mut self, other: &mut MarkovianAbstraction) {
        //start
        {
            let max_len_start = max(
                self.activity_key
                    .get_activity_label(&self.start_activity)
                    .len(),
                other
                    .activity_key
                    .get_activity_label(&other.start_activity)
                    .len(),
            );
            let self_id = self.activity_key.get_id_from_activity(self.start_activity);
            self.activity_key.activity2name[self_id] = "+".repeat(max_len_start);
            let other_id = other
                .activity_key
                .get_id_from_activity(other.start_activity);
            other.activity_key.activity2name[other_id] = "+".repeat(max_len_start);
        }
        //end
        {
            let max_len_end = max(
                self.activity_key
                    .get_activity_label(&self.end_activity)
                    .len(),
                other
                    .activity_key
                    .get_activity_label(&other.end_activity)
                    .len(),
            );
            let self_id = self.activity_key.get_id_from_activity(self.end_activity);
            self.activity_key.activity2name[self_id] = "-".repeat(max_len_end);
            let other_id = other.activity_key.get_id_from_activity(other.end_activity);
            other.activity_key.activity2name[other_id] = "-".repeat(max_len_end);
        }
    }
}

impl Display for MarkovianAbstraction {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        for (trace, probability) in &self.abstraction {
            writeln!(
                f,
                "[{}]: {}",
                trace
                    .iter()
                    .map(|activity| self.activity_key.deprocess_activity(activity))
                    .join(", "),
                probability
            )?;
        }
        write!(f, "")
    }
}

/// Convert a Markovian abstraction to a Finite Stochastic Language.
/// Note that the start and end of the traces will show up as normal activities.
impl From<MarkovianAbstraction> for FiniteStochasticLanguage {
    fn from(ma: MarkovianAbstraction) -> Self {
        //repack the traces
        let traces = ma.abstraction.into_iter().collect();

        // new_raw is used because normalisation is guaranteed by construction
        FiniteStochasticLanguage::new_raw(traces, ma.activity_key)
    }
}

pub trait AbstractMarkovian {
    /// Create a Markovian abstraction, that is, a stochastic language of sub-traces.
    fn abstract_markovian(&mut self, order: usize) -> Result<MarkovianAbstraction>;
}

impl AbstractMarkovian for StochasticLabelledPetriNet {
    fn abstract_markovian(&mut self, order: usize) -> Result<MarkovianAbstraction> {
        if order < 1 {
            return Err(anyhow!(
                "order must be at least 1 for Markovian abstraction"
            ));
        }

        // 1 Build embedded SNFA
        let mut snfa = build_embedded_snfa(self)?;

        // println!("snfa\n{}", snfa);

        snfa.abstract_markovian(order)
    }
}

impl AbstractMarkovian for StochasticNondeterministicFiniteAutomaton {
    /// Please note that this method will remove the taus from the SNFA.
    /// As such, it will be potentially changed considerably.
    fn abstract_markovian(&mut self, order: usize) -> Result<MarkovianAbstraction> {
        // 1.1 Remove tau transitions
        self.remove_tau_transitions()?;

        // println!("snfa removed taus\n{}", snfa);

        // 2 Add artificial start and end
        let (start_activity, end_activity) =
            MarkovianAbstraction::create_start_end(&mut self.activity_key);
        add_artificial_start_end_to_snfa(self, start_activity, end_activity)?;

        // println!("snfa start end\n{}", snfa);

        //check whether the SNFA has an empty language
        let initial_state = if let Some(initial) = self.initial_state {
            initial
        } else {
            //empty language
            return Ok(MarkovianAbstraction {
                activity_key: self.activity_key.clone(),
                order,
                abstraction: FxHashMap::default(),
                start_activity,
                end_activity,
            });
        };

        // println!("no empty language");

        // 3 Build matrix and solve for x
        let n = self.number_of_states();
        // Build sparse A = (I - Delta)^T
        let delta = build_delta(&self);

        // println!("delta {:?}", delta);

        let mut a_sparse: Vec<FxHashMap<usize, Fraction>> = vec![FxHashMap::default(); n];

        // compute A = I-delta
        for i in 0..n {
            // Identity contribution
            a_sparse[i].insert(i, Fraction::one());

            // Subtract row i of Delta into column i of A
            for (&j, p) in &delta[i] {
                // A[j,i] = I[j,i] - Delta[i,j]
                a_sparse[j]
                    .entry(i)
                    .and_modify(|v| *v -= p)
                    .or_insert_with(|| -p);
            }
        }
        let mut b = vec![Fraction::zero(); n];
        b[initial_state] = Fraction::one();

        // println!("a_sparse {:?}", a_sparse);

        let x = if n < 100 {
            // small matrices -> simpler hash map solver avoids conversion overhead
            solve_sparse_linear_system(&mut a_sparse, b)?
        } else {
            solve_sparse_linear_system_optimized(&mut a_sparse, b)?
        };

        // println!("x {:?}", x);

        // 4 Compute phi for each state
        // Compute phi on ID space then translate back to Strings using the shared ActivityKey
        let phi_ids = compute_phi_ids(&self, order, start_activity, end_activity);

        // println!("phi ids {:?}", phi_ids);

        // helper to translate a trace of usize IDs back to Activitys
        let translate = |ids: &Vec<usize>| -> Vec<Activity> {
            let mut vec: Vec<Activity> = Vec::with_capacity(ids.len());
            for id in ids.iter() {
                vec.push(self.activity_key.get_activity_by_id(*id));
            }
            vec
        };

        // 5 Compute f_l^k
        let mut f_l_k: FxHashMap<Vec<Activity>, Fraction> = FxHashMap::default();
        for (q, map) in phi_ids.iter().enumerate() {
            for (gamma_ids, phi_val) in map {
                let gamma = translate(gamma_ids);
                let contribution = &x[q] * phi_val;
                f_l_k
                    .entry(gamma)
                    .and_modify(|v| *v = &*v + &contribution)
                    .or_insert(contribution);
            }
        }

        // 6 Normalise
        let total: Fraction = f_l_k.values().sum();
        let mut abstraction = FxHashMap::default();
        for (gamma, val) in f_l_k {
            abstraction.insert(gamma, &val / &total);
        }

        Ok(MarkovianAbstraction {
            activity_key: self.activity_key.clone(),
            order,
            abstraction,
            start_activity,
            end_activity,
        })
    }
}

impl AbstractMarkovian for dyn EbiTraitFiniteStochasticLanguage {
    /// This implements the calculation of the k-th order Stochastic Markovian abstraction
    /// for a finite stochastic language. First it computes the k-th order multiset
    /// markovian abstraction by adding the special start '+' and end '-' markers and then
    /// computes the k-trimmed subtraces. Afterwards, the k-th order stochastic markovian
    /// abstraction gets computed by normalising the multiset markovian abstraction.
    fn abstract_markovian(&mut self, order: usize) -> Result<MarkovianAbstraction> {
        // Validate k
        if order < 1 {
            return Err(anyhow!(
                "the order must be at least 1 for Markovian abstraction"
            ));
        }

        // Initialise f_l^k which stores the expected number of occurrences of each subtrace
        let mut f_l_k: FxHashMap<Vec<Activity>, Fraction> = FxHashMap::default();

        let mut activity_key = self.activity_key().clone();
        let (start_activity, end_activity) =
            MarkovianAbstraction::create_start_end(&mut activity_key);

        // For each trace in the log with its probability
        for (trace, probability) in self.iter_traces_probabilities() {
            // Compute M_σ^k for this trace (k-th order multiset Markovian abstraction)
            let m_sigma_k = compute_multiset_abstraction_for_trace_with_key(
                &trace,
                order,
                start_activity,
                end_activity,
            );

            // Add contribution to f_l^k
            for (subtrace, occurrences) in m_sigma_k {
                let occurrences_as_fraction = Fraction::from(occurrences);
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

        // Calculate the total sum for normalisation
        let total: Fraction = f_l_k.values().sum();

        // Normalise f_l^k to get m_l^k
        let mut abstraction = FxHashMap::default();
        for (subtrace, count) in f_l_k {
            let count_ref: &Fraction = &count;
            let total_ref: &Fraction = &total;
            abstraction.insert(subtrace, count_ref / total_ref);
        }

        Ok(MarkovianAbstraction {
            activity_key,
            order,
            abstraction,
            start_activity,
            end_activity,
        })
    }
}

/// Compute the multiset of k-trimmed subtraces for a given trace
fn compute_multiset_abstraction_for_trace_with_key(
    trace: &[Activity],
    k: usize,
    start_activity: Activity,
    end_activity: Activity,
) -> FxHashMap<Vec<Activity>, usize> {
    // Convert labels to IDs and add start/end markers in ID space
    let mut augmented_ids = Vec::with_capacity(trace.len() + 2);
    augmented_ids.push(start_activity);
    augmented_ids.extend_from_slice(trace);
    augmented_ids.push(end_activity);

    // Compute multiset over IDs
    let id_multiset = compute_multiset_k_trimmed_subtraces_iterative_ids(augmented_ids, k);

    // Reconstruct subtrace keys via the reverse map
    let mut result: FxHashMap<Vec<Activity>, usize> = FxHashMap::default();
    result.reserve(id_multiset.len());
    for (sub_ids, cnt) in id_multiset {
        result.insert(sub_ids, cnt);
    }
    result
}

/// Operates on a slice of ActivityStartEnd and returns a
/// multiset keyed by `Arc<[ActivityStartEnd]>`. This avoids heap traffic except when a new
/// *unique* subtrace is inserted into the map.
fn compute_multiset_k_trimmed_subtraces_iterative_ids(
    trace: Vec<Activity>,
    k: usize,
) -> FxHashMap<Vec<Activity>, usize> {
    let mut result: FxHashMap<Vec<Activity>, usize> = FxHashMap::default();

    if trace.len() <= k {
        result.insert(trace, 1);
        return result;
    }

    let mut ring = Vec::with_capacity(k);
    ring.extend_from_slice(&trace[..k]);
    let mut head: usize = 0; // index of the oldest element

    // Reusable key construction
    let mut tmp = Vec::with_capacity(k);
    // Helper to build an Arc key representing the current window
    let make_key = |ring: &Vec<Activity>, head: usize, tmp: &mut Vec<Activity>| -> Vec<Activity> {
        tmp.clear();
        tmp.extend_from_slice(&ring[head..]);
        tmp.extend_from_slice(&ring[..head]);
        tmp.clone()
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

/// Create an SNFA for an SLPN. May not terminate if the SLPN has livelocks or is unbounded.
pub fn build_embedded_snfa(
    net: &StochasticLabelledPetriNet,
) -> Result<StochasticNondeterministicFiniteAutomaton> {
    // Create a new SNFA without the default initial state
    let mut snfa = StochasticNondeterministicFiniteAutomaton::new();
    snfa.activity_key = net.activity_key().clone();

    // Reachability exploration queue
    let mut state2snfa_state: FxHashMap<LPNMarking, usize> = FxHashMap::default();
    let mut queue: VecDeque<LPNMarking> = VecDeque::new();

    // Insert the initial state of the Petri net
    let initial_state = if let Some(initial) = net.get_initial_state() {
        initial
    } else {
        return Ok(snfa);
    };
    state2snfa_state.insert(initial_state.clone(), 0);
    queue.push_back(initial_state);

    // println!("{:?}", state2snfa_state);

    while let Some(state) = queue.pop_front() {
        let snfa_state = *state2snfa_state.get(&state).unwrap();

        // println!("marking {}, snfa state {}", state, snfa_state);

        // Collect enabled transitions and the total enabled weight in this state
        let enabled_transitions = net.get_enabled_transitions(&state);
        if enabled_transitions.is_empty() {
            // Deadlock state -> it will already have a termination probability 1
            continue;
        }

        let weight_sum = net.get_total_weight_of_enabled_transitions(&state)?;

        for &transition in &enabled_transitions {
            let weight = net.get_transition_weight(&state, transition).clone();
            let prob = &weight / &weight_sum;

            // Fire transition (creates a successor state)
            let mut next_state = state.clone();
            net.execute_transition(&mut next_state, transition)?;

            // Map / enqueue successor
            let snfa_target = *state2snfa_state
                .entry(next_state.clone())
                .or_insert_with(|| {
                    let new_state = snfa.add_state();
                    queue.push_back(next_state);
                    new_state
                });

            // println!("\t{:?}", state2snfa_state);

            // Transition label
            let label = net.get_transition_label(transition);

            // println!("\t\ttransition {}, label {:?}", transition, label);

            snfa.add_transition(snfa_state, label, snfa_target, prob)?;
        }
    }

    // The first discovered marking is the initial state of the SNFA
    snfa.initial_state = Some(0);
    Ok(snfa)
}

/// Patch the SNFA by adding start and end transitions
fn add_artificial_start_end_to_snfa(
    snfa: &mut StochasticNondeterministicFiniteAutomaton,
    start_activity: Activity,
    end_activity: Activity,
) -> Result<()> {
    let initial_state = if let Some(initial) = snfa.initial_state {
        initial
    } else {
        return Ok(());
    };

    let q_minus = snfa.add_state();

    // Redirect original finals to q_minus via '-'
    for state in 0..snfa.number_of_states() - 1 {
        if !snfa.terminating_probabilities[state].is_zero() {
            // Copy the existing p_final value
            let final_prob = snfa.terminating_probabilities[state].clone();
            // Add a "-" transition with the original final probability
            snfa.add_transition(state, Some(end_activity), q_minus, final_prob)?;
        }
    }

    // q_plus with + transition to original initial state
    let q_plus = snfa.add_state();
    snfa.add_transition(q_plus, Some(start_activity), initial_state, Fraction::one())?;

    snfa.initial_state = Some(q_plus);

    Ok(())
}

// Build sparse transition matrix
fn build_delta(
    snfa: &StochasticNondeterministicFiniteAutomaton,
) -> Vec<FxHashMap<usize, Fraction>> {
    let n = snfa.number_of_states();
    let mut delta = vec![FxHashMap::<usize, Fraction>::default(); n];

    for transition in 0..snfa.number_of_transitions() {
        delta[snfa.sources[transition]]
            .entry(snfa.targets[transition])
            .and_modify(|v| *v += &snfa.probabilities[transition])
            .or_insert_with(|| snfa.probabilities[transition].clone());
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
    snfa: &StochasticNondeterministicFiniteAutomaton,
    k: usize,
    start_activity: Activity,
    end_activity: Activity,
) -> Vec<FxHashMap<Vec<usize>, Fraction>> {
    let n = snfa.number_of_states();
    // Compute numeric IDs for "+" and "-" for quick comparisons
    let id_plus = snfa.activity_key().get_id_from_activity(start_activity);
    let id_minus = snfa.activity_key().get_id_from_activity(end_activity);
    let mut phi: Vec<FxHashMap<Vec<usize>, Fraction>> = vec![FxHashMap::default(); n];

    // println!("\tstart act {}", id_plus);
    // println!("\tend act {}", id_minus);

    // Memoisation cache mapping (state, remaining_len) -> suffix map
    // Suffix map: subtrace (starting at the first symbol that leaves the current state) -> probability
    type SuffixMap = FxHashMap<Vec<usize>, Fraction>;
    // Cache stores Arc<SuffixMap> so a hit clones only the pointer
    let mut cache: FxHashMap<(usize, usize), SuffixMap> = FxHashMap::default();

    // Recursively collect every suffix of length <= remaining that can be produced
    // from state_idx, together with its probability relative to the current state.
    // The probabilities stored in the map do not include any prefix probability.
    // This allows us to multiply the prefix probability later and still reuse the suffixes.
    fn collect_suffixes(
        state_idx: usize,
        remaining: usize,
        snfa: &StochasticNondeterministicFiniteAutomaton,
        start_activity: Activity,
        end_activity: Activity,
        cache: &mut FxHashMap<(usize, usize), SuffixMap>,
    ) -> SuffixMap {
        // Fast path -> already computed
        if let Some(m) = cache.get(&(state_idx, remaining)) {
            return m.clone();
        }

        let mut result: SuffixMap = FxHashMap::default();

        if remaining == 0 {
            // No more symbols allowed -> empty suffix with probability 1
            result.insert(Vec::<usize>::new(), Fraction::one());
        } else {
            for (_, target, label, probability) in snfa.outgoing_edges(state_idx) {
                if label == &Some(end_activity) {
                    // End marker -> stop exploring beyond this symbol
                    let arc = vec![snfa.activity_key().get_id_from_activity(end_activity)];
                    result
                        .entry(arc)
                        .and_modify(|v| *v += probability)
                        .or_insert_with(|| probability.clone());
                } else {
                    // Recurse to target with one fewer remaining symbol
                    let child_map = collect_suffixes(
                        *target,
                        remaining - 1,
                        snfa,
                        start_activity,
                        end_activity,
                        cache,
                    );
                    for (suf, w) in &child_map {
                        let mut vec = Vec::with_capacity(1 + suf.len());
                        //at this point, every label should have a transition
                        let label = label.unwrap();
                        vec.push(snfa.activity_key().get_id_from_activity(label));
                        vec.extend_from_slice(&suf[..]);
                        let weight = probability * w;
                        result
                            .entry(vec)
                            .and_modify(|v| *v += &weight)
                            .or_insert(weight);
                    }
                }
            }
        }

        cache.insert((state_idx, remaining), result.clone());
        result
    }

    // Build phi for every potential start state
    for state in 0..n {
        let suffixes = collect_suffixes(state, k, snfa, start_activity, end_activity, &mut cache);

        // println!("\tstate {}, suffixes {:?}", state, suffixes);

        for (trace_slice, prob_suffix) in suffixes.iter() {
            // Valid subtraces follow exactly these conditions
            let is_exact_k = trace_slice.len() == k;
            let is_short_with_end = !trace_slice.is_empty()
                && trace_slice.len() <= k
                && trace_slice.last().unwrap() == &id_minus
                && trace_slice.first().unwrap() == &id_plus;

            if is_exact_k || is_short_with_end {
                phi[state].insert(trace_slice.clone(), prob_suffix.clone());
            }
        }
    }

    phi
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::{
        ebi_traits::ebi_trait_finite_stochastic_language::EbiTraitFiniteStochasticLanguage,
        techniques::stochastic_markovian_abstraction_conformance::{
            DistanceMeasure, StochasticMarkovianConformance,
        },
    };
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
    fn empty_slpn_abstract() {
        let fin = fs::read_to_string("testfiles/empty.slpn").unwrap();
        let mut slang = fin.parse::<StochasticLabelledPetriNet>().unwrap();
        let abst = slang.abstract_markovian(2).unwrap();
        abst.to_string();
    }

    #[test]
    fn slpn_abstract() {
        let fin = fs::read_to_string("testfiles/simple_markovian_abstraction.slpn").unwrap();
        let mut slang = fin.parse::<StochasticLabelledPetriNet>().unwrap();
        let abst = slang.abstract_markovian(2).unwrap();
        abst.to_string();
    }

    #[test]
    fn log_abstract() {
        let fin = fs::read_to_string("testfiles/simple_log_markovian_abstraction.xes").unwrap();
        let mut slang: Box<dyn EbiTraitFiniteStochasticLanguage> = Box::new(
            FiniteStochasticLanguage::from(fin.parse::<EventLog>().unwrap()),
        );
        let abst = slang.abstract_markovian(2).unwrap();
        abst.to_string();
    }

    #[test]
    fn test_compute_abstraction_for_example_log() {
        let file_content =
            fs::read_to_string("testfiles/simple_log_markovian_abstraction.xes").unwrap();
        let event_log = file_content.parse::<EventLog>().unwrap();
        let mut finite_lang: Box<dyn EbiTraitFiniteStochasticLanguage> =
            Box::new(FiniteStochasticLanguage::from(event_log));

        // Compute abstraction with k=2 for example log [<a,b>^{5}, <a,a,b,c>^{2}, <a,a,c,b>^{1}]
        let abstraction = finite_lang.abstract_markovian(2).unwrap();

        // println!("\nComputed abstraction for example log with k=2:");

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
                .map(|s| s.to_string())
                .collect::<Vec<_>>()
                .join(",");
            // println!("{:<12} : {}", key, prob);
            check.insert(key, prob.clone());
        }

        // println!("{:?}", check);

        assert_eq!(
            check[&format!("{},ac0", abstraction.start_activity)],
            Fraction::from((4, 15))
        );
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
        assert_pair(
            &check,
            &format!("ac1,{}", abstraction.end_activity),
            &format!("ac2,{}", abstraction.end_activity),
            pair2,
        );
        assert_pair(&check, "ac1,ac2", "ac2,ac1", pair3);
    }

    #[test]
    fn test_compute_abstraction_for_petri_net() {
        let file_content =
            fs::read_to_string("testfiles/simple_markovian_abstraction.slpn").unwrap();
        let mut petri_net = file_content.parse::<StochasticLabelledPetriNet>().unwrap();

        // Check that k < 1 is rejected (k = 0)
        let result = petri_net.clone().abstract_markovian(0);
        assert!(result.is_err(), "Should reject k < 1");

        // Compute abstraction with k=2
        let abstraction = petri_net.abstract_markovian(2).unwrap();
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
        let mut finite_lang: Box<dyn EbiTraitFiniteStochasticLanguage> =
            Box::new(FiniteStochasticLanguage::from(event_log));

        let finite_lang_abstraction = finite_lang.abstract_markovian(2).unwrap();

        // println!("finite language");
        // println!("{}", finite_lang_abstraction);

        // println!("\nPetri net");

        // Load the Petri net model
        let file_content =
            fs::read_to_string("testfiles/simple_markovian_abstraction.slpn").unwrap();
        let mut petri_net = file_content.parse::<StochasticLabelledPetriNet>().unwrap();

        // Ensure both share a common ActivityKey to avoid nondeterministic mappings
        petri_net.translate_using_activity_key(finite_lang.activity_key_mut());

        let petri_net_abstraction = petri_net.abstract_markovian(2).unwrap();

        // println!("{}", petri_net_abstraction);

        // Compute the m^2-uEMSC distance (k = 2)
        let conformance = finite_lang_abstraction
            .markovian_conformance(petri_net_abstraction, DistanceMeasure::UEMSC)
            .unwrap();
        assert_eq!(conformance, Fraction::from((4, 5))); // Expect 4/5
    }

    #[test]
    fn markovian_eduardo_process_science_journal_paper_test() {
        // Load the Petri net model
        let file_content = fs::read_to_string("testfiles/loop(a,tau)and(bc).slpn").unwrap();
        let mut petri_net = file_content.parse::<StochasticLabelledPetriNet>().unwrap();

        let fin2 = fs::read_to_string("testfiles/loop(a,tau)and(bc).snfa").unwrap();
        let snfa2 = fin2
            .parse::<StochasticNondeterministicFiniteAutomaton>()
            .unwrap();

        let snfa = build_embedded_snfa(&petri_net).unwrap();

        assert_eq!(snfa.to_string(), snfa2.to_string());

        let abstraction = petri_net.abstract_markovian(2).unwrap();

        assert!(!abstraction.abstraction.is_empty());
    }
}
