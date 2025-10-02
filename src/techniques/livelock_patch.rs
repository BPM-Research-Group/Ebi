use anyhow::Result;
use std::collections::{HashMap, HashSet, VecDeque};
use ebi_arithmetic::{Fraction, Signed};

use ebi_objects::StochasticLabelledPetriNet;

use crate::{
    semantics::{
        labelled_petri_net_semantics::LPNMarking,
        semantics::Semantics,
    },
    stochastic_semantics::stochastic_semantics::StochasticSemantics,
    techniques::livelock::IsPartOfLivelock,
};

/// Add a silent timeout transition that allows the model to escape livelock SCCs.
/// The function clones the input net and returns a patched version in which each
/// detected livelock SCC contains one extra silent transition `tau_timeout`
/// leading to a global sink place.  The rate of that transition is
/// `delta * (total_out_weight(m))`, where `m` is the representative marking for
/// the SCC.
pub fn patch_livelocks(
    net: &StochasticLabelledPetriNet,
    delta: Fraction,
) -> Result<StochasticLabelledPetriNet> {
    // Guard against invalid delta values
    if !delta.is_positive() {
        return Err(anyhow::anyhow!("Delta parameter must be positive for livelock patching"));
    }
    
    let mut visited: HashSet<LPNMarking> = HashSet::new();
    let mut queue: VecDeque<LPNMarking> = VecDeque::new();
    let mut livelock_markings: Vec<LPNMarking> = Vec::new();
    
    queue.push_back(net.get_initial_state().unwrap());
    visited.insert(net.get_initial_state().unwrap());

    // Reuse a single cache instance for livelock queries
    let mut ll_cache = net.get_livelock_cache();

    // First pass -> find all livelock markings
    while let Some(mark) = queue.pop_front() {
        // If the marking is part of a livelock SCC, record it
        if ll_cache.is_state_part_of_livelock(&mark)? {
            livelock_markings.push(mark.clone());
            // Still explore successors to find all reachable states
        }

        // explore successors
        for t in net.get_enabled_transitions(&mark) {
            let mut child = mark.clone();
            net.execute_transition(&mut child, t)?;
            if visited.insert(child.clone()) {
                queue.push_back(child);
            }
        }
    }

    // If no livelocks found, return original net unchanged
    if livelock_markings.is_empty() {
        return Ok(net.clone());
    }

    // Group livelock markings by SCC and select deterministic representative
    let scc_representatives = group_markings_by_scc(&livelock_markings, net)?;

    // Second pass -> patch the net by adding timeouts for SCC representatives only
    let mut patched = net.clone();
    let mut sink_place: Option<usize> = None;

    for mark in scc_representatives {
        // compute total exit weight using semantics helper
        let total_weight = patched.get_total_weight_of_enabled_transitions(&mark)?;
        let timeout_weight = &delta * &total_weight;

        // add silent transition enabled only in mark
        add_timeout_transition(&mut patched, &mark, &mut sink_place, timeout_weight)?;
    }

    Ok(patched)
}

/// Group livelock markings by SCC and return one deterministic representative per SCC.
/// Selects the lexicographically smallest marking (with token count tie-breaker) as representative.
fn group_markings_by_scc(
    livelock_markings: &[LPNMarking],
    net: &StochasticLabelledPetriNet,
) -> Result<Vec<LPNMarking>> {
    if livelock_markings.is_empty() {
        return Ok(Vec::new());
    }

    // Build adjacency list for livelock markings only
    let marking_to_idx: HashMap<LPNMarking, usize> = livelock_markings
        .iter()
        .enumerate()
        .map(|(i, m)| (m.clone(), i))
        .collect();
    
    let n = livelock_markings.len();
    let mut adj: Vec<Vec<usize>> = vec![vec![]; n];
    
    // Build edges between livelock markings
    for (i, marking) in livelock_markings.iter().enumerate() {
        for t in net.get_enabled_transitions(marking) {
            let mut child = marking.clone();
            net.execute_transition(&mut child, t)?;
            if let Some(&j) = marking_to_idx.get(&child) {
                adj[i].push(j);
            }
        }
    }

    // Run Tarjan's algorithm
    let sccs = compute_tarjan_scc(n, &adj);
    
    // Select deterministic representative for each SCC
    let mut representatives = Vec::new();
    for scc in sccs {
        if !scc.is_empty() {
            // Get markings in this SCC
            let scc_markings: Vec<_> = scc.iter().map(|&i| &livelock_markings[i]).collect();
            
            // Select representative with tie-breaking:
            // 1. Lexicographically smallest token vector
            // 2. Smallest total token count
            let representative = scc_markings
                .iter()
                .min_by(|a, b| {
                    let token_cmp = a.marking.get_place2token().cmp(b.marking.get_place2token());
                    if token_cmp != std::cmp::Ordering::Equal {
                        return token_cmp;
                    }
                    // Tie-breaker -> smallest total token count
                    let total_a: u64 = a.marking.get_place2token().iter().sum();
                    let total_b: u64 = b.marking.get_place2token().iter().sum();
                    total_a.cmp(&total_b)
                })
                .unwrap();
            
            representatives.push((*representative).clone());
        }
    }

    Ok(representatives)
}

/// Tarjan's algorithm to compute SCCs of a directed graph (adjacency list).
/// Returns vector of SCCs, where each SCC is a vector of node indices.
fn compute_tarjan_scc(n: usize, adj: &[Vec<usize>]) -> Vec<Vec<usize>> {
    let mut index = 0;
    let mut stack = Vec::new();
    let mut indices = vec![None; n];
    let mut lowlinks = vec![0; n];
    let mut on_stack = vec![false; n];
    let mut components = Vec::new();
    
    fn strongconnect(
        v: usize,
        adj: &[Vec<usize>],
        index: &mut usize,
        stack: &mut Vec<usize>,
        indices: &mut [Option<usize>],
        lowlinks: &mut [usize],
        on_stack: &mut [bool],
        components: &mut Vec<Vec<usize>>,
    ) {
        // Set the depth index for v to the smallest unused index
        indices[v] = Some(*index);
        lowlinks[v] = *index;
        *index += 1;
        stack.push(v);
        on_stack[v] = true;
        
        // Consider successors of v
        for &w in &adj[v] {
            if indices[w].is_none() {
                // Successor w has not yet been visited -> recurse on it
                strongconnect(w, adj, index, stack, indices, lowlinks, on_stack, components);
                lowlinks[v] = lowlinks[v].min(lowlinks[w]);
            } else if on_stack[w] {
                // Successor w is in stack and hence in the current SCC
                lowlinks[v] = lowlinks[v].min(indices[w].unwrap());
            }
        }
        
        // If v is a root node, pop the stack and create an SCC
        if lowlinks[v] == indices[v].unwrap() {
            let mut component = Vec::new();
            loop {
                let w = stack.pop().unwrap();
                on_stack[w] = false;
                component.push(w);
                if w == v {
                    break;
                }
            }
            components.push(component);
        }
    }
    
    for v in 0..n {
        if indices[v].is_none() {
            strongconnect(
                v,
                adj,
                &mut index,
                &mut stack,
                &mut indices,
                &mut lowlinks,
                &mut on_stack,
                &mut components,
            );
        }
    }
    
    components
}

/// Add a silent transition that is enabled exactly in marking
/// and leads to a global sink place. sink_place is created once and then
/// reused for subsequent calls.
fn add_timeout_transition(
    net: &mut StochasticLabelledPetriNet,
    marking: &LPNMarking,
    sink_place: &mut Option<usize>,
    weight: Fraction,
) -> Result<()> {
    // Create silent transition
    let tau = net.add_transition(None, weight);

    // preset -> require the exact token multiset of marking
    for (place_idx, tokens) in marking.marking.get_place2token().iter().enumerate() {
        if *tokens > 0 {
            net.add_place_transition_arc(place_idx, tau, *tokens)?;
        }
    }

    // Ensure global sink place exists
    let sink = match sink_place {
        Some(idx) => *idx,
        None => {
            let idx = net.add_place();
            *sink_place = Some(idx);
            idx
        }
    };

    // postset -> move one token to sink
    net.add_transition_place_arc(tau, sink, 1)?;

    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::{
        semantics::semantics::Semantics,
        techniques::livelock::IsPartOfLivelock,
    };

    #[test]
    fn test_patch_livelocks_simple() {
        // Create a simple livelock net: p0 -a-> p0 (self-loop)
        let mut slpn = StochasticLabelledPetriNet::new();
        let p0 = slpn.add_place();
        
        // Add initial token
        slpn.get_initial_marking_mut().increase(p0, 1).unwrap();
        
        // Add self-loop transition: p0 -a-> p0 (creates livelock)
        let activity = slpn.activity_key.process_activity("a");
        let t0 = slpn.add_transition(Some(activity), Fraction::from((1, 1)));
        slpn.add_place_transition_arc(p0, t0, 1).unwrap();
        slpn.add_transition_place_arc(t0, p0, 1).unwrap();
        
        // Verify initial state is in livelock
        let initial = slpn.get_initial_state().unwrap();
        assert!(slpn.is_state_part_of_livelock(&initial).unwrap());
        
        // Apply patch with small delta
        let delta = Fraction::from((1, 1000));
        let patched = patch_livelocks(&slpn, delta).unwrap();
        
        // Verify patch was applied:
        // More transitions than original (timeout added)
        assert!(patched.get_number_of_transitions() > slpn.get_number_of_transitions());
        
        // More places than original (sink added)
        assert!(patched.get_number_of_places() > slpn.get_number_of_places());
        
        // Initial state should no longer be in livelock
        let patched_initial = patched.get_initial_state().unwrap();
        assert!(!patched.is_state_part_of_livelock(&patched_initial).unwrap());
        
        // Verify exact timeout weight: δ × total_weight
        let total = slpn.get_total_weight_of_enabled_transitions(&initial).unwrap();
        let expected = &Fraction::from((1, 1000)) * &total;
        
        // Find the newly added timeout transition (last one added)
        let new_tau = patched.get_number_of_transitions() - 1;
        assert!(patched.is_transition_silent(new_tau));
        assert_eq!(patched.get_transition_weight(&patched_initial, new_tau), &expected);
    }

    #[test]
    fn test_patch_livelocks_no_livelock() {
        // Create a simple net that terminates (no livelocks)
        let mut slpn = StochasticLabelledPetriNet::new();
        let p0 = slpn.add_place();
        let p1 = slpn.add_place();
        
        // Add initial token
        slpn.get_initial_marking_mut().increase(p0, 1).unwrap();
        
        // Add transition that consumes from p0 and produces to p1 (terminates)
        let activity = slpn.activity_key.process_activity("a");
        let t0 = slpn.add_transition(Some(activity), Fraction::from((1, 1)));
        slpn.add_place_transition_arc(p0, t0, 1).unwrap();
        slpn.add_transition_place_arc(t0, p1, 1).unwrap();
        
        println!("Original transitions: {}, places: {}", slpn.get_number_of_transitions(), slpn.get_number_of_places());
        
        // Verify initial state is NOT in livelock
        let initial = slpn.get_initial_state().unwrap();
        let is_livelock = slpn.is_state_part_of_livelock(&initial).unwrap();
        println!("Initial state is livelock: {}", is_livelock);
        assert!(!is_livelock);
        
        // Apply patch
        let delta = Fraction::from((1, 1000));
        let patched = patch_livelocks(&slpn, delta).unwrap();
        
        println!("Patched transitions: {}, places: {}", patched.get_number_of_transitions(), patched.get_number_of_places());
        
        // Should be unchanged (no timeouts added)
        assert_eq!(patched.get_number_of_transitions(), slpn.get_number_of_transitions());
        assert_eq!(patched.get_number_of_places(), slpn.get_number_of_places());
    }

    #[test]
    fn test_patch_livelocks_deterministic() {
        // Create a livelock net: p0 -a-> p0 (self-loop)
        let mut slpn = StochasticLabelledPetriNet::new();
        let p0 = slpn.add_place();
        
        // Add initial token
        slpn.get_initial_marking_mut().increase(p0, 1).unwrap();
        
        // Add self-loop transition
        let activity = slpn.activity_key.process_activity("a");
        let t0 = slpn.add_transition(Some(activity), Fraction::from((1, 1)));
        slpn.add_place_transition_arc(p0, t0, 1).unwrap();
        slpn.add_transition_place_arc(t0, p0, 1).unwrap();
        
        let delta = Fraction::from((1, 1000));
        
        // Apply patch multiple times
        let patched1 = patch_livelocks(&slpn, delta.clone()).unwrap();
        let patched2 = patch_livelocks(&slpn, delta.clone()).unwrap();
        let patched3 = patch_livelocks(&slpn, delta).unwrap();
        
        // Results should be identical (same number of transitions/places)
        assert_eq!(patched1.get_number_of_transitions(), patched2.get_number_of_transitions());
        assert_eq!(patched1.get_number_of_places(), patched2.get_number_of_places());
        assert_eq!(patched2.get_number_of_transitions(), patched3.get_number_of_transitions());
        assert_eq!(patched2.get_number_of_places(), patched3.get_number_of_places());
        
        // Verify deterministic behavior: same timeout transitions should be added
        assert_eq!(patched1.weights.len(), patched2.weights.len());
        assert_eq!(patched2.weights.len(), patched3.weights.len());
    }

    #[test]
    fn test_patch_livelocks_chained_sccs() {
        // Create a net with two chained livelock SCCs: A -> B where A and B are separate SCCs
        let mut slpn = StochasticLabelledPetriNet::new();
        
        // Places: p0 (A's place), p1 (B's place)
        let p0 = slpn.add_place();
        let p1 = slpn.add_place();
        
        // Initial marking: A has a token
        slpn.get_initial_marking_mut().increase(p0, 1).unwrap();
        
        // Transitions
        let act_a = slpn.activity_key.process_activity("a");
        let act_b = slpn.activity_key.process_activity("b");
        let act_ab = slpn.activity_key.process_activity("a_to_b");
        
        // A's self-loop (SCC 1): p0 -a-> p0
        let t_a = slpn.add_transition(Some(act_a), Fraction::from((1, 1)));
        slpn.add_place_transition_arc(p0, t_a, 1).unwrap();
        slpn.add_transition_place_arc(t_a, p0, 1).unwrap();
        
        // A to B transition: p0 -a_to_b-> p1
        let t_ab = slpn.add_transition(Some(act_ab), Fraction::from((1, 1)));
        slpn.add_place_transition_arc(p0, t_ab, 1).unwrap();
        slpn.add_transition_place_arc(t_ab, p1, 1).unwrap();
        
        // B's self-loop (SCC 2): p1 -b-> p1
        let t_b = slpn.add_transition(Some(act_b), Fraction::from((1, 1)));
        slpn.add_place_transition_arc(p1, t_b, 1).unwrap();
        slpn.add_transition_place_arc(t_b, p1, 1).unwrap();
        
        println!("Original transitions: {}, places: {}", slpn.get_number_of_transitions(), slpn.get_number_of_places());
        
        // Both initial state (A) and B state should be in livelock
        let initial_a = slpn.get_initial_state().unwrap();
        assert!(slpn.is_state_part_of_livelock(&initial_a).unwrap());
        
        // Create B state by executing the a_to_b transition from initial state
        let mut state_b = initial_a.clone();
        slpn.execute_transition(&mut state_b, t_ab).unwrap();
        assert!(slpn.is_state_part_of_livelock(&state_b).unwrap());
        
        // Apply patch
        let delta = Fraction::from((1, 1000));
        let patched = patch_livelocks(&slpn, delta).unwrap();
        
        println!("Patched transitions: {}, places: {}", patched.get_number_of_transitions(), patched.get_number_of_places());
        
        // Should add exactly 2 timeout transitions (one per SCC) + 1 sink place
        assert_eq!(patched.get_number_of_transitions(), slpn.get_number_of_transitions() + 2);
        assert_eq!(patched.get_number_of_places(), slpn.get_number_of_places() + 1);
        
        // Both SCCs should no longer be livelocked after patching
        let patched_initial = patched.get_initial_state().unwrap();
        assert!(!patched.is_state_part_of_livelock(&patched_initial).unwrap());
    }

    #[test]
    fn test_patch_livelocks_multi_state_scc() {
        // Create a net with one SCC containing multiple mutually reachable markings
        // Structure: p0 <-a-> p1 (forms one SCC with 2 markings)
        let mut slpn = StochasticLabelledPetriNet::new();
        
        let p0 = slpn.add_place();
        let p1 = slpn.add_place();
        
        // Initial marking: token in p0
        slpn.get_initial_marking_mut().increase(p0, 1).unwrap();
        
        let act_forward = slpn.activity_key.process_activity("forward");
        let act_backward = slpn.activity_key.process_activity("backward");
        
        // Forward transition: p0 -> p1
        let t_forward = slpn.add_transition(Some(act_forward), Fraction::from((1, 1)));
        slpn.add_place_transition_arc(p0, t_forward, 1).unwrap();
        slpn.add_transition_place_arc(t_forward, p1, 1).unwrap();
        
        // Backward transition: p1 -> p0
        let t_backward = slpn.add_transition(Some(act_backward), Fraction::from((1, 1)));
        slpn.add_place_transition_arc(p1, t_backward, 1).unwrap();
        slpn.add_transition_place_arc(t_backward, p0, 1).unwrap();
        
        println!("Multi-state SCC - Original transitions: {}, places: {}", slpn.get_number_of_transitions(), slpn.get_number_of_places());
        
        // Both states should be in livelock (they form one SCC)
        let state_p0 = slpn.get_initial_state().unwrap();
        assert!(slpn.is_state_part_of_livelock(&state_p0).unwrap());
        
        let mut state_p1 = state_p0.clone();
        slpn.execute_transition(&mut state_p1, t_forward).unwrap();
        assert!(slpn.is_state_part_of_livelock(&state_p1).unwrap());
        
        println!("Both states are in livelock: p0={}, p1={}", 
                 slpn.is_state_part_of_livelock(&state_p0).unwrap(),
                 slpn.is_state_part_of_livelock(&state_p1).unwrap());
        
        // Apply patch
        let delta = Fraction::from((1, 1000));
        let patched = patch_livelocks(&slpn, delta).unwrap();
        
        println!("Multi-state SCC - Patched transitions: {}, places: {}", patched.get_number_of_transitions(), patched.get_number_of_places());
        
        // Should add exactly 1 timeout transition (one per SCC) + 1 sink place
        // Even though the SCC has 2 markings, we should only add 1 timeout
        assert_eq!(patched.get_number_of_transitions(), slpn.get_number_of_transitions() + 1);
        assert_eq!(patched.get_number_of_places(), slpn.get_number_of_places() + 1);
        
        // Both states should no longer be livelocked after patching
        let patched_p0 = patched.get_initial_state().unwrap();
        assert!(!patched.is_state_part_of_livelock(&patched_p0).unwrap());
        
        // Test that we can still reach the other state and it's also not livelocked
        let mut patched_p1 = patched_p0.clone();
        patched.execute_transition(&mut patched_p1, t_forward).unwrap();
        assert!(!patched.is_state_part_of_livelock(&patched_p1).unwrap());
        
        println!("After patching - both states are no longer livelocked: p0={}, p1={}", 
                 !patched.is_state_part_of_livelock(&patched_p0).unwrap(),
                 !patched.is_state_part_of_livelock(&patched_p1).unwrap());
        
        // Assert sink place is reused -> every newly added tau must put its single token in "the same" sink
        let sink_candidates: Vec<_> = (0..patched.get_number_of_places())
            .filter(|p| patched.place2output_transitions[*p].is_empty()) // places with no outgoing arcs
            .collect();
        assert_eq!(sink_candidates.len(), 1, "exactly one global sink place expected");
    }

    #[test]
    fn test_patch_livelocks_idempotence() {
        // Create a simple livelock net: p0 -a-> p0 (self-loop)
        let mut slpn = StochasticLabelledPetriNet::new();
        let p0 = slpn.add_place();
        
        // Add initial token
        slpn.get_initial_marking_mut().increase(p0, 1).unwrap();
        
        // Add self-loop transition
        let activity = slpn.activity_key.process_activity("a");
        let t0 = slpn.add_transition(Some(activity), Fraction::from((1, 1)));
        slpn.add_place_transition_arc(p0, t0, 1).unwrap();
        slpn.add_transition_place_arc(t0, p0, 1).unwrap();
        
        let delta = Fraction::from((1, 1000));
        
        // Apply patch once
        let once = patch_livelocks(&slpn, delta.clone()).unwrap();
        
        // Apply patch twice (on already-patched net)
        let twice = patch_livelocks(&once, delta).unwrap();
        
        // Results should be identical - patching is idempotent
        assert_eq!(once.get_number_of_transitions(), twice.get_number_of_transitions());
        assert_eq!(once.get_number_of_places(), twice.get_number_of_places());
        assert_eq!(once.weights.len(), twice.weights.len());
        
        // Both should have no livelock states (because every SCC now has an escape)
        let initial_once = once.get_initial_state().unwrap();
        let initial_twice = twice.get_initial_state().unwrap();
        assert!(!once.is_state_part_of_livelock(&initial_once).unwrap());
        assert!(!twice.is_state_part_of_livelock(&initial_twice).unwrap());
    }
}
