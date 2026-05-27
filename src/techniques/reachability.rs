use std::collections::VecDeque;

use crate::ebi_framework::displayable::Displayable;
use ebi_objects::{StochasticDeterministicFiniteAutomaton, anyhow::Result};

pub trait IsReachable {
    type ReaState: Displayable;

    /// Returns a cache to compute whether states are reachable from the initial state.
    fn get_reachability_cache(&self) -> Box<dyn ReachabilityCache<ReaState = Self::ReaState> + '_>;
}

pub trait ReachabilityCache {
    type ReaState;

    /// Returns whether the given state is reachable from the initial state.
    fn is_state_reachable(&mut self, state: &Self::ReaState) -> Result<bool>;
}

impl IsReachable for StochasticDeterministicFiniteAutomaton {
    type ReaState = usize;

    fn get_reachability_cache(&self) -> Box<dyn ReachabilityCache<ReaState = Self::ReaState> + '_> {
        Box::new(StochasticDeterministicFiniteAutomatonReachabilityCache::new(self))
    }
}

pub struct StochasticDeterministicFiniteAutomatonReachabilityCache {
    reachable: Vec<bool>,
}

impl StochasticDeterministicFiniteAutomatonReachabilityCache {
    fn new(sdfa: &StochasticDeterministicFiniteAutomaton) -> Self {
        let mut reachable = vec![false; sdfa.number_of_states()];

        if let Some(initial_state) = sdfa.get_initial_state() {
            let mut queue = VecDeque::new();
            queue.push_back(initial_state);
            while let Some(state) = queue.pop_front() {
                reachable[state] = true;

                let (_, mut transition) = sdfa.binary_search(state, 0);
                while transition < sdfa.sources.len() && sdfa.sources[transition] == state {
                    //found a neighbour
                    let neighbour = sdfa.targets[transition];
                    if !reachable[neighbour] {
                        queue.push_back(neighbour);
                        reachable[neighbour] = true;
                    }

                    transition += 1;
                }
            }
        }

        Self { reachable }
    }
}

impl ReachabilityCache for StochasticDeterministicFiniteAutomatonReachabilityCache {
    type ReaState = usize;

    fn is_state_reachable(&mut self, state: &Self::ReaState) -> Result<bool> {
        Ok(self.reachable[*state])
    }
}
