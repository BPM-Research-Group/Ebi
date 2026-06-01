use crate::{ebi_framework::displayable::Displayable, semantics::semantics::Semantics};
use ebi_objects::{
    AutomatonSemantics, AutomatonState, DeterministicFiniteAutomaton, DirectlyFollowsGraph,
    StochasticDeterministicFiniteAutomaton, StochasticNondeterministicFiniteAutomaton,
    anyhow::Result,
};
use std::collections::VecDeque;

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

macro_rules! aut {
    ($t:ty, $c:ident) => {
        impl IsReachable for $t {
            type ReaState = AutomatonState;

            fn get_reachability_cache(
                &self,
            ) -> Box<dyn ReachabilityCache<ReaState = Self::ReaState> + '_> {
                Box::new($c::new(self))
            }
        }

        pub struct $c {
            reachable: Vec<bool>,
        }

        impl $c {
            fn new(sdfa: &$t) -> Self {
                let mut reachable = vec![false; sdfa.number_of_states()];

                if let Some(initial_state) = sdfa.get_initial_state() {
                    let mut queue = VecDeque::new();
                    queue.push_back(initial_state);
                    while let Some(state) = queue.pop_front() {
                        reachable[state] = true;
                        for transition in sdfa.outgoing_transitions(state) {
                            //found a neighbour
                            let neighbour = sdfa.transition_2_target(transition).unwrap();
                            if !reachable[neighbour] {
                                queue.push_back(neighbour);
                                reachable[neighbour] = true;
                            }
                        }
                    }
                }

                Self { reachable }
            }
        }

        impl ReachabilityCache for $c {
            type ReaState = AutomatonState;

            fn is_state_reachable(&mut self, state: &Self::ReaState) -> Result<bool> {
                Ok(self.reachable[*state])
            }
        }
    };
}

aut!(
    DeterministicFiniteAutomaton,
    DeterministicFiniteAutomatonReachabilityCache
);
aut!(
    StochasticDeterministicFiniteAutomaton,
    StochasticDeterministicFiniteAutomatonReachabilityCache
);
aut!(DirectlyFollowsGraph, DirectlyFollowsGraphReachabilityCache);
aut!(
    StochasticNondeterministicFiniteAutomaton,
    StochasticNondeterministicFiniteAutomatonReachabilityCache
);
