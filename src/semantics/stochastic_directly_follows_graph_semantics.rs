use ebi_objects::{
    Activity, DirectlyFollowsModel, StochasticDirectlyFollowsModel,
    ebi_objects::labelled_petri_net::TransitionIndex,
};

use crate::semantics::semantics::Semantics;

/**
 * States:
 * Initial state = nodes[len]
 * Final state = nodes[len] + 1. A final state is reached after executing a silent step from an end activity node.
 *
 * Transitions:
 * End activity -> final state = edges[len]
 * initial state -> start activity = edges[len] + 1 ...
 */
macro_rules! semantics_for_automaton {
    ($t:ident) => {
        impl Semantics for $t {
            type SemState = usize;

            fn get_initial_state(&self) -> Option<<Self as Semantics>::SemState> {
                if self.node_2_activity.is_empty() && !self.has_empty_traces() {
                    None
                } else {
                    Some(self.node_2_activity.len())
                }
            }

            fn execute_transition(
                &self,
                state: &mut <Self as Semantics>::SemState,
                transition: TransitionIndex,
            ) -> anyhow::Result<()> {
                if transition == self.sources.len() {
                    //end
                    *state = self.node_2_activity.len() + 1
                } else if transition < self.sources.len() {
                    //edge
                    *state = self.targets[transition];
                } else {
                    //start
                    *state = transition - (self.sources.len() + 1);
                }
                Ok(())
            }

            fn is_final_state(&self, state: &<Self as Semantics>::SemState) -> bool {
                state > &self.node_2_activity.len()
            }

            fn is_transition_silent(&self, transition: TransitionIndex) -> bool {
                transition == self.sources.len()
            }

            fn get_transition_activity(&self, transition: TransitionIndex) -> Option<Activity> {
                if transition == self.sources.len() {
                    //end
                    None
                } else if transition < self.sources.len() {
                    //edge
                    let node = transition;
                    Some(self.node_2_activity[self.targets[node]])
                } else {
                    //start
                    let node = transition - (self.sources.len() + 1);
                    Some(self.node_2_activity[node])
                }
            }

            fn get_enabled_transitions(
                &self,
                state: &<Self as Semantics>::SemState,
            ) -> Vec<TransitionIndex> {
                if state == &self.node_2_activity.len() {
                    //we are in the initial state
                    let mut result = vec![];
                    for node in 0..self.node_2_activity.len() {
                        if self.is_start_node(node) {
                            result.push(self.sources.len() + 1 + node)
                        }
                    }

                    if self.has_empty_traces() {
                        result.push(self.sources.len())
                    }

                    result
                } else if state > &self.node_2_activity.len() {
                    //we are in the final state
                    vec![]
                } else {
                    let node = *state;
                    //we are not in the initial state
                    let mut result = vec![];

                    //add edges
                    let (_, mut i) = self.binary_search(node, 0);
                    while i < self.sources.len() && self.sources[i] == node {
                        if self.can_execute_edge(i) {
                            result.push(i);
                        }
                        i += 1;
                    }

                    //add transition to final state
                    if self.is_end_node(node) && self.can_terminate_in_node(node) {
                        result.push(self.sources.len())
                    }

                    result
                }
            }

            fn get_number_of_transitions(&self) -> usize {
                self.sources.len() + 1 + self.sources.len()
            }
        }
    };
}
semantics_for_automaton!(DirectlyFollowsModel);
semantics_for_automaton!(StochasticDirectlyFollowsModel);
