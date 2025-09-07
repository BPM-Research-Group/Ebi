use ebi_arithmetic::ebi_number::Signed;
use ebi_objects::{ebi_objects::labelled_petri_net::TransitionIndex, Activity, DirectlyFollowsGraph};

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
impl Semantics for DirectlyFollowsGraph {
    type SemState = usize;

    fn get_initial_state(&self) -> Option<<Self as Semantics>::SemState> {
        if !self.has_empty_traces() && !self.start_activities.iter().any(|(_, w)| w.is_positive()) {
            None
        } else {
            Some(self.activity_key.get_number_of_activities())
        }
    }

    fn execute_transition(
        &self,
        state: &mut <Self as Semantics>::SemState,
        transition: TransitionIndex,
    ) -> anyhow::Result<()> {
        if transition == self.sources.len() {
            //end
            *state = self.activity_key.get_number_of_activities() + 1
        } else if transition < self.sources.len() {
            //edge
            *state = self
                .activity_key
                .get_id_from_activity(self.targets[transition]);
        } else {
            //start
            *state = transition - (self.sources.len() + 1);
        }
        Ok(())
    }

    fn is_final_state(&self, state: &<Self as Semantics>::SemState) -> bool {
        state > &self.activity_key.get_number_of_activities()
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
            Some(self.targets[node])
        } else {
            //start
            let node = transition - (self.sources.len() + 1);
            Some(self.activity_key.get_activity_by_id(node))
        }
    }

    fn get_enabled_transitions(
        &self,
        state: &<Self as Semantics>::SemState,
    ) -> Vec<TransitionIndex> {
        if state == &self.activity_key.get_number_of_activities() {
            //we are in the initial state
            let mut result = self
                .start_activities
                .iter()
                .filter_map(|(a, w)| {
                    if w.is_positive() {
                        Some(self.activity_key.get_id_from_activity(a))
                    } else {
                        None
                    }
                })
                .collect::<Vec<_>>();

            if self.empty_traces_weight.is_positive() {
                result.push(self.sources.len())
            }

            result
        } else if state > &self.activity_key.get_number_of_activities() {
            //we are in the final state
            vec![]
        } else {
            let node = self.activity_key.get_activity_by_id(*state);
            //we are not in the initial state
            let mut result = vec![];

            //add edges
            let (_, mut i) = self.binary_search(node, self.activity_key.get_activity_by_id(0));
            while i < self.sources.len() && self.sources[i] == node {
                if self.weights[i].is_positive() {
                    result.push(i);
                }
                i += 1;
            }

            //add transition to final state
            if self.is_end_node(node) {
                result.push(self.sources.len())
            }

            result
        }
    }

    fn get_number_of_transitions(&self) -> usize {
        self.sources.len() + 1 + self.sources.len()
    }
}
