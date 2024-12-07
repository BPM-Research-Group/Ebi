use crate::{ebi_framework::activity_key::Activity, ebi_traits::{ebi_trait_semantics::Semantics, ebi_trait_stochastic_semantics::TransitionIndex}};

use super::directly_follows_model::DirectlyFollowsModel;

impl Semantics for DirectlyFollowsModel {
    /**
     * Map of states: 
     * 0..nodes:        after executing the activity, we end up in this state.
     * nodes:           end
     * nodes + 1:       start
     * 
     * Map of transitions:
     * 0..nodes:        the activity
     * nodes:           terminate
     */
    type SemState = usize;

    fn get_initial_state(&self) -> <Self as Semantics>::SemState {
        self.get_number_of_nodes() + 1
    }

    fn execute_transition(&self, state: &mut <Self as Semantics>::SemState, transition: TransitionIndex) -> anyhow::Result<()> {
        *state = transition;
        Ok(())
    }

    fn is_final_state(&self, state: &<Self as Semantics>::SemState) -> bool {
        state == &self.get_number_of_nodes()
    }

    fn is_transition_silent(&self, transition: TransitionIndex) -> bool {
        return transition == self.get_number_of_nodes()
    }

    fn get_transition_activity(&self, transition: TransitionIndex) -> Option<Activity> {
        if transition > self.get_number_of_nodes() {
            None
        } else {
            self.node_2_activity.get(transition).copied()
        }
    }

    fn get_enabled_transitions(&self, state: &<Self as Semantics>::SemState) -> Vec<TransitionIndex> {

        if state == &self.get_initial_state() {
            println!("state {} initial state", state);
            //initial state

            //start in start node
            let mut result = self.start_nodes.iter().cloned().collect::<Vec<_>>();

            //terminate from initial state
            if self.empty_traces {
                result.push(self.get_number_of_nodes());
            }

            result
        } else if state < &self.get_number_of_nodes() {
            println!("state {} node state", state);
            //non-initial state

            //outgoing edges
            let mut result = self.edges[*state].iter().enumerate().filter_map(|(node, value)| if *value {Some(node)} else {None}).collect::<Vec<_>>();

            //terminate from end node
            if self.end_nodes.contains(state) {
                result.push(self.get_number_of_nodes());
            }

            result
        } else {
            println!("state {} final state", state);
            //final state
            vec![]
        }
    }

    fn get_number_of_transitions(&self) -> usize {
        self.get_number_of_nodes() + 1
    }
}