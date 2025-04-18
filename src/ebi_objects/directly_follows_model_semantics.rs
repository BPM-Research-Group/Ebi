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

    fn get_initial_state(&self) -> Option<<Self as Semantics>::SemState> {
        if self.start_nodes.is_empty() {
            None
        } else {
            Some(self.get_number_of_nodes() + 1)
        }
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

        if state == &self.get_initial_state().unwrap() {
            //initial state

            //start in start node
            let mut result = self.start_nodes.iter().cloned().collect::<Vec<_>>();

            //terminate from initial state
            if self.empty_traces {
                result.push(self.get_number_of_nodes());
            }

            result
        } else if state < &self.get_number_of_nodes() {
            //non-initial state

            //outgoing edges
            let mut result = self.edges[*state].iter().enumerate().filter_map(|(node, value)| if *value {Some(node)} else {None}).collect::<Vec<_>>();

            //terminate from end node
            if self.end_nodes.contains(state) {
                result.push(self.get_number_of_nodes());
            }

            result
        } else {
            //final state
            vec![]
        }
    }

    fn get_number_of_transitions(&self) -> usize {
        self.get_number_of_nodes() + 1
    }
}

#[cfg(test)]
mod tests {
    use std::fs;

    use crate::{ebi_objects::directly_follows_model::DirectlyFollowsModel, ebi_traits::ebi_trait_semantics::Semantics};

    #[test]
    fn dfm_semantics() {
        let fin = fs::read_to_string("testfiles/a-b_star.dfm").unwrap();
        let dfm = fin.parse::<DirectlyFollowsModel>().unwrap();

        assert_eq!(dfm.get_number_of_transitions(), 3);

        let mut state = dfm.get_initial_state().unwrap();

        assert_eq!(dfm.get_enabled_transitions(&state), vec![0]);
        assert!(!dfm.is_transition_silent(1));

        dfm.execute_transition(&mut state, 0).unwrap();

        assert_eq!(dfm.get_enabled_transitions(&state), vec![1]);
        assert!(!dfm.is_final_state(&state));

        dfm.execute_transition(&mut state, 1).unwrap();

        assert_eq!(dfm.get_enabled_transitions(&state), vec![1, 2]);

        dfm.execute_transition(&mut state, 1).unwrap();

        assert_eq!(dfm.get_enabled_transitions(&state), vec![1, 2]);

        dfm.execute_transition(&mut state, 2).unwrap();
        let empty: Vec<usize> = vec![];
        assert_eq!(dfm.get_enabled_transitions(&state), empty);
        assert!(dfm.is_transition_silent(2));
        assert_eq!(dfm.get_transition_activity(2), None);
        assert_eq!(dfm.get_transition_activity(3), None);
        assert!(dfm.is_final_state(&state));
    }

    #[test]
    fn dfm_empty_traces() {
        let fin = fs::read_to_string("testfiles/a-b_star_empty.dfm").unwrap();
        let dfm = fin.parse::<DirectlyFollowsModel>().unwrap();

        let state = dfm.get_initial_state().unwrap();
        assert_eq!(dfm.get_enabled_transitions(&state), vec![0, 2]);
    }

    #[test]
    fn dfm_empty() {
        let fin = fs::read_to_string("testfiles/empty.dfm").unwrap();
        let dfm = fin.parse::<DirectlyFollowsModel>().unwrap();

        assert!(dfm.get_initial_state().is_none());
    }
}