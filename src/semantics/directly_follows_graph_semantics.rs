use crate::semantics::semantics::Semantics;
use ebi_objects::{
    Activity, AutomatonState, DirectlyFollowsGraph, anyhow::Result,
    ebi_arithmetic::ebi_number::Signed, ebi_objects::labelled_petri_net::TransitionIndex,
};

/**
 * States:
 * Initial state = nodes[len]
 * Final state = nodes[len] + 1. A final state is reached after executing a silent step from an end activity node.
 *
 * Transitions:
 * End activity -> final state = edges[len]
 * initial state -> start activity = edges[len] + 1 + ...
 */
impl Semantics for DirectlyFollowsGraph {
    type SemState = AutomatonState;

    fn get_initial_state(&self) -> Option<<Self as Semantics>::SemState> {
        if !self.has_empty_traces() && !self.start_activities.iter().any(|(_, w)| w.is_positive()) {
            None
        } else {
            Some(AutomatonState::of(self.state_2_activity.len()))
        }
    }

    fn execute_transition(
        &self,
        state: &mut <Self as Semantics>::SemState,
        transition: TransitionIndex,
    ) -> Result<()> {
        if transition == self.sources.len() {
            //end
            *state = AutomatonState::of(self.activity_key.next_index + 1);
        } else if transition < self.sources.len() {
            //edge
            *state = AutomatonState::of(self.targets[transition].0);
        } else {
            //start
            *state = AutomatonState::of(transition - (self.sources.len() + 1));
        }
        Ok(())
    }

    fn is_final_state(&self, state: &<Self as Semantics>::SemState) -> bool {
        state.0 > self.state_2_activity.len()
    }

    fn is_transition_silent(
        &self,
        transition: TransitionIndex,
        _state: &<Self as Semantics>::SemState,
    ) -> bool {
        transition == self.sources.len()
    }

    fn get_transition_activity(
        &self,
        transition: TransitionIndex,
        _state: &<Self as Semantics>::SemState,
    ) -> Option<Activity> {
        if transition == self.sources.len() {
            //end
            None
        } else if transition < self.sources.len() {
            //edge
            let node = transition;
            Some(self.state_2_activity[self.targets[node].0])
        } else {
            //start
            let node = transition - (self.sources.len() + 1);
            Some(self.state_2_activity[node])
        }
    }

    fn get_enabled_transitions(
        &self,
        state: &<Self as Semantics>::SemState,
    ) -> Vec<TransitionIndex> {
        if state.0 == self.state_2_activity.len() {
            //we are in the initial state
            let mut result = self
                .start_activities
                .iter()
                .filter_map(|(node, w)| {
                    if w.is_positive() {
                        Some(self.sources.len() + 1 + node.0)
                    } else {
                        None
                    }
                })
                .collect::<Vec<_>>();

            if self.empty_traces_weight.is_positive() {
                result.push(self.sources.len())
            }

            result
        } else if state.0 > self.state_2_activity.len() {
            //we are in the final state
            vec![]
        } else {
            //we are not in the initial state
            let mut result = vec![];

            //add edges
            let (_, mut i) = self.binary_search(*state, AutomatonState::zero());
            while i < self.sources.len() && &self.sources[i] == state {
                if self.weights[i].is_positive() {
                    result.push(i);
                }
                i += 1;
            }

            //add transition to final state
            if self.is_end_node(self.state_2_activity[state]) {
                result.push(self.sources.len())
            }

            result
        }
    }

    fn number_of_transitions(&self, _state: &<Self as Semantics>::SemState) -> usize {
        self.sources.len() + 1 + self.sources.len()
    }
}

#[cfg(test)]
mod tests {
    use crate::semantics::semantics::Semantics;
    use ebi_objects::{DirectlyFollowsGraph, HasActivityKey};
    use std::fs;

    #[cfg(test)]
    macro_rules! assert_execute_expect {
        ($tree:ident, $state:ident, $t:expr, $e:expr) => {
            println!("execute {} {}", ::std::stringify!($t), $t);
            assert!($tree.get_enabled_transitions(&$state).contains(&$t));
            $tree.execute_transition(&mut $state, $t).unwrap();
            println!("state {}\n", $state);
            assert_eq!($tree.get_enabled_transitions(&$state), $e);
        };
    }

    #[test]
    fn dfg_semantics() {
        let fin = fs::read_to_string("testfiles/bpic12-a.xes.gz-dfg.dfg").unwrap();
        let mut dfg = fin.parse::<DirectlyFollowsGraph>().unwrap();

        // let fin2 = fs::read_to_string("testfiles/bpic12-a-sample.slang").unwrap();
        // let mut slang = fin2.parse::<FiniteStochasticLanguage>().unwrap();

        // dfg.translate_using_activity_key(slang.activity_key_mut());

        let a_submitted = dfg.activity_key_mut().process_activity("A_SUBMITTED");
        let a_partlysubmitted = dfg.activity_key_mut().process_activity("A_PARTLYSUBMITTED");

        println!("{:?}", dfg.activity_key);
        println!("node_2_activity  {:?}", dfg.state_2_activity);
        println!("start activities {:?}", dfg.start_activities);

        let mut state = dfg.get_initial_state().unwrap();
        println!("state {}\n", state);
        assert_eq!(dfg.get_enabled_transitions(&state), [16]);
        assert_eq!(
            dfg.get_transition_activity(16, &state).unwrap(),
            a_submitted
        );

        assert_execute_expect!(dfg, state, 16, [0]);
        assert_eq!(
            dfg.get_transition_activity(0, &state).unwrap(),
            a_partlysubmitted
        );
    }
}
