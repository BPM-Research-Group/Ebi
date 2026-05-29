use crate::semantics::semantics::Semantics;
use ebi_objects::{
    Activity, AutomatonSemantics, AutomatonState, DirectlyFollowsGraph,
    anyhow::{Result, anyhow},
    ebi_objects::labelled_petri_net::TransitionIndex,
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
        self.initial_state()
    }

    fn execute_transition(
        &self,
        state: &mut <Self as Semantics>::SemState,
        transition: TransitionIndex,
    ) -> Result<()> {
        *state = self
            .transition_2_target(transition)
            .ok_or_else(|| anyhow!("Non-existing state."))?;
        Ok(())
    }

    fn is_final_state(&self, state: &<Self as Semantics>::SemState) -> bool {
        AutomatonSemantics::is_state_final(self, *state)
    }

    fn is_transition_silent(
        &self,
        transition: TransitionIndex,
        _state: &<Self as Semantics>::SemState,
    ) -> bool {
        AutomatonSemantics::transition_2_activity(self, transition).is_none()
    }

    fn get_transition_activity(
        &self,
        transition: TransitionIndex,
        _state: &<Self as Semantics>::SemState,
    ) -> Option<Activity> {
        AutomatonSemantics::transition_2_activity(self, transition)
    }

    fn get_enabled_transitions(
        &self,
        state: &<Self as Semantics>::SemState,
    ) -> Vec<TransitionIndex> {
        self.outgoing_transitions(*state)
    }

    fn number_of_transitions(&self, _state: &<Self as Semantics>::SemState) -> usize {
        AutomatonSemantics::number_of_transitions(self)
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
