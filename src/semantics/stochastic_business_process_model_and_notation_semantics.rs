use crate::semantics::semantics::Semantics;
use ebi_objects::{
    Activity, StochasticBusinessProcessModelAndNotation, anyhow::Result,
    ebi_bpmn::semantics::BPMNMarking, ebi_objects::labelled_petri_net::TransitionIndex,
};

impl Semantics for StochasticBusinessProcessModelAndNotation {
    type SemState = BPMNMarking;

    fn get_initial_state(&self) -> Option<<Self as Semantics>::SemState> {
        Some(self.get_initial_marking().unwrap())
    }

    fn execute_transition(
        &self,
        state: &mut <Self as Semantics>::SemState,
        transition: TransitionIndex,
    ) -> Result<()> {
        StochasticBusinessProcessModelAndNotation::execute_transition(&self, state, transition)
    }

    fn is_final_state(&self, state: &<Self as Semantics>::SemState) -> bool {
        self.is_final_marking(state).unwrap()
    }

    fn is_transition_silent(
        &self,
        transition: TransitionIndex,
        state: &<Self as Semantics>::SemState,
    ) -> bool {
        StochasticBusinessProcessModelAndNotation::is_transition_silent(&self, transition, state)
    }

    fn get_transition_activity(
        &self,
        transition: TransitionIndex,
        state: &<Self as Semantics>::SemState,
    ) -> Option<Activity> {
        StochasticBusinessProcessModelAndNotation::get_transition_activity(&self, transition, state)
    }

    fn get_enabled_transitions(
        &self,
        state: &<Self as Semantics>::SemState,
    ) -> Vec<TransitionIndex> {
        StochasticBusinessProcessModelAndNotation::get_enabled_transitions(&self, state).unwrap()
    }

    fn number_of_transitions(&self, state: &<Self as Semantics>::SemState) -> usize {
        StochasticBusinessProcessModelAndNotation::number_of_transitions(self, state)
    }
}
