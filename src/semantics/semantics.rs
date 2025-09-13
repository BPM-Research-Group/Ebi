use anyhow::Result;
use ebi_objects::{Activity, HasActivityKey, ebi_objects::labelled_petri_net::TransitionIndex};
use std::fmt::Debug;

use crate::{ebi_framework::displayable::Displayable, techniques::align::AlignmentHeuristics};

pub trait Semantics:
    Debug + Send + Sync + AlignmentHeuristics<AliState = Self::SemState> + HasActivityKey
{
    type SemState: Displayable;

    /**
     * Get the initial state.
     * If it does not exist, then the language is empty.
     */
    fn get_initial_state(&self) -> Option<<Self as Semantics>::SemState>;

    /**
     * Update the state to reflect execution of the transition. This alters the state to avoid repeated memory allocations in simple walkthroughs.
     * May return an error when the transition is not enabled, or when the marking cannot be represented (unbounded).
     *
     * @param transition
     */
    fn execute_transition(
        &self,
        state: &mut <Self as Semantics>::SemState,
        transition: TransitionIndex,
    ) -> Result<()>;

    /**
     *
     * @return whether the current state is a final state. In a final state, no other transitions may be enabled.
     */
    fn is_final_state(&self, state: &<Self as Semantics>::SemState) -> bool;

    fn is_transition_silent(&self, transition: TransitionIndex) -> bool;

    fn get_transition_activity(&self, transition: TransitionIndex) -> Option<Activity>;

    fn get_enabled_transitions(
        &self,
        state: &<Self as Semantics>::SemState,
    ) -> Vec<TransitionIndex>;

    fn get_number_of_transitions(&self) -> usize;
}
