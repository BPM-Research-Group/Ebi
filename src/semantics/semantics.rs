use anyhow::Result;
use ebi_objects::{Activity, HasActivityKey, ebi_objects::labelled_petri_net::TransitionIndex};
use std::fmt::Debug;

use crate::{ebi_framework::displayable::Displayable, techniques::align::AlignmentHeuristics};

pub trait Semantics:
    Debug + Send + Sync + AlignmentHeuristics<AliState = Self::SemState> + HasActivityKey
{
    type SemState: Displayable;

    /// Returns the initial state, if it exists.
    /// If it does not exist, then the language is empty.
    fn get_initial_state(&self) -> Option<<Self as Semantics>::SemState>;

    /// Update the state to reflect execution of `transition`.
    /// May (up to the implementation) return an error when `transition` is not enabled, or when the marking cannot be represented (unbounded).
    /// This alters the state to avoid repeated memory allocations in simple walkthroughs.
    fn execute_transition(
        &self,
        state: &mut <Self as Semantics>::SemState,
        transition: TransitionIndex,
    ) -> Result<()>;

    /// Returns whether the current state is a final state. In a final state, no other transitions may be enabled.
    /// That is, if and only if `get_enabled_transitions(state)` returns an empty `Vec`, then `is_final_state(state)` returns `true`.
    /// Depending on the implementation, it may be more efficient ot call `is_final_state` rather than `get_enabled_transitions`.
    fn is_final_state(&self, state: &<Self as Semantics>::SemState) -> bool;

    fn is_transition_silent(&self, transition: TransitionIndex) -> bool;

    fn get_transition_activity(&self, transition: TransitionIndex) -> Option<Activity>;

    /// Returns the enabled transitions in `state`.
    /// If and only if there are no transitions enabled, then `is_final_state(state)` returns `true`.
    fn get_enabled_transitions(
        &self,
        state: &<Self as Semantics>::SemState,
    ) -> Vec<TransitionIndex>;

    fn get_number_of_transitions(&self) -> usize;
}
