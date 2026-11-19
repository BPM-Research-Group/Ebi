use crate::semantics::semantics::Semantics;
use ebi_objects::{ebi_arithmetic::Fraction, ebi_objects::labelled_petri_net::TransitionIndex};

pub trait StochasticPartiallyOrderedSemantics:
    Semantics<SemState = <Self as StochasticPartiallyOrderedSemantics>::StoPOSemState>
{
    type StoPOSemState;

    /// Returns the probabilistic penalty of firing the transition when in the state.
    /// Returns a value between 0 (infinitely large penalty) and 1 (no penalty).
    /// Returns None if the transition does not exist.
    fn get_transition_probabilistic_penalty(
        &self,
        state: &<Self as StochasticPartiallyOrderedSemantics>::StoPOSemState,
        transition: TransitionIndex,
    ) -> Option<Fraction>;
}
