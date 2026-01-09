use crate::semantics::semantics::Semantics;
use ebi_objects::{ebi_arithmetic::Fraction, ebi_objects::labelled_petri_net::TransitionIndex};

pub trait StochasticPartiallyOrderedSemantics:
    Semantics<SemState = <Self as StochasticPartiallyOrderedSemantics>::StoPOSemState>
{
    type StoPOSemState;

    /// Returns the probabilistic penalty of firing the transition when in the state.
    /// Returns a value between Some(0) (extremely large penalty) and Some(1) (no penalty).
    /// The value Some(1) will not be returned (in exact mode) and is instead represented by None.
    fn get_transition_probabilistic_penalty(
        &self,
        state: &<Self as StochasticPartiallyOrderedSemantics>::StoPOSemState,
        transition: TransitionIndex,
    ) -> Option<&Fraction>;
}
