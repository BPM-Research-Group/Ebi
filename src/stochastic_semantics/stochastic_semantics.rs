use ebi_arithmetic::Fraction;
use ebi_objects::ebi_objects::labelled_petri_net::TransitionIndex;

use crate::semantics::semantics::Semantics;

pub trait StochasticSemantics:
    Semantics<SemState = <Self as StochasticSemantics>::StoSemState>
{
    type StoSemState;

    /**
     *
     * @param transition
     * @return the weight of the transition. This might depend on the state.
     */
    fn get_transition_weight(
        &self,
        state: &<Self as StochasticSemantics>::StoSemState,
        transition: TransitionIndex,
    ) -> &Fraction;

    /**
     *
     * @param enabledTransitions
     * @return the sum of the weight of the enabled transitions
     */
    fn get_total_weight_of_enabled_transitions(
        &self,
        state: &<Self as StochasticSemantics>::StoSemState,
    ) -> anyhow::Result<Fraction>;
}
