use crate::stochastic_semantics::stochastic_semantics::StochasticSemantics;
use ebi_objects::{
    AutomatonState, StochasticAutomatonSemantics, StochasticDeterministicFiniteAutomaton,
    anyhow::{Result, anyhow},
    ebi_arithmetic::Fraction,
    ebi_objects::labelled_petri_net::TransitionIndex,
};

impl StochasticSemantics for StochasticDeterministicFiniteAutomaton {
    type StoSemState = AutomatonState;

    fn get_transition_weight(
        &self,
        state: &AutomatonState,
        transition: TransitionIndex,
    ) -> Result<&Fraction> {
        StochasticAutomatonSemantics::transition_2_weight(self, *state, transition).ok_or_else(
            || {
                anyhow!(
                    "Transition {} does not exist or is not enabled.",
                    transition
                )
            },
        )
    }

    fn get_total_weight_of_enabled_transitions(&self, state: &AutomatonState) -> Result<Fraction> {
        Ok(StochasticAutomatonSemantics::outgoing_transitions_weight_sum(self, *state))
    }
}
