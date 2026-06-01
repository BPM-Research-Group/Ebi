use ebi_objects::{
    AutomatonState, StochasticNondeterministicFiniteAutomaton, anyhow::Result, ebi_arithmetic::{Fraction, One}, ebi_objects::labelled_petri_net::TransitionIndex
};

use crate::stochastic_semantics::stochastic_semantics::StochasticSemantics;

impl StochasticSemantics for StochasticNondeterministicFiniteAutomaton {
    type StoSemState = AutomatonState;

    fn get_transition_weight(&self, state: &AutomatonState, transition: TransitionIndex) -> &Fraction {
        if transition == self.sources.len() {
            //terminating transition
            &self.get_termination_probability(*state)
        } else {
            &self.probabilities[transition]
        }
    }

    fn get_total_weight_of_enabled_transitions(&self, _: &AutomatonState) -> Result<Fraction> {
        Ok(Fraction::one())
    }
}
