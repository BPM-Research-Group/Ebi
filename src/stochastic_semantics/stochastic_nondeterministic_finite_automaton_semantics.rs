use anyhow::Result;
use ebi_objects::{
    StochasticNondeterministicFiniteAutomaton,
    ebi_arithmetic::{Fraction, One},
    ebi_objects::labelled_petri_net::TransitionIndex,
};

use crate::stochastic_semantics::stochastic_semantics::StochasticSemantics;

impl StochasticSemantics for StochasticNondeterministicFiniteAutomaton {
    type StoSemState = usize;

    fn get_transition_weight(&self, state: &usize, transition: TransitionIndex) -> &Fraction {
        if transition == self.number_of_transitions() {
            //terminating transition
            &self.get_termination_probability(*state)
        } else {
            &self.get_probabilities()[transition]
        }
    }

    fn get_total_weight_of_enabled_transitions(&self, _: &usize) -> Result<Fraction> {
        Ok(Fraction::one())
    }
}
