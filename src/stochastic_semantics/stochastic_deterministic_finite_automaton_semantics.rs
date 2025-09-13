use anyhow::Result;
use ebi_arithmetic::{Fraction, One};
use ebi_objects::{
    StochasticDeterministicFiniteAutomaton, ebi_objects::labelled_petri_net::TransitionIndex,
};

use crate::stochastic_semantics::stochastic_semantics::StochasticSemantics;

impl StochasticSemantics for StochasticDeterministicFiniteAutomaton {
    type StoSemState = usize;

    fn get_transition_weight(&self, state: &usize, transition: TransitionIndex) -> &Fraction {
        if transition == self.get_number_of_transitions() {
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
