use anyhow::anyhow;
use ebi_arithmetic::{Fraction, Zero};
use ebi_objects::StochasticLabelledPetriNet;

use crate::{
    semantics::labelled_petri_net_semantics::LPNMarking,
    stochastic_semantics::stochastic_semantics::StochasticSemantics,
};

impl StochasticSemantics for StochasticLabelledPetriNet {
    type StoSemState = LPNMarking;

    fn get_transition_weight(&self, _state: &LPNMarking, transition: usize) -> &Fraction {
        &self.weights[transition]
    }

    fn get_total_weight_of_enabled_transitions(
        &self,
        state: &LPNMarking,
    ) -> anyhow::Result<Fraction> {
        let mut sum = Fraction::zero();
        for index in state.enabled_transitions.iter_ones() {
            sum += &self.weights[index];
        }
        if sum.is_zero() {
            return Err(anyhow!("total enabled weight is 0"));
        }
        Ok(sum)
    }
}
