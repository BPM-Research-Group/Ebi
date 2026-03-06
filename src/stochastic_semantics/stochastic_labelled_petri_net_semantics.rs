use crate::{
    semantics::labelled_petri_net_semantics::LPNMarking,
    stochastic_semantics::stochastic_semantics::StochasticSemantics,
};
use ebi_objects::{
    StochasticLabelledPetriNet,
    anyhow::{Result, anyhow},
    ebi_arithmetic::{Fraction, Zero},
};

impl StochasticSemantics for StochasticLabelledPetriNet {
    type StoSemState = LPNMarking;

    fn get_transition_weight(&self, _state: &LPNMarking, transition: usize) -> &Fraction {
        &self.weights[transition]
    }

    fn get_total_weight_of_enabled_transitions(&self, state: &LPNMarking) -> Result<Fraction> {
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
