use crate::{
    semantics::finite_stochastic_language_semantics::FiniteStochasticLanguageSemantics,
    stochastic_semantics::stochastic_semantics::StochasticSemantics,
};
use ebi_objects::{
    anyhow::{Result, anyhow},
    ebi_arithmetic::{Fraction, Zero},
    ebi_objects::labelled_petri_net::TransitionIndex,
};

impl StochasticSemantics for FiniteStochasticLanguageSemantics {
    type StoSemState = usize;

    fn get_transition_weight(
        &self,
        state: &usize,
        transition: TransitionIndex,
    ) -> Result<&Fraction> {
        let activity = self.transition_index_to_activity(transition);

        Ok(&self.nodes[*state]
            .get(&activity)
            .ok_or_else(|| anyhow!("Activity {:?} does not exist.", activity))?
            .1)
    }

    fn get_total_weight_of_enabled_transitions(&self, state: &usize) -> Result<Fraction> {
        let mut sum = Fraction::zero();
        for (_, (_, probability)) in &self.nodes[*state] {
            sum += probability;
        }
        if sum == Fraction::zero() {
            return Err(anyhow!("total enabled weight is 0"));
        }
        Ok(sum)
    }
}
