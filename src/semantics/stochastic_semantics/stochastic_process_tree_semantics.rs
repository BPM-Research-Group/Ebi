use crate::semantics::stochastic_semantics::stochastic_semantics_trait::StochasticSemantics;
use ebi_objects::{
    anyhow::{anyhow, Result},
    ebi_arithmetic::Fraction,
    ebi_objects::{
        labelled_petri_net::TransitionIndex,
        stochastic_process_tree::{get_total_weight_of_enabled_transitions, get_transition_weight},
    },
    StochasticProcessTree, TreeMarking,
};

impl StochasticSemantics for StochasticProcessTree {
    type StoSemState = TreeMarking;

    fn get_transition_weight(
        &self,
        state: &<Self as StochasticSemantics>::StoSemState,
        transition: TransitionIndex,
    ) -> Result<&Fraction> {
        get_transition_weight(self, state, transition).ok_or_else(|| {
            anyhow!(
                "Transition {} does not exist or is not enabled.",
                transition
            )
        })
    }

    fn get_total_weight_of_enabled_transitions(
        &self,
        state: &<Self as StochasticSemantics>::StoSemState,
    ) -> Result<Fraction> {
        Ok(get_total_weight_of_enabled_transitions(self, state))
    }
}
