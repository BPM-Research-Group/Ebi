use crate::stochastic_semantics::stochastic_semantics::StochasticSemantics;
use anyhow::Result;
use ebi_objects::{
    StochasticProcessTree,
    ebi_arithmetic::Fraction,
    ebi_objects::{
        labelled_petri_net::TransitionIndex,
        process_tree::TreeMarking,
        stochastic_process_tree::{get_total_weight_of_enabled_transitions, get_transition_weight},
    },
};

impl StochasticSemantics for StochasticProcessTree {
    type StoSemState = TreeMarking;

    fn get_transition_weight(
        &self,
        state: &<Self as StochasticSemantics>::StoSemState,
        transition: TransitionIndex,
    ) -> &Fraction {
        get_transition_weight(self, state, transition)
    }

    fn get_total_weight_of_enabled_transitions(
        &self,
        state: &<Self as StochasticSemantics>::StoSemState,
    ) -> Result<Fraction> {
        Ok(get_total_weight_of_enabled_transitions(self, state))
    }
}
