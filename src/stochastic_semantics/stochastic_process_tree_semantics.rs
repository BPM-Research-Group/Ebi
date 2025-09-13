use ebi_arithmetic::{Fraction, Zero};
use ebi_objects::{StochasticProcessTree, ebi_objects::labelled_petri_net::TransitionIndex};
use anyhow::Result;
use crate::{
    semantics::{process_tree_semantics::NodeStates, stochastic_process_tree_semantics::{can_execute, can_terminate}},
    stochastic_semantics::stochastic_semantics::StochasticSemantics,
};

impl StochasticSemantics for StochasticProcessTree {
    type StoSemState = NodeStates;

    fn get_transition_weight(
        &self,
        _state: &<Self as StochasticSemantics>::StoSemState,
        transition: TransitionIndex,
    ) -> &Fraction {
        if transition < self.transition2node.len() {
            &self.weights[transition]
        } else {
            &self.termination_weight
        }
    }

    fn get_total_weight_of_enabled_transitions(
        &self,
        state: &<Self as StochasticSemantics>::StoSemState,
    ) -> Result<Fraction> {
        let mut sum = if !state.terminated && can_terminate(self, state, self.root()) {
            self.termination_weight.clone()
        } else {
            Fraction::zero()
        };

        for (transition, node) in self.transition2node.iter().enumerate() {
            if can_execute(self, state, *node) {
                sum += &self.weights[transition];
            }
        }

        Ok(sum)
    }
}
