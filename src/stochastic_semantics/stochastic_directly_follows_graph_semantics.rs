use anyhow::Result;
use ebi_arithmetic::{Fraction, Signed, Zero};
use ebi_objects::{
    StochasticDirectlyFollowsModel, ebi_objects::labelled_petri_net::TransitionIndex,
};

use crate::stochastic_semantics::stochastic_semantics::StochasticSemantics;

impl StochasticSemantics for StochasticDirectlyFollowsModel {
    type StoSemState = usize;

    fn get_transition_weight(
        &self,
        state: &<Self as StochasticSemantics>::StoSemState,
        transition: TransitionIndex,
    ) -> &Fraction {
        if transition == self.sources.len() {
            //end
            let node = *state;
            &self.end_node_weights[node]
        } else if transition < self.sources.len() {
            //edge
            &self.weights[transition]
        } else {
            //start
            let node = transition - (self.sources.len() + 1);
            &self.start_node_weights[node]
        }
    }

    fn get_total_weight_of_enabled_transitions(
        &self,
        state: &<Self as StochasticSemantics>::StoSemState,
    ) -> Result<Fraction> {
        if state == &self.node_2_activity.len() {
            //we are in the initial state
            Ok(self
                .start_node_weights
                .iter()
                .filter(|x| x.is_positive())
                .sum())
        } else if state > &self.node_2_activity.len() {
            //we are in the final state
            Ok(Fraction::zero())
        } else {
            let node = *state;
            //we are not in the initial state
            let mut result = Fraction::zero();

            //add edges
            let (_, mut i) = self.binary_search(node, 0);
            while i < self.sources.len() && self.sources[i] == node {
                if self.weights[i].is_positive() {
                    result += &self.weights[i];
                }
                i += 1;
            }

            //add transition to final state
            if self.end_node_weights[node].is_positive() {
                result += &self.end_node_weights[node];
            }

            Ok(result)
        }
    }
}
