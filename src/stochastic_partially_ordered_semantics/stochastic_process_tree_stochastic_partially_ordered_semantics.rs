use crate::stochastic_partially_ordered_semantics::stochastic_partially_ordered_semantics::StochasticPartiallyOrderedSemantics;
use ebi_objects::{
    StochasticBusinessProcessModelAndNotation, ebi_arithmetic::Fraction, ebi_bpmn::BPMNMarking,
    ebi_objects::labelled_petri_net::TransitionIndex,
};

impl StochasticPartiallyOrderedSemantics for StochasticBusinessProcessModelAndNotation {
    type StoPOSemState = BPMNMarking;

    fn get_transition_probabilistic_penalty(
        &self,
        state: &<Self as StochasticPartiallyOrderedSemantics>::StoPOSemState,
        transition: TransitionIndex,
    ) -> Option<Fraction> {
        self.get_transition_weight(transition, state)
    }
}
