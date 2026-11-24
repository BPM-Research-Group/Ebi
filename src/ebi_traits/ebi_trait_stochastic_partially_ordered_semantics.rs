use crate::{
    ebi_framework::trait_importers::ToStochasticPartiallyOrderedSemanticsTrait,
    semantics::stochastic_partially_ordered_semantics::stochastic_partially_ordered_semantics_trait::StochasticPartiallyOrderedSemantics,
    trait_definition_finalisation,
};
use ebi_objects::{ebi_bpmn::BPMNMarking, StochasticBusinessProcessModelAndNotation};

/// Allows for traversal of a state space and its probabilities if that state space is partially ordered.
///
/// This is a wrapper enum in order to be able to implement algorithms that are agnostic of the marking/state type, amongst other things.
/// This enum does not allow access to the underlying Semantics trait methods directly, but through a match the generics work out.
/// See the implementation of [`crate::techniques::sample::Sampler`] for an example.
///
/// For documentation on Ebi traits and mathematical definitions, please refer to the [manual](https://github.com/BPM-Research-Group/Ebi/releases/latest/download/manual.pdf).
pub enum EbiTraitStochasticPartiallyOrderedSemantics {
    BPMNMarking(
        Box<
            dyn StochasticPartiallyOrderedSemantics<
                StoPOSemState = BPMNMarking,
                SemState = BPMNMarking,
                AliState = BPMNMarking,
            >,
        >,
    ),
}

impl EbiTraitStochasticPartiallyOrderedSemantics {
    pub fn trait_explanation() -> &'static str
    where
        Self: Sized,
    {
        concat!(
            "The trait ``stochastic partially ordered semantics'' allows for state space traversal, with probabilities that reflect the partially ordered nature, that is, probabilities do not need to sum to one.",
            trait_definition_finalisation!()
        )
    }
}

impl ToStochasticPartiallyOrderedSemanticsTrait for StochasticBusinessProcessModelAndNotation {
    fn to_stochastic_partially_ordered_semantics_trait(
        self,
    ) -> EbiTraitStochasticPartiallyOrderedSemantics {
        EbiTraitStochasticPartiallyOrderedSemantics::BPMNMarking(Box::new(self))
    }
}
