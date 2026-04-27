use crate::{
    ebi_framework::trait_importers::ToStochasticPartiallyOrderedSemanticsTrait,
    stochastic_partially_ordered_semantics::stochastic_partially_ordered_semantics::StochasticPartiallyOrderedSemantics,
    trait_definition_finalisation,
};
use ebi_objects::{StochasticBusinessProcessModelAndNotation, ebi_bpmn::BPMNMarking};

pub const TRAIT_DEFINITION_LATEX: &str = concat!(
    "The trait ``stochastic partially ordered semantics'' allows for state space traversal, with probabilities that reflect the partially ordered nature, that is, probabilities do not need to sum to one.",
    trait_definition_finalisation!()
);

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

impl ToStochasticPartiallyOrderedSemanticsTrait for StochasticBusinessProcessModelAndNotation {
    fn to_stochastic_partially_ordered_semantics_trait(
        self,
    ) -> EbiTraitStochasticPartiallyOrderedSemantics {
        EbiTraitStochasticPartiallyOrderedSemantics::BPMNMarking(Box::new(self))
    }
}
