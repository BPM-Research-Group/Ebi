use crate::{
    ebi_framework::trait_importers::ToStochasticPartiallyOrderedSemanticsTrait,
    stochastic_partially_ordered_semantics::stochastic_partially_ordered_semantics::StochasticPartiallyOrderedSemantics,
};
use ebi_objects::{StochasticBusinessProcessModelAndNotation, ebi_bpmn::BPMNMarking};

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
