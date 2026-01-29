use crate::{
    ebi_framework::{
        ebi_input::EbiInput, ebi_trait::FromEbiTraitObject, ebi_trait_object::EbiTraitObject,
        trait_importers::ToStochasticSemanticsTrait,
    },
    semantics::{
        finite_stochastic_language_semantics::FiniteStochasticLanguageSemantics,
        labelled_petri_net_semantics::LPNMarking,
    },
    stochastic_semantics::stochastic_semantics::StochasticSemantics,
};
use anyhow::{Result, anyhow};
use ebi_objects::{
    ActivityKey, DirectlyFollowsGraph, EventLog, EventLogPython, EventLogTraceAttributes,
    EventLogXes, FiniteStochasticLanguage, HasActivityKey, StochasticDeterministicFiniteAutomaton,
    StochasticDirectlyFollowsModel, StochasticLabelledPetriNet,
    StochasticNondeterministicFiniteAutomaton, StochasticProcessTree, TranslateActivityKey,
    ebi_objects::{
        compressed_event_log::CompressedEventLog,
        compressed_event_log_trace_attributes::CompressedEventLogTraceAttributes,
        event_log_csv::EventLogCsv, process_tree::TreeMarking,
    },
};

pub enum EbiTraitStochasticSemantics {
    Usize(Box<dyn StochasticSemantics<StoSemState = usize, SemState = usize, AliState = usize>>),
    Marking(
        Box<
            dyn StochasticSemantics<
                    StoSemState = LPNMarking,
                    SemState = LPNMarking,
                    AliState = LPNMarking,
                >,
        >,
    ),
    TreeMarking(
        Box<
            dyn StochasticSemantics<
                    StoSemState = TreeMarking,
                    SemState = TreeMarking,
                    AliState = TreeMarking,
                >,
        >,
    ),
}

impl FromEbiTraitObject for EbiTraitStochasticSemantics {
    fn from_trait_object(object: EbiInput) -> Result<Box<Self>> {
        match object {
            EbiInput::Trait(EbiTraitObject::StochasticSemantics(e), _) => Ok(Box::new(e)),
            _ => Err(anyhow!(
                "cannot read {} {} as a stochastic semantics",
                object.get_type().get_article(),
                object.get_type()
            )),
        }
    }
}

impl HasActivityKey for EbiTraitStochasticSemantics {
    fn activity_key(&self) -> &ActivityKey {
        match self {
            EbiTraitStochasticSemantics::Marking(semantics) => semantics.activity_key(),
            EbiTraitStochasticSemantics::Usize(semantics) => semantics.activity_key(),
            EbiTraitStochasticSemantics::TreeMarking(semantics) => semantics.activity_key(),
        }
    }

    fn activity_key_mut(&mut self) -> &mut ActivityKey {
        match self {
            EbiTraitStochasticSemantics::Marking(semantics) => semantics.activity_key_mut(),
            EbiTraitStochasticSemantics::Usize(semantics) => semantics.activity_key_mut(),
            EbiTraitStochasticSemantics::TreeMarking(semantics) => semantics.activity_key_mut(),
        }
    }
}

impl TranslateActivityKey for EbiTraitStochasticSemantics {
    fn translate_using_activity_key(&mut self, to_activity_key: &mut ActivityKey) {
        match self {
            EbiTraitStochasticSemantics::Marking(semantics) => {
                semantics.translate_using_activity_key(to_activity_key)
            }
            EbiTraitStochasticSemantics::Usize(semantics) => {
                semantics.translate_using_activity_key(to_activity_key)
            }
            EbiTraitStochasticSemantics::TreeMarking(semantics) => {
                semantics.translate_using_activity_key(to_activity_key)
            }
        }
    }
}

macro_rules! via_fslang {
    ($t:ident) => {
        impl ToStochasticSemanticsTrait for $t {
            fn to_stochastic_semantics_trait(self) -> EbiTraitStochasticSemantics {
                Into::<FiniteStochasticLanguage>::into(self).to_stochastic_semantics_trait()
            }
        }
    };
}
via_fslang!(CompressedEventLog);
via_fslang!(CompressedEventLogTraceAttributes);
via_fslang!(EventLog);
via_fslang!(EventLogTraceAttributes);
via_fslang!(EventLogXes);
via_fslang!(EventLogCsv);
via_fslang!(EventLogPython);

impl ToStochasticSemanticsTrait for StochasticProcessTree {
    fn to_stochastic_semantics_trait(self) -> EbiTraitStochasticSemantics {
        EbiTraitStochasticSemantics::TreeMarking(Box::new(self))
    }
}

impl ToStochasticSemanticsTrait for StochasticLabelledPetriNet {
    fn to_stochastic_semantics_trait(self) -> EbiTraitStochasticSemantics {
        EbiTraitStochasticSemantics::Marking(Box::new(self))
    }
}

impl ToStochasticSemanticsTrait for DirectlyFollowsGraph {
    fn to_stochastic_semantics_trait(self) -> EbiTraitStochasticSemantics {
        let dfm: StochasticDirectlyFollowsModel = self.into();
        EbiTraitStochasticSemantics::Usize(Box::new(dfm))
    }
}

impl ToStochasticSemanticsTrait for StochasticDirectlyFollowsModel {
    fn to_stochastic_semantics_trait(self) -> EbiTraitStochasticSemantics {
        EbiTraitStochasticSemantics::Usize(Box::new(self))
    }
}

impl ToStochasticSemanticsTrait for StochasticDeterministicFiniteAutomaton {
    fn to_stochastic_semantics_trait(self) -> EbiTraitStochasticSemantics {
        EbiTraitStochasticSemantics::Usize(Box::new(self))
    }
}

impl ToStochasticSemanticsTrait for StochasticNondeterministicFiniteAutomaton {
    fn to_stochastic_semantics_trait(self) -> EbiTraitStochasticSemantics {
        EbiTraitStochasticSemantics::Usize(Box::new(self))
    }
}

impl ToStochasticSemanticsTrait for FiniteStochasticLanguage {
    fn to_stochastic_semantics_trait(self) -> EbiTraitStochasticSemantics {
        EbiTraitStochasticSemantics::Usize(Box::new(
            FiniteStochasticLanguageSemantics::from_language(&self),
        ))
    }
}
