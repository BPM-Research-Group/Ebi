use crate::{
    ebi_framework::{
        ebi_input::EbiInput, ebi_trait::FromEbiTraitObject, ebi_trait_object::EbiTraitObject,
        trait_importers::ToSemanticsTrait,
    },
    semantics::{
        finite_stochastic_language_semantics::FiniteStochasticLanguageSemantics,
        labelled_petri_net_semantics::LPNMarking, semantics::Semantics,
    },
};
use anyhow::{Result, anyhow};
use ebi_objects::{
    ActivityKey, DeterministicFiniteAutomaton, DirectlyFollowsGraph, DirectlyFollowsModel,
    EventLog, EventLogPython, EventLogTraceAttributes, EventLogXes, FiniteLanguage,
    FiniteStochasticLanguage, HasActivityKey, LabelledPetriNet, LolaNet, PetriNetMarkupLanguage,
    ProcessTree, ProcessTreeMarkupLanguage, StochasticDeterministicFiniteAutomaton,
    StochasticDirectlyFollowsModel, StochasticLabelledPetriNet,
    StochasticNondeterministicFiniteAutomaton, StochasticProcessTree, TranslateActivityKey,
    ebi_objects::{
        compressed_event_log::CompressedEventLog,
        compressed_event_log_trace_attributes::CompressedEventLogTraceAttributes,
        event_log_csv::EventLogCsv, process_tree::TreeMarking,
    },
};

///
/// This is a wrapper enum in order to be able to implement algorithms that are agnostic of the marking/state type, amongst other things.
/// This enum does not allow access to the underlying Semantics trait methods.
/// For these methods, such as 'get_initial_state' and 'get_enabled_transitions', please access the struct directly.
pub enum EbiTraitSemantics {
    Usize(Box<dyn Semantics<SemState = usize, AliState = usize>>),
    Marking(Box<dyn Semantics<SemState = LPNMarking, AliState = LPNMarking>>),
    TreeMarking(Box<dyn Semantics<SemState = TreeMarking, AliState = TreeMarking>>),
}

impl HasActivityKey for EbiTraitSemantics {
    fn activity_key(&self) -> &ActivityKey {
        match self {
            EbiTraitSemantics::Marking(semantics) => semantics.activity_key(),
            EbiTraitSemantics::Usize(semantics) => semantics.activity_key(),
            EbiTraitSemantics::TreeMarking(semantics) => semantics.activity_key(),
        }
    }

    fn activity_key_mut(&mut self) -> &mut ActivityKey {
        match self {
            EbiTraitSemantics::Marking(semantics) => semantics.activity_key_mut(),
            EbiTraitSemantics::Usize(semantics) => semantics.activity_key_mut(),
            EbiTraitSemantics::TreeMarking(semantics) => semantics.activity_key_mut(),
        }
    }
}

impl TranslateActivityKey for EbiTraitSemantics {
    fn translate_using_activity_key(&mut self, to_activity_key: &mut ActivityKey) {
        match self {
            EbiTraitSemantics::Marking(semantics) => {
                semantics.translate_using_activity_key(to_activity_key)
            }
            EbiTraitSemantics::Usize(semantics) => {
                semantics.translate_using_activity_key(to_activity_key)
            }
            EbiTraitSemantics::TreeMarking(semantics) => {
                semantics.translate_using_activity_key(to_activity_key)
            }
        }
    }
}

impl FromEbiTraitObject for EbiTraitSemantics {
    fn from_trait_object(object: EbiInput) -> Result<Box<Self>> {
        match object {
            EbiInput::Trait(EbiTraitObject::Semantics(e), _) => Ok(Box::new(e)),
            _ => Err(anyhow!(
                "Cannot read {} {} as a semantics.",
                object.get_type().get_article(),
                object.get_type()
            )),
        }
    }
}

macro_rules! via_log {
    ($t:ident) => {
        impl ToSemanticsTrait for $t {
            fn to_semantics_trait(self) -> EbiTraitSemantics {
                Into::<FiniteLanguage>::into(self).to_semantics_trait()
            }
        }
    };
}
via_log!(CompressedEventLog);
via_log!(CompressedEventLogTraceAttributes);
via_log!(EventLog);
via_log!(EventLogTraceAttributes);
via_log!(EventLogXes);
via_log!(EventLogCsv);
via_log!(EventLogPython);

impl ToSemanticsTrait for DeterministicFiniteAutomaton {
    fn to_semantics_trait(self) -> EbiTraitSemantics {
        EbiTraitSemantics::Usize(Box::new(self))
    }
}

impl ToSemanticsTrait for LolaNet {
    fn to_semantics_trait(self) -> EbiTraitSemantics {
        self.0.to_semantics_trait()
    }
}

impl ToSemanticsTrait for StochasticProcessTree {
    fn to_semantics_trait(self) -> EbiTraitSemantics {
        EbiTraitSemantics::TreeMarking(Box::new(self))
    }
}

impl ToSemanticsTrait for LabelledPetriNet {
    fn to_semantics_trait(self) -> EbiTraitSemantics {
        EbiTraitSemantics::Marking(Box::new(self))
    }
}

impl ToSemanticsTrait for PetriNetMarkupLanguage {
    fn to_semantics_trait(self) -> EbiTraitSemantics {
        self.0.to_semantics_trait()
    }
}

impl ToSemanticsTrait for StochasticLabelledPetriNet {
    fn to_semantics_trait(self) -> EbiTraitSemantics {
        EbiTraitSemantics::Marking(Box::new(self))
    }
}

impl ToSemanticsTrait for StochasticDirectlyFollowsModel {
    fn to_semantics_trait(self) -> EbiTraitSemantics {
        EbiTraitSemantics::Usize(Box::new(self))
    }
}

impl ToSemanticsTrait for DirectlyFollowsGraph {
    fn to_semantics_trait(self) -> EbiTraitSemantics {
        let dfm: DirectlyFollowsModel = self.into();
        EbiTraitSemantics::Usize(Box::new(dfm))
    }
}

impl ToSemanticsTrait for DirectlyFollowsModel {
    fn to_semantics_trait(self) -> EbiTraitSemantics {
        EbiTraitSemantics::Usize(Box::new(self))
    }
}

impl ToSemanticsTrait for StochasticDeterministicFiniteAutomaton {
    fn to_semantics_trait(self) -> EbiTraitSemantics {
        EbiTraitSemantics::Usize(Box::new(self))
    }
}

impl ToSemanticsTrait for StochasticNondeterministicFiniteAutomaton {
    fn to_semantics_trait(self) -> EbiTraitSemantics {
        EbiTraitSemantics::Usize(Box::new(self))
    }
}

impl ToSemanticsTrait for FiniteLanguage {
    fn to_semantics_trait(self) -> EbiTraitSemantics {
        Into::<DeterministicFiniteAutomaton>::into(self).to_semantics_trait()
    }
}

impl ToSemanticsTrait for FiniteStochasticLanguage {
    fn to_semantics_trait(self) -> EbiTraitSemantics {
        EbiTraitSemantics::Usize(Box::new(FiniteStochasticLanguageSemantics::from_language(
            &self,
        )))
    }
}

impl ToSemanticsTrait for ProcessTreeMarkupLanguage {
    fn to_semantics_trait(self) -> EbiTraitSemantics {
        let lpn: LabelledPetriNet = self.into();
        EbiTraitSemantics::Marking(Box::new(lpn))
    }
}

impl ToSemanticsTrait for ProcessTree {
    fn to_semantics_trait(self) -> EbiTraitSemantics {
        EbiTraitSemantics::TreeMarking(Box::new(self))
    }
}
