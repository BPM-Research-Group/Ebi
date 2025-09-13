use crate::{
    ebi_framework::{
        ebi_input::EbiInput, ebi_trait::FromEbiTraitObject, ebi_trait_object::EbiTraitObject,
    },
    semantics::{
        finite_stochastic_language_semantics::FiniteStochasticLanguageSemantics,
        labelled_petri_net_semantics::LPNMarking, process_tree_semantics::NodeStates,
        semantics::Semantics,
    },
};
use anyhow::{Result, anyhow};
use ebi_objects::{
    ActivityKey, DeterministicFiniteAutomaton, DirectlyFollowsGraph, DirectlyFollowsModel,
    EventLog, FiniteLanguage, FiniteStochasticLanguage, HasActivityKey, Importable,
    LabelledPetriNet, LolaNet, PetriNetMarkupLanguage, ProcessTree, ProcessTreeMarkupLanguage,
    StochasticDeterministicFiniteAutomaton, StochasticDirectlyFollowsModel,
    StochasticLabelledPetriNet, StochasticProcessTree, TranslateActivityKey,
    ebi_objects::compressed_event_log::CompressedEventLog,
};
use std::io::BufRead;

///
/// This is a wrapper enum in order to be able to implement algorithms that are agnostic of the marking/state type, amongst other things.
/// This enum does not allow access to the underlying Semantics trait methods.
/// For these methods, such as 'get_initial_state' and 'get_enabled_transitions', please access the struct directly.
pub enum EbiTraitSemantics {
    Usize(Box<dyn Semantics<SemState = usize, AliState = usize>>),
    Marking(Box<dyn Semantics<SemState = LPNMarking, AliState = LPNMarking>>),
    NodeStates(Box<dyn Semantics<SemState = NodeStates, AliState = NodeStates>>),
}

impl HasActivityKey for EbiTraitSemantics {
    fn activity_key(&self) -> &ActivityKey {
        match self {
            EbiTraitSemantics::Marking(semantics) => semantics.activity_key(),
            EbiTraitSemantics::Usize(semantics) => semantics.activity_key(),
            EbiTraitSemantics::NodeStates(semantics) => semantics.activity_key(),
        }
    }

    fn activity_key_mut(&mut self) -> &mut ActivityKey {
        match self {
            EbiTraitSemantics::Marking(semantics) => semantics.activity_key_mut(),
            EbiTraitSemantics::Usize(semantics) => semantics.activity_key_mut(),
            EbiTraitSemantics::NodeStates(semantics) => semantics.activity_key_mut(),
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
            EbiTraitSemantics::NodeStates(semantics) => {
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

pub trait ToSemantics: Importable + Sized {
    fn to_semantics(self) -> EbiTraitSemantics;

    fn import_as_semantics(reader: &mut dyn BufRead) -> Result<EbiTraitSemantics> {
        Ok(Self::import(reader)?.to_semantics())
    }
}

impl ToSemantics for CompressedEventLog {
    fn to_semantics(self) -> EbiTraitSemantics {
        self.log.to_semantics()
    }
}

impl ToSemantics for DeterministicFiniteAutomaton {
    fn to_semantics(self) -> EbiTraitSemantics {
        EbiTraitSemantics::Usize(Box::new(self))
    }
}

impl ToSemantics for LolaNet {
    fn to_semantics(self) -> EbiTraitSemantics {
        self.0.to_semantics()
    }
}

impl ToSemantics for StochasticProcessTree {
    fn to_semantics(self) -> EbiTraitSemantics {
        EbiTraitSemantics::NodeStates(Box::new(self))
    }
}

impl ToSemantics for LabelledPetriNet {
    fn to_semantics(self) -> EbiTraitSemantics {
        EbiTraitSemantics::Marking(Box::new(self))
    }
}

impl ToSemantics for PetriNetMarkupLanguage {
    fn to_semantics(self) -> EbiTraitSemantics {
        self.0.to_semantics()
    }
}

impl ToSemantics for StochasticLabelledPetriNet {
    fn to_semantics(self) -> EbiTraitSemantics {
        EbiTraitSemantics::Marking(Box::new(self))
    }
}

impl ToSemantics for StochasticDirectlyFollowsModel {
    fn to_semantics(self) -> EbiTraitSemantics {
        EbiTraitSemantics::Usize(Box::new(self))
    }
}

impl ToSemantics for DirectlyFollowsGraph {
    fn to_semantics(self) -> EbiTraitSemantics {
        let dfm: DirectlyFollowsModel = self.into();
        EbiTraitSemantics::Usize(Box::new(dfm))
    }
}

impl ToSemantics for DirectlyFollowsModel {
    fn to_semantics(self) -> EbiTraitSemantics {
        EbiTraitSemantics::Usize(Box::new(self))
    }
}

impl ToSemantics for StochasticDeterministicFiniteAutomaton {
    fn to_semantics(self) -> EbiTraitSemantics {
        EbiTraitSemantics::Usize(Box::new(self))
    }
}

impl ToSemantics for EventLog {
    fn to_semantics(self) -> EbiTraitSemantics {
        Into::<FiniteStochasticLanguage>::into(self).to_semantics()
    }
}

impl ToSemantics for FiniteLanguage {
    fn to_semantics(self) -> EbiTraitSemantics {
        Into::<DeterministicFiniteAutomaton>::into(self).to_semantics()
    }
}

impl ToSemantics for FiniteStochasticLanguage {
    fn to_semantics(self) -> EbiTraitSemantics {
        EbiTraitSemantics::Usize(Box::new(FiniteStochasticLanguageSemantics::from_language(
            &self,
        )))
    }
}

impl ToSemantics for ProcessTreeMarkupLanguage {
    fn to_semantics(self) -> EbiTraitSemantics {
        let lpn: LabelledPetriNet = self.into();
        EbiTraitSemantics::Marking(Box::new(lpn))
    }
}

impl ToSemantics for ProcessTree {
    fn to_semantics(self) -> EbiTraitSemantics {
        EbiTraitSemantics::NodeStates(Box::new(self))
    }
}
