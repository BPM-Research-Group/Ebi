use anyhow::{Result, anyhow};
use ebi_objects::{
    ActivityKey, DirectlyFollowsGraph, EventLog, FiniteStochasticLanguage, HasActivityKey,
    Importable, StochasticDeterministicFiniteAutomaton, StochasticDirectlyFollowsModel,
    StochasticLabelledPetriNet, StochasticProcessTree, TranslateActivityKey,
    ebi_objects::compressed_event_log::CompressedEventLog,
};
use std::io::BufRead;

use crate::{
    ebi_framework::{
        ebi_input::EbiInput, ebi_trait::FromEbiTraitObject, ebi_trait_object::EbiTraitObject,
    },
    semantics::{
        finite_stochastic_language_semantics::FiniteStochasticLanguageSemantics,
        labelled_petri_net_semantics::LPNMarking, process_tree_semantics::NodeStates,
    },
    stochastic_semantics::stochastic_semantics::StochasticSemantics,
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
    NodeStates(
        Box<
            dyn StochasticSemantics<
                    StoSemState = NodeStates,
                    SemState = NodeStates,
                    AliState = NodeStates,
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
            EbiTraitStochasticSemantics::NodeStates(semantics) => semantics.activity_key(),
        }
    }

    fn activity_key_mut(&mut self) -> &mut ActivityKey {
        match self {
            EbiTraitStochasticSemantics::Marking(semantics) => semantics.activity_key_mut(),
            EbiTraitStochasticSemantics::Usize(semantics) => semantics.activity_key_mut(),
            EbiTraitStochasticSemantics::NodeStates(semantics) => semantics.activity_key_mut(),
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
            EbiTraitStochasticSemantics::NodeStates(semantics) => {
                semantics.translate_using_activity_key(to_activity_key)
            }
        }
    }
}

pub trait ToStochasticSemantics: Importable + Sized {
    fn to_stochastic_semantics(self) -> EbiTraitStochasticSemantics;

    fn import_as_stochastic_semantics(
        reader: &mut dyn BufRead,
    ) -> Result<EbiTraitStochasticSemantics> {
        Ok(Self::import(reader)?.to_stochastic_semantics())
    }
}

impl ToStochasticSemantics for CompressedEventLog {
    fn to_stochastic_semantics(self) -> EbiTraitStochasticSemantics {
        self.log.to_stochastic_semantics()
    }
}

impl ToStochasticSemantics for StochasticProcessTree {
    fn to_stochastic_semantics(self) -> EbiTraitStochasticSemantics {
        EbiTraitStochasticSemantics::NodeStates(Box::new(self))
    }
}

impl ToStochasticSemantics for StochasticLabelledPetriNet {
    fn to_stochastic_semantics(self) -> EbiTraitStochasticSemantics {
        EbiTraitStochasticSemantics::Marking(Box::new(self))
    }
}

impl ToStochasticSemantics for DirectlyFollowsGraph {
    fn to_stochastic_semantics(self) -> EbiTraitStochasticSemantics {
        let dfm: StochasticDirectlyFollowsModel = self.into();
        EbiTraitStochasticSemantics::Usize(Box::new(dfm))
    }
}

impl ToStochasticSemantics for StochasticDirectlyFollowsModel {
    fn to_stochastic_semantics(self) -> EbiTraitStochasticSemantics {
        EbiTraitStochasticSemantics::Usize(Box::new(self))
    }
}

impl ToStochasticSemantics for StochasticDeterministicFiniteAutomaton {
    fn to_stochastic_semantics(self) -> EbiTraitStochasticSemantics {
        EbiTraitStochasticSemantics::Usize(Box::new(self))
    }
}

impl ToStochasticSemantics for EventLog {
    fn to_stochastic_semantics(self) -> EbiTraitStochasticSemantics {
        Into::<FiniteStochasticLanguage>::into(self).to_stochastic_semantics()
    }
}

impl ToStochasticSemantics for FiniteStochasticLanguage {
    fn to_stochastic_semantics(self) -> EbiTraitStochasticSemantics {
        EbiTraitStochasticSemantics::Usize(Box::new(
            FiniteStochasticLanguageSemantics::from_language(&self),
        ))
    }
}
