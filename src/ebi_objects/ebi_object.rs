use std::{any::Any, collections::{BTreeSet, HashSet}, fmt::Display, hash::Hash, io::BufRead, str::FromStr};
use anyhow::{anyhow, Error, Result};
use strum::IntoEnumIterator;
use strum_macros::EnumIter;

use crate::{ebi_commands::{ebi_command::{EbiCommand, EBI_COMMANDS}, ebi_command_info::Infoable}, ebi_input_output::EbiInputType, ebi_objects::{compressed_event_log::EBI_COMPRESSED_EVENT_LOG, event_log::{EventLog, EBI_EVENT_LOG}, finite_language::{FiniteLanguage, EBI_FINITE_LANGUAGE}, finite_stochastic_language::{FiniteStochasticLanguage, EBI_FINITE_STOCHASTIC_LANGUAGE}, labelled_petri_net::{LabelledPetriNet, EBI_LABELLED_PETRI_NET}, stochastic_deterministic_finite_automaton::{StochasticDeterministicFiniteAutomaton, EBI_STOCHASTIC_DETERMINISTIC_FINITE_AUTOMATON}, stochastic_labelled_petri_net::{StochasticLabelledPetriNet, EBI_STOCHASTIC_LABELLED_PETRI_NET}}, ebi_traits::{ebi_semantics::EbiTraitSemantics, ebi_trait::{EbiTrait, FromEbiTraitObject}, ebi_trait_event_log::EbiTraitEventLog, ebi_trait_finite_language::EbiTraitFiniteLanguage, ebi_trait_finite_stochastic_language::EbiTraitFiniteStochasticLanguage, ebi_trait_iterable_language::EbiTraitIterableLanguage, ebi_trait_iterable_stochastic_language::EbiTraitIterableStochasticLanguage, ebi_trait_labelled_petri_net::EbiTraitLabelledPetriNet, ebi_trait_queriable_stochastic_language::EbiTraitQueriableStochasticLanguage, ebi_trait_stochastic_deterministic_semantics::EbiTraitStochasticDeterministicSemantics, ebi_trait_stochastic_semantics::EbiTraitStochasticSemantics}, export::Exportable, file_handler::{self, EbiFileHandler, EBI_FILE_HANDLERS}, import::EbiTraitImporter, math::{fraction::Fraction, log_div::LogDiv, root::ContainsRoot}};

use super::directly_follows_model::{DirectlyFollowsModel, EBI_DIRCTLY_FOLLOWS_MODEL};

#[derive(PartialEq,Clone,EnumIter,Hash)]
pub enum EbiObjectType {
    DirectlyFollowsModel,
    EventLog,
    FiniteLanguage,
    FiniteStochasticLanguage,
    LabelledPetriNet,
    StochasticDeterministicFiniteAutomaton,
    StochasticLabelledPetriNet,
}

impl EbiObjectType {

    pub fn get_article(&self) -> &str {
        match self {
            EbiObjectType::LabelledPetriNet => "a",
            EbiObjectType::StochasticLabelledPetriNet => "a",
            EbiObjectType::FiniteStochasticLanguage => "a",
            EbiObjectType::StochasticDeterministicFiniteAutomaton => "a",
            EbiObjectType::EventLog => "an",
            EbiObjectType::FiniteLanguage => "a",
            EbiObjectType::DirectlyFollowsModel => "a"
        }
    }
    
    pub fn get_applicable_commands(&self) -> BTreeSet<Vec<&'static EbiCommand>> {
        let mut result = EBI_COMMANDS.get_command_paths();
        result.retain(|path| {
            if let EbiCommand::Command { name_short, name_long, explanation_short, explanation_long, latex_link, cli_command, exact_arithmetic, input_types, input_names, input_helps, execute, output } = path[path.len() - 1] {
                for input_typess in input_types.iter() {
                    for input_typesss in input_typess.iter() {
                        if input_typesss == &&EbiInputType::AnyObject || input_typesss == &&EbiInputType::ObjectType(self.clone()) {
                            return true;
                        }
                    }
                }
            }
            false
        });
        result
    }
    
    pub fn get_file_handlers(&self) -> Vec<&'static EbiFileHandler> {
        let mut result = vec![];
        for file_handler in EBI_FILE_HANDLERS.iter() {
            for importer in file_handler.object_importers {
                if &importer.get_type() == self {
                    result.push(file_handler);
                    break;
                }
            }
        }
        result
    }
}

impl Eq for EbiObjectType {}

impl Display for EbiObjectType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", match self {
            EbiObjectType::LabelledPetriNet => "labelled Petri net",
            EbiObjectType::StochasticLabelledPetriNet => "stochastic labelled Petri net",
            EbiObjectType::FiniteStochasticLanguage => "finite stochastic language",
            EbiObjectType::StochasticDeterministicFiniteAutomaton => "stochastic deterministic finite automaton",
            EbiObjectType::EventLog => "event log",
            EbiObjectType::FiniteLanguage => "finite language",
            EbiObjectType::DirectlyFollowsModel => "directly follows model"
        })
    }
}

pub enum EbiObject {
    LabelledPetriNet(LabelledPetriNet),
    StochasticLabelledPetriNet(StochasticLabelledPetriNet),
    FiniteStochasticLanguage(FiniteStochasticLanguage),
    StochasticDeterministicFiniteAutomaton(StochasticDeterministicFiniteAutomaton),
    EventLog(EventLog),
    FiniteLanguage(FiniteLanguage),
    DirectlyFollowsModel(DirectlyFollowsModel)
}

impl EbiObject {
    pub fn get_type(&self) -> EbiObjectType {
        match self {
            EbiObject::LabelledPetriNet(_) => EbiObjectType::LabelledPetriNet,
            EbiObject::StochasticLabelledPetriNet(_) => EbiObjectType::StochasticLabelledPetriNet,
            EbiObject::FiniteStochasticLanguage(_) => EbiObjectType::FiniteStochasticLanguage,
            EbiObject::StochasticDeterministicFiniteAutomaton(_) => EbiObjectType::StochasticDeterministicFiniteAutomaton,
            EbiObject::EventLog(_) => EbiObjectType::EventLog,
            EbiObject::FiniteLanguage(_) => EbiObjectType::FiniteLanguage,
            EbiObject::DirectlyFollowsModel(_) => EbiObjectType::DirectlyFollowsModel
        }
    }
}

impl Display for EbiObject {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            EbiObject::LabelledPetriNet(o) => write!(f, "{}", o),
            EbiObject::StochasticLabelledPetriNet(o) => write!(f, "{}", o),
            EbiObject::FiniteStochasticLanguage(o) => write!(f, "{}", o),
            EbiObject::StochasticDeterministicFiniteAutomaton(o) => write!(f, "{}", o),
            EbiObject::EventLog(o) => write!(f, "{}", o),
            EbiObject::FiniteLanguage(o) => write!(f, "{}", o),
            EbiObject::DirectlyFollowsModel(o) => write!(f, "{}", o)
        }
    }
}

impl Infoable for EbiObject {
    fn info(&self, f: &mut impl std::io::Write) -> Result<()> {
        match self {
            EbiObject::LabelledPetriNet(o) => o.info(f),
            EbiObject::StochasticLabelledPetriNet(o) => o.info(f),
            EbiObject::FiniteStochasticLanguage(o) => o.info(f),
            EbiObject::StochasticDeterministicFiniteAutomaton(o) => o.info(f),
            EbiObject::EventLog(o) => o.info(f),
            EbiObject::FiniteLanguage(o) => o.info(f),
            EbiObject::DirectlyFollowsModel(o) => o.info(f)
        }
    }
}

pub enum EbiTraitObject {
    EventLog(Box<dyn EbiTraitEventLog>),
    IterableLanguage(Box<dyn EbiTraitIterableLanguage>),
    FiniteLanguage(Box<dyn EbiTraitFiniteLanguage>),
    FiniteStochasticLanguage(Box<dyn EbiTraitFiniteStochasticLanguage>),
    IterableStochasticLanguage(Box<dyn EbiTraitIterableStochasticLanguage>),
    QueriableStochasticLanguage(Box<dyn EbiTraitQueriableStochasticLanguage>),
    StochasticDeterministicSemantics(EbiTraitStochasticDeterministicSemantics),
    StochasticSemantics(EbiTraitStochasticSemantics),
    LabelledPetriNet(Box<dyn EbiTraitLabelledPetriNet>),
    Semantics(EbiTraitSemantics),
}

impl EbiTraitObject {
    pub fn get_trait(&self) -> EbiTrait {
        match self {
            EbiTraitObject::EventLog(_) => EbiTrait::EventLog,
            EbiTraitObject::IterableLanguage(_) => EbiTrait::IterableLanguage,
            EbiTraitObject::FiniteLanguage(_) => EbiTrait::FiniteLanguage,
            EbiTraitObject::FiniteStochasticLanguage(_) => EbiTrait::FiniteStochasticLanguage,
            EbiTraitObject::IterableStochasticLanguage(_) => EbiTrait::IterableStochasticLanguage,
            EbiTraitObject::QueriableStochasticLanguage(_) => EbiTrait::QueriableStochasticLanguage,
            EbiTraitObject::StochasticDeterministicSemantics(_) => EbiTrait::StochasticDeterministicSemantics,
            EbiTraitObject::StochasticSemantics(_) => EbiTrait::StochasticSemantics,
            EbiTraitObject::LabelledPetriNet(_) => EbiTrait::LabelledPetriNet,
            EbiTraitObject::Semantics(_) => EbiTrait::Semantics,
        }
    }
}