use std::{collections::{BTreeSet, HashSet}, fmt::{Debug, Display}, path::PathBuf};
use anyhow::{anyhow, Result};
use clap::{builder::ValueParser, value_parser};
use strum_macros::EnumIter;

use crate::{ebi_commands::ebi_command::{EbiCommand, EBI_COMMANDS}, ebi_input_output::{EbiInput, EbiInputType}, ebi_objects::ebi_object::{EbiObjectType, EbiTraitObject}, file_handler::{EbiFileHandler, EBI_FILE_HANDLERS}, math::fraction::Fraction};

#[derive(Clone, Copy, PartialEq, Eq, EnumIter, Hash)]
pub enum EbiTrait {
    EventLog,
    IterableLanguage,
    FiniteLanguage,
    FiniteStochasticLanguage,
    IterableStochasticLanguage,
    QueriableStochasticLanguage,
    Semantics,
    StochasticDeterministicSemantics,
    StochasticSemantics,
    LabelledPetriNet,
}

impl EbiTrait {
    /**
     * Get all file handlers that can import to this trait.
     */
    pub fn get_file_handlers(&self) -> Vec<&'static EbiFileHandler> {
        let mut result = HashSet::new();
        for file_handler in EBI_FILE_HANDLERS {
            for importer in file_handler.trait_importers {
                if &importer.get_trait() == self {
                    result.insert(file_handler);
                }
            }
        };

        let mut result: Vec<&EbiFileHandler> = result.into_iter().collect();
        result.sort();
        result
    }

    pub fn get_article(&self) -> &str {
        match self {
            EbiTrait::EventLog => "an",
            EbiTrait::IterableLanguage => "an",
            EbiTrait::FiniteLanguage => "a",
            EbiTrait::FiniteStochasticLanguage => "a",
            EbiTrait::IterableStochasticLanguage => "an",
            EbiTrait::QueriableStochasticLanguage => "a",
            EbiTrait::StochasticDeterministicSemantics => "a",
            EbiTrait::StochasticSemantics => "a",
            EbiTrait::LabelledPetriNet => "a",
            EbiTrait::Semantics => "a",
        }
    }
    
    pub fn get_applicable_commands(&self) -> BTreeSet<Vec<&'static EbiCommand>> {
        let mut result = EBI_COMMANDS.get_command_paths();
        result.retain(|path| {
            if let EbiCommand::Command { name_short, name_long, explanation_short, explanation_long, latex_link, cli_command, exact_arithmetic, input_types, input_names, input_helps, execute, output } = path[path.len() - 1] {
                for input_typess in input_types.iter() {
                    for input_typesss in input_typess.iter() {
                        if input_typesss == &&EbiInputType::Trait(self.clone()) {
                            return true;
                        }
                    }
                }
            }
            false
        });
        result
    }
}

impl Display for EbiTrait {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", match self {
            EbiTrait::EventLog => "event log",
            EbiTrait::IterableLanguage => "iterable language",
            EbiTrait::FiniteLanguage => "finite language",
            EbiTrait::FiniteStochasticLanguage => "finite stochastic language",
            EbiTrait::IterableStochasticLanguage => "iterable stochastic language",
            EbiTrait::QueriableStochasticLanguage => "queriable stochastic language",
            EbiTrait::StochasticDeterministicSemantics => "stochastic deterministic semantics",
            EbiTrait::StochasticSemantics => "stochastic semantics",
            EbiTrait::LabelledPetriNet => "labelled Petri net",
            EbiTrait::Semantics => "semantics",
        })
    }
}

impl Debug for EbiTrait {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        std::fmt::Display::fmt(&self, f)
    }
}

pub trait FromEbiTraitObject {
    fn from_trait_object(object: EbiInput) -> Result<Box<Self>>;
}

impl FromEbiTraitObject for usize {
    fn from_trait_object(object: EbiInput) -> Result<Box<Self>> {
        match object {
            EbiInput::Usize(e) => Ok(Box::new(e)),
            _ => Err(anyhow!("cannot read {} {} as an integer", object.get_type().get_article(), object.get_type()))
        } 
    }
}

impl FromEbiTraitObject for String {
    fn from_trait_object(object: EbiInput) -> Result<Box<Self>> {
        match object {
            EbiInput::String(e) => Ok(Box::new(e)),
            _ => Err(anyhow!("cannot read {} {} as an integer", object.get_type().get_article(), object.get_type()))
        } 
    }
}