use std::{collections::BTreeSet, fmt::Display, hash::Hash, io::BufRead, str::FromStr};
use anyhow::{anyhow, Result, Error};

use crate::{ebi_commands::ebi_command_validate::EBI_VALIDATE, ebi_objects::{alignments::EBI_ALIGNMENTS, compressed_event_log::EBI_COMPRESSED_EVENT_LOG, deterministic_finite_automaton::EBI_DETERMINISTIC_FINITE_AUTOMATON, directly_follows_model::EBI_DIRCTLY_FOLLOWS_MODEL, event_log::EBI_EVENT_LOG, finite_language::EBI_FINITE_LANGUAGE, finite_stochastic_language::EBI_FINITE_STOCHASTIC_LANGUAGE, labelled_petri_net::EBI_LABELLED_PETRI_NET, petri_net_markup_language::EBI_PETRI_NET_MARKUP_LANGUAGE, stochastic_deterministic_finite_automaton::EBI_STOCHASTIC_DETERMINISTIC_FINITE_AUTOMATON, stochastic_labelled_petri_net::EBI_STOCHASTIC_LABELLED_PETRI_NET}};

use super::{ebi_command::{EbiCommand, EBI_COMMANDS}, ebi_input::{EbiInput, EbiObjectImporter, EbiTraitImporter}, ebi_output::EbiObjectExporter, ebi_trait::FromEbiTraitObject};

pub const EBI_FILE_HANDLERS: &'static [EbiFileHandler] = &[
    EBI_ALIGNMENTS,
    EBI_FINITE_LANGUAGE,
    EBI_LABELLED_PETRI_NET,
    EBI_STOCHASTIC_LABELLED_PETRI_NET,
    EBI_FINITE_STOCHASTIC_LANGUAGE,
    EBI_STOCHASTIC_DETERMINISTIC_FINITE_AUTOMATON,
    EBI_EVENT_LOG,
    EBI_COMPRESSED_EVENT_LOG,
    EBI_DIRCTLY_FOLLOWS_MODEL,
    EBI_PETRI_NET_MARKUP_LANGUAGE,
    EBI_DETERMINISTIC_FINITE_AUTOMATON,
];

#[derive(Clone,Debug)]
pub struct EbiFileHandler {
    pub name: &'static str,
    pub article: &'static str, //a or an
    pub file_extension: &'static str,
    pub validator: fn(&mut dyn BufRead) -> Result<()>,
    pub trait_importers: &'static [EbiTraitImporter],
    pub object_importers: &'static [EbiObjectImporter],
    pub object_exporters: &'static [EbiObjectExporter] //the order matters, as if multiple file handlers can export an object, the one that mentions the object earliest is preferred.
}

impl EbiFileHandler {
    pub fn get_applicable_commands(&self) -> BTreeSet<Vec<&'static EbiCommand>> {
        let mut result = BTreeSet::new();

        for importer in self.trait_importers {
            result.extend(importer.get_trait().get_applicable_commands());
        }
        for importer in self.object_importers {
            result.extend(importer.get_type().get_applicable_commands());
        }
        result.insert(vec![&EBI_COMMANDS, &EBI_VALIDATE]);

        result
    }
}

impl FromStr for EbiFileHandler {
    type Err = Error;

    fn from_str(s: &str) -> std::result::Result<Self, Self::Err> {
        for file_handler in EBI_FILE_HANDLERS {
            if file_handler.name == s || file_handler.file_extension == s {
                return Ok(file_handler.clone());
            }
        }
        return Err(anyhow!("{} is not an Ebi file handler.", s));
    }
}

impl FromEbiTraitObject for EbiFileHandler {
    fn from_trait_object(object: EbiInput) -> Result<Box<Self>> {
        match object {
            EbiInput::FileHandler(e) => Ok(Box::new(e)),
            _ => Err(anyhow!("cannot read {} {} as an integer", object.get_type().get_article(), object.get_type()))
        } 
    }
}

impl Eq for EbiFileHandler {}

impl PartialEq for EbiFileHandler {
    fn eq(&self, other: &Self) -> bool {
        self.name == other.name
    }
}

impl PartialOrd for EbiFileHandler {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        self.name.partial_cmp(&other.name)
    }
}

impl Ord for EbiFileHandler {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        self.name.cmp(&other.name)
    }
}

impl Hash for EbiFileHandler {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.name.hash(state);
    }
}

impl Display for EbiFileHandler {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{} (.{})", self.name, self.file_extension)
    }
}