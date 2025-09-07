use anyhow::{Error, Result, anyhow};
use ebi_objects::EbiObjectType;
use std::{collections::BTreeSet, fmt::Display, hash::Hash, io::BufRead, str::FromStr};

use crate::{
    ebi_commands::ebi_command_validate::EBI_VALIDATE,
    ebi_file_handlers::{
        compressed_event_log::EBI_COMPRESSED_EVENT_LOG,
        deterministic_finite_automaton::EBI_DETERMINISTIC_FINITE_AUTOMATON,
        directly_follows_graph::EBI_DIRECTLY_FOLLOWS_GRAPH,
        directly_follows_model::EBI_DIRECTLY_FOLLOWS_MODEL, event_log::EBI_EVENT_LOG,
        executions::EBI_EXECUTIONS, finite_language::EBI_FINITE_LANGUAGE,
        finite_stochastic_language::EBI_FINITE_STOCHASTIC_LANGUAGE,
        labelled_petri_net::EBI_LABELLED_PETRI_NET,
        language_of_alignments::EBI_LANGUAGE_OF_ALIGNMENTS, lola_net::EBI_LOLA_NET,
        petri_net_markup_language::EBI_PETRI_NET_MARKUP_LANGUAGE,
        portable_document_format::EBI_PORTABLE_DOCUMENT_FORMAT, process_tree::EBI_PROCESS_TREE,
        process_tree_markup_language::EBI_PROCESS_TREE_MARKUP_LANGUAGE,
        scalable_vector_graphics::EBI_SCALABLE_VECTOR_GRAPHICS,
        stochastic_deterministic_finite_automaton::EBI_STOCHASTIC_DETERMINISTIC_FINITE_AUTOMATON,
        stochastic_directly_follows_model::EBI_STOCHASTIC_DIRECTLY_FOLLOWS_MODEL,
        stochastic_labelled_petri_net::EBI_STOCHASTIC_LABELLED_PETRI_NET,
        stochastic_language_of_alignments::EBI_STOCHASTIC_LANGUAGE_OF_ALIGNMENTS,
        stochastic_process_tree::EBI_STOCHASTIC_PROCESS_TREE,
    },
    ebi_framework::ebi_command::get_applicable_commands,
};

use super::{
    ebi_command::{EBI_COMMANDS, EbiCommand},
    ebi_input::{EbiInput, EbiObjectImporter, EbiTraitImporter},
    ebi_output::{EbiObjectExporter, EbiOutputType},
    ebi_trait::FromEbiTraitObject,
    prom_link::JavaObjectHandler,
};

/**
 * The order of this list is important: for the "any object" input type and for trait importers,
 * they are attempted in order. Thus, the more restrictive formats should come first.
 */
pub const EBI_FILE_HANDLERS: &'static [EbiFileHandler] = &[
    EBI_COMPRESSED_EVENT_LOG,
    EBI_DIRECTLY_FOLLOWS_GRAPH,
    EBI_DETERMINISTIC_FINITE_AUTOMATON,
    EBI_DIRECTLY_FOLLOWS_MODEL,
    EBI_STOCHASTIC_DIRECTLY_FOLLOWS_MODEL,
    EBI_EVENT_LOG,
    EBI_EXECUTIONS,
    EBI_FINITE_LANGUAGE,
    EBI_FINITE_STOCHASTIC_LANGUAGE,
    EBI_LABELLED_PETRI_NET,
    EBI_LANGUAGE_OF_ALIGNMENTS,
    EBI_LOLA_NET,
    EBI_PETRI_NET_MARKUP_LANGUAGE,
    EBI_STOCHASTIC_DETERMINISTIC_FINITE_AUTOMATON,
    EBI_STOCHASTIC_LABELLED_PETRI_NET,
    EBI_PROCESS_TREE,
    EBI_PORTABLE_DOCUMENT_FORMAT,
    EBI_SCALABLE_VECTOR_GRAPHICS,
    EBI_STOCHASTIC_LANGUAGE_OF_ALIGNMENTS,
    EBI_STOCHASTIC_PROCESS_TREE,
    EBI_PROCESS_TREE_MARKUP_LANGUAGE,
];

#[derive(Clone, Debug)]
pub struct EbiFileHandler {
    pub name: &'static str,
    pub article: &'static str, //a or an
    pub file_extension: &'static str,
    pub is_binary: bool,
    pub format_specification: &'static str,
    pub validator: Option<fn(&mut dyn BufRead) -> Result<()>>,
    pub trait_importers: &'static [EbiTraitImporter],
    pub object_importers: &'static [EbiObjectImporter],
    pub object_exporters: &'static [EbiObjectExporter], //the order matters, because if multiple file handlers can export an object, the one that mentions the object earliest is preferred.
    pub java_object_handlers: &'static [JavaObjectHandler],
}

impl EbiFileHandler {
    pub fn get_applicable_commands(&self) -> BTreeSet<Vec<&'static EbiCommand>> {
        let mut result = BTreeSet::new();

        for importer in self.trait_importers {
            result.extend(importer.get_trait().get_applicable_commands());
        }
        for importer in self.object_importers {
            result.extend(get_applicable_commands(&importer.get_type()));
        }
        if self.validator.is_some() {
            result.insert(vec![&EBI_COMMANDS, &EBI_VALIDATE]);
        }

        result
    }

    pub fn get_producing_commands(&self) -> BTreeSet<Vec<&'static EbiCommand>> {
        //get objects that can export to this file handler
        let mut objects = vec![];
        for exporter in self.object_exporters {
            objects.push(exporter.get_type());
        }

        //get commands that can output any of the objects
        let mut result = BTreeSet::new();
        for command_path in EBI_COMMANDS.get_command_paths() {
            if let Some(EbiCommand::Command { output_type, .. }) = command_path.last() {
                if let EbiOutputType::ObjectType(x) = output_type {
                    if objects.contains(x) {
                        result.insert(command_path);
                    }
                }
            }
        }

        result
    }
}

impl FromStr for EbiFileHandler {
    type Err = Error;

    fn from_str(s: &str) -> std::result::Result<Self, Self::Err> {
        for file_handler in EBI_FILE_HANDLERS {
            if file_handler.validator.is_some()
                && (file_handler.name == s || file_handler.file_extension == s)
            {
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
            _ => Err(anyhow!(
                "cannot read {} {} as an file handler",
                object.get_type().get_article(),
                object.get_type()
            )),
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
        self.name.partial_cmp(other.name)
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

pub fn get_file_handlers(object_type: &EbiObjectType) -> Vec<&'static EbiFileHandler> {
    let mut result = vec![];
    for file_handler in EBI_FILE_HANDLERS.iter() {
        for importer in file_handler.object_importers {
            if &importer.get_type() == object_type {
                result.push(file_handler);
                break;
            }
        }
    }
    result
}

#[cfg(test)]
mod tests {
    use std::str::FromStr;

    use crate::{
        ebi_file_handlers::{
            executions::EBI_EXECUTIONS, finite_stochastic_language::EBI_FINITE_STOCHASTIC_LANGUAGE,
            process_tree::EBI_PROCESS_TREE,
            stochastic_labelled_petri_net::EBI_STOCHASTIC_LABELLED_PETRI_NET,
        },
        ebi_framework::{
            ebi_file_handler::EbiFileHandler, ebi_input::EbiInput, ebi_trait::FromEbiTraitObject,
        },
    };

    #[test]
    fn file_handlers() {
        assert_eq!(
            EbiFileHandler::from_str("slang").unwrap(),
            EBI_FINITE_STOCHASTIC_LANGUAGE
        );
        assert!(EbiFileHandler::from_str("blablabla44252435").is_err());

        EbiFileHandler::get_producing_commands(&EBI_PROCESS_TREE);
        EbiFileHandler::get_producing_commands(&EBI_STOCHASTIC_LABELLED_PETRI_NET);

        assert!(
            EBI_PROCESS_TREE
                .cmp(&EBI_STOCHASTIC_LABELLED_PETRI_NET)
                .is_lt()
        );

        EbiFileHandler::from_trait_object(EbiInput::FileHandler(EBI_EXECUTIONS)).unwrap();
    }
}
