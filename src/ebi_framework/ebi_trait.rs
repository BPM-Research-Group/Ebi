use super::{
    ebi_command::{EbiCommand, EBI_COMMANDS},
    ebi_file_handler::{EbiFileHandler, EBI_FILE_HANDLERS},
    ebi_input::{EbiInput, EbiInputType},
};
use crate::ebi_traits::{
    EbiTraitActivities, EbiTraitEventLog, EbiTraitEventLogEventAttributes,
    EbiTraitEventLogTraceAttributes, EbiTraitFiniteLanguage, EbiTraitFiniteStochasticLanguage,
    EbiTraitGraphable, EbiTraitIterableLanguage, EbiTraitIterableStochasticLanguage,
    EbiTraitQueriableStochasticLanguage, EbiTraitSemantics,
    EbiTraitStochasticDeterministicSemantics, EbiTraitStochasticPartiallyOrderedSemantics,
};
use ebi_objects::{
    anyhow::{anyhow, Result},
    ebi_arithmetic::Fraction,
};
use std::{
    collections::{BTreeSet, HashSet},
    fmt::Debug,
};
use strum_macros::{Display, EnumIter, EnumString};

#[derive(Clone, Copy, PartialEq, Eq, EnumIter, Hash, Default, EnumString, Display)]
pub enum EbiTrait {
    #[strum(serialize = "activities")]
    Activities,
    #[default]
    #[strum(serialize = "event log")]
    EventLog,
    #[strum(serialize = "event log with event attributes")]
    EventLogEventAttributes,
    #[strum(serialize = "event log with trace attributes")]
    EventLogTraceAttributes,
    #[strum(serialize = "finite language")]
    FiniteLanguage,
    #[strum(serialize = "finite stochastic language")]
    FiniteStochasticLanguage,
    #[strum(serialize = "graphable")]
    Graphable,
    #[strum(serialize = "iterable language")]
    IterableLanguage,
    #[strum(serialize = "iterable stochastic language")]
    IterableStochasticLanguage,
    #[strum(serialize = "queriable stochastic language")]
    QueriableStochasticLanguage,
    #[strum(serialize = "semantics")]
    Semantics,
    #[strum(serialize = "stochastic deterministic semantics")]
    StochasticDeterministicSemantics,
    #[strum(serialize = "stochastic partially ordered semantics")]
    StochasticPartiallyOrderedSemantics,
    #[strum(serialize = "stochastic semantics")]
    StochasticSemantics,
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
        }

        let mut result: Vec<&EbiFileHandler> = result.into_iter().collect();
        result.sort();
        result
    }

    pub fn get_article(&self) -> &str {
        match self {
            EbiTrait::EventLog => "an",
            EbiTrait::EventLogEventAttributes => "an",
            EbiTrait::EventLogTraceAttributes => "an",
            EbiTrait::IterableLanguage => "an",
            EbiTrait::FiniteLanguage => "a",
            EbiTrait::FiniteStochasticLanguage => "a",
            EbiTrait::IterableStochasticLanguage => "an",
            EbiTrait::QueriableStochasticLanguage => "a",
            EbiTrait::StochasticDeterministicSemantics => "a",
            EbiTrait::StochasticPartiallyOrderedSemantics => "a",
            EbiTrait::StochasticSemantics => "a",
            EbiTrait::Semantics => "a",
            EbiTrait::Graphable => "a",
            EbiTrait::Activities => "",
        }
    }

    pub fn get_applicable_commands(&self) -> BTreeSet<Vec<&'static EbiCommand>> {
        let mut result = EBI_COMMANDS.get_command_paths();
        result.retain(|path| {
            if let EbiCommand::Command { input_types, .. } = path[path.len() - 1] {
                for input_typess in input_types.iter() {
                    for input_typesss in input_typess.iter() {
                        if input_typesss == &&EbiInputType::Trait(*self) {
                            return true;
                        }
                    }
                }
            }
            false
        });
        result
    }

    pub fn get_explanation(&self) -> &str {
        match self {
            EbiTrait::EventLog => <ebi_objects::EventLog as EbiTraitEventLog>::trait_explanation(),
            EbiTrait::EventLogEventAttributes => {
                <ebi_objects::EventLogEventAttributes as EbiTraitEventLogEventAttributes>::trait_explanation()
            }
            EbiTrait::EventLogTraceAttributes => {
                <ebi_objects::EventLogTraceAttributes as EbiTraitEventLogTraceAttributes>::trait_explanation()
            }
            EbiTrait::FiniteLanguage => <ebi_objects::FiniteLanguage as EbiTraitFiniteLanguage>::trait_explanation(),
            EbiTrait::FiniteStochasticLanguage => {
                <ebi_objects::FiniteStochasticLanguage as EbiTraitFiniteStochasticLanguage>::trait_explanation()
            }
            EbiTrait::IterableLanguage => <ebi_objects::FiniteLanguage as EbiTraitIterableLanguage>::trait_explanation(),
            EbiTrait::IterableStochasticLanguage => {
                <ebi_objects::FiniteStochasticLanguage as EbiTraitIterableStochasticLanguage>::trait_explanation()
            }
            EbiTrait::QueriableStochasticLanguage => {
                <ebi_objects::StochasticProcessTree as EbiTraitQueriableStochasticLanguage>::trait_explanation()
            }
            EbiTrait::Semantics => EbiTraitSemantics::trait_explanation(),
            EbiTrait::StochasticPartiallyOrderedSemantics => {
                EbiTraitStochasticPartiallyOrderedSemantics::trait_explanation()
            }
            EbiTrait::StochasticDeterministicSemantics => {
                EbiTraitStochasticDeterministicSemantics::trait_explanation()
            }
            EbiTrait::StochasticSemantics => EbiTraitSemantics::trait_explanation(),
            EbiTrait::Graphable => <ebi_objects::LabelledPetriNet as EbiTraitGraphable>::trait_explanation(),
            EbiTrait::Activities => <ebi_objects::EventLog as EbiTraitActivities>::trait_explanation(),
        }
    }
}

impl Ord for EbiTrait {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        self.to_string()
            .to_lowercase()
            .cmp(&other.to_string().to_lowercase())
    }
}

impl PartialOrd for EbiTrait {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        Some(self.cmp(other))
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

impl FromEbiTraitObject for Fraction {
    fn from_trait_object(object: EbiInput) -> Result<Box<Self>> {
        match object {
            EbiInput::Fraction(e, _) => Ok(Box::new(e)),
            _ => Err(anyhow!(
                "Cannot read {} {} as a fraction.",
                object.get_type().get_article(),
                object.get_type()
            )),
        }
    }
}

impl FromEbiTraitObject for usize {
    fn from_trait_object(object: EbiInput) -> Result<Box<Self>> {
        match object {
            EbiInput::Usize(e, _) => Ok(Box::new(e)),
            _ => Err(anyhow!(
                "Cannot read {} {} as an integer.",
                object.get_type().get_article(),
                object.get_type()
            )),
        }
    }
}

impl FromEbiTraitObject for String {
    fn from_trait_object(object: EbiInput) -> Result<Box<Self>> {
        match object {
            EbiInput::String(e, _) => Ok(Box::new(e)),
            _ => Err(anyhow!(
                "Cannot read {} {} as an integer.",
                object.get_type().get_article(),
                object.get_type()
            )),
        }
    }
}

impl FromEbiTraitObject for Vec<String> {
    fn from_trait_object(object: EbiInput) -> Result<Box<Self>> {
        match object {
            EbiInput::StringList(e, _) => Ok(Box::new(e)),
            _ => Err(anyhow!(
                "Cannot read {} {} as a list of text.",
                object.get_type().get_article(),
                object.get_type()
            )),
        }
    }
}

#[cfg(test)]
mod tests {
    use strum::IntoEnumIterator;

    use crate::{
        ebi_framework::ebi_input::{EbiInput, TEST_INPUT_TYPE_STRING, TEST_INPUT_TYPE_USIZE},
        surfaces::java::java_object_handler::JavaObjectHandlerQueryImport,
    };

    use super::{EbiTrait, FromEbiTraitObject};

    #[test]
    fn traits() {
        for etrait in EbiTrait::iter() {
            etrait.get_applicable_commands();
            etrait.get_article();
            etrait.get_java_object_handlers_that_can_import();
            etrait.to_string();
            let _ = format!("{:?}", etrait);
        }

        let _ =
            String::from_trait_object(EbiInput::String("xyz".to_string(), &TEST_INPUT_TYPE_STRING));
    }

    #[test]
    #[should_panic]
    fn unreachable_string() {
        String::from_trait_object(EbiInput::Usize(1, &TEST_INPUT_TYPE_USIZE)).unwrap();
    }

    #[test]
    #[should_panic]
    fn unreachable_usize() {
        usize::from_trait_object(EbiInput::String("abc".to_string(), &TEST_INPUT_TYPE_STRING))
            .unwrap();
    }
}
