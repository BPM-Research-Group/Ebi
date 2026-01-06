use super::{
    ebi_command::{EBI_COMMANDS, EbiCommand},
    ebi_file_handler::{EBI_FILE_HANDLERS, EbiFileHandler},
    ebi_input::{EbiInput, EbiInputType},
};
use anyhow::{Result, anyhow};
use ebi_objects::ebi_arithmetic::Fraction;
use std::{
    collections::{BTreeSet, HashSet},
    fmt::{Debug, Display},
};
use strum_macros::EnumIter;

#[derive(Clone, Copy, PartialEq, Eq, EnumIter, Hash, Default)]
pub enum EbiTrait {
    Activities,
    #[default]
    EventLog,
    EventLogTraceAttributes,
    FiniteLanguage,
    FiniteStochasticLanguage,
    Graphable,
    IterableLanguage,
    IterableStochasticLanguage,
    QueriableStochasticLanguage,
    Semantics,
    StochasticDeterministicSemantics,
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
            EbiTrait::EventLogTraceAttributes => "an",
            EbiTrait::IterableLanguage => "an",
            EbiTrait::FiniteLanguage => "a",
            EbiTrait::FiniteStochasticLanguage => "a",
            EbiTrait::IterableStochasticLanguage => "an",
            EbiTrait::QueriableStochasticLanguage => "a",
            EbiTrait::StochasticDeterministicSemantics => "a",
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

    pub fn get_explanation(&self) -> &str {
        match self {
            EbiTrait::EventLog => "Iterating over an event log will yield traces.
            For full access to an underlying XES log, use the EventLogXes object (not trait).
            \\\\
            Definition: let $\\Sigma$ be an alphabet of activities.
            Then, a \\emph{trace} $\\sigma \\in \\Sigma^*$ is a finite sequence of activities, and 
            an \\emph{event log} $L \\in (\\Sigma^*)^*$ is a sequence of traces.",
            EbiTrait::EventLogTraceAttributes => {
                "An ``event log with trace attributes'' provides an iterator over traces, where each trace may have trace attributes attached.
                For full access to an underlying XES log, use the EventLogXes object (not trait).
                \\\\
                Definition: let $\\Sigma$ be an alphabet of activities.
                Let $A \\colon \\text{attribute} \\mapsto \\text{value}$ be an attribute-value mapping, and let $\\mathcal{A}$ be the set of all attribute-value mappings.
                A \\emph{trace with attributes} $\\sigma^{A} \\in \\Sigma^* \\times \\mathcal{A}$ is a finite sequence of activities, where the sequence is annotated with attributes.
                An \\emph{event log with trace attributes} $L^A \\in (\\Sigma^* \\times \\mathcal{A})^*$ is a sequence of traces with attributes."
            }
            EbiTrait::FiniteLanguage => "Iterating over a finite language will yield each trace variant once.
            \\\\
            Definition: let $\\Sigma$ be an alphabet of activities.
            Then, a \\emph{trace} $\\sigma \\in \\Sigma^*$ is a finite sequence of activities, and 
            a \\emph{finite language} $F \\subseteq \\Sigma^*$ is a finite set of traces.",
            EbiTrait::FiniteStochasticLanguage => "Iterating over a finite stochastic language will yield each trace variant once, as well as how likely that trace variant is in the language.
            The probabilities are positive and their sum must be smaller than or equal to one.
            The ``missing'' probability means ``no trace'' or ``livelock''.
            \\\\
            Definition: let $\\Sigma$ be an alphabet of activities.
            Then, a \\emph{trace} $\\sigma \\in \\Sigma^*$ is a finite sequence of activities, and 
            a \\emph{finite stochastic language} $S \\subseteq \\Sigma^* \\mapsto \\mathcal{R}^+$ is a finite set of trace-probability tuples, such that $1 \\geq \\sum_{\\sigma \\in \\Sigma^*} S(\\sigma) $.",
            EbiTrait::IterableLanguage => {
                "An iterable language allows to walk over its traces. May iterate over infinitely many traces and not terminate."
            }
            EbiTrait::IterableStochasticLanguage => {
                "An iterable stochastic language allows to walk over its traces and their probabilities. May iterate over infinitely many traces and not terminate."
            }
            EbiTrait::QueriableStochasticLanguage => {
                "A queriable stochastic language can be given a trace, and it will return the probability of the trace."
            }
            EbiTrait::Semantics => {
                "An object in which the state space can be traversed. Each deadlock is a final state, and each final state is a deadlock. Does not need to terminate, and may end up in livelocks."
            }
            EbiTrait::StochasticDeterministicSemantics => {
                "An object in which the state space can be traversed deterministically, that is, in each state every activity appears at most once and silent steps are not present. Each deadlock is a final state, and each final state is a deadlock. Does not need to terminate, and may end up in livelocks."
            }
            EbiTrait::StochasticSemantics => {
                "An object in which the state space can be traversed, with probabilities. Each deadlock is a final state, and each final state is a deadlock. Does not need to terminate, and may end up in livelocks."
            }
            EbiTrait::Graphable => "The trait ``graphable'' allows to create a graph representation.",
            EbiTrait::Activities => "The trait ``activities'' provides access to activities.",
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
        self.to_string()
            .to_lowercase()
            .partial_cmp(&other.to_string().to_lowercase())
    }
}

impl Display for EbiTrait {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}",
            match self {
                EbiTrait::EventLog => "event log",
                EbiTrait::EventLogTraceAttributes => "event log with trace attributes",
                EbiTrait::IterableLanguage => "iterable language",
                EbiTrait::FiniteLanguage => "finite language",
                EbiTrait::FiniteStochasticLanguage => "finite stochastic language",
                EbiTrait::IterableStochasticLanguage => "iterable stochastic language",
                EbiTrait::QueriableStochasticLanguage => "queriable stochastic language",
                EbiTrait::StochasticDeterministicSemantics => "stochastic deterministic semantics",
                EbiTrait::StochasticSemantics => "stochastic semantics",
                EbiTrait::Semantics => "semantics",
                EbiTrait::Graphable => "graphable",
                EbiTrait::Activities => "activities",
            }
        )
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
                "cannot read {} {} as a fraction",
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
                "cannot read {} {} as an integer",
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
                "cannot read {} {} as an integer",
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
        prom::java_object_handler::JavaObjectHandlerQueryImport,
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
