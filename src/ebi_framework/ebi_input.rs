use super::{
    ebi_command::{EBI_COMMANDS, EbiCommand},
    ebi_file_handler::EBI_FILE_HANDLERS,
    ebi_trait::{EbiTrait, FromEbiTraitObject},
};
use crate::{
    ebi_framework::{
        ebi_file_handler::{EbiFileHandler, get_file_handlers},
        ebi_importer_parameters,
        ebi_trait_object::EbiTraitObject,
    },
    ebi_traits::{
        ebi_trait_activities::EbiTraitActivities, ebi_trait_event_log::EbiTraitEventLog,
        ebi_trait_event_log_trace_attributes::EbiTraitEventLogTraceAttributes,
        ebi_trait_finite_language::EbiTraitFiniteLanguage,
        ebi_trait_finite_stochastic_language::EbiTraitFiniteStochasticLanguage,
        ebi_trait_graphable::EbiTraitGraphable,
        ebi_trait_iterable_language::EbiTraitIterableLanguage,
        ebi_trait_iterable_stochastic_language::EbiTraitIterableStochasticLanguage,
        ebi_trait_queriable_stochastic_language::EbiTraitQueriableStochasticLanguage,
        ebi_trait_semantics::EbiTraitSemantics,
        ebi_trait_stochastic_deterministic_semantics::EbiTraitStochasticDeterministicSemantics,
        ebi_trait_stochastic_semantics::EbiTraitStochasticSemantics,
    },
    ebi_validate,
    multiple_reader::MultipleReader,
    text::Joiner,
};
use anyhow::{Context, Result, anyhow};
use clap::{ArgMatches, builder::ValueParser, value_parser};
use ebi_objects::{
    EbiObject, EbiObjectType,
    ebi_arithmetic::{ConstFraction, Fraction, parsing::FractionNotParsedYet},
    traits::importable::{ImporterParameter, ImporterParameterValues},
};
use std::{
    collections::{BTreeSet, HashSet},
    fmt::{self, Debug, Display},
    fs::File,
    io::BufRead,
    path::PathBuf,
};
use strum_macros::{EnumIter, IntoStaticStr};

pub enum EbiInput {
    Trait(EbiTraitObject, &'static EbiFileHandler),
    Object(EbiObject, &'static EbiFileHandler),
    String(String, &'static EbiInputType),
    Usize(usize, &'static EbiInputType),
    FileHandler(EbiFileHandler),
    Fraction(Fraction, &'static EbiInputType),
}

impl EbiInput {
    pub fn to_type<T: FromEbiTraitObject + ?Sized>(self) -> Result<Box<T>> {
        FromEbiTraitObject::from_trait_object(self)
    }

    pub fn get_type(&self) -> EbiInputType {
        match self {
            EbiInput::Trait(t, _) => EbiInputType::Trait(t.get_trait()),
            EbiInput::Object(o, _) => EbiInputType::Object(o.get_type()),
            EbiInput::String(_, input_type) => (*input_type).clone(),
            EbiInput::Usize(_, input_type) => (*input_type).clone(),
            EbiInput::FileHandler(_) => EbiInputType::FileHandler,
            EbiInput::Fraction(_, input_type) => (*input_type).clone(),
        }
    }
}

#[macro_export]
macro_rules! EbiInputTypeEnum {
    ($t:ident) => {
        EbiInputType::String(Some($t::VARIANTS), None)
    };
}

#[derive(PartialEq, Eq, EnumIter, Clone, Debug)]
pub enum EbiInputType {
    Trait(EbiTrait),
    Object(EbiObjectType),
    AnyObject,
    FileHandler,

    ///Fields: allowed values, default value.
    /// If the allowed values are limited, consider using an enum with a #[derive(EbiInputEnum)] and &[&EbiInputTypeEnum!(type)] as input type.
    String(Option<&'static [&'static str]>, Option<&'static str>),

    ///Fields: minimum, maximum, default value.
    Usize(Option<usize>, Option<usize>, Option<usize>),

    //Fields: minimum, maximum, default value.
    Fraction(
        Option<ConstFraction>,
        Option<ConstFraction>,
        Option<ConstFraction>,
    ),
}

impl EbiInputType {
    pub fn default(&self) -> Option<String> {
        match self {
            EbiInputType::Trait(_) => None,
            EbiInputType::Object(_) => None,
            EbiInputType::AnyObject => None,
            EbiInputType::FileHandler => None,
            EbiInputType::String(_, string) => match string {
                Some(string) => Some(string.to_string()),
                None => None,
            },
            EbiInputType::Usize(_, _, integer) => match integer {
                Some(integer) => Some(integer.to_string()),
                None => None,
            },
            EbiInputType::Fraction(_, _, default) => match default {
                Some(fraction) => Some(fraction.to_string()),
                None => None,
            },
        }
    }

    pub fn get_article(&self) -> &str {
        match self {
            EbiInputType::Trait(t) => t.get_article(),
            EbiInputType::Object(o) => o.get_article(),
            EbiInputType::AnyObject => "an",
            EbiInputType::String(_, _) => "a",
            EbiInputType::Usize(_, _, _) => "an",
            EbiInputType::FileHandler => "a",
            EbiInputType::Fraction(_, _, _) => "a",
        }
    }

    pub fn get_parser_of_list(traits: &[&EbiInputType]) -> ValueParser {
        match traits[0] {
            EbiInputType::Trait(_) => value_parser!(PathBuf),
            EbiInputType::Object(_) => value_parser!(PathBuf),
            EbiInputType::AnyObject => value_parser!(PathBuf),
            EbiInputType::String(_, _) => value_parser!(String).into(),
            EbiInputType::Usize(_, _, _) => value_parser!(usize).into(),
            EbiInputType::FileHandler => value_parser!(EbiFileHandler).into(),
            EbiInputType::Fraction(_, _, _) => value_parser!(FractionNotParsedYet).into(),
        }
    }

    pub fn get_possible_inputs(traits: &[&EbiInputType]) -> Vec<String> {
        let mut result = HashSet::new();

        for input_type in traits {
            match input_type {
                EbiInputType::Trait(t) => {
                    result.extend(Self::show_file_handlers(t.get_file_handlers()));
                }
                EbiInputType::Object(o) => {
                    result.extend(Self::show_file_handlers(get_file_handlers(o)));
                }
                EbiInputType::AnyObject => {
                    result.extend(Self::show_file_handlers(EBI_FILE_HANDLERS.iter().collect()));
                }
                EbiInputType::String(Some(allowed_values), _) => {
                    result.insert(format!("either one of `{}`", allowed_values.join("`, `")));
                }
                EbiInputType::String(None, _) => {
                    result.insert("text".to_string());
                }
                EbiInputType::Usize(Some(min), Some(max), _) => {
                    result.insert(format!("integer between {} and {}", min, max));
                }
                EbiInputType::Usize(None, Some(max), _) => {
                    result.insert(format!("integer below or equal to {}", max));
                }
                EbiInputType::Usize(Some(min), _, _) => {
                    result.insert(format!("integer above or equal to {}", min));
                }
                EbiInputType::Usize(_, _, _) => {
                    result.insert("integer".to_string());
                }
                EbiInputType::FileHandler => {
                    let extensions: Vec<String> = EBI_FILE_HANDLERS
                        .iter()
                        .filter_map(|file_type| {
                            if file_type.validator.is_some() {
                                Some(file_type.file_extension.to_string())
                            } else {
                                None
                            }
                        })
                        .collect();
                    result.insert(
                        "the file extension of any file type supported by Ebi (".to_owned()
                            + extensions.join_with(", ", " or ").as_str()
                            + ")",
                    );
                }
                EbiInputType::Fraction(Some(min), Some(max), _) => {
                    result.insert(format!("fraction between {} and {}", min, max));
                }
                EbiInputType::Fraction(None, Some(max), _) => {
                    result.insert(format!("fraction below {}", max));
                }
                EbiInputType::Fraction(Some(min), _, _) => {
                    result.insert(format!("fraction above {}", min));
                }
                EbiInputType::Fraction(_, _, _) => {
                    result.insert("fraction".to_string());
                }
            };
        }

        result.into_iter().collect::<Vec<_>>()
    }

    pub fn get_possible_inputs_with_latex(traits: &[&EbiInputType]) -> Vec<String> {
        let mut result = HashSet::new();

        for input_type in traits {
            match input_type {
                EbiInputType::Trait(t) => {
                    result.extend(Self::show_file_handlers_latex(t.get_file_handlers()));
                }
                EbiInputType::Object(o) => {
                    result.extend(Self::show_file_handlers_latex(get_file_handlers(o)));
                }
                EbiInputType::AnyObject => {
                    result.extend(Self::show_file_handlers_latex(
                        EBI_FILE_HANDLERS.iter().collect(),
                    ));
                }
                EbiInputType::String(Some(allowed_values), _) => {
                    result.insert(format!("either one of `{}`", allowed_values.join("`, `")));
                }
                EbiInputType::String(None, _) => {
                    result.insert("text".to_string());
                }
                EbiInputType::Usize(Some(min), Some(max), _) => {
                    result.insert(format!("integer between {} and {}", min, max));
                }
                EbiInputType::Usize(None, Some(max), _) => {
                    result.insert(format!("integer below {}", max));
                }
                EbiInputType::Usize(Some(min), _, _) => {
                    result.insert(format!("integer above {}", min));
                }
                EbiInputType::Usize(_, _, _) => {
                    result.insert("integer".to_string());
                }
                EbiInputType::FileHandler => {
                    let extensions: Vec<String> = EBI_FILE_HANDLERS
                        .iter()
                        .filter_map(|file_type| {
                            if file_type.validator.is_some() {
                                Some(file_type.file_extension.to_string())
                            } else {
                                None
                            }
                        })
                        .collect();
                    result.insert(
                        "the file extension of any file type supported by Ebi (".to_owned()
                            + extensions.join_with(", ", " or ").as_str()
                            + ")",
                    );
                }
                EbiInputType::Fraction(Some(min), Some(max), _) => {
                    result.insert(format!("fraction between {} and {}", min, max));
                }
                EbiInputType::Fraction(None, Some(max), _) => {
                    result.insert(format!("fraction below {}", max));
                }
                EbiInputType::Fraction(Some(min), _, _) => {
                    result.insert(format!("fraction above {}", min));
                }
                EbiInputType::Fraction(_, _, _) => {
                    result.insert("fraction".to_string());
                }
            };
        }

        result.into_iter().collect::<Vec<_>>()
    }

    pub fn possible_inputs_as_strings_with_articles(
        traits: &[&EbiInputType],
        last_connector: &str,
    ) -> String {
        let list = Self::get_possible_inputs(traits);
        list.join_with(", ", last_connector)
    }

    pub fn show_file_handlers(file_handlers: Vec<&'static EbiFileHandler>) -> Vec<String> {
        file_handlers
            .iter()
            .map(|file_handler| format!("{}", file_handler))
            .collect::<Vec<_>>()
    }

    pub fn show_file_handlers_latex(file_handlers: Vec<&'static EbiFileHandler>) -> Vec<String> {
        file_handlers
            .iter()
            .map(|file_handler| {
                format!(
                    "\\hyperref[filehandler:{}]{{{}}}",
                    file_handler.name, file_handler
                )
            })
            .collect::<Vec<_>>()
    }

    pub fn get_applicable_commands(&self) -> BTreeSet<Vec<&'static EbiCommand>> {
        let mut result = EBI_COMMANDS.get_command_paths();
        result.retain(|path| {
            if let EbiCommand::Command { input_types, .. } = path[path.len() - 1] {
                for input_typess in input_types.iter() {
                    for input_typesss in input_typess.iter() {
                        if input_typesss == &self {
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

impl Display for EbiInputType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            EbiInputType::Trait(t) => Display::fmt(t, f),
            EbiInputType::Object(o) => Display::fmt(o, f),
            EbiInputType::AnyObject => write!(f, "object"),
            EbiInputType::String(Some(allowed_values), _) => {
                write!(f, "either one of `{}`", allowed_values.join("`, `"))
            }
            EbiInputType::String(None, _) => write!(f, "text"),
            EbiInputType::Usize(Some(min), Some(max), _) => {
                write!(f, "integer between {} and {}", min, max)
            }
            EbiInputType::Usize(None, Some(max), _) => {
                write!(f, "integer below {}", max)
            }
            EbiInputType::Usize(Some(min), _, _) => {
                write!(f, "integer above {}", min)
            }
            EbiInputType::Usize(_, _, _) => {
                write!(f, "integer")
            }
            EbiInputType::FileHandler => write!(f, "file"),
            EbiInputType::Fraction(Some(min), Some(max), _) => {
                write!(f, "fraction between {} and {}", min, max)
            }
            EbiInputType::Fraction(None, Some(max), _) => {
                write!(f, "fraction below {}", max)
            }
            EbiInputType::Fraction(Some(min), _, _) => {
                write!(f, "fraction above {}", min)
            }
            EbiInputType::Fraction(_, _, _) => {
                write!(f, "fraction")
            }
        }
    }
}

/**
 * Returns true if one of the input types has a default.
 */
pub fn default(input_types: &[&EbiInputType]) -> Option<String> {
    input_types
        .iter()
        .filter_map(|input_type| input_type.default())
        .next()
}

#[derive(Debug, Clone)]
pub enum EbiTraitImporter {
    FiniteLanguage(
        fn(&mut dyn BufRead, &ImporterParameterValues) -> Result<Box<dyn EbiTraitFiniteLanguage>>,
        &'static [ImporterParameter],
    ),
    FiniteStochasticLanguage(
        fn(
            &mut dyn BufRead,
            &ImporterParameterValues,
        ) -> Result<Box<dyn EbiTraitFiniteStochasticLanguage>>,
        &'static [ImporterParameter],
    ),
    QueriableStochasticLanguage(
        fn(
            &mut dyn BufRead,
            &ImporterParameterValues,
        ) -> Result<Box<dyn EbiTraitQueriableStochasticLanguage>>,
        &'static [ImporterParameter],
    ),
    IterableLanguage(
        fn(&mut dyn BufRead, &ImporterParameterValues) -> Result<Box<dyn EbiTraitIterableLanguage>>,
        &'static [ImporterParameter],
    ),
    IterableStochasticLanguage(
        fn(
            &mut dyn BufRead,
            &ImporterParameterValues,
        ) -> Result<Box<dyn EbiTraitIterableStochasticLanguage>>,
        &'static [ImporterParameter],
    ),
    EventLog(
        fn(&mut dyn BufRead, &ImporterParameterValues) -> Result<Box<dyn EbiTraitEventLog>>,
        &'static [ImporterParameter],
    ),
    EventLogTraceAttributes(
        fn(
            &mut dyn BufRead,
            &ImporterParameterValues,
        ) -> Result<Box<dyn EbiTraitEventLogTraceAttributes>>,
        &'static [ImporterParameter],
    ),
    Semantics(
        fn(&mut dyn BufRead, &ImporterParameterValues) -> Result<EbiTraitSemantics>,
        &'static [ImporterParameter],
    ),
    StochasticSemantics(
        fn(&mut dyn BufRead, &ImporterParameterValues) -> Result<EbiTraitStochasticSemantics>,
        &'static [ImporterParameter],
    ),
    StochasticDeterministicSemantics(
        fn(
            &mut dyn BufRead,
            &ImporterParameterValues,
        ) -> Result<EbiTraitStochasticDeterministicSemantics>,
        &'static [ImporterParameter],
    ),
    Graphable(
        fn(&mut dyn BufRead, &ImporterParameterValues) -> Result<Box<dyn EbiTraitGraphable>>,
        &'static [ImporterParameter],
    ),
    Activities(
        fn(&mut dyn BufRead, &ImporterParameterValues) -> Result<Box<dyn EbiTraitActivities>>,
        &'static [ImporterParameter],
    ),
}

impl EbiTraitImporter {
    pub fn get_trait(&self) -> EbiTrait {
        match self {
            EbiTraitImporter::FiniteLanguage(_, _) => EbiTrait::FiniteLanguage,
            EbiTraitImporter::FiniteStochasticLanguage(_, _) => EbiTrait::FiniteStochasticLanguage,
            EbiTraitImporter::QueriableStochasticLanguage(_, _) => {
                EbiTrait::QueriableStochasticLanguage
            }
            EbiTraitImporter::IterableLanguage(_, _) => EbiTrait::IterableLanguage,
            EbiTraitImporter::IterableStochasticLanguage(_, _) => {
                EbiTrait::IterableStochasticLanguage
            }
            EbiTraitImporter::EventLog(_, _) => EbiTrait::EventLog,
            EbiTraitImporter::EventLogTraceAttributes(_, _) => EbiTrait::EventLogTraceAttributes,
            EbiTraitImporter::Semantics(_, _) => EbiTrait::Semantics,
            EbiTraitImporter::StochasticSemantics(_, _) => EbiTrait::StochasticSemantics,
            EbiTraitImporter::StochasticDeterministicSemantics(_, _) => {
                EbiTrait::StochasticDeterministicSemantics
            }
            EbiTraitImporter::Graphable(_, _) => EbiTrait::Graphable,
            EbiTraitImporter::Activities(_, _) => EbiTrait::Activities,
        }
    }

    pub fn parameters(&self) -> &'static [ImporterParameter] {
        match self {
            EbiTraitImporter::FiniteLanguage(_, importer_parameters)
            | EbiTraitImporter::FiniteStochasticLanguage(_, importer_parameters)
            | EbiTraitImporter::QueriableStochasticLanguage(_, importer_parameters)
            | EbiTraitImporter::IterableLanguage(_, importer_parameters)
            | EbiTraitImporter::IterableStochasticLanguage(_, importer_parameters)
            | EbiTraitImporter::EventLog(_, importer_parameters)
            | EbiTraitImporter::EventLogTraceAttributes(_, importer_parameters)
            | EbiTraitImporter::Semantics(_, importer_parameters)
            | EbiTraitImporter::StochasticSemantics(_, importer_parameters)
            | EbiTraitImporter::StochasticDeterministicSemantics(_, importer_parameters)
            | EbiTraitImporter::Graphable(_, importer_parameters)
            | EbiTraitImporter::Activities(_, importer_parameters) => importer_parameters,
        }
    }

    pub fn default_parameter_values(&self) -> ImporterParameterValues {
        let mut result = ImporterParameterValues::new();
        for parameter in self.parameters() {
            result.insert(*parameter, parameter.default());
        }
        result
    }

    pub fn import(
        &self,
        reader: &mut dyn BufRead,
        parameter_values: &ImporterParameterValues,
    ) -> Result<EbiTraitObject> {
        Ok(match self {
            EbiTraitImporter::FiniteLanguage(f, _) => {
                EbiTraitObject::FiniteLanguage((f)(reader, parameter_values)?)
            }
            EbiTraitImporter::FiniteStochasticLanguage(f, _) => {
                EbiTraitObject::FiniteStochasticLanguage((f)(reader, parameter_values)?)
            }
            EbiTraitImporter::QueriableStochasticLanguage(f, _) => {
                EbiTraitObject::QueriableStochasticLanguage((f)(reader, parameter_values)?)
            }
            EbiTraitImporter::IterableLanguage(f, _) => {
                EbiTraitObject::IterableLanguage((f)(reader, parameter_values)?)
            }
            EbiTraitImporter::IterableStochasticLanguage(f, _) => {
                EbiTraitObject::IterableStochasticLanguage((f)(reader, parameter_values)?)
            }
            EbiTraitImporter::EventLog(f, _) => {
                EbiTraitObject::EventLog((f)(reader, parameter_values)?)
            }
            EbiTraitImporter::EventLogTraceAttributes(f, _) => {
                EbiTraitObject::EventLogTraceAttributes((f)(reader, parameter_values)?)
            }
            EbiTraitImporter::Semantics(f, _) => {
                EbiTraitObject::Semantics((f)(reader, parameter_values)?)
            }
            EbiTraitImporter::StochasticSemantics(f, _) => {
                EbiTraitObject::StochasticSemantics((f)(reader, parameter_values)?)
            }
            EbiTraitImporter::StochasticDeterministicSemantics(f, _) => {
                EbiTraitObject::StochasticDeterministicSemantics((f)(reader, parameter_values)?)
            }
            EbiTraitImporter::Graphable(f, _) => {
                EbiTraitObject::Graphable((f)(reader, parameter_values)?)
            }
            EbiTraitImporter::Activities(f, _) => {
                EbiTraitObject::Activities((f)(reader, parameter_values)?)
            }
        })
    }
}

impl Display for EbiTraitImporter {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.get_trait().to_string())
    }
}

#[derive(Clone, IntoStaticStr)]
pub enum EbiObjectImporter {
    EventLog(
        fn(&mut dyn BufRead, &ImporterParameterValues) -> Result<EbiObject>,
        &'static [ImporterParameter],
    ),
    EventLogTraceAttributes(
        fn(&mut dyn BufRead, &ImporterParameterValues) -> Result<EbiObject>,
        &'static [ImporterParameter],
    ),
    EventLogCsv(
        fn(&mut dyn BufRead, &ImporterParameterValues) -> Result<EbiObject>,
        &'static [ImporterParameter],
    ),
    EventLogXes(
        fn(&mut dyn BufRead, &ImporterParameterValues) -> Result<EbiObject>,
        &'static [ImporterParameter],
    ),
    DirectlyFollowsGraph(
        fn(&mut dyn BufRead, &ImporterParameterValues) -> Result<EbiObject>,
        &'static [ImporterParameter],
    ),
    DirectlyFollowsModel(
        fn(&mut dyn BufRead, &ImporterParameterValues) -> Result<EbiObject>,
        &'static [ImporterParameter],
    ),
    StochasticDirectlyFollowsModel(
        fn(&mut dyn BufRead, &ImporterParameterValues) -> Result<EbiObject>,
        &'static [ImporterParameter],
    ),
    FiniteLanguage(
        fn(&mut dyn BufRead, &ImporterParameterValues) -> Result<EbiObject>,
        &'static [ImporterParameter],
    ),
    FiniteStochasticLanguage(
        fn(&mut dyn BufRead, &ImporterParameterValues) -> Result<EbiObject>,
        &'static [ImporterParameter],
    ),
    LabelledPetriNet(
        fn(&mut dyn BufRead, &ImporterParameterValues) -> Result<EbiObject>,
        &'static [ImporterParameter],
    ),
    StochasticDeterministicFiniteAutomaton(
        fn(&mut dyn BufRead, &ImporterParameterValues) -> Result<EbiObject>,
        &'static [ImporterParameter],
    ),
    StochasticNondeterministicFiniteAutomaton(
        fn(&mut dyn BufRead, &ImporterParameterValues) -> Result<EbiObject>,
        &'static [ImporterParameter],
    ),
    StochasticLabelledPetriNet(
        fn(&mut dyn BufRead, &ImporterParameterValues) -> Result<EbiObject>,
        &'static [ImporterParameter],
    ),
    LanguageOfAlignments(
        fn(&mut dyn BufRead, &ImporterParameterValues) -> Result<EbiObject>,
        &'static [ImporterParameter],
    ),
    DeterministicFiniteAutomaton(
        fn(&mut dyn BufRead, &ImporterParameterValues) -> Result<EbiObject>,
        &'static [ImporterParameter],
    ),
    ProcessTree(
        fn(&mut dyn BufRead, &ImporterParameterValues) -> Result<EbiObject>,
        &'static [ImporterParameter],
    ),
    Executions(
        fn(&mut dyn BufRead, &ImporterParameterValues) -> Result<EbiObject>,
        &'static [ImporterParameter],
    ),
    StochasticLanguageOfAlignments(
        fn(&mut dyn BufRead, &ImporterParameterValues) -> Result<EbiObject>,
        &'static [ImporterParameter],
    ),
    StochasticProcessTree(
        fn(&mut dyn BufRead, &ImporterParameterValues) -> Result<EbiObject>,
        &'static [ImporterParameter],
    ),
}

impl EbiObjectImporter {
    pub fn get_type(&self) -> EbiObjectType {
        match self {
            EbiObjectImporter::EventLog(_, _) => EbiObjectType::EventLog,
            EbiObjectImporter::EventLogCsv(_, _) => EbiObjectType::EventLogCsv,
            EbiObjectImporter::EventLogTraceAttributes(_, _) => {
                EbiObjectType::EventLogTraceAttributes
            }
            EbiObjectImporter::EventLogXes(_, _) => EbiObjectType::EventLogXes,
            EbiObjectImporter::DirectlyFollowsGraph(_, _) => EbiObjectType::DirectlyFollowsGraph,
            EbiObjectImporter::DirectlyFollowsModel(_, _) => EbiObjectType::DirectlyFollowsModel,
            EbiObjectImporter::StochasticDirectlyFollowsModel(_, _) => {
                EbiObjectType::StochasticDirectlyFollowsModel
            }
            EbiObjectImporter::FiniteLanguage(_, _) => EbiObjectType::FiniteLanguage,
            EbiObjectImporter::FiniteStochasticLanguage(_, _) => {
                EbiObjectType::FiniteStochasticLanguage
            }
            EbiObjectImporter::LabelledPetriNet(_, _) => EbiObjectType::LabelledPetriNet,
            EbiObjectImporter::StochasticDeterministicFiniteAutomaton(_, _) => {
                EbiObjectType::StochasticDeterministicFiniteAutomaton
            }
            EbiObjectImporter::StochasticNondeterministicFiniteAutomaton(_, _) => {
                EbiObjectType::StochasticNondeterministicFiniteAutomaton
            }
            EbiObjectImporter::StochasticLabelledPetriNet(_, _) => {
                EbiObjectType::StochasticLabelledPetriNet
            }
            EbiObjectImporter::LanguageOfAlignments(_, _) => EbiObjectType::LanguageOfAlignments,
            EbiObjectImporter::StochasticLanguageOfAlignments(_, _) => {
                EbiObjectType::StochasticLanguageOfAlignments
            }
            EbiObjectImporter::DeterministicFiniteAutomaton(_, _) => {
                EbiObjectType::DeterministicFiniteAutomaton
            }
            EbiObjectImporter::ProcessTree(_, _) => EbiObjectType::ProcessTree,
            EbiObjectImporter::StochasticProcessTree(_, _) => EbiObjectType::StochasticProcessTree,
            EbiObjectImporter::Executions(_, _) => EbiObjectType::Executions,
        }
    }

    pub fn parameters(&self) -> &'static [ImporterParameter] {
        match self {
            EbiObjectImporter::EventLog(_, parameters) => parameters,
            EbiObjectImporter::EventLogCsv(_, parameters) => parameters,
            EbiObjectImporter::EventLogTraceAttributes(_, parameters) => parameters,
            EbiObjectImporter::EventLogXes(_, parameters) => parameters,
            EbiObjectImporter::DirectlyFollowsGraph(_, parameters) => parameters,
            EbiObjectImporter::DirectlyFollowsModel(_, parameters) => parameters,
            EbiObjectImporter::StochasticDirectlyFollowsModel(_, parameters) => parameters,
            EbiObjectImporter::FiniteLanguage(_, parameters) => parameters,
            EbiObjectImporter::FiniteStochasticLanguage(_, parameters) => parameters,
            EbiObjectImporter::LabelledPetriNet(_, parameters) => parameters,
            EbiObjectImporter::StochasticDeterministicFiniteAutomaton(_, parameters) => parameters,
            EbiObjectImporter::StochasticNondeterministicFiniteAutomaton(_, parameters) => {
                parameters
            }
            EbiObjectImporter::StochasticLabelledPetriNet(_, parameters) => parameters,
            EbiObjectImporter::LanguageOfAlignments(_, parameters) => parameters,
            EbiObjectImporter::DeterministicFiniteAutomaton(_, parameters) => parameters,
            EbiObjectImporter::ProcessTree(_, parameters) => parameters,
            EbiObjectImporter::Executions(_, parameters) => parameters,
            EbiObjectImporter::StochasticLanguageOfAlignments(_, parameters) => parameters,
            EbiObjectImporter::StochasticProcessTree(_, parameters) => parameters,
        }
    }

    pub fn default_parameter_values(&self) -> ImporterParameterValues {
        let mut result = ImporterParameterValues::new();
        for parameter in self.parameters() {
            result.insert(*parameter, parameter.default());
        }
        result
    }

    pub fn get_importer(
        &self,
    ) -> fn(&mut dyn BufRead, &ImporterParameterValues) -> Result<EbiObject> {
        match self {
            EbiObjectImporter::EventLog(importer, _) => *importer,
            EbiObjectImporter::EventLogCsv(importer, _) => *importer,
            EbiObjectImporter::EventLogTraceAttributes(importer, _) => *importer,
            EbiObjectImporter::EventLogXes(importer, _) => *importer,
            EbiObjectImporter::DirectlyFollowsGraph(importer, _) => *importer,
            EbiObjectImporter::DirectlyFollowsModel(importer, _) => *importer,
            EbiObjectImporter::StochasticDirectlyFollowsModel(importer, _) => *importer,
            EbiObjectImporter::FiniteLanguage(importer, _) => *importer,
            EbiObjectImporter::FiniteStochasticLanguage(importer, _) => *importer,
            EbiObjectImporter::LabelledPetriNet(importer, _) => *importer,
            EbiObjectImporter::StochasticDeterministicFiniteAutomaton(importer, _) => *importer,
            EbiObjectImporter::StochasticNondeterministicFiniteAutomaton(importer, _) => *importer,
            EbiObjectImporter::StochasticLabelledPetriNet(importer, _) => *importer,
            EbiObjectImporter::LanguageOfAlignments(importer, _) => *importer,
            EbiObjectImporter::StochasticLanguageOfAlignments(importer, _) => *importer,
            EbiObjectImporter::DeterministicFiniteAutomaton(importer, _) => *importer,
            EbiObjectImporter::ProcessTree(importer, _) => *importer,
            EbiObjectImporter::StochasticProcessTree(importer, _) => *importer,
            EbiObjectImporter::Executions(importer, _) => *importer,
        }
    }
}

impl Display for EbiObjectImporter {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let right: &'static str = self.into();
        write!(f, "{}", right)
    }
}

impl Debug for EbiObjectImporter {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let right: &'static str = self.into();
        write!(f, "{}", right)
    }
}

pub fn get_reader_file(from_file: &PathBuf) -> Result<MultipleReader> {
    if from_file.as_os_str() == "-" {
        return MultipleReader::from_stdin();
    } else {
        let file = File::open(from_file)
            .with_context(|| format!("Could not read file `{}`.", from_file.display()))?;
        return Ok(MultipleReader::from_file(file));
    }
}

pub fn get_reader(cli_matches: &ArgMatches, cli_id: &str) -> Result<MultipleReader> {
    if let Some(from_file) = cli_matches.try_get_one::<PathBuf>(cli_id)? {
        if from_file.as_os_str() == "-" {
            return MultipleReader::from_stdin();
        } else {
            let file = File::open(from_file)
                .with_context(|| format!("Could not read file `{}`.", from_file.display()))?;
            return Ok(MultipleReader::from_file(file));
        }
    } else {
        return Err(anyhow!(
            "No argument given, or it could not be parsed as a path."
        ));
    }
}

pub fn read_as_trait(
    etrait: &EbiTrait,
    reader: &mut MultipleReader,
    cli_matches: Option<&ArgMatches>,
    input_index: usize,
) -> Result<(EbiTraitObject, &'static EbiFileHandler)> {
    let mut error = None;
    for file_handler in EBI_FILE_HANDLERS {
        for importer in file_handler.trait_importers {
            if &importer.get_trait() == etrait {
                //attempt to import
                match ebi_importer_parameters::extract_parameter_values(
                    cli_matches,
                    importer.parameters(),
                    input_index,
                ) {
                    Ok(importer_parameter_values) => {
                        match importer.import(reader.get().with_context(|| "Obtaining reader.")?.as_mut(), &importer_parameter_values)
                            .with_context(|| {format!("The last attempted importer was: {}.", file_handler)})
                            .with_context(|| format!("Attempting to parse file as either {}.\nIf you know the type of your file, use `Ebi {}` to check it.", EbiInputType::show_file_handlers(etrait.get_file_handlers()).join(", "), ebi_validate!())) {
                            Ok(object) => {return Ok((object, file_handler));}, //object parsed, return it
                            Err(err) => {error = Some(err);}
                        }
                    }
                    Err(e) => error = Some(e),
                }
            }
        }
    }
    Err(error.unwrap())
}

pub fn read_as_object(
    etype: &EbiObjectType,
    reader: &mut MultipleReader,
    cli_matches: Option<&ArgMatches>,
    input_index: usize,
) -> Result<(EbiObject, &'static EbiFileHandler)> {
    for file_handler in EBI_FILE_HANDLERS {
        for importer in file_handler.object_importers {
            if &importer.get_type() == etype {
                //attempt to import
                match ebi_importer_parameters::extract_parameter_values(
                    cli_matches,
                    importer.parameters(),
                    input_index,
                ) {
                    Ok(importer_parameter_values) => {
                        if let Ok(object) = (importer.get_importer())(
                            reader.get().context("Could not obtain reader.")?.as_mut(),
                            &importer_parameter_values,
                        ) {
                            //object parsed; return it
                            return Ok((object, file_handler));
                        }
                    }
                    Err(_) => {}
                }
            }
        }
    }
    Err(anyhow!(
        "File could not be recognised. If you know the file format, try validating it with `Ebi validate [file type]'."
    ))
}

pub fn read_as_any_object(
    reader: &mut MultipleReader,
    cli_matches: Option<&ArgMatches>,
    input_index: usize,
) -> Result<(EbiObject, &'static EbiFileHandler)> {
    for file_handler in EBI_FILE_HANDLERS {
        //attempt to import
        for importer in file_handler.object_importers {
            match ebi_importer_parameters::extract_parameter_values(
                cli_matches,
                importer.parameters(),
                input_index,
            ) {
                Ok(importer_parameter_values) => {
                    if let Ok(object) = (importer.get_importer())(
                        reader.get().context("Could not obtain reader.")?.as_mut(),
                        &importer_parameter_values,
                    ) {
                        //object parsed; return it
                        return Ok((object, file_handler));
                    }
                }
                Err(_) => {}
            }
        }
    }
    Err(anyhow!(
        "File could not be recognised. If you know the file type, try validating it with `Ebi validate [file type]'."
    ))
}

pub fn validate_object_of(
    reader: &mut MultipleReader,
    file_handler: &EbiFileHandler,
) -> Result<()> {
    if let Some(validator) = file_handler.validator {
        let result = (validator)(reader.get()?.as_mut());
        return result;
    } else {
        return Err(anyhow!(
            "file hanlder {} does not have a validator",
            file_handler
        ));
    }
}

pub const TEST_INPUT_TYPE_FRACTION: EbiInputType = EbiInputType::Fraction(None, None, None);
pub const TEST_INPUT_TYPE_USIZE: EbiInputType = EbiInputType::Usize(None, None, None);
pub const TEST_INPUT_TYPE_STRING: EbiInputType = EbiInputType::String(None, None);

#[cfg(test)]
mod tests {
    use super::{EbiInputType, validate_object_of};
    use crate::ebi_framework::ebi_input;
    use crate::{
        ebi_file_handlers::compressed_event_log::EBI_COMPRESSED_EVENT_LOG,
        ebi_framework::{
            ebi_file_handler::EBI_FILE_HANDLERS,
            ebi_input::{EbiInput, TEST_INPUT_TYPE_FRACTION, TEST_INPUT_TYPE_USIZE},
        },
        multiple_reader::MultipleReader,
    };
    use ebi_objects::ebi_arithmetic::{Fraction, Zero};
    use std::{
        fs::{self, File},
        path::PathBuf,
    };
    use strum::IntoEnumIterator;

    #[test]
    fn input_primitives() {
        EbiInput::FileHandler(EBI_COMPRESSED_EVENT_LOG).get_type();
        EbiInput::Fraction(Fraction::zero(), &TEST_INPUT_TYPE_FRACTION).get_type();
        EbiInput::Usize(0, &TEST_INPUT_TYPE_USIZE).get_type();
    }

    #[test]
    fn input_types() {
        for input_type in EbiInputType::iter() {
            let list = vec![&input_type];
            input_type.get_article();
            EbiInputType::get_parser_of_list(&list);
            input_type.get_applicable_commands();
            input_type.to_string();
            EbiInputType::get_possible_inputs(&list);
            EbiInputType::get_possible_inputs_with_latex(&list);
            EbiInputType::possible_inputs_as_strings_with_articles(&list, "xyz");
        }
    }

    #[test]
    fn validators() {
        for file_handler in EBI_FILE_HANDLERS {
            let files = fs::read_dir("./testfiles").unwrap();
            for path in files {
                let file = path.unwrap();
                println!("file {:?}", file.file_name());

                let mut reader = MultipleReader::from_file(File::open(file.path()).unwrap());

                if file_handler.validator.is_some() {
                    println!("\tfile handler {}", file_handler);
                    if !file.file_name().into_string().unwrap().contains("invalid")
                        && file
                            .file_name()
                            .into_string()
                            .unwrap()
                            .ends_with(&(".".to_string() + file_handler.file_extension))
                    {
                        //file handler should be able to accept this file
                        assert!(validate_object_of(&mut reader, file_handler).is_ok());
                    } else {
                        //file handler should not accept this file
                        assert!(validate_object_of(&mut reader, file_handler).is_err());
                    }
                }
            }
        }
    }

    #[test]
    fn reader_file() {
        let file = "testfiles/a-b.xes".parse::<PathBuf>().unwrap();
        ebi_input::get_reader_file(&file).unwrap();
    }
}
