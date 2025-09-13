use anyhow::{Context, Result, anyhow};
use clap::{ArgMatches, builder::ValueParser, value_parser};
use ebi_arithmetic::{Fraction, parsing::FractionNotParsedYet};
use ebi_objects::{EbiObject, EbiObjectType};
use std::{
    collections::{BTreeSet, HashSet},
    fmt::Display,
    fs::File,
    io::BufRead,
    path::PathBuf,
};
use strum_macros::EnumIter;

use crate::{
    ebi_framework::{
        ebi_file_handler::{EbiFileHandler, get_file_handlers},
        ebi_trait_object::EbiTraitObject,
    },
    ebi_traits::{
        ebi_trait_activities::EbiTraitActivities, ebi_trait_event_log::EbiTraitEventLog,
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
    math::constant_fraction::ConstFraction,
    multiple_reader::MultipleReader,
    text::Joiner,
};

use super::{
    ebi_command::{EBI_COMMANDS, EbiCommand},
    ebi_file_handler::EBI_FILE_HANDLERS,
    ebi_trait::{EbiTrait, FromEbiTraitObject},
    prom_link::{
        JAVA_OBJECT_HANDLERS_FRACTION, JAVA_OBJECT_HANDLERS_STRING, JAVA_OBJECT_HANDLERS_USIZE,
        JavaObjectHandler,
    },
};

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

#[derive(PartialEq, Eq, EnumIter, Clone)]
pub enum EbiInputType {
    Trait(EbiTrait),
    Object(EbiObjectType),
    AnyObject,
    FileHandler,

    ///Fields: allowed values, default value.
    String(Option<&'static [&'static str]>, Option<&'static str>),

    //Fields: minimum, maximum, default value.
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

    pub fn get_java_object_handlers(&self) -> Vec<&JavaObjectHandler> {
        match self {
            EbiInputType::Trait(t) => Self::get_file_handlers_java(t.get_file_handlers()),
            EbiInputType::Object(o) => Self::get_file_handlers_java(get_file_handlers(o)),
            EbiInputType::AnyObject => {
                Self::get_file_handlers_java(EBI_FILE_HANDLERS.iter().collect())
            }
            EbiInputType::String(_, _) => {
                let mut x = vec![];
                x.extend(JAVA_OBJECT_HANDLERS_STRING);
                x
            }
            EbiInputType::Usize(_, _, _) => {
                let mut x = vec![];
                x.extend(JAVA_OBJECT_HANDLERS_USIZE);
                x
            }
            EbiInputType::FileHandler => {
                //not supported in Java;
                vec![]
            }
            EbiInputType::Fraction(_, _, _) => {
                let mut x = vec![];
                x.extend(JAVA_OBJECT_HANDLERS_FRACTION);
                x
            }
        }
    }

    pub fn get_java_object_handlers_that_can_import(&self) -> HashSet<JavaObjectHandler> {
        let mut result = HashSet::new();
        for java_object_handler in self.get_java_object_handlers() {
            if java_object_handler.translator_java_to_ebi.is_some() {
                result.insert(java_object_handler.to_owned());
            }
        }
        result
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

    pub fn get_possible_inputs_with_java(traits: &[&EbiInputType]) -> Vec<JavaObjectHandler> {
        let mut result = HashSet::new();

        for input_type in traits {
            result.extend(input_type.get_java_object_handlers());
        }

        result = result
            .into_iter()
            .filter(|java_object_handler| java_object_handler.translator_java_to_ebi.is_some())
            .collect();

        result.into_iter().cloned().collect::<Vec<_>>()
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

    pub fn get_file_handlers_java(
        file_handlers: Vec<&'static EbiFileHandler>,
    ) -> Vec<&'static JavaObjectHandler> {
        file_handlers.iter().fold(vec![], |mut list, file_handler| {
            list.extend(file_handler.java_object_handlers);
            list
        })
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
            EbiInputType::Trait(t) => t.fmt(f),
            EbiInputType::Object(o) => o.fmt(f),
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
    FiniteLanguage(fn(&mut dyn BufRead) -> Result<Box<dyn EbiTraitFiniteLanguage>>), //finite set of traces
    FiniteStochasticLanguage(
        fn(&mut dyn BufRead) -> Result<Box<dyn EbiTraitFiniteStochasticLanguage>>,
    ), //finite number of traces
    QueriableStochasticLanguage(
        fn(&mut dyn BufRead) -> Result<Box<dyn EbiTraitQueriableStochasticLanguage>>,
    ), //can query for the probability of a trace
    IterableLanguage(fn(&mut dyn BufRead) -> Result<Box<dyn EbiTraitIterableLanguage>>), //can walk over the traces, potentially forever
    IterableStochasticLanguage(
        fn(&mut dyn BufRead) -> Result<Box<dyn EbiTraitIterableStochasticLanguage>>,
    ), //can walk over the traces, potentially forever
    EventLog(fn(&mut dyn BufRead) -> Result<Box<dyn EbiTraitEventLog>>), //full XES; access to traces and attributes
    Semantics(fn(&mut dyn BufRead) -> Result<EbiTraitSemantics>), //can walk over states using transitions, potentially forever
    StochasticSemantics(fn(&mut dyn BufRead) -> Result<EbiTraitStochasticSemantics>), //can walk over states  using transitions, potentially forever
    StochasticDeterministicSemantics(
        fn(&mut dyn BufRead) -> Result<EbiTraitStochasticDeterministicSemantics>,
    ), //can walk over states using activities, potentially forever
    Graphable(fn(&mut dyn BufRead) -> Result<Box<dyn EbiTraitGraphable>>), //can produce a Dot graph
    Activities(fn(&mut dyn BufRead) -> Result<Box<dyn EbiTraitActivities>>), //has activities
}

impl EbiTraitImporter {
    pub fn get_trait(&self) -> EbiTrait {
        match self {
            EbiTraitImporter::FiniteLanguage(_) => EbiTrait::FiniteLanguage,
            EbiTraitImporter::FiniteStochasticLanguage(_) => EbiTrait::FiniteStochasticLanguage,
            EbiTraitImporter::QueriableStochasticLanguage(_) => {
                EbiTrait::QueriableStochasticLanguage
            }
            EbiTraitImporter::IterableLanguage(_) => EbiTrait::IterableLanguage,
            EbiTraitImporter::IterableStochasticLanguage(_) => EbiTrait::IterableStochasticLanguage,
            EbiTraitImporter::EventLog(_) => EbiTrait::EventLog,
            EbiTraitImporter::Semantics(_) => EbiTrait::Semantics,
            EbiTraitImporter::StochasticSemantics(_) => EbiTrait::StochasticSemantics,
            EbiTraitImporter::StochasticDeterministicSemantics(_) => {
                EbiTrait::StochasticDeterministicSemantics
            }
            EbiTraitImporter::Graphable(_) => EbiTrait::Graphable,
            EbiTraitImporter::Activities(_) => EbiTrait::Activities,
        }
    }

    pub fn import(&self, reader: &mut dyn BufRead) -> Result<EbiTraitObject> {
        Ok(match self {
            EbiTraitImporter::FiniteLanguage(f) => EbiTraitObject::FiniteLanguage((f)(reader)?),
            EbiTraitImporter::FiniteStochasticLanguage(f) => {
                EbiTraitObject::FiniteStochasticLanguage((f)(reader)?)
            }
            EbiTraitImporter::QueriableStochasticLanguage(f) => {
                EbiTraitObject::QueriableStochasticLanguage((f)(reader)?)
            }
            EbiTraitImporter::IterableLanguage(f) => EbiTraitObject::IterableLanguage((f)(reader)?),
            EbiTraitImporter::IterableStochasticLanguage(f) => {
                EbiTraitObject::IterableStochasticLanguage((f)(reader)?)
            }
            EbiTraitImporter::EventLog(f) => EbiTraitObject::EventLog((f)(reader)?),
            EbiTraitImporter::Semantics(f) => EbiTraitObject::Semantics((f)(reader)?),
            EbiTraitImporter::StochasticSemantics(f) => {
                EbiTraitObject::StochasticSemantics((f)(reader)?)
            }
            EbiTraitImporter::StochasticDeterministicSemantics(f) => {
                EbiTraitObject::StochasticDeterministicSemantics((f)(reader)?)
            }
            EbiTraitImporter::Graphable(f) => EbiTraitObject::Graphable((f)(reader)?),
            EbiTraitImporter::Activities(f) => EbiTraitObject::Activities((f)(reader)?),
        })
    }
}

impl Display for EbiTraitImporter {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.get_trait().to_string())
    }
}

#[derive(Debug, Clone)]
pub enum EbiObjectImporter {
    EventLog(fn(&mut dyn BufRead) -> Result<EbiObject>),
    DirectlyFollowsGraph(fn(&mut dyn BufRead) -> Result<EbiObject>),
    DirectlyFollowsModel(fn(&mut dyn BufRead) -> Result<EbiObject>),
    StochasticDirectlyFollowsModel(fn(&mut dyn BufRead) -> Result<EbiObject>),
    FiniteLanguage(fn(&mut dyn BufRead) -> Result<EbiObject>),
    FiniteStochasticLanguage(fn(&mut dyn BufRead) -> Result<EbiObject>),
    LabelledPetriNet(fn(&mut dyn BufRead) -> Result<EbiObject>),
    StochasticDeterministicFiniteAutomaton(fn(&mut dyn BufRead) -> Result<EbiObject>),
    StochasticLabelledPetriNet(fn(&mut dyn BufRead) -> Result<EbiObject>),
    LanguageOfAlignments(fn(&mut dyn BufRead) -> Result<EbiObject>),
    DeterministicFiniteAutomaton(fn(&mut dyn BufRead) -> Result<EbiObject>),
    ProcessTree(fn(&mut dyn BufRead) -> Result<EbiObject>),
    Executions(fn(&mut dyn BufRead) -> Result<EbiObject>),
    StochasticLanguageOfAlignments(fn(&mut dyn BufRead) -> Result<EbiObject>),
    StochasticProcessTree(fn(&mut dyn BufRead) -> Result<EbiObject>),
}

impl EbiObjectImporter {
    pub fn get_type(&self) -> EbiObjectType {
        match self {
            EbiObjectImporter::EventLog(_) => EbiObjectType::EventLog,
            EbiObjectImporter::DirectlyFollowsGraph(_) => EbiObjectType::DirectlyFollowsGraph,
            EbiObjectImporter::DirectlyFollowsModel(_) => EbiObjectType::DirectlyFollowsModel,
            EbiObjectImporter::StochasticDirectlyFollowsModel(_) => {
                EbiObjectType::StochasticDirectlyFollowsModel
            }
            EbiObjectImporter::FiniteLanguage(_) => EbiObjectType::FiniteLanguage,
            EbiObjectImporter::FiniteStochasticLanguage(_) => {
                EbiObjectType::FiniteStochasticLanguage
            }
            EbiObjectImporter::LabelledPetriNet(_) => EbiObjectType::LabelledPetriNet,
            EbiObjectImporter::StochasticDeterministicFiniteAutomaton(_) => {
                EbiObjectType::StochasticDeterministicFiniteAutomaton
            }
            EbiObjectImporter::StochasticLabelledPetriNet(_) => {
                EbiObjectType::StochasticLabelledPetriNet
            }
            EbiObjectImporter::LanguageOfAlignments(_) => EbiObjectType::LanguageOfAlignments,
            EbiObjectImporter::StochasticLanguageOfAlignments(_) => {
                EbiObjectType::StochasticLanguageOfAlignments
            }
            EbiObjectImporter::DeterministicFiniteAutomaton(_) => {
                EbiObjectType::DeterministicFiniteAutomaton
            }
            EbiObjectImporter::ProcessTree(_) => EbiObjectType::ProcessTree,
            EbiObjectImporter::StochasticProcessTree(_) => EbiObjectType::StochasticProcessTree,
            EbiObjectImporter::Executions(_) => EbiObjectType::Executions,
        }
    }

    pub fn get_importer(&self) -> fn(&mut dyn BufRead) -> Result<EbiObject> {
        match self {
            EbiObjectImporter::EventLog(importer) => *importer,
            EbiObjectImporter::DirectlyFollowsGraph(importer) => *importer,
            EbiObjectImporter::DirectlyFollowsModel(importer) => *importer,
            EbiObjectImporter::StochasticDirectlyFollowsModel(importer) => *importer,
            EbiObjectImporter::FiniteLanguage(importer) => *importer,
            EbiObjectImporter::FiniteStochasticLanguage(importer) => *importer,
            EbiObjectImporter::LabelledPetriNet(importer) => *importer,
            EbiObjectImporter::StochasticDeterministicFiniteAutomaton(importer) => *importer,
            EbiObjectImporter::StochasticLabelledPetriNet(importer) => *importer,
            EbiObjectImporter::LanguageOfAlignments(importer) => *importer,
            EbiObjectImporter::StochasticLanguageOfAlignments(importer) => *importer,
            EbiObjectImporter::DeterministicFiniteAutomaton(importer) => *importer,
            EbiObjectImporter::ProcessTree(importer) => *importer,
            EbiObjectImporter::StochasticProcessTree(importer) => *importer,
            EbiObjectImporter::Executions(importer) => *importer,
        }
    }
}

impl Display for EbiObjectImporter {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.get_type().to_string())
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
) -> Result<(EbiTraitObject, &'static EbiFileHandler)> {
    let mut error = None;
    for file_handler in EBI_FILE_HANDLERS {
        for importer in file_handler.trait_importers {
            if &importer.get_trait() == etrait {
                //attempt to import
                match importer.import(reader.get().context("Obtaining reader.")?.as_mut())
                    .with_context(|| {format!("The last attempted importer was: {}.", file_handler)})
                    .with_context(|| format!("Attempting to parse file as either {}.\nIf you know the type of your file, use `Ebi {}` to check it.", EbiInputType::show_file_handlers(etrait.get_file_handlers()).join(", "), ebi_validate!())) {
                    Ok(object) => {return Ok((object, file_handler));}, //object parsed, return it
                    Err(err) => {error = Some(err);}
                }
            }
        }
    }
    Err(error.unwrap())
}

pub fn read_as_object(
    etype: &EbiObjectType,
    reader: &mut MultipleReader,
) -> Result<(EbiObject, &'static EbiFileHandler)> {
    for file_handler in EBI_FILE_HANDLERS {
        for importer in file_handler.object_importers {
            if &importer.get_type() == etype {
                //attempt to import
                if let Ok(object) = (importer.get_importer())(
                    reader.get().context("Could not obtain reader.")?.as_mut(),
                ) {
                    //object parsed; return it
                    return Ok((object, file_handler));
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
) -> Result<(EbiObject, &'static EbiFileHandler)> {
    for file_handler in EBI_FILE_HANDLERS {
        //attempt to import
        for importer in file_handler.object_importers {
            if let Ok(object) = (importer.get_importer())(
                reader.get().context("Could not obtain reader.")?.as_mut(),
            ) {
                //object parsed; return it
                return Ok((object, file_handler));
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
    use std::{
        fs::{self, File},
        path::PathBuf,
    };

    use ebi_arithmetic::{Fraction, Zero};
    use strum::IntoEnumIterator;

    use crate::{
        ebi_file_handlers::compressed_event_log::EBI_COMPRESSED_EVENT_LOG,
        ebi_framework::{
            ebi_file_handler::EBI_FILE_HANDLERS,
            ebi_input::{EbiInput, TEST_INPUT_TYPE_FRACTION, TEST_INPUT_TYPE_USIZE},
        },
        multiple_reader::MultipleReader,
    };

    use super::{EbiInputType, validate_object_of};
    use crate::ebi_framework::ebi_input;

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
            input_type.get_java_object_handlers();
            input_type.get_applicable_commands();
            input_type.to_string();
            EbiInputType::get_possible_inputs(&list);
            EbiInputType::get_possible_inputs_with_latex(&list);
            EbiInputType::get_possible_inputs_with_java(&list);
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
