use anyhow::{Context, Result, anyhow};
use clap::{ArgMatches, builder::ValueParser, value_parser};
use std::{
    collections::{BTreeSet, HashSet},
    fmt::Display,
    fs::File,
    io::BufRead,
    path::PathBuf,
};
use strum_macros::EnumIter;

use crate::{
    ebi_framework::ebi_file_handler::EbiFileHandler,
    ebi_traits::{
        ebi_trait_event_log::EbiTraitEventLog, ebi_trait_finite_language::EbiTraitFiniteLanguage,
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
    math::fraction::{Fraction, FractionNotParsedYet},
    multiple_reader::MultipleReader,
    text::Joiner,
};

use super::{
    ebi_command::{EBI_COMMANDS, EbiCommand},
    ebi_file_handler::EBI_FILE_HANDLERS,
    ebi_object::{EbiObject, EbiObjectType, EbiTraitObject},
    ebi_trait::{EbiTrait, FromEbiTraitObject},
    importable::Importable,
    prom_link::{
        JAVA_OBJECT_HANDLERS_FRACTION, JAVA_OBJECT_HANDLERS_STRING, JAVA_OBJECT_HANDLERS_USIZE,
        JavaObjectHandler,
    },
};

pub enum EbiInput {
    Trait(EbiTraitObject, &'static EbiFileHandler),
    Object(EbiObject, &'static EbiFileHandler),
    String(String),
    Usize(usize),
    FileHandler(EbiFileHandler),
    Fraction(Fraction),
}

impl EbiInput {
    pub fn to_type<T: FromEbiTraitObject + ?Sized>(self) -> Result<Box<T>> {
        FromEbiTraitObject::from_trait_object(self)
    }

    pub fn get_type(&self) -> EbiInputType {
        match self {
            EbiInput::Trait(t, _) => EbiInputType::Trait(t.get_trait()),
            EbiInput::Object(o, _) => EbiInputType::Object(o.get_type()),
            EbiInput::String(_) => EbiInputType::String,
            EbiInput::Usize(_) => EbiInputType::Usize,
            EbiInput::FileHandler(_) => EbiInputType::FileHandler,
            EbiInput::Fraction(_) => EbiInputType::Fraction,
        }
    }
}

#[derive(PartialEq, Eq, EnumIter)]
pub enum EbiInputType {
    Trait(EbiTrait),
    Object(EbiObjectType),
    AnyObject,
    FileHandler,
    String,
    Usize,
    Fraction,
}

impl EbiInputType {
    pub fn get_article(&self) -> &str {
        match self {
            EbiInputType::Trait(t) => t.get_article(),
            EbiInputType::Object(o) => o.get_article(),
            EbiInputType::AnyObject => "an",
            EbiInputType::String => "a",
            EbiInputType::Usize => "an",
            EbiInputType::FileHandler => "a",
            EbiInputType::Fraction => "a",
        }
    }

    pub fn get_parser_of_list(traits: &[&EbiInputType]) -> ValueParser {
        match traits[0] {
            EbiInputType::Trait(_) => value_parser!(PathBuf),
            EbiInputType::Object(_) => value_parser!(PathBuf),
            EbiInputType::AnyObject => value_parser!(PathBuf),
            EbiInputType::String => value_parser!(String).into(),
            EbiInputType::Usize => value_parser!(usize).into(),
            EbiInputType::FileHandler => value_parser!(EbiFileHandler).into(),
            EbiInputType::Fraction => value_parser!(FractionNotParsedYet).into(),
        }
    }

    pub fn get_java_object_handlers(&self) -> Vec<&JavaObjectHandler> {
        match self {
            EbiInputType::Trait(t) => Self::get_file_handlers_java(t.get_file_handlers()),
            EbiInputType::Object(o) => Self::get_file_handlers_java(o.get_file_handlers()),
            EbiInputType::AnyObject => {
                Self::get_file_handlers_java(EBI_FILE_HANDLERS.iter().collect())
            }
            EbiInputType::String => {
                let mut x = vec![];
                x.extend(JAVA_OBJECT_HANDLERS_STRING);
                x
            }
            EbiInputType::Usize => {
                let mut x = vec![];
                x.extend(JAVA_OBJECT_HANDLERS_USIZE);
                x
            }
            EbiInputType::FileHandler => {
                //not supported in Java;
                vec![]
            }
            EbiInputType::Fraction => {
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
                    result.extend(Self::show_file_handlers(o.get_file_handlers()));
                }
                EbiInputType::AnyObject => {
                    result.extend(Self::show_file_handlers(EBI_FILE_HANDLERS.iter().collect()));
                }
                EbiInputType::String => {
                    result.insert("text".to_string());
                }
                EbiInputType::Usize => {
                    result.insert("integer".to_string());
                }
                EbiInputType::FileHandler => {
                    let extensions: Vec<String> = EBI_FILE_HANDLERS
                        .iter()
                        .map(|file_type| file_type.file_extension.to_string())
                        .collect();
                    result.insert(
                        "the file extension of any file type supported by Ebi (".to_owned()
                            + &extensions.join_with(", ", " or ")
                            + ")",
                    );
                }
                EbiInputType::Fraction => {
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
                    result.extend(Self::show_file_handlers_latex(o.get_file_handlers()));
                }
                EbiInputType::AnyObject => {
                    result.extend(Self::show_file_handlers_latex(
                        EBI_FILE_HANDLERS.iter().collect(),
                    ));
                }
                EbiInputType::String => {
                    result.insert("text".to_string());
                }
                EbiInputType::Usize => {
                    result.insert("integer".to_string());
                }
                EbiInputType::FileHandler => {
                    let extensions: Vec<String> = EBI_FILE_HANDLERS
                        .iter()
                        .map(|file_type| file_type.file_extension.to_string())
                        .collect();
                    result.insert(
                        "the file extension of any file type supported by Ebi (".to_owned()
                            + &extensions.join_with(", ", " or ")
                            + ")",
                    );
                }
                EbiInputType::Fraction => {
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
            EbiInputType::String => write!(f, "text"),
            EbiInputType::Usize => write!(f, "integer"),
            EbiInputType::FileHandler => write!(f, "file"),
            EbiInputType::Fraction => write!(f, "fraction"),
        }
    }
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
    DirectlyFollowsModel(fn(&mut dyn BufRead) -> Result<EbiObject>),
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
            EbiObjectImporter::DirectlyFollowsModel(_) => EbiObjectType::DirectlyFollowsModel,
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
            EbiObjectImporter::DirectlyFollowsModel(importer) => *importer,
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

/**
 * This is a convenience method: if the object can be imported, then it validates.
 */
pub fn validate<X: Importable>(reader: &mut dyn BufRead) -> Result<()> {
    match X::import(reader) {
        Ok(_) => Ok(()),
        Err(x) => Err(x),
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
        return MultipleReader::from_stdin();
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
    Err(anyhow!("File could not be recognised."))
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
    Err(anyhow!("File could not be recognised."))
}

pub fn validate_object_of(
    reader: &mut MultipleReader,
    file_handler: &EbiFileHandler,
) -> Result<()> {
    let result = (file_handler.validator)(reader.get()?.as_mut());
    return result;
}

#[cfg(test)]
mod tests {
    use std::{
        fs::{self, File},
        path::PathBuf,
    };

    use strum::IntoEnumIterator;

    use crate::{
        ebi_framework::{ebi_file_handler::EBI_FILE_HANDLERS, ebi_input::EbiInput},
        ebi_objects::compressed_event_log::EBI_COMPRESSED_EVENT_LOG,
        math::{fraction::Fraction, traits::Zero},
        multiple_reader::MultipleReader,
    };

    use super::{EbiInputType, validate_object_of};
    use crate::ebi_framework::ebi_input;

    #[test]
    fn input_primitives() {
        EbiInput::FileHandler(EBI_COMPRESSED_EVENT_LOG).get_type();
        EbiInput::Fraction(Fraction::zero()).get_type();
        EbiInput::Usize(0).get_type();
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

    #[test]
    fn reader_file() {
        let file = "testfiles/a-b.xes".parse::<PathBuf>().unwrap();
        ebi_input::get_reader_file(&file).unwrap();
    }
}
