use std::{collections::HashSet, fmt::Display, fs::File, io::{self, BufRead, BufReader, Bytes, Cursor, IsTerminal, Read, Seek}, path::PathBuf};

use clap::{ArgMatches, Command, value_parser, arg};
use anyhow::{Result, Context, anyhow};
use process_mining::EventLog;

use crate::{ebi_commands::{ebi_command_analyse::EBI_ANALYSE, ebi_command_discover::EBI_DISCOVER, ebi_command_validate::{self, EBI_VALIDATE}}, ebi_input_output::EbiInputType, ebi_objects::{alignments::Alignments, ebi_object::{EbiObject, EbiObjectType, EbiTraitObject}}, ebi_traits::{ebi_trait::EbiTrait, ebi_trait_alignments::EbiTraitAlignments, ebi_trait_event_log::EbiTraitEventLog, ebi_trait_finite_language::EbiTraitFiniteLanguage, ebi_trait_finite_stochastic_language::EbiTraitFiniteStochasticLanguage, ebi_trait_iterable_stochastic_language::EbiTraitIterableStochasticLanguage, ebi_trait_labelled_petri_net::EbiTraitLabelledPetriNet, ebi_trait_queriable_stochastic_language::EbiTraitQueriableStochasticLanguage, ebi_trait_semantics::EbiTraitSemantics, ebi_trait_stochastic_deterministic_semantics::EbiTraitStochasticDeterministicSemantics, ebi_trait_stochastic_semantics::EbiTraitStochasticSemantics}, ebi_validate, file_handler::{EbiFileHandler, EBI_FILE_HANDLERS}};

#[derive(Debug)]
pub enum EbiTraitImporter {
    FiniteLanguage(fn(&mut dyn BufRead) -> Result<Box<dyn EbiTraitFiniteLanguage>>), //finite set of traces
    FiniteStochasticLanguage(fn(&mut dyn BufRead) -> Result<Box<dyn EbiTraitFiniteStochasticLanguage>>), //finite number of traces
    QueriableStochasticLanguage(fn(&mut dyn BufRead) -> Result<Box<dyn EbiTraitQueriableStochasticLanguage>>), //can query for the probability of a trace
    IterableStochasticLanguage(fn(&mut dyn BufRead) -> Result<Box<dyn EbiTraitIterableStochasticLanguage>>), //can walk over the traces, potentially forever
    EventLog(fn(&mut dyn BufRead) -> Result<Box<dyn EbiTraitEventLog>>), //full XES; access to traces and attributes
    Semantics(fn(&mut dyn BufRead) -> Result<EbiTraitSemantics>), //can walk over states  using transitions, potentially forever
    StochasticSemantics(fn(&mut dyn BufRead) -> Result<EbiTraitStochasticSemantics>), //can walk over states  using transitions, potentially forever
    StochasticDeterministicSemantics(fn(&mut dyn BufRead) -> Result<EbiTraitStochasticDeterministicSemantics>), //can walk over states using activities, potentially forever
    LabelledPetriNet(fn(&mut dyn BufRead) -> Result<Box<dyn EbiTraitLabelledPetriNet>>), //labelled Petri net
    Alignments(fn(&mut dyn BufRead) -> Result<Box<dyn EbiTraitAlignments>>) //alignments
}

impl EbiTraitImporter {
    pub fn get_trait(&self) -> EbiTrait {
        match self {
            EbiTraitImporter::FiniteLanguage(_) => EbiTrait::FiniteLanguage,
            EbiTraitImporter::FiniteStochasticLanguage(_) => EbiTrait::FiniteStochasticLanguage,
            EbiTraitImporter::QueriableStochasticLanguage(_) => EbiTrait::QueriableStochasticLanguage,
            EbiTraitImporter::IterableStochasticLanguage(_) => EbiTrait::IterableStochasticLanguage,
            EbiTraitImporter::EventLog(_) => EbiTrait::EventLog,
            EbiTraitImporter::Semantics(_) => EbiTrait::Semantics,
            EbiTraitImporter::StochasticSemantics(_) => EbiTrait::StochasticSemantics,
            EbiTraitImporter::StochasticDeterministicSemantics(_) => EbiTrait::StochasticDeterministicSemantics,
            EbiTraitImporter::LabelledPetriNet(_) => EbiTrait::LabelledPetriNet,
            EbiTraitImporter::Alignments(_) => EbiTrait::Alignments
        }
    }

    pub fn import(&self, reader: &mut dyn BufRead) -> Result<EbiTraitObject> {
        Ok(match self {
            EbiTraitImporter::FiniteLanguage(f) => EbiTraitObject::FiniteLanguage((f)(reader)?),
            EbiTraitImporter::FiniteStochasticLanguage(f) => EbiTraitObject::FiniteStochasticLanguage((f)(reader)?),
            EbiTraitImporter::QueriableStochasticLanguage(f) => EbiTraitObject::QueriableStochasticLanguage((f)(reader)?),
            EbiTraitImporter::IterableStochasticLanguage(f) => EbiTraitObject::IterableStochasticLanguage((f)(reader)?),
            EbiTraitImporter::EventLog(f) => EbiTraitObject::EventLog((f)(reader)?),
            EbiTraitImporter::Semantics(f) => EbiTraitObject::Semantics((f)(reader)?),
            EbiTraitImporter::StochasticSemantics(f) => EbiTraitObject::StochasticSemantics((f)(reader)?),
            EbiTraitImporter::StochasticDeterministicSemantics(f) => EbiTraitObject::StochasticDeterministicSemantics((f)(reader)?),
            EbiTraitImporter::LabelledPetriNet(f) => EbiTraitObject::LabelledPetriNet((f)(reader)?),
            EbiTraitImporter::Alignments(f) => EbiTraitObject::Alignments((f)(reader)?),
        })
    }
}

impl Display for EbiTraitImporter {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.get_trait().to_string())
    }
}

#[derive(Debug)]
pub enum EbiObjectImporter {
    EventLog(fn(&mut dyn BufRead) -> Result<EbiObject>),
    DirectlyFollowsModel(fn(&mut dyn BufRead) -> Result<EbiObject>),
    FiniteLanguage(fn(&mut dyn BufRead) -> Result<EbiObject>),
    FiniteStochasticLanguage(fn(&mut dyn BufRead) -> Result<EbiObject>),
    LabelledPetriNet(fn(&mut dyn BufRead) -> Result<EbiObject>),
    StochasticDeterministicFiniteAutomaton(fn(&mut dyn BufRead) -> Result<EbiObject>),
    StochasticLabelledPetriNet(fn(&mut dyn BufRead) -> Result<EbiObject>),
    Alignments(fn(&mut dyn BufRead) -> Result<EbiObject>),
}

impl EbiObjectImporter {
    pub fn get_type(&self) -> EbiObjectType {
        match self {
            EbiObjectImporter::EventLog(_) => EbiObjectType::EventLog,
            EbiObjectImporter::DirectlyFollowsModel(_) => EbiObjectType::DirectlyFollowsModel,
            EbiObjectImporter::FiniteLanguage(_) => EbiObjectType::FiniteLanguage,
            EbiObjectImporter::FiniteStochasticLanguage(_) => EbiObjectType::FiniteStochasticLanguage,
            EbiObjectImporter::LabelledPetriNet(_) => EbiObjectType::LabelledPetriNet,
            EbiObjectImporter::StochasticDeterministicFiniteAutomaton(_) => EbiObjectType::StochasticDeterministicFiniteAutomaton,
            EbiObjectImporter::StochasticLabelledPetriNet(_) => EbiObjectType::StochasticLabelledPetriNet,
            EbiObjectImporter::Alignments(_) => EbiObjectType::Alignments,
        }
    }
    
    pub fn get_importer(&self) -> (fn(&mut dyn BufRead) -> Result<EbiObject>) {
        match self {
            EbiObjectImporter::EventLog(importer) => *importer,
            EbiObjectImporter::DirectlyFollowsModel(importer) => *importer,
            EbiObjectImporter::FiniteLanguage(importer) => *importer,
            EbiObjectImporter::FiniteStochasticLanguage(importer) => *importer,
            EbiObjectImporter::LabelledPetriNet(importer) => *importer,
            EbiObjectImporter::StochasticDeterministicFiniteAutomaton(importer) => *importer,
            EbiObjectImporter::StochasticLabelledPetriNet(importer) => *importer,
            EbiObjectImporter::Alignments(importer) => *importer,
        }
    }
}

impl Display for EbiObjectImporter {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.get_type().to_string())
    }
}

pub trait Importable {
    fn import_as_object(reader: &mut dyn BufRead) -> Result<EbiObject>;
    fn import(reader: &mut dyn BufRead) -> Result<Self> where Self: Sized;
}

pub fn validate<X: Importable> (reader: &mut dyn BufRead) -> Result<()> {
    match X::import(reader) {
        Ok(_) => Ok(()),
        Err(x) => Err(x),
    }
}

pub enum MultipleReader {
    String(String),
    File(File),
    Bytes(Vec<u8>)
}

impl MultipleReader {
    pub fn from_stdin() -> Result<Self> {
        let stdin = io::stdin();
        let mut reader = stdin.lock();
        if cfg!(windows) { //windows does not support reading bytes from STDIN, so read it as text
            let mut buf = String::new();
            reader.read_to_string(&mut buf).context("Could not read text from STDIN (on Windows, reading bytes from STDIN is not supported.");
            log::info!("read from stdin in text mode with length {}", buf.len());
            return Ok(Self::String(buf));
        } else {
            let mut buf = Vec::new();
            reader.read_to_end(&mut buf)?;
            log::info!("read from stdin in binary mode with length {}", buf.len());
            return Ok(Self::Bytes(buf));
        }
    }

    pub fn from_file(file: File) -> Self {
        return Self::File(file);
    }

    pub fn get(&mut self) -> Result<Box<dyn BufRead + '_>> {
        match self {
            MultipleReader::String(s) => Ok(Box::new(Cursor::new(s))),
            MultipleReader::File(ref mut file) => {
                file.seek(io::SeekFrom::Start(0))?;
                return Ok(Box::new(BufReader::new(file)));
            },
            MultipleReader::Bytes(b) => Ok(Box::new(Cursor::new(b))),
        }
    }
}

pub fn get_reader_file(from_file: &PathBuf) -> Result<MultipleReader> {
    if from_file.as_os_str() == "-" {
        return MultipleReader::from_stdin();
    } else {
        let file = File::open(from_file).with_context(|| format!("Could not read file `{}`.", from_file.display()))?;
        return Ok(MultipleReader::from_file(file));
    }
}

pub fn get_reader(cli_matches: &ArgMatches, cli_id: &str) -> Result<MultipleReader> {
    if let Some(from_file) = cli_matches.get_one::<PathBuf>(cli_id) {
        if from_file.as_os_str() == "-" {
            return MultipleReader::from_stdin();
        } else {
            let file = File::open(from_file).with_context(|| format!("Could not read file `{}`.", from_file.display()))?;
            return Ok(MultipleReader::from_file(file));
        }
    } else {
        return MultipleReader::from_stdin();
    }
}

//"read" functions try to load a specific struct

pub fn read_as_finite_language<X: 'static + Importable + EbiTraitFiniteLanguage> (reader: &mut dyn BufRead) -> Result<Box<dyn EbiTraitFiniteLanguage>> {
    match X::import(reader) {
        Ok(x) => Ok(Box::new(x)),
        Err(x) => Err(x),
    }
}

pub fn read_as_finite_stochastic_language<X: 'static + Importable + EbiTraitFiniteStochasticLanguage> (reader: &mut dyn BufRead) -> Result<Box<dyn EbiTraitFiniteStochasticLanguage>> {
    match X::import(reader) {
        Ok(x) => Ok(Box::new(x)),
        Err(x) => Err(x),
    }
}

pub fn read_as_queriable_stochastic_language<X: 'static + Importable + EbiTraitQueriableStochasticLanguage> (reader: &mut dyn BufRead) -> Result<Box<dyn EbiTraitQueriableStochasticLanguage>> {
    match X::import(reader) {
        Ok(x) => Ok(Box::new(x)),
        Err(x) => Err(x),
    }
}

pub fn read_as_iterable_stochastic_language<X: 'static + Importable + EbiTraitIterableStochasticLanguage> (reader: &mut dyn BufRead) -> Result<Box<dyn EbiTraitIterableStochasticLanguage>> {
    match X::import(reader) {
        Ok(x) => Ok(Box::new(x)),
        Err(x) => Err(x),
    }
}

pub fn read_as_labelled_petri_net<X: 'static + Importable + EbiTraitLabelledPetriNet> (reader: &mut dyn BufRead) -> Result<Box<dyn EbiTraitLabelledPetriNet>> {
    match X::import(reader) {
        Ok(x) => Ok(Box::new(x)),
        Err(x) => Err(x),
    }
}

pub fn read_as_trait(etrait: &EbiTrait, reader: &mut MultipleReader) -> Result<(EbiTraitObject, &'static EbiFileHandler)> {
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

pub fn read_as_object(etype: &EbiObjectType, reader: &mut MultipleReader) -> Result<(EbiObject, &'static EbiFileHandler)> {
    for file_handler in EBI_FILE_HANDLERS {
        for importer in file_handler.object_importers {
            if &importer.get_type() == etype {
                //attempt to import
                if let Ok(object) = (importer.get_importer())(reader.get().context("Could not obtain reader.")?.as_mut()) {
                    //object parsed; return it
                    return Ok((object, file_handler));
                }
            }
        }
    }
    Err(anyhow!("File could not be recognised."))
}

pub fn read_as_any_object(reader: &mut MultipleReader) -> Result<(EbiObject, &'static EbiFileHandler)> {
    for file_handler in EBI_FILE_HANDLERS {
        //attempt to import
        for importer in file_handler.object_importers {
            if let Ok(object) = (importer.get_importer())(reader.get().context("Could not obtain reader.")?.as_mut()) {
                //object parsed; return it
                return Ok((object, file_handler));
            }
        }
    }
    Err(anyhow!("File could not be recognised."))
}

pub fn validate_object_of(reader: &mut MultipleReader, file_handler: &EbiFileHandler) -> Result<()> {
    let result = (file_handler.validator)(reader.get()?.as_mut());
    return result;
}

//"import" functions attempt to load a particular trait

pub fn import_finite_language(reader: &mut MultipleReader) -> Result<Box<dyn EbiTraitFiniteLanguage>> {
    for file_handler in EBI_FILE_HANDLERS {
        for importer in file_handler.trait_importers {
            if let EbiTraitImporter::FiniteLanguage(f) = importer {
                if let Ok(object) = (f)(reader.get()?.as_mut()) {
                    return Ok(object);
                }
            }
        }
    }

    return Err(anyhow!("File not recognised or does not have a finite language. To see parsing errors use `ebi {}`, or discovering a stochastic perspective using `ebi {}`.", EBI_VALIDATE.short_name(), EBI_DISCOVER.short_name()));
}

pub fn import_finite_stochastic_language(reader: &mut MultipleReader) -> Result<Box<dyn EbiTraitFiniteStochasticLanguage>> {
    for file_handler in EBI_FILE_HANDLERS {
        for importer in file_handler.trait_importers {
            if let EbiTraitImporter::FiniteStochasticLanguage(f) = importer {
                if let Ok(object) = (f)(reader.get()?.as_mut()) {
                    return Ok(object);
                }
            }
        }
    }

    return Err(anyhow!("File not recognised or does not have a finite stochastic language. To see parsing errors use `ebi {}`, or discovering a stochastic perspective using `ebi {}`.", EBI_VALIDATE.short_name(), EBI_DISCOVER.short_name()));
}

pub fn import_queriable_stochastic_language(reader: &mut MultipleReader) -> Result<Box<dyn EbiTraitQueriableStochasticLanguage>> {
    for file_handler in EBI_FILE_HANDLERS {
        for importer in file_handler.trait_importers {
            if let EbiTraitImporter::QueriableStochasticLanguage(f) = importer {
                if let Ok(object) = (f)(reader.get()?.as_mut()) {
                    return Ok(object);
                }
            }
        }
    }

    return Err(anyhow!("File not recognised or does not have a stochastic language. To see parsing errors use `ebi {}`, or discovering a stochastic perspective using `ebi {}`.", EBI_VALIDATE.short_name(), EBI_DISCOVER.short_name()));
}

pub fn import_iterable_stochastic_language(reader: &mut MultipleReader) -> Result<Box<dyn EbiTraitIterableStochasticLanguage>> {
    for file_handler in EBI_FILE_HANDLERS {
        for importer in file_handler.trait_importers {
            if let EbiTraitImporter::IterableStochasticLanguage(f) = importer {
                if let Ok(object) = (f)(reader.get()?.as_mut()) {
                    return Ok(object);
                }
            }
        }
    }

    return Err(anyhow!("File not recognised or does not have a finite stochastic language. To see parsing errors use `ebi {}`, or try to sample it using `ebi {}`.", EBI_VALIDATE.short_name(), EBI_ANALYSE.short_name()));
}

pub fn import_event_log(reader: &mut MultipleReader) -> Result<Box<dyn EbiTraitEventLog>> {
    let mut extensions = vec![];
    for file_handler in EBI_FILE_HANDLERS {
        for importer in file_handler.trait_importers {
            if let EbiTraitImporter::EventLog(f) = importer {
                extensions.push(file_handler.file_extension);
                if let Ok(object) = (f)(reader.get()?.as_mut()) {
                    return Ok(object);
                }
            }
        }
    }

    return Err(anyhow!("File not supported or invalid. File types {} are supported. To see parsing errors use `ebi {}`.", extensions.join(", "), EBI_VALIDATE.short_name()));
}

pub fn import_stochastic_semantics(reader: &mut MultipleReader) -> Result<EbiTraitStochasticSemantics> {
    let mut extensions = vec![];
    for file_handler in EBI_FILE_HANDLERS {
        for importer in file_handler.trait_importers {
            if let EbiTraitImporter::StochasticSemantics(f) = importer {
                extensions.push(file_handler.file_extension);
                if let Ok(object) = (f)(reader.get()?.as_mut()) {
                    return Ok(object);
                }
            }
        }
    }

    return Err(anyhow!("File not supported or invalid. File types {} are supported. To see parsing errors use `ebi {}`.", extensions.join(", "), EBI_VALIDATE.short_name()));
}

pub fn import_stochastic_deterministic_semantics(reader: &mut MultipleReader) -> Result<EbiTraitStochasticDeterministicSemantics> {
    let mut extensions = vec![];
    for file_handler in EBI_FILE_HANDLERS {
        for importer in file_handler.trait_importers {
            if let EbiTraitImporter::StochasticDeterministicSemantics(f) = importer {
                extensions.push(file_handler.file_extension);
                if let Ok(object) = (f)(reader.get()?.as_mut()) {
                    return Ok(object);
                }
            }
        }
    }

    return Err(anyhow!("File not recognised or does not have stochastic deterministic semantics. File types {} are supported. To see parsing errors use `ebi {}`, or try discovering a stochastic perspective using `ebi {}`.", extensions.join(", "), EBI_VALIDATE.short_name(), EBI_DISCOVER.short_name()));
}