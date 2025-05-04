use anyhow::{Context, Result};
use std::{
    collections::{BTreeSet, HashSet},
    fmt::{self, Display},
    fs::File,
    io::Write,
    path::PathBuf,
};
use strum_macros::{Display, EnumIter};

use crate::{
    ebi_objects::{
        compressed_event_log::{CompressedEventLog, EBI_COMPRESSED_EVENT_LOG},
        deterministic_finite_automaton::{
            DeterministicFiniteAutomaton, EBI_DETERMINISTIC_FINITE_AUTOMATON,
        },
        directly_follows_model::{DirectlyFollowsModel, EBI_DIRCTLY_FOLLOWS_MODEL},
        executions::{EBI_EXECUTIONS, Executions},
        finite_language::{EBI_FINITE_LANGUAGE, FiniteLanguage},
        finite_stochastic_language::{EBI_FINITE_STOCHASTIC_LANGUAGE, FiniteStochasticLanguage},
        labelled_petri_net::{EBI_LABELLED_PETRI_NET, LabelledPetriNet},
        language_of_alignments::{EBI_LANGUAGE_OF_ALIGNMENTS, LanguageOfAlignments},
        process_tree::{EBI_PROCESS_TREE, ProcessTree},
        stochastic_deterministic_finite_automaton::{
            EBI_STOCHASTIC_DETERMINISTIC_FINITE_AUTOMATON, StochasticDeterministicFiniteAutomaton,
        },
        stochastic_labelled_petri_net::{
            EBI_STOCHASTIC_LABELLED_PETRI_NET, StochasticLabelledPetriNet,
        },
        stochastic_language_of_alignments::{
            EBI_STOCHASTIC_LANGUAGE_OF_ALIGNMENTS, StochasticLanguageOfAlignments,
        },
        stochastic_process_tree::StochasticProcessTree,
    },
    math::{fraction::Fraction, log_div::LogDiv, root::ContainsRoot, root_log_div::RootLogDiv},
};

use super::{
    ebi_command::{EBI_COMMANDS, EbiCommand},
    ebi_file_handler::{EBI_FILE_HANDLERS, EbiFileHandler},
    ebi_object::{EbiObject, EbiObjectType},
    exportable::Exportable,
    prom_link::{
        JAVA_OBJECT_HANDLERS_BOOL, JAVA_OBJECT_HANDLERS_CONTAINSROOT,
        JAVA_OBJECT_HANDLERS_FRACTION, JAVA_OBJECT_HANDLERS_LOGDIV, JAVA_OBJECT_HANDLERS_PDF,
        JAVA_OBJECT_HANDLERS_ROOTLOGDIV, JAVA_OBJECT_HANDLERS_STRING, JAVA_OBJECT_HANDLERS_SVG,
        JAVA_OBJECT_HANDLERS_USIZE, JavaObjectHandler,
    },
};

#[derive(Display, Clone)]
pub enum EbiOutput {
    Object(EbiObject),
    String(String),
    SVG(String),
    PDF(Vec<u8>),
    Usize(usize),
    Fraction(Fraction),
    LogDiv(LogDiv),
    ContainsRoot(ContainsRoot),
    RootLogDiv(RootLogDiv),
    Bool(bool),
}

impl EbiOutput {
    pub fn get_type(&self) -> EbiOutputType {
        match self {
            EbiOutput::Object(o) => EbiOutputType::ObjectType(o.get_type()),
            EbiOutput::String(_) => EbiOutputType::String,
            EbiOutput::SVG(_) => EbiOutputType::SVG,
            EbiOutput::PDF(_) => EbiOutputType::PDF,
            EbiOutput::Usize(_) => EbiOutputType::Usize,
            EbiOutput::Fraction(_) => EbiOutputType::Fraction,
            EbiOutput::LogDiv(_) => EbiOutputType::LogDiv,
            EbiOutput::ContainsRoot(_) => EbiOutputType::ContainsRoot,
            EbiOutput::RootLogDiv(_) => EbiOutputType::RootLogDiv,
            EbiOutput::Bool(_) => EbiOutputType::Bool,
        }
    }
}

#[derive(PartialEq, Eq, EnumIter, Hash, Clone, Debug)]
pub enum EbiOutputType {
    ObjectType(EbiObjectType),
    String,
    SVG,
    PDF,
    Usize,
    Fraction,
    LogDiv,
    ContainsRoot,
    RootLogDiv,
    Bool,
}

impl EbiOutputType {
    /**
     * Get all commands that output this type.
     */
    pub fn get_applicable_commands(&self) -> BTreeSet<Vec<&'static EbiCommand>> {
        let mut result = EBI_COMMANDS.get_command_paths();
        result.retain(|path| {
            if let EbiCommand::Command {
                output_type: output,
                ..
            } = path[path.len() - 1]
            {
                if output == &self {
                    return true;
                }
            }
            false
        });
        result
    }

    /**
     * Returns all exporters that can handle this output type
     */
    pub fn get_exporters(&self) -> Vec<EbiExporter> {
        match self {
            EbiOutputType::ObjectType(etype) => {
                let mut result = vec![];
                for file_handler in EBI_FILE_HANDLERS {
                    for exporter in file_handler.object_exporters {
                        if &exporter.get_type() == etype {
                            result.push(EbiExporter::Object(exporter, file_handler))
                        }
                    }
                }
                result
            }
            EbiOutputType::String => vec![EbiExporter::String],
            EbiOutputType::SVG => vec![EbiExporter::SVG],
            EbiOutputType::PDF => vec![EbiExporter::PDF],
            EbiOutputType::Usize => vec![EbiExporter::Usize],
            EbiOutputType::Fraction => vec![EbiExporter::Fraction],
            EbiOutputType::LogDiv => vec![EbiExporter::LogDiv],
            EbiOutputType::ContainsRoot => vec![EbiExporter::ContainsRoot],
            EbiOutputType::RootLogDiv => vec![EbiExporter::RootLogDiv],
            EbiOutputType::Bool => vec![EbiExporter::Bool],
        }
    }

    pub fn get_default_exporter(&self) -> EbiExporter {
        match self {
            EbiOutputType::ObjectType(EbiObjectType::LanguageOfAlignments) => EbiExporter::Object(
                &EbiObjectExporter::LanguageOfAlignments(LanguageOfAlignments::export_from_object),
                &EBI_LANGUAGE_OF_ALIGNMENTS,
            ),
            EbiOutputType::ObjectType(EbiObjectType::StochasticLanguageOfAlignments) => {
                EbiExporter::Object(
                    &EbiObjectExporter::StochasticLanguageOfAlignments(
                        StochasticLanguageOfAlignments::export_from_object,
                    ),
                    &EBI_STOCHASTIC_LANGUAGE_OF_ALIGNMENTS,
                )
            }
            EbiOutputType::ObjectType(EbiObjectType::DeterministicFiniteAutomaton) => {
                EbiExporter::Object(
                    &EbiObjectExporter::DeterministicFiniteAutomaton(
                        DeterministicFiniteAutomaton::export_from_object,
                    ),
                    &EBI_DETERMINISTIC_FINITE_AUTOMATON,
                )
            }
            EbiOutputType::ObjectType(EbiObjectType::DirectlyFollowsModel) => EbiExporter::Object(
                &EbiObjectExporter::DirectlyFollowsModel(DirectlyFollowsModel::export_from_object),
                &EBI_DIRCTLY_FOLLOWS_MODEL,
            ),
            EbiOutputType::ObjectType(EbiObjectType::EventLog) => EbiExporter::Object(
                &EbiObjectExporter::EventLog(CompressedEventLog::export_from_object),
                &EBI_COMPRESSED_EVENT_LOG,
            ),
            EbiOutputType::ObjectType(EbiObjectType::Executions) => EbiExporter::Object(
                &EbiObjectExporter::Executions(Executions::export_from_object),
                &EBI_EXECUTIONS,
            ),
            EbiOutputType::ObjectType(EbiObjectType::FiniteLanguage) => EbiExporter::Object(
                &EbiObjectExporter::FiniteLanguage(FiniteLanguage::export_from_object),
                &EBI_FINITE_LANGUAGE,
            ),
            EbiOutputType::ObjectType(EbiObjectType::FiniteStochasticLanguage) => {
                EbiExporter::Object(
                    &EbiObjectExporter::FiniteStochasticLanguage(
                        FiniteStochasticLanguage::export_from_object,
                    ),
                    &EBI_FINITE_STOCHASTIC_LANGUAGE,
                )
            }
            EbiOutputType::ObjectType(EbiObjectType::LabelledPetriNet) => EbiExporter::Object(
                &EbiObjectExporter::LabelledPetriNet(LabelledPetriNet::export_from_object),
                &EBI_LABELLED_PETRI_NET,
            ),
            EbiOutputType::ObjectType(EbiObjectType::StochasticDeterministicFiniteAutomaton) => {
                EbiExporter::Object(
                    &&EbiObjectExporter::StochasticDeterministicFiniteAutomaton(
                        StochasticDeterministicFiniteAutomaton::export_from_object,
                    ),
                    &EBI_STOCHASTIC_DETERMINISTIC_FINITE_AUTOMATON,
                )
            }
            EbiOutputType::ObjectType(EbiObjectType::StochasticLabelledPetriNet) => {
                EbiExporter::Object(
                    &&EbiObjectExporter::StochasticLabelledPetriNet(
                        StochasticLabelledPetriNet::export_from_object,
                    ),
                    &EBI_STOCHASTIC_LABELLED_PETRI_NET,
                )
            }
            EbiOutputType::ObjectType(EbiObjectType::ProcessTree) => EbiExporter::Object(
                &&EbiObjectExporter::ProcessTree(ProcessTree::export_from_object),
                &EBI_PROCESS_TREE,
            ),
            EbiOutputType::ObjectType(EbiObjectType::StochasticProcessTree) => EbiExporter::Object(
                &&EbiObjectExporter::StochasticProcessTree(
                    StochasticProcessTree::export_from_object,
                ),
                &EBI_PROCESS_TREE,
            ),
            EbiOutputType::String => EbiExporter::String,
            EbiOutputType::SVG => EbiExporter::SVG,
            EbiOutputType::PDF => EbiExporter::PDF,
            EbiOutputType::Usize => EbiExporter::Usize,
            EbiOutputType::Fraction => EbiExporter::Fraction,
            EbiOutputType::LogDiv => EbiExporter::LogDiv,
            EbiOutputType::ContainsRoot => EbiExporter::ContainsRoot,
            EbiOutputType::RootLogDiv => EbiExporter::RootLogDiv,
            EbiOutputType::Bool => EbiExporter::Bool,
        }
    }

    pub fn get_java_object_handlers_that_can_export(&self) -> HashSet<JavaObjectHandler> {
        let mut result = HashSet::new();
        for exporter in self.get_exporters() {
            for java_object_handler in exporter.get_java_object_handlers() {
                if java_object_handler.translator_ebi_to_java.is_some() {
                    result.insert(java_object_handler.to_owned());
                }
            }
        }
        result
    }
}

impl Display for EbiOutputType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            EbiOutputType::ObjectType(t) => t.fmt(f),
            EbiOutputType::String => Display::fmt(&"text", f),
            EbiOutputType::SVG => Display::fmt(&"svg", f),
            EbiOutputType::PDF => Display::fmt(&"pdf", f),
            EbiOutputType::Usize => Display::fmt(&"integer", f),
            EbiOutputType::Fraction => Display::fmt(&"fraction", f),
            EbiOutputType::LogDiv => Display::fmt(&"logarithm", f),
            EbiOutputType::ContainsRoot => Display::fmt(&"root", f),
            EbiOutputType::RootLogDiv => Display::fmt(&"rootlog", f),
            EbiOutputType::Bool => Display::fmt(&"bool", f),
        }
    }
}

#[derive(Debug, Clone, Eq, PartialEq, Hash)]
pub enum EbiExporter {
    Object(&'static EbiObjectExporter, &'static EbiFileHandler),
    String,
    SVG,
    PDF,
    Usize,
    Fraction,
    LogDiv,
    ContainsRoot,
    RootLogDiv,
    Bool,
}

impl EbiExporter {
    pub fn export_from_object(&self, output: EbiOutput, f: &mut dyn std::io::Write) -> Result<()> {
        match (self, output) {
            (EbiExporter::Object(exporter, _), object) => exporter.export(object, f),
            (EbiExporter::String, EbiOutput::String(object)) => object.export(f),
            (EbiExporter::String, _) => unreachable!(),
            (EbiExporter::SVG, EbiOutput::SVG(object)) => object.export(f),
            (EbiExporter::SVG, _) => unreachable!(),
            (EbiExporter::PDF, EbiOutput::PDF(object)) => Ok(f.write_all(&object)?),
            (EbiExporter::PDF, _) => unreachable!(),
            (EbiExporter::Usize, EbiOutput::Usize(object)) => object.export(f),
            (EbiExporter::Usize, _) => unreachable!(),
            (EbiExporter::Fraction, EbiOutput::Fraction(object)) => object.export(f),
            (EbiExporter::Fraction, _) => unreachable!(),
            (EbiExporter::LogDiv, EbiOutput::LogDiv(object)) => object.export(f),
            (EbiExporter::LogDiv, _) => unreachable!(),
            (EbiExporter::ContainsRoot, EbiOutput::ContainsRoot(object)) => object.export(f),
            (EbiExporter::ContainsRoot, _) => unreachable!(),
            (EbiExporter::RootLogDiv, EbiOutput::RootLogDiv(object)) => object.export(f),
            (EbiExporter::RootLogDiv, _) => unreachable!(),
            (EbiExporter::Bool, EbiOutput::Bool(object)) => object.export(f),
            (EbiExporter::Bool, _) => unreachable!(),
        }
    }

    pub fn get_article(&self) -> &str {
        match self {
            EbiExporter::Object(_, file_handler) => file_handler.article,
            EbiExporter::String => "",
            EbiExporter::SVG => "an",
            EbiExporter::PDF => "a",
            EbiExporter::Usize => "an",
            EbiExporter::Fraction => "a",
            EbiExporter::LogDiv => "a",
            EbiExporter::ContainsRoot => "a",
            EbiExporter::RootLogDiv => "a",
            EbiExporter::Bool => "a",
        }
    }

    pub fn get_name(&self) -> &str {
        match self {
            EbiExporter::Object(_, file_handler) => file_handler.name,
            EbiExporter::String => "string",
            EbiExporter::SVG => "SVG",
            EbiExporter::PDF => "PDF",
            EbiExporter::Usize => "integer",
            EbiExporter::Fraction => "fraction",
            EbiExporter::LogDiv => "logdiv",
            EbiExporter::ContainsRoot => "containsroot",
            EbiExporter::RootLogDiv => "rootlogdiv",
            EbiExporter::Bool => "boolean",
        }
    }

    pub fn get_java_object_handlers(&self) -> &'static [JavaObjectHandler] {
        match self {
            EbiExporter::Object(_, file_handler) => file_handler.java_object_handlers,
            EbiExporter::String => JAVA_OBJECT_HANDLERS_STRING,
            EbiExporter::SVG => JAVA_OBJECT_HANDLERS_SVG,
            EbiExporter::PDF => JAVA_OBJECT_HANDLERS_PDF,
            EbiExporter::Usize => JAVA_OBJECT_HANDLERS_USIZE,
            EbiExporter::Fraction => JAVA_OBJECT_HANDLERS_FRACTION,
            EbiExporter::LogDiv => JAVA_OBJECT_HANDLERS_LOGDIV,
            EbiExporter::ContainsRoot => JAVA_OBJECT_HANDLERS_CONTAINSROOT,
            EbiExporter::RootLogDiv => JAVA_OBJECT_HANDLERS_ROOTLOGDIV,
            EbiExporter::Bool => JAVA_OBJECT_HANDLERS_BOOL,
        }
    }

    pub fn get_extension(&self) -> &str {
        match self {
            EbiExporter::Object(_, file_handler) => file_handler.file_extension,
            EbiExporter::String => "txt",
            EbiExporter::SVG => "svg",
            EbiExporter::PDF => "pdf",
            EbiExporter::Usize => "int",
            EbiExporter::Fraction => "frac",
            EbiExporter::LogDiv => "logdiv",
            EbiExporter::ContainsRoot => "croot",
            EbiExporter::RootLogDiv => "rldiv",
            EbiExporter::Bool => "bool",
        }
    }

    pub fn is_binary(&self) -> bool {
        match self {
            EbiExporter::Object(_, _) => false,
            EbiExporter::String => false,
            EbiExporter::SVG => false,
            EbiExporter::PDF => true,
            EbiExporter::Usize => false,
            EbiExporter::Fraction => false,
            EbiExporter::LogDiv => false,
            EbiExporter::ContainsRoot => false,
            EbiExporter::RootLogDiv => false,
            EbiExporter::Bool => false,
        }
    }
}

impl Display for EbiExporter {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            EbiExporter::Object(_, file_handler) => Display::fmt(file_handler, f),
            EbiExporter::String => Display::fmt(&"text", f),
            EbiExporter::SVG => Display::fmt(&"svg", f),
            EbiExporter::PDF => Display::fmt(&"pdf", f),
            EbiExporter::Usize => Display::fmt(&"integer", f),
            EbiExporter::Fraction => Display::fmt(&"fraction", f),
            EbiExporter::LogDiv => Display::fmt(&"logarithm", f),
            EbiExporter::ContainsRoot => Display::fmt(&"root", f),
            EbiExporter::RootLogDiv => Display::fmt(&"rootlog", f),
            EbiExporter::Bool => Display::fmt(&"boolean", f),
        }
    }
}

#[derive(Debug, Eq, PartialEq, Hash)]
pub enum EbiObjectExporter {
    EventLog(fn(object: EbiOutput, &mut dyn std::io::Write) -> Result<()>),
    DirectlyFollowsModel(fn(object: EbiOutput, &mut dyn std::io::Write) -> Result<()>),
    FiniteLanguage(fn(object: EbiOutput, &mut dyn std::io::Write) -> Result<()>),
    FiniteStochasticLanguage(fn(object: EbiOutput, &mut dyn std::io::Write) -> Result<()>),
    LabelledPetriNet(fn(object: EbiOutput, &mut dyn std::io::Write) -> Result<()>),
    StochasticDeterministicFiniteAutomaton(
        fn(object: EbiOutput, &mut dyn std::io::Write) -> Result<()>,
    ),
    StochasticLabelledPetriNet(fn(object: EbiOutput, &mut dyn std::io::Write) -> Result<()>),
    LanguageOfAlignments(fn(object: EbiOutput, &mut dyn std::io::Write) -> Result<()>),
    StochasticLanguageOfAlignments(fn(object: EbiOutput, &mut dyn std::io::Write) -> Result<()>),
    DeterministicFiniteAutomaton(fn(object: EbiOutput, &mut dyn std::io::Write) -> Result<()>),
    ProcessTree(fn(object: EbiOutput, &mut dyn std::io::Write) -> Result<()>),
    StochasticProcessTree(fn(object: EbiOutput, &mut dyn std::io::Write) -> Result<()>),
    Executions(fn(object: EbiOutput, &mut dyn std::io::Write) -> Result<()>),
}

impl EbiObjectExporter {
    pub fn get_type(&self) -> EbiObjectType {
        match self {
            EbiObjectExporter::EventLog(_) => EbiObjectType::EventLog,
            EbiObjectExporter::DirectlyFollowsModel(_) => EbiObjectType::DirectlyFollowsModel,
            EbiObjectExporter::FiniteLanguage(_) => EbiObjectType::FiniteLanguage,
            EbiObjectExporter::FiniteStochasticLanguage(_) => {
                EbiObjectType::FiniteStochasticLanguage
            }
            EbiObjectExporter::LabelledPetriNet(_) => EbiObjectType::LabelledPetriNet,
            EbiObjectExporter::StochasticDeterministicFiniteAutomaton(_) => {
                EbiObjectType::StochasticDeterministicFiniteAutomaton
            }
            EbiObjectExporter::StochasticLabelledPetriNet(_) => {
                EbiObjectType::StochasticLabelledPetriNet
            }
            EbiObjectExporter::LanguageOfAlignments(_) => EbiObjectType::LanguageOfAlignments,
            EbiObjectExporter::StochasticLanguageOfAlignments(_) => {
                EbiObjectType::StochasticLanguageOfAlignments
            }
            EbiObjectExporter::DeterministicFiniteAutomaton(_) => {
                EbiObjectType::DeterministicFiniteAutomaton
            }
            EbiObjectExporter::ProcessTree(_) => EbiObjectType::ProcessTree,
            EbiObjectExporter::StochasticProcessTree(_) => EbiObjectType::StochasticProcessTree,
            EbiObjectExporter::Executions(_) => EbiObjectType::Executions,
        }
    }

    pub fn export(&self, object: EbiOutput, f: &mut dyn std::io::Write) -> Result<()> {
        match self {
            EbiObjectExporter::EventLog(exporter) => (exporter)(object, f),
            EbiObjectExporter::DirectlyFollowsModel(exporter) => (exporter)(object, f),
            EbiObjectExporter::FiniteLanguage(exporter) => (exporter)(object, f),
            EbiObjectExporter::FiniteStochasticLanguage(exporter) => (exporter)(object, f),
            EbiObjectExporter::LabelledPetriNet(exporter) => (exporter)(object, f),
            EbiObjectExporter::StochasticDeterministicFiniteAutomaton(exporter) => {
                (exporter)(object, f)
            }
            EbiObjectExporter::StochasticLabelledPetriNet(exporter) => (exporter)(object, f),
            EbiObjectExporter::LanguageOfAlignments(exporter) => (exporter)(object, f),
            EbiObjectExporter::StochasticLanguageOfAlignments(exporter) => (exporter)(object, f),
            EbiObjectExporter::DeterministicFiniteAutomaton(exporter) => (exporter)(object, f),
            EbiObjectExporter::ProcessTree(exporter) => (exporter)(object, f),
            EbiObjectExporter::StochasticProcessTree(exporter) => (exporter)(object, f),
            EbiObjectExporter::Executions(exporter) => (exporter)(object, f),
        }
    }
}

pub fn export_object(to_file: &PathBuf, object: EbiOutput, exporter: EbiExporter) -> Result<()> {
    let file =
        File::create(to_file).with_context(|| format!("Writing result to file {:?}.", to_file))?;
    let mut writer = std::io::BufWriter::new(&file);
    exporter
        .export_from_object(object, &mut writer)
        .with_context(|| format!("Writing result to file {:?}.", to_file))?;
    return writer
        .flush()
        .with_context(|| format!("writing result to file {:?}", to_file));
}

pub fn export_to_string(object: EbiOutput, exporter: EbiExporter) -> Result<String> {
    let mut f = vec![];
    exporter.export_from_object(object, &mut f)?;
    Ok(String::from_utf8(f)?)
}

pub fn export_to_bytes(object: EbiOutput, exporter: EbiExporter) -> Result<Vec<u8>> {
    let mut f = vec![];
    exporter.export_from_object(object, &mut f)?;
    Ok(f)
}

impl Display for EbiObjectExporter {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.get_type().to_string())
    }
}

#[cfg(test)]
mod tests {
    use std::{io::Cursor, path::PathBuf};

    use strum::IntoEnumIterator;

    use crate::{
        ebi_framework::{
            ebi_command::EbiCommand, ebi_file_handler::EBI_FILE_HANDLERS, ebi_input::EbiInput,
            ebi_output::EbiOutput,
        },
        math::{
            fraction::Fraction,
            log_div::LogDiv,
            root::{ContainsRoot, Root},
            root_log_div::RootLogDiv,
            traits::{One, Zero},
        },
    };

    use super::{EbiExporter, EbiOutputType, export_to_bytes, export_to_string};

    #[test]
    fn all_exporters() {
        for (object, importer, _) in crate::tests::get_all_test_files() {
            if let EbiInput::Object(object, _) = object {
                for file_handler2 in EBI_FILE_HANDLERS {
                    for exporter in file_handler2.object_exporters {
                        if exporter.get_type() == importer.clone().unwrap().get_type() {
                            let mut c = Cursor::new(Vec::new());
                            exporter
                                .export(EbiOutput::Object(object.clone()), &mut c)
                                .unwrap();
                        }
                    }
                }
            }
        }
    }

    #[test]
    fn ebi_output() {
        let mut outputs = vec![
            EbiOutput::String("bla".to_string()),
            EbiOutput::SVG("xyz".to_string()),
            EbiOutput::PDF(vec![]),
            EbiOutput::Usize(0),
            EbiOutput::Fraction(Fraction::one()),
            EbiOutput::LogDiv(LogDiv::zero()),
            EbiOutput::ContainsRoot(ContainsRoot::of(Root::of(Fraction::one()))),
            EbiOutput::RootLogDiv(RootLogDiv::sqrt(LogDiv::one())),
        ];

        //gather output objects from the test files
        for (input, _, _) in crate::tests::get_all_test_files() {
            if let EbiInput::Object(object, _) = input {
                outputs.push(EbiOutput::Object(object));
            }
        }

        for output in outputs {
            let output_type = output.get_type();
            output_type.get_applicable_commands();
            output_type.get_default_exporter();
            output_type.to_string();
            EbiCommand::select_exporter(&output_type, None);
            EbiCommand::select_exporter(&output_type, Some(&PathBuf::from(".xes.gz")));
            for exporter in output_type.get_exporters() {
                exporter.get_article();
                exporter.get_name();
                exporter.get_java_object_handlers();
                exporter.get_extension();
                exporter.is_binary();
                exporter.to_string();

                println!("\toutput   {}", output);
                println!("\texporter {}", exporter);

                let _ = export_to_bytes(output.clone(), exporter.clone());
                let _ = export_to_string(output.clone(), exporter.clone());

                // let mut f = vec![];
                // _ = exporter.export_from_object(output.clone(), &mut f);
            }
        }
        EbiOutputType::iter();
    }

    #[test]
    #[should_panic]
    fn unreachable_string() {
        let mut f = vec![];
        let _ = EbiExporter::String.export_from_object(EbiOutput::Usize(10), &mut f);
    }

    #[test]
    #[should_panic]
    fn unreachable_usize() {
        let mut f = vec![];
        let _ = EbiExporter::Usize.export_from_object(EbiOutput::String("a".to_string()), &mut f);
    }

    #[test]
    #[should_panic]
    fn unreachable_svg() {
        let mut f = vec![];
        let _ = EbiExporter::SVG.export_from_object(EbiOutput::Usize(10), &mut f);
    }

    #[test]
    #[should_panic]
    fn unreachable_pdf() {
        let mut f = vec![];
        let _ = EbiExporter::PDF.export_from_object(EbiOutput::Usize(10), &mut f);
    }

    #[test]
    #[should_panic]
    fn unreachable_fraction() {
        let mut f = vec![];
        let _ = EbiExporter::Fraction.export_from_object(EbiOutput::Usize(10), &mut f);
    }

    #[test]
    #[should_panic]
    fn unreachable_logdiv() {
        let mut f = vec![];
        let _ = EbiExporter::LogDiv.export_from_object(EbiOutput::Usize(10), &mut f);
    }

    #[test]
    #[should_panic]
    fn unreachable_containsroot() {
        let mut f = vec![];
        let _ = EbiExporter::ContainsRoot.export_from_object(EbiOutput::Usize(10), &mut f);
    }

    #[test]
    #[should_panic]
    fn unreachable_rootlogdiv() {
        let mut f = vec![];
        let _ = EbiExporter::RootLogDiv.export_from_object(EbiOutput::Usize(10), &mut f);
    }
}
