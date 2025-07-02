use anyhow::anyhow;
use std::fmt::Display;

use crate::ebi_framework::{
    ebi_file_handler::EbiFileHandler,
    ebi_input::{self, EbiObjectImporter},
    ebi_object::EbiObject,
    ebi_output::{EbiObjectExporter, EbiOutput},
    exportable::Exportable,
    importable::Importable,
    infoable::Infoable,
};

pub const FORMAT_SPECIFICATION: &str =
    "A Business Process Modelling and Notation (BPMN) file follows the BPMN 2.0.2 XML format~\\cite{bpmn}.
    Please note that Ebi supports only a small subset of BPMN elements.
For instance:
    \\lstinputlisting[language=xml, style=boxed]{../testfiles/a.bpmn}";

pub const EBI_BUSINESS_PROCESS_MODEL_AND_NOTATION: EbiFileHandler = EbiFileHandler {
    name: "business process model and notation",
    article: "a",
    file_extension: "bpmn",
    is_binary: false,
    format_specification: &FORMAT_SPECIFICATION,
    validator: Some(ebi_input::validate::<BusinessProcessModelAndNotation>),
    trait_importers: &[],
    object_importers: &[EbiObjectImporter::BusinessProcessModelAndNotation(
        BusinessProcessModelAndNotation::import_as_object,
    )],
    object_exporters: &[EbiObjectExporter::BusinessProcessModelAndNotation(
        BusinessProcessModelAndNotation::export_from_object,
    )],
    java_object_handlers: &[],
};

#[derive(Clone)]
pub struct BusinessProcessModelAndNotation {}

impl Importable for BusinessProcessModelAndNotation {
    fn import_as_object(reader: &mut dyn std::io::BufRead) -> anyhow::Result<EbiObject> {
        Ok(EbiObject::BusinessProcessModelAndNotation(Self::import(
            reader,
        )?))
    }

    fn import(reader: &mut dyn std::io::BufRead) -> anyhow::Result<Self>
    where
        Self: Sized,
    {
        todo!()
    }
}

impl Exportable for BusinessProcessModelAndNotation {
    fn export_from_object(object: EbiOutput, f: &mut dyn std::io::Write) -> anyhow::Result<()> {
        match object {
            EbiOutput::Object(EbiObject::BusinessProcessModelAndNotation(bpmn)) => bpmn.export(f),
            _ => Err(anyhow!("Cannot export object as BPMN.")),
        }
    }

    fn export(&self, f: &mut dyn std::io::Write) -> anyhow::Result<()> {
        Ok(write!(f, "{}", self)?)
    }
}

impl Display for BusinessProcessModelAndNotation {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        todo!()
    }
}

impl Infoable for BusinessProcessModelAndNotation {
    fn info(&self, f: &mut impl std::io::Write) -> anyhow::Result<()> {
        todo!()
    }
}
