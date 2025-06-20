use anyhow::{Result, anyhow};
use std::fmt::Display;
use svg2pdf::{ConversionOptions, PageOptions};

use crate::{
    ebi_framework::{
        ebi_file_handler::EbiFileHandler,
        ebi_object::EbiObject,
        ebi_output::{EbiObjectExporter, EbiOutput},
        exportable::Exportable,
    },
    ebi_objects::scalable_vector_graphics::{ScalableVectorGraphics, ToSVG},
};

pub const FORMAT_SPECIFICATION: &str = "Ebi does not support importing of PDF files.";

pub const EBI_PORTABLE_DOCUMENT_FORMAT: EbiFileHandler = EbiFileHandler {
    name: "portable document format",
    article: "a",
    file_extension: "pdf",
    is_binary: true,
    format_specification: &FORMAT_SPECIFICATION,
    validator: None,
    trait_importers: &[],
    object_importers: &[],
    object_exporters: &[
        EbiObjectExporter::DeterministicFiniteAutomaton(PortableDocumentFormat::export_from_object),
        EbiObjectExporter::DirectlyFollowsModel(PortableDocumentFormat::export_from_object),
        EbiObjectExporter::DirectlyFollowsGraph(PortableDocumentFormat::export_from_object),
        EbiObjectExporter::LabelledPetriNet(PortableDocumentFormat::export_from_object),
        EbiObjectExporter::ProcessTree(PortableDocumentFormat::export_from_object),
        EbiObjectExporter::StochasticDeterministicFiniteAutomaton(
            PortableDocumentFormat::export_from_object,
        ),
        EbiObjectExporter::StochasticLabelledPetriNet(PortableDocumentFormat::export_from_object),
        EbiObjectExporter::StochasticProcessTree(PortableDocumentFormat::export_from_object),
        EbiObjectExporter::ScalableVectorGraphics(PortableDocumentFormat::export_from_object),
    ],
    java_object_handlers: &[],
};

pub struct PortableDocumentFormat(Vec<u8>);

impl Display for PortableDocumentFormat {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        "pdf".fmt(f)
    }
}

impl From<Vec<u8>> for PortableDocumentFormat {
    fn from(value: Vec<u8>) -> Self {
        Self(value)
    }
}

impl Exportable for PortableDocumentFormat {
    fn export_from_object(object: EbiOutput, f: &mut dyn std::io::Write) -> Result<()> {
        match object {
            EbiOutput::Object(EbiObject::DeterministicFiniteAutomaton(object)) => {
                object.to_pdf()?.export(f)
            }
            EbiOutput::Object(EbiObject::DirectlyFollowsModel(object)) => {
                object.to_pdf()?.export(f)
            }
            EbiOutput::Object(EbiObject::StochasticDirectlyFollowsModel(object)) => {
                object.to_pdf()?.export(f)
            }
            EbiOutput::Object(EbiObject::EventLog(_)) => {
                Err(anyhow!("cannot export event log as PDF"))
            }
            EbiOutput::Object(EbiObject::Executions(_)) => {
                Err(anyhow!("cannot export executions as PDF"))
            }
            EbiOutput::Object(EbiObject::FiniteLanguage(_)) => {
                Err(anyhow!("cannot export finite language as PDF"))
            }
            EbiOutput::Object(EbiObject::FiniteStochasticLanguage(_)) => {
                Err(anyhow!("cannot export finite stochastic as PDF"))
            }
            EbiOutput::Object(EbiObject::LabelledPetriNet(object)) => object.to_pdf()?.export(f),
            EbiOutput::Object(EbiObject::LanguageOfAlignments(_)) => {
                Err(anyhow!("cannot export language of alignments as PDF"))
            }
            EbiOutput::Object(EbiObject::ProcessTree(object)) => object.to_pdf()?.export(f),
            EbiOutput::Object(EbiObject::StochasticDeterministicFiniteAutomaton(object)) => {
                object.to_pdf()?.export(f)
            }
            EbiOutput::Object(EbiObject::StochasticLabelledPetriNet(object)) => {
                object.to_pdf()?.export(f)
            }
            EbiOutput::Object(EbiObject::StochasticLanguageOfAlignments(_)) => Err(anyhow!(
                "cannot export stochastic language of alignments as PDF"
            )),
            EbiOutput::Object(EbiObject::StochasticProcessTree(object)) => {
                object.to_pdf()?.export(f)
            }
            EbiOutput::Object(EbiObject::DirectlyFollowsGraph(object)) => {
                object.to_pdf()?.export(f)
            }
            EbiOutput::Object(EbiObject::ScalableVectorGraphics(object)) => {
                object.to_pdf()?.export(f)
            }
            EbiOutput::String(_) => Err(anyhow!("cannot export string as PDF")),
            EbiOutput::Usize(_) => Err(anyhow!("cannot export usize as PDF")),
            EbiOutput::Fraction(_) => Err(anyhow!("cannot export fraction as PDF")),
            EbiOutput::LogDiv(_) => Err(anyhow!("cannot export lopdiv as PDF")),
            EbiOutput::ContainsRoot(_) => Err(anyhow!("cannot export containsroot as PDF")),
            EbiOutput::RootLogDiv(_) => Err(anyhow!("cannot export rootlogdiv as PDF")),
            EbiOutput::Bool(_) => Err(anyhow!("cannot export boolean as PDF")),
        }
    }

    fn export(&self, f: &mut dyn std::io::Write) -> anyhow::Result<()> {
        Ok(f.write_all(&self.0)?)
    }
}

pub trait ToPDF {
    fn to_pdf(&self) -> Result<PortableDocumentFormat>;
}

impl<T> ToPDF for T
where
    T: ToSVG,
{
    fn to_pdf(&self) -> Result<PortableDocumentFormat> {
        self.to_svg()?.to_pdf()
    }
}

impl ToPDF for ScalableVectorGraphics {
    fn to_pdf(&self) -> Result<PortableDocumentFormat> {
        let mut options = svg2pdf::usvg::Options::default();
        options.fontdb_mut().load_system_fonts();
        let tree = svg2pdf::usvg::Tree::from_str(&self.0, &options)?;
        match svg2pdf::to_pdf(&tree, ConversionOptions::default(), PageOptions::default()) {
            Ok(x) => Ok(x.into()),
            Err(err) => Err(anyhow!(err)),
        }
    }
}
