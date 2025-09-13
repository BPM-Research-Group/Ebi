use ebi_objects::{
    Exportable, PortableDocumentFormat, ebi_objects::portable_document_format::FORMAT_SPECIFICATION,
};

use crate::ebi_framework::{ebi_file_handler::EbiFileHandler, ebi_output::EbiObjectExporter};

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
