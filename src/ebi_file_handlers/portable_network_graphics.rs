use crate::ebi_framework::{ebi_file_handler::EbiFileHandler, ebi_output::EbiObjectExporter};
use ebi_objects::{
    Exportable, PortableNetworkGraphics, ebi_objects::portable_network_graphics::FORMAT_SPECIFICATION
};

pub const EBI_PORTABLE_NETWORK_GRAPHCIS: EbiFileHandler = EbiFileHandler {
    name: "portable network graphics",
    article: "a",
    file_extension: "png",
    is_binary: true,
    format_specification: &FORMAT_SPECIFICATION,
    validator: None,
    trait_importers: &[],
    object_importers: &[],
    object_exporters: &[
        EbiObjectExporter::DeterministicFiniteAutomaton(
            PortableNetworkGraphics::export_from_object,
        ),
        EbiObjectExporter::DirectlyFollowsModel(PortableNetworkGraphics::export_from_object),
        EbiObjectExporter::DirectlyFollowsGraph(PortableNetworkGraphics::export_from_object),
        EbiObjectExporter::LabelledPetriNet(PortableNetworkGraphics::export_from_object),
        EbiObjectExporter::ProcessTree(PortableNetworkGraphics::export_from_object),
        EbiObjectExporter::StochasticDeterministicFiniteAutomaton(
            PortableNetworkGraphics::export_from_object,
        ),
        EbiObjectExporter::StochasticNondeterministicFiniteAutomaton(
            PortableNetworkGraphics::export_from_object,
        ),
        EbiObjectExporter::StochasticLabelledPetriNet(PortableNetworkGraphics::export_from_object),
        EbiObjectExporter::StochasticProcessTree(PortableNetworkGraphics::export_from_object),
        EbiObjectExporter::ScalableVectorGraphics(PortableNetworkGraphics::export_from_object),
    ],
    object_exporters_fallible: &[],
    java_object_handlers: &[],
};
