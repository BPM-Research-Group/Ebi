use ebi_objects::{
    Exportable, ScalableVectorGraphics, ebi_objects::scalable_vector_graphics::FORMAT_SPECIFICATION,
};

use crate::ebi_framework::{ebi_file_handler::EbiFileHandler, ebi_output::EbiObjectExporter};

pub const EBI_SCALABLE_VECTOR_GRAPHICS: EbiFileHandler = EbiFileHandler {
    name: "scalable vector graphics",
    article: "a",
    file_extension: "svg",
    is_binary: false,
    format_specification: &FORMAT_SPECIFICATION,
    validator: None,
    trait_importers: &[],
    object_importers: &[],
    object_exporters: &[
        EbiObjectExporter::DeterministicFiniteAutomaton(ScalableVectorGraphics::export_from_object),
        EbiObjectExporter::DirectlyFollowsModel(ScalableVectorGraphics::export_from_object),
        EbiObjectExporter::DirectlyFollowsGraph(ScalableVectorGraphics::export_from_object),
        EbiObjectExporter::LabelledPetriNet(ScalableVectorGraphics::export_from_object),
        EbiObjectExporter::ProcessTree(ScalableVectorGraphics::export_from_object),
        EbiObjectExporter::StochasticDeterministicFiniteAutomaton(
            ScalableVectorGraphics::export_from_object,
        ),
        EbiObjectExporter::StochasticLabelledPetriNet(ScalableVectorGraphics::export_from_object),
        EbiObjectExporter::StochasticProcessTree(ScalableVectorGraphics::export_from_object),
        EbiObjectExporter::ScalableVectorGraphics(ScalableVectorGraphics::export_from_object),
    ],
    java_object_handlers: &[
    //     JavaObjectHandler {
    //     name: "svg",
    //     java_class: "com.kitfox.svg.SVGDiagram",
    //     translator_ebi_to_java: Some("org.processmining.ebi.objects.EbiSvg.fromEbiString"),
    //     translator_java_to_ebi: None,
    //     input_gui: None,
    // }
    ],
};
