use crate::{
    ebi_framework::{
        ebi_file_handler::EbiFileHandler, ebi_input::EbiTraitImporter,
        ebi_output::EbiObjectExporter, validate::Validate,
    },
    ebi_traits::{
        ebi_trait_activities::ToActivities, ebi_trait_graphable::ToGraphable,
        ebi_trait_semantics::ToSemantics,
    },
};
use ebi_objects::{Exportable, Importable, LolaNet};

pub const EBI_LOLA_NET: EbiFileHandler = EbiFileHandler {
    name: "LoLa Petri net",
    article: "a",
    file_extension: "lola",
    is_binary: false,
    format_specification: LolaNet::FILE_FORMAT_SPECIFICATION_LATEX,
    validator: Some(LolaNet::validate),
    trait_importers: &[
        EbiTraitImporter::Activities(LolaNet::import_as_activities, LolaNet::IMPORTER_PARAMETERS),
        EbiTraitImporter::Semantics(LolaNet::import_as_semantics, LolaNet::IMPORTER_PARAMETERS),
        EbiTraitImporter::Graphable(LolaNet::import_as_graphable, LolaNet::IMPORTER_PARAMETERS),
    ],
    object_importers: &[],
    object_exporters: &[
        EbiObjectExporter::LabelledPetriNet(LolaNet::export_from_object),
        EbiObjectExporter::DeterministicFiniteAutomaton(LolaNet::export_from_object),
        EbiObjectExporter::StochasticLabelledPetriNet(LolaNet::export_from_object),
        EbiObjectExporter::StochasticDeterministicFiniteAutomaton(LolaNet::export_from_object),
        EbiObjectExporter::StochasticDirectlyFollowsModel(LolaNet::export_from_object),
        EbiObjectExporter::DirectlyFollowsModel(LolaNet::export_from_object),
        EbiObjectExporter::DirectlyFollowsGraph(LolaNet::export_from_object),
        EbiObjectExporter::ProcessTree(LolaNet::export_from_object),
        EbiObjectExporter::StochasticProcessTree(LolaNet::export_from_object),
    ],
    java_object_handlers: &[], //java translations covered by LabelledPetrinet
};
