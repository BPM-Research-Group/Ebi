use crate::ebi_framework::{
    ebi_file_handler::EbiFileHandler,
    ebi_input::{EbiObjectImporter, EbiTraitImporter},
    ebi_output::EbiObjectExporter,
    object_importers::{
        ImportAsDirectlyFollowsModelObject, ImportAsLabelledPetriNetObject,
        ImportAsStochasticLabelledPetriNetObject,
    },
    trait_importers::{
        ImportAsActivitiesTrait, ImportAsGraphableTrait, ImportAsQueriableStochasticLanguageTrait,
        ImportAsSemanticsTrait, ImportAsStochasticDeterministicSemanticsTrait,
        ImportAsStochasticSemanticsTrait,
    },
    validate::Validate,
};
use ebi_objects::{Exportable, Importable, StochasticDirectlyFollowsModel};

pub const EBI_STOCHASTIC_DIRECTLY_FOLLOWS_MODEL: EbiFileHandler = EbiFileHandler {
    name: "stochastic directly follows model",
    article: "a",
    file_extension: "sdfm",
    is_binary: false,
    format_specification: StochasticDirectlyFollowsModel::FILE_FORMAT_SPECIFICATION_LATEX,
    validator: Some(StochasticDirectlyFollowsModel::validate),
    trait_importers: &[
        EbiTraitImporter::Activities(
            StochasticDirectlyFollowsModel::import_as_activities_trait,
            StochasticDirectlyFollowsModel::IMPORTER_PARAMETERS,
        ),
        EbiTraitImporter::Graphable(
            StochasticDirectlyFollowsModel::import_as_graphable_trait,
            StochasticDirectlyFollowsModel::IMPORTER_PARAMETERS,
        ),
        EbiTraitImporter::QueriableStochasticLanguage(
            StochasticDirectlyFollowsModel::import_as_queriable_stochastic_language_trait,
            StochasticDirectlyFollowsModel::IMPORTER_PARAMETERS,
        ),
        EbiTraitImporter::Semantics(
            StochasticDirectlyFollowsModel::import_as_semantics_trait,
            StochasticDirectlyFollowsModel::IMPORTER_PARAMETERS,
        ),
        EbiTraitImporter::StochasticSemantics(
            StochasticDirectlyFollowsModel::import_as_stochastic_semantics_trait,
            StochasticDirectlyFollowsModel::IMPORTER_PARAMETERS,
        ),
        EbiTraitImporter::StochasticDeterministicSemantics(
            StochasticDirectlyFollowsModel::import_as_stochastic_deterministic_semantics_trait,
            StochasticDirectlyFollowsModel::IMPORTER_PARAMETERS,
        ),
        EbiTraitImporter::Graphable(
            StochasticDirectlyFollowsModel::import_as_graphable_trait,
            StochasticDirectlyFollowsModel::IMPORTER_PARAMETERS,
        ),
    ],
    object_importers: &[
        EbiObjectImporter::StochasticDirectlyFollowsModel(
            StochasticDirectlyFollowsModel::import_as_object,
            StochasticDirectlyFollowsModel::IMPORTER_PARAMETERS,
        ),
        EbiObjectImporter::DirectlyFollowsModel(
            StochasticDirectlyFollowsModel::import_as_directly_follows_model_object,
            StochasticDirectlyFollowsModel::IMPORTER_PARAMETERS,
        ),
        EbiObjectImporter::LabelledPetriNet(
            StochasticDirectlyFollowsModel::import_as_labelled_petri_net_object,
            StochasticDirectlyFollowsModel::IMPORTER_PARAMETERS,
        ),
        EbiObjectImporter::StochasticLabelledPetriNet(
            StochasticDirectlyFollowsModel::import_as_stochastic_labelled_petri_net_object,
            StochasticDirectlyFollowsModel::IMPORTER_PARAMETERS,
        ),
    ],
    object_exporters: &[
        EbiObjectExporter::StochasticDirectlyFollowsModel(
            StochasticDirectlyFollowsModel::export_from_object,
        ),
        EbiObjectExporter::DirectlyFollowsGraph(StochasticDirectlyFollowsModel::export_from_object),
    ],
    object_exporters_fallible: &[],
    java_object_handlers: &[],
};
