use ebi_objects::{Exportable, Importable, StochasticDirectlyFollowsModel};

use crate::{
    ebi_framework::{
        ebi_file_handler::EbiFileHandler,
        ebi_input::{EbiObjectImporter, EbiTraitImporter},
        ebi_output::EbiObjectExporter,
        object_importers::{
            ToDirectlyFollowsModelObject, ToLabelledPetriNetObject,
            ToStochasticLabelledPetriNetObject,
        },
        validate::Validate,
    },
    ebi_traits::{
        ebi_trait_activities::ToActivities, ebi_trait_graphable::ToGraphable,
        ebi_trait_queriable_stochastic_language::ToQueriableStochasticLanguage,
        ebi_trait_semantics::ToSemantics,
        ebi_trait_stochastic_deterministic_semantics::ToStochasticDeterministicSemantics,
        ebi_trait_stochastic_semantics::ToStochasticSemantics,
    },
};

pub const EBI_STOCHASTIC_DIRECTLY_FOLLOWS_MODEL: EbiFileHandler = EbiFileHandler {
    name: "stochastic directly follows model",
    article: "a",
    file_extension: "sdfm",
    is_binary: false,
    format_specification: StochasticDirectlyFollowsModel::FILE_FORMAT_SPECIFICATION_LATEX,
    validator: Some(StochasticDirectlyFollowsModel::validate),
    trait_importers: &[
        EbiTraitImporter::Activities(
            StochasticDirectlyFollowsModel::import_as_activities,
            StochasticDirectlyFollowsModel::IMPORTER_PARAMETERS,
        ),
        EbiTraitImporter::Graphable(
            StochasticDirectlyFollowsModel::import_as_graphable,
            StochasticDirectlyFollowsModel::IMPORTER_PARAMETERS,
        ),
        EbiTraitImporter::QueriableStochasticLanguage(
            StochasticDirectlyFollowsModel::import_as_queriable_stochastic_language,
            StochasticDirectlyFollowsModel::IMPORTER_PARAMETERS,
        ),
        EbiTraitImporter::Semantics(
            StochasticDirectlyFollowsModel::import_as_semantics,
            StochasticDirectlyFollowsModel::IMPORTER_PARAMETERS,
        ),
        EbiTraitImporter::StochasticSemantics(
            StochasticDirectlyFollowsModel::import_as_stochastic_semantics,
            StochasticDirectlyFollowsModel::IMPORTER_PARAMETERS,
        ),
        EbiTraitImporter::StochasticDeterministicSemantics(
            StochasticDirectlyFollowsModel::import_as_stochastic_deterministic_semantics,
            StochasticDirectlyFollowsModel::IMPORTER_PARAMETERS,
        ),
        EbiTraitImporter::Graphable(
            StochasticDirectlyFollowsModel::import_as_graphable,
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
    java_object_handlers: &[],
};
