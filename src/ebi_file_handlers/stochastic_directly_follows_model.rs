use ebi_objects::{
    Exportable, Importable, StochasticDirectlyFollowsModel,
    ebi_objects::stochastic_directly_follows_model::FORMAT_SPECIFICATION,
};

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
    format_specification: &FORMAT_SPECIFICATION,
    validator: Some(StochasticDirectlyFollowsModel::validate),
    trait_importers: &[
        EbiTraitImporter::Activities(StochasticDirectlyFollowsModel::import_as_activities),
        EbiTraitImporter::QueriableStochasticLanguage(
            StochasticDirectlyFollowsModel::import_as_queriable_stochastic_language,
        ),
        EbiTraitImporter::Semantics(StochasticDirectlyFollowsModel::import_as_semantics),
        EbiTraitImporter::StochasticSemantics(
            StochasticDirectlyFollowsModel::import_as_stochastic_semantics,
        ),
        EbiTraitImporter::StochasticDeterministicSemantics(
            StochasticDirectlyFollowsModel::import_as_stochastic_deterministic_semantics,
        ),
        EbiTraitImporter::Graphable(StochasticDirectlyFollowsModel::import_as_graphable),
    ],
    object_importers: &[
        EbiObjectImporter::StochasticDirectlyFollowsModel(
            StochasticDirectlyFollowsModel::import_as_object,
        ),
        EbiObjectImporter::DirectlyFollowsModel(
            StochasticDirectlyFollowsModel::import_as_directly_follows_model_object,
        ),
        EbiObjectImporter::LabelledPetriNet(
            StochasticDirectlyFollowsModel::import_as_labelled_petri_net_object,
        ),
        EbiObjectImporter::StochasticLabelledPetriNet(
            StochasticDirectlyFollowsModel::import_as_stochastic_labelled_petri_net_object,
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
