use anyhow::{Result, anyhow};
use ebi_objects::{
    DirectlyFollowsGraph, EbiObject, Exportable, Importable,
    ebi_objects::directly_follows_graph::FORMAT_SPECIFICATION,
};

use crate::{
    ebi_framework::{
        ebi_file_handler::EbiFileHandler,
        ebi_input::{EbiInput, EbiObjectImporter, EbiTraitImporter},
        ebi_output::EbiObjectExporter,
        ebi_trait::FromEbiTraitObject,
        object_importers::{
            ToDirectlyFollowsModelObject, ToLabelledPetriNetObject,
            ToStochasticDirectlyFollowsModelObject, ToStochasticLabelledPetriNetObject,
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

pub const EBI_DIRECTLY_FOLLOWS_GRAPH: EbiFileHandler = EbiFileHandler {
    name: "directly follows graph",
    article: "a",
    file_extension: "dfg",
    is_binary: false,
    format_specification: &FORMAT_SPECIFICATION,
    validator: Some(DirectlyFollowsGraph::validate),
    trait_importers: &[
        EbiTraitImporter::Activities(DirectlyFollowsGraph::import_as_activities),
        EbiTraitImporter::QueriableStochasticLanguage(
            DirectlyFollowsGraph::import_as_queriable_stochastic_language,
        ),
        EbiTraitImporter::Semantics(DirectlyFollowsGraph::import_as_semantics),
        EbiTraitImporter::StochasticSemantics(DirectlyFollowsGraph::import_as_stochastic_semantics),
        EbiTraitImporter::StochasticDeterministicSemantics(
            DirectlyFollowsGraph::import_as_stochastic_deterministic_semantics,
        ),
        EbiTraitImporter::Graphable(DirectlyFollowsGraph::import_as_graphable),
    ],
    object_importers: &[
        EbiObjectImporter::DirectlyFollowsGraph(DirectlyFollowsGraph::import_as_object),
        EbiObjectImporter::StochasticDirectlyFollowsModel(
            DirectlyFollowsGraph::import_as_stochastic_directly_follows_model_object,
        ),
        EbiObjectImporter::DirectlyFollowsModel(
            DirectlyFollowsGraph::import_as_directly_follows_model_object,
        ),
        EbiObjectImporter::LabelledPetriNet(
            DirectlyFollowsGraph::import_as_labelled_petri_net_object,
        ),
        EbiObjectImporter::StochasticLabelledPetriNet(
            DirectlyFollowsGraph::import_as_stochastic_labelled_petri_net_object,
        ),
    ],
    object_exporters: &[EbiObjectExporter::DirectlyFollowsGraph(
        DirectlyFollowsGraph::export_from_object,
    )],
    java_object_handlers: &[], //java translations covered by LabelledPetrinet
};

impl FromEbiTraitObject for DirectlyFollowsGraph {
    fn from_trait_object(object: EbiInput) -> Result<Box<Self>> {
        match object {
            EbiInput::Object(EbiObject::DirectlyFollowsGraph(e), _) => Ok(Box::new(e)),
            _ => Err(anyhow!(
                "cannot read {} {} as a directly follows graph",
                object.get_type().get_article(),
                object.get_type()
            )),
        }
    }
}
