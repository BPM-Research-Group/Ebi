use crate::ebi_framework::{
    ebi_file_handler::EbiFileHandler,
    ebi_input::{EbiInput, EbiObjectImporter, EbiTraitImporter},
    ebi_output::EbiObjectExporter,
    ebi_trait::FromEbiTraitObject,
    object_importers::{
        ImportAsDirectlyFollowsModelObject, ImportAsLabelledPetriNetObject,
        ImportAsStochasticDeterministicFiniteAutomatonObject,
        ImportAsStochasticDirectlyFollowsModelObject, ImportAsStochasticLabelledPetriNetObject,
        ImportAsStochasticNondeterministicFiniteAutomatonObject,
    },
    trait_importers::{
        ImportAsActivitiesTrait, ImportAsGraphableTrait, ImportAsQueriableStochasticLanguageTrait,
        ImportAsSemanticsTrait, ImportAsStochasticDeterministicSemanticsTrait,
        ImportAsStochasticSemanticsTrait,
    },
    validate::Validate,
};
use anyhow::{Result, anyhow};
use ebi_objects::{DirectlyFollowsGraph, EbiObject, Exportable, Importable};

pub const EBI_DIRECTLY_FOLLOWS_GRAPH: EbiFileHandler = EbiFileHandler {
    name: "directly follows graph",
    article: "a",
    file_extension: "dfg",
    is_binary: false,
    format_specification: DirectlyFollowsGraph::FILE_FORMAT_SPECIFICATION_LATEX,
    validator: Some(DirectlyFollowsGraph::validate),
    trait_importers: &[
        EbiTraitImporter::Activities(
            DirectlyFollowsGraph::import_as_activities_trait,
            DirectlyFollowsGraph::IMPORTER_PARAMETERS,
        ),
        EbiTraitImporter::QueriableStochasticLanguage(
            DirectlyFollowsGraph::import_as_queriable_stochastic_language_trait,
            DirectlyFollowsGraph::IMPORTER_PARAMETERS,
        ),
        EbiTraitImporter::Semantics(
            DirectlyFollowsGraph::import_as_semantics_trait,
            DirectlyFollowsGraph::IMPORTER_PARAMETERS,
        ),
        EbiTraitImporter::StochasticSemantics(
            DirectlyFollowsGraph::import_as_stochastic_semantics_trait,
            DirectlyFollowsGraph::IMPORTER_PARAMETERS,
        ),
        EbiTraitImporter::StochasticDeterministicSemantics(
            DirectlyFollowsGraph::import_as_stochastic_deterministic_semantics_trait,
            DirectlyFollowsGraph::IMPORTER_PARAMETERS,
        ),
        EbiTraitImporter::Graphable(
            DirectlyFollowsGraph::import_as_graphable_trait,
            DirectlyFollowsGraph::IMPORTER_PARAMETERS,
        ),
    ],
    object_importers: &[
        EbiObjectImporter::DirectlyFollowsGraph(
            DirectlyFollowsGraph::import_as_object,
            DirectlyFollowsGraph::IMPORTER_PARAMETERS,
        ),
        EbiObjectImporter::StochasticDirectlyFollowsModel(
            DirectlyFollowsGraph::import_as_stochastic_directly_follows_model_object,
            DirectlyFollowsGraph::IMPORTER_PARAMETERS,
        ),
        EbiObjectImporter::DirectlyFollowsModel(
            DirectlyFollowsGraph::import_as_directly_follows_model_object,
            DirectlyFollowsGraph::IMPORTER_PARAMETERS,
        ),
        EbiObjectImporter::LabelledPetriNet(
            DirectlyFollowsGraph::import_as_labelled_petri_net_object,
            DirectlyFollowsGraph::IMPORTER_PARAMETERS,
        ),
        EbiObjectImporter::StochasticLabelledPetriNet(
            DirectlyFollowsGraph::import_as_stochastic_labelled_petri_net_object,
            DirectlyFollowsGraph::IMPORTER_PARAMETERS,
        ),
        EbiObjectImporter::StochasticDeterministicFiniteAutomaton(
            DirectlyFollowsGraph::import_as_stochastic_deterministic_finite_automaton_object,
            DirectlyFollowsGraph::IMPORTER_PARAMETERS,
        ),
        EbiObjectImporter::StochasticNondeterministicFiniteAutomaton(
            DirectlyFollowsGraph::import_as_stochastic_nondeterministic_finite_automaton_object,
            DirectlyFollowsGraph::IMPORTER_PARAMETERS,
        ),
    ],
    object_exporters: &[EbiObjectExporter::DirectlyFollowsGraph(
        DirectlyFollowsGraph::export_from_object,
    )],
    object_exporters_fallible: &[],
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
