use crate::{
    ebi_framework::{
        ebi_file_handler::EbiFileHandler,
        ebi_input::{EbiInput, EbiObjectImporter, EbiTraitImporter},
        ebi_output::EbiObjectExporter,
        ebi_trait::FromEbiTraitObject,
        object_importers::{ToLabelledPetriNetObject, ToProcessTreeObject},
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

use anyhow::{Result, anyhow};
use ebi_objects::{
    EbiObject, Exportable, Importable, StochasticProcessTree,
    ebi_objects::stochastic_process_tree::FORMAT_SPECIFICATION,
};

pub const EBI_STOCHASTIC_PROCESS_TREE: EbiFileHandler = EbiFileHandler {
    name: "stochastic process tree",
    article: "a",
    file_extension: "sptree",
    is_binary: false,
    format_specification: &FORMAT_SPECIFICATION,
    validator: Some(StochasticProcessTree::validate),
    trait_importers: &[
        EbiTraitImporter::Activities(StochasticProcessTree::import_as_activities),
        EbiTraitImporter::QueriableStochasticLanguage(
            StochasticProcessTree::import_as_queriable_stochastic_language,
        ),
        EbiTraitImporter::StochasticDeterministicSemantics(
            StochasticProcessTree::import_as_stochastic_deterministic_semantics,
        ),
        EbiTraitImporter::Semantics(StochasticProcessTree::import_as_semantics),
        EbiTraitImporter::StochasticSemantics(
            StochasticProcessTree::import_as_stochastic_semantics,
        ),
        EbiTraitImporter::Graphable(StochasticProcessTree::import_as_graphable),
    ],
    object_importers: &[
        EbiObjectImporter::StochasticProcessTree(StochasticProcessTree::import_as_object),
        EbiObjectImporter::ProcessTree(StochasticProcessTree::import_as_process_tree_object),
        EbiObjectImporter::LabelledPetriNet(
            StochasticProcessTree::import_as_labelled_petri_net_object,
        ),
    ],
    object_exporters: &[EbiObjectExporter::StochasticProcessTree(
        StochasticProcessTree::export_from_object,
    )],
    java_object_handlers: &[],
};

impl FromEbiTraitObject for StochasticProcessTree {
    fn from_trait_object(object: EbiInput) -> Result<Box<Self>> {
        match object {
            EbiInput::Object(EbiObject::StochasticProcessTree(e), _) => Ok(Box::new(e)),
            _ => Err(anyhow!(
                "cannot read {} {} as a stochastic process tree",
                object.get_type().get_article(),
                object.get_type()
            )),
        }
    }
}
