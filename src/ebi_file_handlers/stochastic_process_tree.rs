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
use ebi_objects::{EbiObject, Exportable, Importable, StochasticProcessTree};

pub const EBI_STOCHASTIC_PROCESS_TREE: EbiFileHandler = EbiFileHandler {
    name: "stochastic process tree",
    article: "a",
    file_extension: "sptree",
    is_binary: false,
    format_specification: StochasticProcessTree::FILE_FORMAT_SPECIFICATION_LATEX,
    validator: Some(StochasticProcessTree::validate),
    trait_importers: &[
        EbiTraitImporter::Activities(
            StochasticProcessTree::import_as_activities,
            StochasticProcessTree::IMPORTER_PARAMETERS,
        ),
        EbiTraitImporter::QueriableStochasticLanguage(
            StochasticProcessTree::import_as_queriable_stochastic_language,
            StochasticProcessTree::IMPORTER_PARAMETERS,
        ),
        EbiTraitImporter::StochasticDeterministicSemantics(
            StochasticProcessTree::import_as_stochastic_deterministic_semantics,
            StochasticProcessTree::IMPORTER_PARAMETERS,
        ),
        EbiTraitImporter::Semantics(
            StochasticProcessTree::import_as_semantics,
            StochasticProcessTree::IMPORTER_PARAMETERS,
        ),
        EbiTraitImporter::StochasticSemantics(
            StochasticProcessTree::import_as_stochastic_semantics,
            StochasticProcessTree::IMPORTER_PARAMETERS,
        ),
        EbiTraitImporter::Graphable(
            StochasticProcessTree::import_as_graphable,
            StochasticProcessTree::IMPORTER_PARAMETERS,
        ),
    ],
    object_importers: &[
        EbiObjectImporter::StochasticProcessTree(
            StochasticProcessTree::import_as_object,
            StochasticProcessTree::IMPORTER_PARAMETERS,
        ),
        EbiObjectImporter::ProcessTree(
            StochasticProcessTree::import_as_process_tree_object,
            StochasticProcessTree::IMPORTER_PARAMETERS,
        ),
        EbiObjectImporter::LabelledPetriNet(
            StochasticProcessTree::import_as_labelled_petri_net_object,
            StochasticProcessTree::IMPORTER_PARAMETERS,
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
