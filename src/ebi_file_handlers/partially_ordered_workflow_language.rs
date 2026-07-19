use crate::ebi_framework::{
    ebi_file_handler::EbiFileHandler,
    ebi_input::{EbiInput, EbiObjectImporter, EbiTraitImporter},
    ebi_output::EbiObjectExporter,
    ebi_trait::FromEbiTraitObject,
    object_importers::{
        ImportAsBusinessProcessModelAndNotationObject, ImportAsLabelledPetriNetObject,
    },
    trait_importers::{ImportAsActivitiesTrait, ImportAsGraphableTrait, ImportAsSemanticsTrait},
    validate::Validate,
};
use ebi_objects::{
    EbiObject, Exportable, Importable,
    anyhow::{Result, anyhow},
    ebi_objects::partially_ordered_workflow_language::PartiallyOrderedWorkflowLanguage,
};

pub const EBI_PARTIALLY_ORDERED_WORKFLOW_LANGUAGE: EbiFileHandler = EbiFileHandler {
    name: "partially ordered workflow language",
    article: "a",
    file_extension: "powl",
    is_binary: false,
    format_specification: PartiallyOrderedWorkflowLanguage::FILE_FORMAT_SPECIFICATION_LATEX,
    validator: Some(PartiallyOrderedWorkflowLanguage::validate),
    trait_importers: &[
        EbiTraitImporter::Activities(
            PartiallyOrderedWorkflowLanguage::import_as_activities_trait,
            PartiallyOrderedWorkflowLanguage::IMPORTER_PARAMETERS,
        ),
        EbiTraitImporter::Semantics(
            PartiallyOrderedWorkflowLanguage::import_as_semantics_trait,
            PartiallyOrderedWorkflowLanguage::IMPORTER_PARAMETERS,
        ),
        EbiTraitImporter::Graphable(
            PartiallyOrderedWorkflowLanguage::import_as_graphable_trait,
            PartiallyOrderedWorkflowLanguage::IMPORTER_PARAMETERS,
        ),
    ],
    object_importers: &[
        EbiObjectImporter::PartiallyOrderedWorkflowLanguage(
            PartiallyOrderedWorkflowLanguage::import_as_object,
            PartiallyOrderedWorkflowLanguage::IMPORTER_PARAMETERS,
        ),
        EbiObjectImporter::LabelledPetriNet(
            PartiallyOrderedWorkflowLanguage::import_as_labelled_petri_net_object,
            PartiallyOrderedWorkflowLanguage::IMPORTER_PARAMETERS,
        ),
        EbiObjectImporter::BusinessProcessModelAndNotation(
            PartiallyOrderedWorkflowLanguage::import_as_business_process_model_and_notation_object,
            PartiallyOrderedWorkflowLanguage::IMPORTER_PARAMETERS,
        ),
    ],
    object_importers_fallible: &[],
    object_exporters: &[
        EbiObjectExporter::PartiallyOrderedWorkflowLanguage(
            PartiallyOrderedWorkflowLanguage::export_from_object,
        ),
        EbiObjectExporter::ProcessTree(PartiallyOrderedWorkflowLanguage::export_from_object),
        EbiObjectExporter::StochasticProcessTree(
            PartiallyOrderedWorkflowLanguage::export_from_object,
        ),
    ],
    object_exporters_fallible: &[],
    java_object_handlers: &[],
};

impl FromEbiTraitObject for PartiallyOrderedWorkflowLanguage {
    fn from_trait_object(object: EbiInput) -> Result<Box<Self>> {
        match object {
            EbiInput::Object(EbiObject::PartiallyOrderedWorkflowLanguage(e), _) => Ok(Box::new(e)),
            _ => Err(anyhow!(
                "Cannot read {} {} as a partially ordered workflow language.",
                object.get_type().get_article(),
                object.get_type()
            )),
        }
    }
}
