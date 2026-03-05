use crate::ebi_framework::{
    ebi_file_handler::EbiFileHandler,
    ebi_input::{EbiInput, EbiObjectImporter, EbiTraitImporter},
    ebi_output::EbiObjectExporter,
    ebi_trait::FromEbiTraitObject,
    trait_importers::{ImportAsActivitiesTrait, ImportAsGraphableTrait, ImportAsSemanticsTrait},
    validate::Validate,
};
use anyhow::{Result, anyhow};
use ebi_objects::{BusinessProcessModelAndNotation, EbiObject, Exportable, Importable};

pub const EBI_BUSINESS_PROCESS_MODEL_AND_NOTATION: EbiFileHandler = EbiFileHandler {
    name: "business process model and notation",
    article: "a",
    file_extension: "bpmn",
    is_binary: false,
    format_specification: BusinessProcessModelAndNotation::FILE_FORMAT_SPECIFICATION_LATEX,
    validator: Some(BusinessProcessModelAndNotation::validate),
    trait_importers: &[
        EbiTraitImporter::Activities(
            BusinessProcessModelAndNotation::import_as_activities_trait,
            BusinessProcessModelAndNotation::IMPORTER_PARAMETERS,
        ),
        EbiTraitImporter::Graphable(
            BusinessProcessModelAndNotation::import_as_graphable_trait,
            BusinessProcessModelAndNotation::IMPORTER_PARAMETERS,
        ),
        EbiTraitImporter::Semantics(
            BusinessProcessModelAndNotation::import_as_semantics_trait,
            BusinessProcessModelAndNotation::IMPORTER_PARAMETERS,
        ),
    ],
    object_importers: &[EbiObjectImporter::BusinessProcessModelAndNotation(
        BusinessProcessModelAndNotation::import_as_object,
        BusinessProcessModelAndNotation::IMPORTER_PARAMETERS,
    )],
    object_importers_fallible: &[],
    object_exporters: &[EbiObjectExporter::BusinessProcessModelAndNotation(
        BusinessProcessModelAndNotation::export_from_object,
    )],
    object_exporters_fallible: &[
        EbiObjectExporter::DeterministicFiniteAutomaton(
            BusinessProcessModelAndNotation::export_from_object,
        ),
        EbiObjectExporter::DirectlyFollowsGraph(
            BusinessProcessModelAndNotation::export_from_object,
        ),
        EbiObjectExporter::DirectlyFollowsModel(
            BusinessProcessModelAndNotation::export_from_object,
        ),
        EbiObjectExporter::StochasticDirectlyFollowsModel(
            BusinessProcessModelAndNotation::export_from_object,
        ),
        EbiObjectExporter::LabelledPetriNet(BusinessProcessModelAndNotation::export_from_object),
        EbiObjectExporter::ProcessTree(BusinessProcessModelAndNotation::export_from_object),
        EbiObjectExporter::StochasticProcessTree(
            BusinessProcessModelAndNotation::export_from_object,
        ),
        EbiObjectExporter::StochasticLabelledPetriNet(
            BusinessProcessModelAndNotation::export_from_object,
        ),
        EbiObjectExporter::StochasticDeterministicFiniteAutomaton(
            BusinessProcessModelAndNotation::export_from_object,
        ),
        EbiObjectExporter::StochasticNondeterministicFiniteAutomaton(
            BusinessProcessModelAndNotation::export_from_object,
        ),
    ],
    java_object_handlers: &[],
};

impl FromEbiTraitObject for BusinessProcessModelAndNotation {
    fn from_trait_object(object: EbiInput) -> Result<Box<Self>> {
        match object {
            EbiInput::Object(EbiObject::BusinessProcessModelAndNotation(e), _) => Ok(Box::new(e)),
            _ => Err(anyhow!(
                "cannot read {} {} as a Business Process Model and Notation",
                object.get_type().get_article(),
                object.get_type()
            )),
        }
    }
}
