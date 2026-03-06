use crate::ebi_framework::{
    ebi_file_handler::EbiFileHandler,
    ebi_input::{EbiInput, EbiObjectImporter, EbiTraitImporter},
    ebi_output::EbiObjectExporter,
    ebi_trait::FromEbiTraitObject,
    object_importers::ImportAsBusinessProcessModelAndNotationObject,
    trait_importers::{ImportAsActivitiesTrait, ImportAsGraphableTrait, ImportAsSemanticsTrait},
    validate::Validate,
};
use ebi_objects::{
    EbiObject, Exportable, Importable, StochasticBusinessProcessModelAndNotation,
    anyhow::{Result, anyhow},
};

pub const EBI_STOCHASTIC_BUSINESS_PROCESS_MODEL_AND_NOTATION: EbiFileHandler = EbiFileHandler {
    name: "stochastic business process model and notation",
    article: "a",
    file_extension: "sbpmn",
    is_binary: false,
    format_specification:
        StochasticBusinessProcessModelAndNotation::FILE_FORMAT_SPECIFICATION_LATEX,
    validator: Some(StochasticBusinessProcessModelAndNotation::validate),
    trait_importers: &[
        EbiTraitImporter::Activities(
            StochasticBusinessProcessModelAndNotation::import_as_activities_trait,
            StochasticBusinessProcessModelAndNotation::IMPORTER_PARAMETERS,
        ),
        EbiTraitImporter::Graphable(
            StochasticBusinessProcessModelAndNotation::import_as_graphable_trait,
            StochasticBusinessProcessModelAndNotation::IMPORTER_PARAMETERS,
        ),
        EbiTraitImporter::Semantics(
            StochasticBusinessProcessModelAndNotation::import_as_semantics_trait,
            StochasticBusinessProcessModelAndNotation::IMPORTER_PARAMETERS,
        ),
    ],
    object_importers: &[
        EbiObjectImporter::StochasticBusinessProcessModelAndNotation(
            StochasticBusinessProcessModelAndNotation::import_as_object,
            StochasticBusinessProcessModelAndNotation::IMPORTER_PARAMETERS,
        ),
        EbiObjectImporter::BusinessProcessModelAndNotation(
            StochasticBusinessProcessModelAndNotation::import_as_business_process_model_and_notation_object,
            StochasticBusinessProcessModelAndNotation::IMPORTER_PARAMETERS,
        ),
    ],
    object_importers_fallible: &[],
    object_exporters: &[
        EbiObjectExporter::StochasticBusinessProcessModelAndNotation(
            StochasticBusinessProcessModelAndNotation::export_from_object,
        ),
    ],
    object_exporters_fallible: &[],
    java_object_handlers: &[],
};

impl FromEbiTraitObject for StochasticBusinessProcessModelAndNotation {
    fn from_trait_object(object: EbiInput) -> Result<Box<Self>> {
        match object {
            EbiInput::Object(EbiObject::StochasticBusinessProcessModelAndNotation(e), _) => {
                Ok(Box::new(e))
            }
            _ => Err(anyhow!(
                "cannot read {} {} as a stochastic Business Process Model and Notation",
                object.get_type().get_article(),
                object.get_type()
            )),
        }
    }
}
