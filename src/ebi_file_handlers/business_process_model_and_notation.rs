use ebi_objects::{BusinessProcessModelAndNotation, Importable};

use crate::ebi_framework::{
    ebi_file_handler::EbiFileHandler,
    ebi_input::{EbiObjectImporter, EbiTraitImporter},
    trait_importers::{ImportAsActivitiesTrait, ImportAsGraphableTrait, ImportAsSemanticsTrait},
    validate::Validate,
};

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
    object_exporters: &[],
    object_exporters_fallible: &[],
    java_object_handlers: &[],
};
