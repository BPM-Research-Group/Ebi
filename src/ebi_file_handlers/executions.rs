use ebi_objects::{ebi_objects::executions::FORMAT_SPECIFICATION, Executions, Exportable, Importable};

use crate::ebi_framework::{
    ebi_file_handler::EbiFileHandler, ebi_input::EbiObjectImporter, ebi_output::EbiObjectExporter,
    validate::Validate,
};

pub const EBI_EXECUTIONS: EbiFileHandler = EbiFileHandler {
    name: "executions",
    article: "",
    file_extension: "exs",
    is_binary: false,
    format_specification: &FORMAT_SPECIFICATION,
    validator: Some(Executions::validate),
    trait_importers: &[],
    object_importers: &[EbiObjectImporter::Executions(Executions::import_as_object)],
    object_exporters: &[EbiObjectExporter::Executions(
        Executions::export_from_object,
    )],
    java_object_handlers: &[],
};
