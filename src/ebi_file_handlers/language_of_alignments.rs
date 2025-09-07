use ebi_objects::{
    Exportable, Importable, LanguageOfAlignments,
    ebi_objects::language_of_alignments::FORMAT_SPECIFICATION,
};

use crate::{
    ebi_framework::{
        ebi_file_handler::EbiFileHandler,
        ebi_input::{EbiObjectImporter, EbiTraitImporter},
        ebi_output::EbiObjectExporter,
        validate::Validate,
    },
    ebi_traits::ebi_trait_activities::ToActivities,
};

pub const EBI_LANGUAGE_OF_ALIGNMENTS: EbiFileHandler = EbiFileHandler {
    name: "language of alignments",
    article: "a",
    file_extension: "ali",
    is_binary: false,
    format_specification: &FORMAT_SPECIFICATION,
    validator: Some(LanguageOfAlignments::validate),
    trait_importers: &[EbiTraitImporter::Activities(
        LanguageOfAlignments::import_as_activities,
    )],
    object_importers: &[EbiObjectImporter::LanguageOfAlignments(
        LanguageOfAlignments::import_as_object,
    )],
    object_exporters: &[
        EbiObjectExporter::LanguageOfAlignments(LanguageOfAlignments::export_from_object),
        EbiObjectExporter::StochasticLanguageOfAlignments(LanguageOfAlignments::export_from_object),
    ],
    java_object_handlers: &[],
};
