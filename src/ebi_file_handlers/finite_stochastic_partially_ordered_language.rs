use crate::ebi_framework::{
    ebi_file_handler::EbiFileHandler,
    ebi_input::{EbiObjectImporter, EbiTraitImporter},
    ebi_output::EbiObjectExporter,
    trait_importers::{ImportAsActivitiesTrait, ImportAsGraphableTrait, ImportAsSemanticsTrait},
    validate::Validate,
};
use ebi_objects::{Exportable, FiniteStochasticPartiallyOrderedLanguage, Importable};

pub const EBI_FINITE_STOCHASTIC_PARTIALLY_ORDERED_LANGUAGE: EbiFileHandler = EbiFileHandler {
    name: "finite stochastic partially ordered language",
    article: "a",
    file_extension: "spolang",
    is_binary: false,
    format_specification: FiniteStochasticPartiallyOrderedLanguage::FILE_FORMAT_SPECIFICATION_LATEX,
    validator: Some(FiniteStochasticPartiallyOrderedLanguage::validate),
    trait_importers: &[
        EbiTraitImporter::Activities(
            FiniteStochasticPartiallyOrderedLanguage::import_as_activities_trait,
            FiniteStochasticPartiallyOrderedLanguage::IMPORTER_PARAMETERS,
        ),
        EbiTraitImporter::Graphable(
            FiniteStochasticPartiallyOrderedLanguage::import_as_graphable_trait,
            FiniteStochasticPartiallyOrderedLanguage::IMPORTER_PARAMETERS,
        ),
        EbiTraitImporter::Semantics(
            FiniteStochasticPartiallyOrderedLanguage::import_as_semantics_trait,
            FiniteStochasticPartiallyOrderedLanguage::IMPORTER_PARAMETERS,
        ),
    ],
    object_importers: &[EbiObjectImporter::FiniteStochasticPartiallyOrderedLanguage(
        FiniteStochasticPartiallyOrderedLanguage::import_as_object,
        FiniteStochasticPartiallyOrderedLanguage::IMPORTER_PARAMETERS,
    )],
    object_importers_fallible: &[],
    object_exporters: &[EbiObjectExporter::FiniteStochasticPartiallyOrderedLanguage(
        FiniteStochasticPartiallyOrderedLanguage::export_from_object,
    )],
    object_exporters_fallible: &[],
    java_object_handlers: &[],
};
