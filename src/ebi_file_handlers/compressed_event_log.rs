use ebi_objects::{
    Exportable, Importable,
    ebi_objects::compressed_event_log::{CompressedEventLog, FORMAT_SPECIFICATION},
};

use crate::{
    ebi_framework::{
        ebi_file_handler::EbiFileHandler,
        ebi_input::{EbiObjectImporter, EbiTraitImporter},
        ebi_output::EbiObjectExporter,
        validate::Validate,
    },
    ebi_traits::{
        ebi_trait_activities::ToActivities, ebi_trait_event_log::ToEventLog, ebi_trait_finite_language::ToFiniteLanguage, ebi_trait_finite_stochastic_language::ToFiniteStochasticLanguage, ebi_trait_iterable_language::ToIterableLanguage, ebi_trait_iterable_stochastic_language::ToIterableStochasticLanguage, ebi_trait_queriable_stochastic_language::ToQueriableStochasticLanguage, ebi_trait_semantics::ToSemantics, ebi_trait_stochastic_deterministic_semantics::ToStochasticDeterministicSemantics, ebi_trait_stochastic_semantics::ToStochasticSemantics
    },
};

pub const EBI_COMPRESSED_EVENT_LOG: EbiFileHandler = EbiFileHandler {
    name: "compressed event log",
    article: "a",
    file_extension: "xes.gz",
    format_specification: &FORMAT_SPECIFICATION,
    is_binary: true,
    validator: Some(CompressedEventLog::validate),
    trait_importers: &[
        EbiTraitImporter::Activities(CompressedEventLog::import_as_activities),
        EbiTraitImporter::IterableLanguage(CompressedEventLog::import_as_iterable_language),
        EbiTraitImporter::FiniteLanguage(CompressedEventLog::import_as_finite_language),
        EbiTraitImporter::FiniteStochasticLanguage(
            CompressedEventLog::import_as_finite_stochastic_language,
        ),
        EbiTraitImporter::QueriableStochasticLanguage(
            CompressedEventLog::import_as_queriable_stochastic_language,
        ),
        EbiTraitImporter::IterableStochasticLanguage(
            CompressedEventLog::import_as_iterable_stochastic_language,
        ),
        EbiTraitImporter::EventLog(CompressedEventLog::import_as_event_log),
        EbiTraitImporter::StochasticDeterministicSemantics(
            CompressedEventLog::import_as_stochastic_deterministic_semantics,
        ),
        EbiTraitImporter::StochasticSemantics(CompressedEventLog::import_as_stochastic_semantics),
        EbiTraitImporter::Semantics(CompressedEventLog::import_as_semantics),
    ],
    object_importers: &[EbiObjectImporter::EventLog(
        CompressedEventLog::import_as_object,
    )],
    object_exporters: &[EbiObjectExporter::EventLog(
        CompressedEventLog::export_from_object,
    )],
    java_object_handlers: &[],
};
