use crate::{
    ebi_framework::{
        ebi_file_handler::EbiFileHandler,
        ebi_input::{EbiObjectImporter, EbiTraitImporter},
        ebi_output::EbiObjectExporter,
        object_importers::{
            ToFiniteLanguageObject, ToFiniteStochasticLanguageObject,
            ToStochasticDeterministicFiniteAutomatonObject,
        },
        validate::Validate,
    },
    ebi_traits::{
        ebi_trait_activities::ToActivities, ebi_trait_event_log::ToEventLog,
        ebi_trait_event_log_trace_attributes::ToEventLogTraceAttributes,
        ebi_trait_finite_language::ToFiniteLanguage,
        ebi_trait_finite_stochastic_language::ToFiniteStochasticLanguage,
        ebi_trait_iterable_language::ToIterableLanguage,
        ebi_trait_iterable_stochastic_language::ToIterableStochasticLanguage,
        ebi_trait_queriable_stochastic_language::ToQueriableStochasticLanguage,
        ebi_trait_semantics::ToSemantics,
        ebi_trait_stochastic_deterministic_semantics::ToStochasticDeterministicSemantics,
        ebi_trait_stochastic_semantics::ToStochasticSemantics,
    },
};
use ebi_objects::{
    CompressedEventLogXes, Exportable, Importable,
    ebi_objects::{
        compressed_event_log::CompressedEventLog,
        compressed_event_log_trace_attributes::CompressedEventLogTraceAttributes,
    },
};

pub const EBI_COMPRESSED_EVENT_LOG: EbiFileHandler = EbiFileHandler {
    name: "compressed event log",
    article: "a",
    file_extension: "xes.gz",
    format_specification: &CompressedEventLogXes::FILE_FORMAT_SPECIFICATION_LATEX,
    is_binary: true,
    validator: Some(CompressedEventLog::validate),
    trait_importers: &[
        EbiTraitImporter::Activities(
            CompressedEventLog::import_as_activities,
            CompressedEventLog::IMPORTER_PARAMETERS,
        ),
        EbiTraitImporter::IterableLanguage(
            CompressedEventLog::import_as_iterable_language,
            CompressedEventLog::IMPORTER_PARAMETERS,
        ),
        EbiTraitImporter::FiniteLanguage(
            CompressedEventLog::import_as_finite_language,
            CompressedEventLog::IMPORTER_PARAMETERS,
        ),
        EbiTraitImporter::FiniteStochasticLanguage(
            CompressedEventLog::import_as_finite_stochastic_language,
            CompressedEventLog::IMPORTER_PARAMETERS,
        ),
        EbiTraitImporter::QueriableStochasticLanguage(
            CompressedEventLog::import_as_queriable_stochastic_language,
            CompressedEventLog::IMPORTER_PARAMETERS,
        ),
        EbiTraitImporter::IterableStochasticLanguage(
            CompressedEventLog::import_as_iterable_stochastic_language,
            CompressedEventLog::IMPORTER_PARAMETERS,
        ),
        EbiTraitImporter::EventLog(
            CompressedEventLog::import_as_event_log,
            CompressedEventLog::IMPORTER_PARAMETERS,
        ),
        EbiTraitImporter::EventLogTraceAttributes(
            CompressedEventLogTraceAttributes::import_as_event_log_trace_attributes,
            CompressedEventLogTraceAttributes::IMPORTER_PARAMETERS,
        ),
        EbiTraitImporter::StochasticDeterministicSemantics(
            CompressedEventLog::import_as_stochastic_deterministic_semantics,
            CompressedEventLog::IMPORTER_PARAMETERS,
        ),
        EbiTraitImporter::StochasticSemantics(
            CompressedEventLog::import_as_stochastic_semantics,
            CompressedEventLog::IMPORTER_PARAMETERS,
        ),
        EbiTraitImporter::Semantics(
            CompressedEventLog::import_as_semantics,
            CompressedEventLog::IMPORTER_PARAMETERS,
        ),
    ],
    object_importers: &[
        EbiObjectImporter::EventLog(
            CompressedEventLog::import_as_object,
            CompressedEventLog::IMPORTER_PARAMETERS,
        ),
        EbiObjectImporter::EventLogTraceAttributes(
            CompressedEventLogTraceAttributes::import_as_object,
            CompressedEventLogTraceAttributes::IMPORTER_PARAMETERS,
        ),
        EbiObjectImporter::EventLogXes(
            CompressedEventLogXes::import_as_object,
            CompressedEventLogXes::IMPORTER_PARAMETERS,
        ),
        EbiObjectImporter::FiniteLanguage(
            CompressedEventLogXes::import_as_finite_language_object,
            CompressedEventLogXes::IMPORTER_PARAMETERS,
        ),
        EbiObjectImporter::FiniteStochasticLanguage(
            CompressedEventLogXes::import_as_finite_stochastic_language_object,
            CompressedEventLogXes::IMPORTER_PARAMETERS,
        ),
        EbiObjectImporter::StochasticDeterministicFiniteAutomaton(
            CompressedEventLogXes::import_as_stochastic_deterministic_finite_automaton_object,
            CompressedEventLogXes::IMPORTER_PARAMETERS,
        ),
    ],
    object_exporters: &[
        EbiObjectExporter::EventLog(CompressedEventLog::export_from_object),
        EbiObjectExporter::EventLogTraceAttributes(CompressedEventLog::export_from_object),
        EbiObjectExporter::EventLogXes(CompressedEventLog::export_from_object),
    ],
    object_exporters_fallible: &[EbiObjectExporter::EventLogCsv(
        CompressedEventLog::export_from_object,
    )],
    java_object_handlers: &[],
};
