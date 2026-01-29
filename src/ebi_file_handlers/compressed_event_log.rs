use crate::ebi_framework::{
    ebi_file_handler::EbiFileHandler,
    ebi_input::{EbiObjectImporter, EbiTraitImporter},
    ebi_output::EbiObjectExporter,
    object_importers::{
        ImportAsFiniteLanguageObject, ImportAsFiniteStochasticLanguageObject,
        ImportAsStochasticDeterministicFiniteAutomatonObject, ImportAsStochasticNondeterministicFiniteAutomatonObject,
    },
    trait_importers::{
        ImportAsActivitiesTrait, ImportAsEventLogTraceAttributesTrait, ImportAsEventLogTrait,
        ImportAsFiniteLanguageTrait, ImportAsFiniteStochasticLanguageTrait,
        ImportAsIterableLanguageTrait, ImportAsIterableStochasticLanguageTrait,
        ImportAsQueriableStochasticLanguageTrait, ImportAsSemanticsTrait,
        ImportAsStochasticDeterministicSemanticsTrait, ImportAsStochasticSemanticsTrait,
    },
    validate::Validate,
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
            CompressedEventLog::import_as_activities_trait,
            CompressedEventLog::IMPORTER_PARAMETERS,
        ),
        EbiTraitImporter::IterableLanguage(
            CompressedEventLog::import_as_iterable_language_trait,
            CompressedEventLog::IMPORTER_PARAMETERS,
        ),
        EbiTraitImporter::FiniteLanguage(
            CompressedEventLog::import_as_finite_language_trait,
            CompressedEventLog::IMPORTER_PARAMETERS,
        ),
        EbiTraitImporter::FiniteStochasticLanguage(
            CompressedEventLog::import_as_finite_stochastic_language_trait,
            CompressedEventLog::IMPORTER_PARAMETERS,
        ),
        EbiTraitImporter::QueriableStochasticLanguage(
            CompressedEventLog::import_as_queriable_stochastic_language_trait,
            CompressedEventLog::IMPORTER_PARAMETERS,
        ),
        EbiTraitImporter::IterableStochasticLanguage(
            CompressedEventLog::import_as_iterable_stochastic_language_trait,
            CompressedEventLog::IMPORTER_PARAMETERS,
        ),
        EbiTraitImporter::EventLog(
            CompressedEventLog::import_as_event_log_trait,
            CompressedEventLog::IMPORTER_PARAMETERS,
        ),
        EbiTraitImporter::EventLogTraceAttributes(
            CompressedEventLogTraceAttributes::import_as_event_log_trace_attributes_trait,
            CompressedEventLogTraceAttributes::IMPORTER_PARAMETERS,
        ),
        EbiTraitImporter::StochasticDeterministicSemantics(
            CompressedEventLog::import_as_stochastic_deterministic_semantics_trait,
            CompressedEventLog::IMPORTER_PARAMETERS,
        ),
        EbiTraitImporter::StochasticSemantics(
            CompressedEventLog::import_as_stochastic_semantics_trait,
            CompressedEventLog::IMPORTER_PARAMETERS,
        ),
        EbiTraitImporter::Semantics(
            CompressedEventLog::import_as_semantics_trait,
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
        EbiObjectImporter::StochasticNondeterministicFiniteAutomaton(
            CompressedEventLogXes::import_as_stochastic_nondeterministic_finite_automaton_object,
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
