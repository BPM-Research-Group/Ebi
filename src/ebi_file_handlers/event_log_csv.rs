use crate::ebi_framework::{
    ebi_file_handler::EbiFileHandler,
    ebi_input::{EbiObjectImporter, EbiTraitImporter},
    ebi_output::EbiObjectExporter,
    object_importers::{
        ImportAsDeterministicFiniteAutomatonObject, ImportAsEventLogObject, ImportAsFiniteLanguageObject, ImportAsFiniteStochasticLanguageObject, ImportAsStochasticDeterministicFiniteAutomatonObject, ImportAsStochasticNondeterministicFiniteAutomatonObject, TryToEventLogXesObject
    },
    trait_importers::{
        ImportAsActivitiesTrait, ImportAsEventLogTrait, ImportAsFiniteLanguageTrait,
        ImportAsFiniteStochasticLanguageTrait, ImportAsIterableLanguageTrait,
        ImportAsIterableStochasticLanguageTrait, ImportAsQueriableStochasticLanguageTrait,
        ImportAsSemanticsTrait, ImportAsStochasticDeterministicSemanticsTrait,
        ImportAsStochasticSemanticsTrait,
    },
    validate::Validate,
};
use ebi_objects::{Exportable, Importable, ebi_objects::event_log_csv::EventLogCsv};

pub const EBI_EVENT_LOG_CSV: EbiFileHandler = EbiFileHandler {
    name: "comma-separated values",
    article: "a",
    file_extension: "csv",
    is_binary: false,
    format_specification: EventLogCsv::FILE_FORMAT_SPECIFICATION_LATEX,
    validator: Some(EventLogCsv::validate),
    trait_importers: &[
        EbiTraitImporter::Activities(
            EventLogCsv::import_as_activities_trait,
            EventLogCsv::IMPORTER_PARAMETERS,
        ),
        EbiTraitImporter::IterableLanguage(
            EventLogCsv::import_as_iterable_language_trait,
            EventLogCsv::IMPORTER_PARAMETERS,
        ),
        EbiTraitImporter::FiniteLanguage(
            EventLogCsv::import_as_finite_language_trait,
            EventLogCsv::IMPORTER_PARAMETERS,
        ),
        EbiTraitImporter::FiniteStochasticLanguage(
            EventLogCsv::import_as_finite_stochastic_language_trait,
            EventLogCsv::IMPORTER_PARAMETERS,
        ),
        EbiTraitImporter::QueriableStochasticLanguage(
            EventLogCsv::import_as_queriable_stochastic_language_trait,
            EventLogCsv::IMPORTER_PARAMETERS,
        ),
        EbiTraitImporter::IterableStochasticLanguage(
            EventLogCsv::import_as_iterable_stochastic_language_trait,
            EventLogCsv::IMPORTER_PARAMETERS,
        ),
        EbiTraitImporter::EventLog(
            EventLogCsv::import_as_event_log_trait,
            EventLogCsv::IMPORTER_PARAMETERS,
        ),
        EbiTraitImporter::StochasticSemantics(
            EventLogCsv::import_as_stochastic_semantics_trait,
            EventLogCsv::IMPORTER_PARAMETERS,
        ),
        EbiTraitImporter::StochasticDeterministicSemantics(
            EventLogCsv::import_as_stochastic_deterministic_semantics_trait,
            EventLogCsv::IMPORTER_PARAMETERS,
        ),
        EbiTraitImporter::Semantics(
            EventLogCsv::import_as_semantics_trait,
            EventLogCsv::IMPORTER_PARAMETERS,
        ),
    ],
    object_importers: &[
        EbiObjectImporter::EventLog(
            EventLogCsv::import_as_event_log_object,
            EventLogCsv::IMPORTER_PARAMETERS,
        ),
        EbiObjectImporter::EventLogXes(
            EventLogCsv::try_import_as_event_log_xes_object,
            EventLogCsv::IMPORTER_PARAMETERS,
        ),
        EbiObjectImporter::EventLogCsv(
            EventLogCsv::import_as_object,
            EventLogCsv::IMPORTER_PARAMETERS,
        ),
        EbiObjectImporter::FiniteLanguage(
            EventLogCsv::import_as_finite_language_object,
            EventLogCsv::IMPORTER_PARAMETERS,
        ),
        EbiObjectImporter::FiniteStochasticLanguage(
            EventLogCsv::import_as_finite_stochastic_language_object,
            EventLogCsv::IMPORTER_PARAMETERS,
        ),
        EbiObjectImporter::DeterministicFiniteAutomaton(
            EventLogCsv::import_as_deterministic_finite_automaton_object,
            EventLogCsv::IMPORTER_PARAMETERS,
        ),
        EbiObjectImporter::StochasticDeterministicFiniteAutomaton(
            EventLogCsv::import_as_stochastic_deterministic_finite_automaton_object,
            EventLogCsv::IMPORTER_PARAMETERS,
        ),
        EbiObjectImporter::StochasticNondeterministicFiniteAutomaton(
            EventLogCsv::import_as_stochastic_nondeterministic_finite_automaton_object,
            EventLogCsv::IMPORTER_PARAMETERS,
        ),
    ],
    object_exporters: &[EbiObjectExporter::EventLogCsv(
        EventLogCsv::export_from_object,
    )],
    object_exporters_fallible: &[
        EbiObjectExporter::EventLog(EventLogCsv::export_from_object),
        EbiObjectExporter::EventLogXes(EventLogCsv::export_from_object),
        EbiObjectExporter::EventLogTraceAttributes(EventLogCsv::export_from_object),
    ],
    java_object_handlers: &[],
};
