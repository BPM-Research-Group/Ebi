use crate::ebi_framework::{
    ebi_file_handler::EbiFileHandler,
    ebi_input::{EbiObjectImporter, EbiTraitImporter},
    ebi_output::EbiObjectExporter,
    object_importers::{
        ImportAsDeterministicFiniteAutomatonObject, ImportAsEventLogObject,
        ImportAsEventLogXesObject, ImportAsFiniteLanguageObject,
        ImportAsFiniteStochasticLanguageObject,
        ImportAsStochasticDeterministicFiniteAutomatonObject, ImportAsStochasticNondeterministicFiniteAutomatonObject,
    },
    trait_importers::{
        ImportAsActivitiesTrait, ImportAsEventLogTrait, ImportAsFiniteLanguageTrait,
        ImportAsFiniteStochasticLanguageTrait, ImportAsIterableLanguageTrait,
        ImportAsIterableStochasticLanguageTrait, ImportAsQueriableStochasticLanguageTrait,
        ImportAsSemanticsTrait, ImportAsStochasticDeterministicSemanticsTrait,
        ImportAsStochasticSemanticsTrait,
    },
};
use ebi_objects::{EventLogPython, Exportable, Importable};

pub const EBI_EVENT_LOG_PYTHON: EbiFileHandler = EbiFileHandler {
    name: "Python event log",
    article: "a",
    file_extension: "pel",
    is_binary: true,
    format_specification: EventLogPython::FILE_FORMAT_SPECIFICATION_LATEX,
    validator: None,
    trait_importers: &[
        EbiTraitImporter::Activities(
            EventLogPython::import_as_activities_trait,
            EventLogPython::IMPORTER_PARAMETERS,
        ),
        EbiTraitImporter::IterableLanguage(
            EventLogPython::import_as_iterable_language_trait,
            EventLogPython::IMPORTER_PARAMETERS,
        ),
        EbiTraitImporter::FiniteLanguage(
            EventLogPython::import_as_finite_language_trait,
            EventLogPython::IMPORTER_PARAMETERS,
        ),
        EbiTraitImporter::FiniteStochasticLanguage(
            EventLogPython::import_as_finite_stochastic_language_trait,
            EventLogPython::IMPORTER_PARAMETERS,
        ),
        EbiTraitImporter::QueriableStochasticLanguage(
            EventLogPython::import_as_queriable_stochastic_language_trait,
            EventLogPython::IMPORTER_PARAMETERS,
        ),
        EbiTraitImporter::IterableStochasticLanguage(
            EventLogPython::import_as_iterable_stochastic_language_trait,
            EventLogPython::IMPORTER_PARAMETERS,
        ),
        EbiTraitImporter::EventLog(
            EventLogPython::import_as_event_log_trait,
            EventLogPython::IMPORTER_PARAMETERS,
        ),
        EbiTraitImporter::StochasticSemantics(
            EventLogPython::import_as_stochastic_semantics_trait,
            EventLogPython::IMPORTER_PARAMETERS,
        ),
        EbiTraitImporter::StochasticDeterministicSemantics(
            EventLogPython::import_as_stochastic_deterministic_semantics_trait,
            EventLogPython::IMPORTER_PARAMETERS,
        ),
        EbiTraitImporter::Semantics(
            EventLogPython::import_as_semantics_trait,
            EventLogPython::IMPORTER_PARAMETERS,
        ),
    ],
    object_importers: &[
        EbiObjectImporter::EventLog(
            EventLogPython::import_as_event_log_object,
            EventLogPython::IMPORTER_PARAMETERS,
        ),
        EbiObjectImporter::EventLogXes(
            EventLogPython::import_as_event_log_xes_object,
            EventLogPython::IMPORTER_PARAMETERS,
        ),
        EbiObjectImporter::FiniteLanguage(
            EventLogPython::import_as_finite_language_object,
            EventLogPython::IMPORTER_PARAMETERS,
        ),
        EbiObjectImporter::FiniteStochasticLanguage(
            EventLogPython::import_as_finite_stochastic_language_object,
            EventLogPython::IMPORTER_PARAMETERS,
        ),
        EbiObjectImporter::DeterministicFiniteAutomaton(
            EventLogPython::import_as_deterministic_finite_automaton_object,
            EventLogPython::IMPORTER_PARAMETERS,
        ),
        EbiObjectImporter::StochasticDeterministicFiniteAutomaton(
            EventLogPython::import_as_stochastic_deterministic_finite_automaton_object,
            EventLogPython::IMPORTER_PARAMETERS,
        ),
        EbiObjectImporter::StochasticNondeterministicFiniteAutomaton(
            EventLogPython::import_as_stochastic_nondeterministic_finite_automaton_object,
            EventLogPython::IMPORTER_PARAMETERS,
        ),
        EbiObjectImporter::StochasticNondeterministicFiniteAutomaton(
            EventLogPython::import_as_stochastic_nondeterministic_finite_automaton_object,
            EventLogPython::IMPORTER_PARAMETERS,
        ),
    ],
    object_exporters: &[
        EbiObjectExporter::EventLog(EventLogPython::export_from_object),
        EbiObjectExporter::EventLogXes(EventLogPython::export_from_object),
        EbiObjectExporter::EventLogCsv(EventLogPython::export_from_object),
    ],
    object_exporters_fallible: &[],
    java_object_handlers: &[],
};
