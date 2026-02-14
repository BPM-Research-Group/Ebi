use ebi_objects::{Exportable, Importable, ebi_objects::event_log_ocel::EventLogOcel};

use crate::ebi_framework::{
    ebi_file_handler::EbiFileHandler,
    ebi_input::{EbiObjectImporter, EbiTraitImporter},
    ebi_output::EbiObjectExporter,
    object_importers::{
        ImportAsDeterministicFiniteAutomatonObject, ImportAsEventLogObject,
        ImportAsFiniteLanguageObject, ImportAsFiniteStochasticLanguageObject,
        ImportAsStochasticDeterministicFiniteAutomatonObject,
        ImportAsStochasticNondeterministicFiniteAutomatonObject,
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

pub const EBI_EVENT_LOG_OCEL: EbiFileHandler = EbiFileHandler {
    name: "object-centric event log",
    article: "an",
    file_extension: "ocel",
    is_binary: false,
    format_specification: EventLogOcel::FILE_FORMAT_SPECIFICATION_LATEX,
    validator: Some(EventLogOcel::validate),
    trait_importers: &[
        EbiTraitImporter::Activities(
            EventLogOcel::import_as_activities_trait,
            EventLogOcel::IMPORTER_PARAMETERS,
        ),
        EbiTraitImporter::IterableLanguage(
            EventLogOcel::import_as_iterable_language_trait,
            EventLogOcel::IMPORTER_PARAMETERS,
        ),
        EbiTraitImporter::FiniteLanguage(
            EventLogOcel::import_as_finite_language_trait,
            EventLogOcel::IMPORTER_PARAMETERS,
        ),
        EbiTraitImporter::FiniteStochasticLanguage(
            EventLogOcel::import_as_finite_stochastic_language_trait,
            EventLogOcel::IMPORTER_PARAMETERS,
        ),
        EbiTraitImporter::QueriableStochasticLanguage(
            EventLogOcel::import_as_queriable_stochastic_language_trait,
            EventLogOcel::IMPORTER_PARAMETERS,
        ),
        EbiTraitImporter::IterableStochasticLanguage(
            EventLogOcel::import_as_iterable_stochastic_language_trait,
            EventLogOcel::IMPORTER_PARAMETERS,
        ),
        EbiTraitImporter::EventLog(
            EventLogOcel::import_as_event_log_trait,
            EventLogOcel::IMPORTER_PARAMETERS,
        ),
        EbiTraitImporter::EventLogTraceAttributes(
            EventLogOcel::import_as_event_log_trace_attributes_trait,
            EventLogOcel::IMPORTER_PARAMETERS,
        ),
        EbiTraitImporter::StochasticSemantics(
            EventLogOcel::import_as_stochastic_semantics_trait,
            EventLogOcel::IMPORTER_PARAMETERS,
        ),
        EbiTraitImporter::StochasticDeterministicSemantics(
            EventLogOcel::import_as_stochastic_deterministic_semantics_trait,
            EventLogOcel::IMPORTER_PARAMETERS,
        ),
        EbiTraitImporter::Semantics(
            EventLogOcel::import_as_semantics_trait,
            EventLogOcel::IMPORTER_PARAMETERS,
        ),
    ],
    object_importers: &[
        EbiObjectImporter::EventLog(
            EventLogOcel::import_as_event_log_object,
            EventLogOcel::IMPORTER_PARAMETERS,
        ),
        EbiObjectImporter::EventLogOcel(
            EventLogOcel::import_as_object,
            EventLogOcel::IMPORTER_PARAMETERS,
        ),
        EbiObjectImporter::FiniteLanguage(
            EventLogOcel::import_as_finite_language_object,
            EventLogOcel::IMPORTER_PARAMETERS,
        ),
        EbiObjectImporter::FiniteStochasticLanguage(
            EventLogOcel::import_as_finite_stochastic_language_object,
            EventLogOcel::IMPORTER_PARAMETERS,
        ),
        EbiObjectImporter::DeterministicFiniteAutomaton(
            EventLogOcel::import_as_deterministic_finite_automaton_object,
            EventLogOcel::IMPORTER_PARAMETERS,
        ),
        EbiObjectImporter::StochasticDeterministicFiniteAutomaton(
            EventLogOcel::import_as_stochastic_deterministic_finite_automaton_object,
            EventLogOcel::IMPORTER_PARAMETERS,
        ),
        EbiObjectImporter::StochasticNondeterministicFiniteAutomaton(
            EventLogOcel::import_as_stochastic_nondeterministic_finite_automaton_object,
            EventLogOcel::IMPORTER_PARAMETERS,
        ),
    ],
    object_exporters: &[EbiObjectExporter::EventLogOcel(
        EventLogOcel::export_from_object,
    )],
    object_exporters_fallible: &[],
    java_object_handlers: &[],
};
