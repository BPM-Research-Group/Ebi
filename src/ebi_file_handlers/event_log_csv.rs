use crate::{
    ebi_framework::{
        ebi_file_handler::EbiFileHandler,
        ebi_input::{EbiObjectImporter, EbiTraitImporter},
        ebi_output::EbiObjectExporter,
        object_importers::{
            ToEventLogObject, ToFiniteStochasticLanguageObject,
            ToStochasticDeterministicFiniteAutomatonObject,
        },
        validate::Validate,
    },
    ebi_traits::{
        ebi_trait_activities::ToActivities, ebi_trait_event_log::ToEventLog, ebi_trait_finite_language::ToFiniteLanguage, ebi_trait_finite_stochastic_language::ToFiniteStochasticLanguage, ebi_trait_iterable_language::ToIterableLanguage, ebi_trait_iterable_stochastic_language::ToIterableStochasticLanguage, ebi_trait_queriable_stochastic_language::ToQueriableStochasticLanguage, ebi_trait_semantics::ToSemantics, ebi_trait_stochastic_deterministic_semantics::ToStochasticDeterministicSemantics, ebi_trait_stochastic_semantics::ToStochasticSemantics
    },
};
use ebi_objects::{Exportable, Importable, ebi_objects::event_log_csv::EventLogCsv};

pub const EBI_EVENT_LOG_CSV: EbiFileHandler = EbiFileHandler {
    name: "event log",
    article: "an",
    file_extension: "csv",
    is_binary: false,
    format_specification: EventLogCsv::FILE_FORMAT_SPECIFICATION_LATEX,
    validator: Some(EventLogCsv::validate),
    trait_importers: &[
        EbiTraitImporter::Activities(
            EventLogCsv::import_as_activities,
            EventLogCsv::IMPORTER_PARAMETERS,
        ),
        EbiTraitImporter::IterableLanguage(
            EventLogCsv::import_as_iterable_language,
            EventLogCsv::IMPORTER_PARAMETERS,
        ),
        EbiTraitImporter::FiniteLanguage(
            EventLogCsv::import_as_finite_language,
            EventLogCsv::IMPORTER_PARAMETERS,
        ),
        EbiTraitImporter::FiniteStochasticLanguage(
            EventLogCsv::import_as_finite_stochastic_language,
            EventLogCsv::IMPORTER_PARAMETERS,
        ),
        EbiTraitImporter::QueriableStochasticLanguage(
            EventLogCsv::import_as_queriable_stochastic_language,
            EventLogCsv::IMPORTER_PARAMETERS,
        ),
        EbiTraitImporter::IterableStochasticLanguage(
            EventLogCsv::import_as_iterable_stochastic_language,
            EventLogCsv::IMPORTER_PARAMETERS,
        ),
        EbiTraitImporter::EventLog(
            EventLogCsv::import_as_event_log,
            EventLogCsv::IMPORTER_PARAMETERS,
        ),
        EbiTraitImporter::StochasticSemantics(
            EventLogCsv::import_as_stochastic_semantics,
            EventLogCsv::IMPORTER_PARAMETERS,
        ),
        EbiTraitImporter::StochasticDeterministicSemantics(
            EventLogCsv::import_as_stochastic_deterministic_semantics,
            EventLogCsv::IMPORTER_PARAMETERS,
        ),
        EbiTraitImporter::Semantics(
            EventLogCsv::import_as_semantics,
            EventLogCsv::IMPORTER_PARAMETERS,
        ),
    ],
    object_importers: &[
        EbiObjectImporter::EventLog(
            EventLogCsv::import_as_event_log_object,
            EventLogCsv::IMPORTER_PARAMETERS,
        ),
        EbiObjectImporter::EventLogCsv(
            EventLogCsv::import_as_object,
            EventLogCsv::IMPORTER_PARAMETERS,
        ),
        EbiObjectImporter::FiniteStochasticLanguage(
            EventLogCsv::import_as_finite_stochastic_language_object,
            EventLogCsv::IMPORTER_PARAMETERS,
        ),
        EbiObjectImporter::StochasticDeterministicFiniteAutomaton(
            EventLogCsv::import_as_stochastic_deterministic_finite_automaton_object,
            EventLogCsv::IMPORTER_PARAMETERS,
        ),
    ],
    object_exporters: &[
        EbiObjectExporter::EventLog(EventLogCsv::export_from_object),
        EbiObjectExporter::EventLogCsv(EventLogCsv::export_from_object),
    ],
    java_object_handlers: &[],
};
