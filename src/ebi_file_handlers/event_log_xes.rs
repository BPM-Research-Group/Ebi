use crate::{
    ebi_framework::{
        ebi_file_handler::EbiFileHandler,
        ebi_input::{self, EbiInput, EbiObjectImporter, EbiTraitImporter},
        ebi_output::EbiObjectExporter,
        ebi_trait::FromEbiTraitObject,
        object_importers::{
            ImportAsDeterministicFiniteAutomatonObject, ImportAsFiniteLanguageObject,
            ImportAsFiniteStochasticLanguageObject,
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
    },
    prom::java_object_handler::JavaObjectHandler,
};
use anyhow::{Result, anyhow};
use ebi_objects::{
    EbiObject, EventLog, EventLogTraceAttributes, EventLogXes, Exportable, Importable,
};

pub const EBI_EVENT_LOG_XES: EbiFileHandler = EbiFileHandler {
    name: "extensible event stream",
    article: "an",
    file_extension: "xes",
    is_binary: false,
    format_specification: EventLogXes::FILE_FORMAT_SPECIFICATION_LATEX,
    validator: Some(EventLog::validate),
    trait_importers: &[
        EbiTraitImporter::Activities(
            EventLog::import_as_activities_trait,
            EventLog::IMPORTER_PARAMETERS,
        ),
        EbiTraitImporter::IterableLanguage(
            EventLog::import_as_iterable_language_trait,
            EventLog::IMPORTER_PARAMETERS,
        ),
        EbiTraitImporter::FiniteLanguage(
            EventLog::import_as_finite_language_trait,
            EventLog::IMPORTER_PARAMETERS,
        ),
        EbiTraitImporter::FiniteStochasticLanguage(
            EventLog::import_as_finite_stochastic_language_trait,
            EventLog::IMPORTER_PARAMETERS,
        ),
        EbiTraitImporter::QueriableStochasticLanguage(
            EventLog::import_as_queriable_stochastic_language_trait,
            EventLog::IMPORTER_PARAMETERS,
        ),
        EbiTraitImporter::IterableStochasticLanguage(
            EventLog::import_as_iterable_stochastic_language_trait,
            EventLog::IMPORTER_PARAMETERS,
        ),
        EbiTraitImporter::EventLog(
            EventLog::import_as_event_log_trait,
            EventLog::IMPORTER_PARAMETERS,
        ),
        EbiTraitImporter::EventLogTraceAttributes(
            EventLogTraceAttributes::import_as_event_log_trace_attributes_trait,
            EventLogTraceAttributes::IMPORTER_PARAMETERS,
        ),
        EbiTraitImporter::StochasticSemantics(
            EventLog::import_as_stochastic_semantics_trait,
            EventLog::IMPORTER_PARAMETERS,
        ),
        EbiTraitImporter::StochasticDeterministicSemantics(
            EventLog::import_as_stochastic_deterministic_semantics_trait,
            EventLog::IMPORTER_PARAMETERS,
        ),
        EbiTraitImporter::Semantics(
            EventLog::import_as_semantics_trait,
            EventLog::IMPORTER_PARAMETERS,
        ),
    ],
    object_importers: &[
        EbiObjectImporter::EventLog(EventLog::import_as_object, EventLog::IMPORTER_PARAMETERS),
        EbiObjectImporter::EventLogTraceAttributes(
            EventLogTraceAttributes::import_as_object,
            EventLogTraceAttributes::IMPORTER_PARAMETERS,
        ),
        EbiObjectImporter::EventLogXes(
            EventLogXes::import_as_object,
            EventLogXes::IMPORTER_PARAMETERS,
        ),
        EbiObjectImporter::FiniteLanguage(
            EventLog::import_as_finite_language_object,
            EventLog::IMPORTER_PARAMETERS,
        ),
        EbiObjectImporter::FiniteStochasticLanguage(
            EventLog::import_as_finite_stochastic_language_object,
            EventLog::IMPORTER_PARAMETERS,
        ),
        EbiObjectImporter::DeterministicFiniteAutomaton(
            EventLog::import_as_deterministic_finite_automaton_object,
            EventLog::IMPORTER_PARAMETERS,
        ),
        EbiObjectImporter::StochasticDeterministicFiniteAutomaton(
            EventLog::import_as_stochastic_deterministic_finite_automaton_object,
            EventLog::IMPORTER_PARAMETERS,
        ),
        EbiObjectImporter::StochasticNondeterministicFiniteAutomaton(
            EventLog::import_as_stochastic_nondeterministic_finite_automaton_object,
            EventLog::IMPORTER_PARAMETERS,
        ),
        EbiObjectImporter::StochasticNondeterministicFiniteAutomaton(
            EventLog::import_as_stochastic_nondeterministic_finite_automaton_object,
            EventLog::IMPORTER_PARAMETERS,
        ),
    ],
    object_exporters: &[
        EbiObjectExporter::EventLog(EventLog::export_from_object),
        EbiObjectExporter::EventLogTraceAttributes(EventLog::export_from_object),
        EbiObjectExporter::EventLogXes(EventLog::export_from_object),
        EbiObjectExporter::EventLogCsv(EventLog::export_from_object),
    ],
    object_exporters_fallible: &[],
    java_object_handlers: &[JavaObjectHandler {
        name: "XLog",
        translator_ebi_to_java: Some("org.processmining.ebi.objects.EbiEventLog.EbiStringToXLog"),
        translator_java_to_ebi: Some("org.processmining.ebi.objects.EbiEventLog.XLogToEbiString"),
        java_class: "org.deckfour.xes.model.XLog",
        input_gui: None,
    }],
};

impl FromEbiTraitObject for EventLog {
    fn from_trait_object(object: ebi_input::EbiInput) -> Result<Box<Self>> {
        match object {
            EbiInput::Object(EbiObject::EventLog(e), _) => Ok(Box::new(e)),
            _ => Err(anyhow!(
                "cannot read {} {} as an event log",
                object.get_type().get_article(),
                object.get_type()
            )),
        }
    }
}

impl FromEbiTraitObject for EventLogTraceAttributes {
    fn from_trait_object(object: EbiInput) -> Result<Box<Self>> {
        match object {
            EbiInput::Object(EbiObject::EventLogTraceAttributes(e), _) => Ok(Box::new(e)),
            _ => Err(anyhow!(
                "cannot read {} {} as an event log",
                object.get_type().get_article(),
                object.get_type()
            )),
        }
    }
}

impl FromEbiTraitObject for EventLogXes {
    fn from_trait_object(object: ebi_input::EbiInput) -> Result<Box<Self>> {
        match object {
            EbiInput::Object(EbiObject::EventLogXes(e), _) => Ok(Box::new(e)),
            _ => Err(anyhow!(
                "cannot read {} {} as an event log",
                object.get_type().get_article(),
                object.get_type()
            )),
        }
    }
}

#[cfg(test)]
mod tests {
    use std::fs;

    use ebi_objects::{
        ActivityKey, FiniteStochasticLanguage, NumberOfTraces, TranslateActivityKey,
    };

    use crate::{
        ebi_framework::trait_importers::ToSemanticsTrait,
        ebi_traits::{
            ebi_trait_event_log::EbiTraitEventLog, ebi_trait_semantics::EbiTraitSemantics,
        },
    };

    use super::EventLog;

    #[test]
    fn log_to_slang() {
        let fin = fs::read_to_string("testfiles/a-b.xes").unwrap();
        let log = fin.parse::<EventLog>().unwrap();

        let fin1 = fs::read_to_string("testfiles/a-b.slang").unwrap();
        let slang = fin1.parse::<FiniteStochasticLanguage>().unwrap();

        assert_eq!(Into::<FiniteStochasticLanguage>::into(log), slang);
    }

    #[test]
    fn log_activity_key() {
        let fin = fs::read_to_string("testfiles/a-b.xes").unwrap();
        let mut log = fin.parse::<EventLog>().unwrap();

        let mut activity_key = ActivityKey::new();
        log.translate_using_activity_key(&mut activity_key);
    }

    #[test]
    fn log_display() {
        let fin = fs::read_to_string("testfiles/a-b.xes").unwrap();
        let log = fin.parse::<EventLog>().unwrap();

        assert_eq!(format!("{}", log), "event log with 2 traces");
    }

    #[test]
    fn log_empty() {
        let fin = fs::read_to_string("testfiles/empty.xes").unwrap();
        let log = fin.parse::<EventLog>().unwrap();

        if let EbiTraitSemantics::Usize(semantics) = log.to_semantics_trait() {
            assert!(semantics.get_initial_state().is_none());
        }
    }

    #[test]
    fn len_retain() {
        let fin = fs::read_to_string("testfiles/a-b.xes").unwrap();
        let mut log = fin.parse::<EventLog>().unwrap();

        assert_eq!(log.number_of_traces(), 2);

        log.retain_traces(Box::new(|_| false));

        assert_eq!(log.number_of_traces(), 0);
    }

    #[test]
    fn len_retain_mut() {
        let fin = fs::read_to_string("testfiles/a-b.xes").unwrap();
        let mut log = fin.parse::<EventLog>().unwrap();

        assert_eq!(log.number_of_traces(), 2);

        log.retain_traces_mut(&mut |_| false);

        assert_eq!(log.number_of_traces(), 0);
    }
}
