use anyhow::{Result, anyhow};
use ebi_objects::{
    EbiObject, EventLog, Exportable, Importable, ebi_objects::event_log::FORMAT_SPECIFICATION,
};

use crate::{
    ebi_framework::{
        ebi_file_handler::EbiFileHandler,
        ebi_input::{self, EbiInput, EbiObjectImporter, EbiTraitImporter},
        ebi_output::EbiObjectExporter,
        ebi_trait::FromEbiTraitObject,
        object_importers::{
            ToFiniteStochasticLanguageObject, ToStochasticDeterministicFiniteAutomatonObject,
        },
        prom_link::JavaObjectHandler,
        validate::Validate,
    },
    ebi_traits::{
        ebi_trait_activities::ToActivities, ebi_trait_event_log::ToEventLog,
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

pub const EBI_EVENT_LOG: EbiFileHandler = EbiFileHandler {
    name: "event log",
    article: "an",
    file_extension: "xes",
    is_binary: false,
    format_specification: &FORMAT_SPECIFICATION,
    validator: Some(EventLog::validate),
    trait_importers: &[
        EbiTraitImporter::Activities(EventLog::import_as_activities),
        EbiTraitImporter::IterableLanguage(EventLog::import_as_iterable_language),
        EbiTraitImporter::FiniteLanguage(EventLog::import_as_finite_language),
        EbiTraitImporter::FiniteStochasticLanguage(EventLog::import_as_finite_stochastic_language),
        EbiTraitImporter::QueriableStochasticLanguage(
            EventLog::import_as_queriable_stochastic_language,
        ),
        EbiTraitImporter::IterableStochasticLanguage(
            EventLog::import_as_iterable_stochastic_language,
        ),
        EbiTraitImporter::EventLog(EventLog::import_as_event_log),
        EbiTraitImporter::StochasticSemantics(EventLog::import_as_stochastic_semantics),
        EbiTraitImporter::StochasticDeterministicSemantics(
            EventLog::import_as_stochastic_deterministic_semantics,
        ),
        EbiTraitImporter::StochasticSemantics(EventLog::import_as_stochastic_semantics),
        EbiTraitImporter::Semantics(EventLog::import_as_semantics),
    ],
    object_importers: &[
        EbiObjectImporter::EventLog(EventLog::import_as_object),
        EbiObjectImporter::FiniteStochasticLanguage(
            EventLog::import_as_finite_stochastic_language_object,
        ),
        EbiObjectImporter::StochasticDeterministicFiniteAutomaton(
            EventLog::import_as_stochastic_deterministic_finite_automaton_object,
        ),
    ],
    object_exporters: &[EbiObjectExporter::EventLog(EventLog::export_from_object)],
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

#[cfg(test)]
mod tests {
    use std::fs;

    use ebi_objects::{ActivityKey, FiniteStochasticLanguage, IndexTrace, TranslateActivityKey};

    use crate::ebi_traits::{
        ebi_trait_event_log::EbiTraitEventLog,
        ebi_trait_semantics::{EbiTraitSemantics, ToSemantics},
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

        if let EbiTraitSemantics::Usize(semantics) = log.to_semantics() {
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
        assert_eq!(log.rust4pm_log.traces.len(), 2);

        log.retain_traces_mut(&mut |_| false);

        assert_eq!(log.number_of_traces(), 0);
        assert_eq!(log.rust4pm_log.traces.len(), 0);
    }
}
