use anyhow::{Error, Result, anyhow};
use core::fmt;
use ebi_derive::ActivityKey;
use process_mining::{
    XESImportOptions,
    event_log::{Trace, event_log_struct::EventLogClassifier},
};
use std::{
    collections::HashMap,
    io::{self, BufRead, Write},
    str::FromStr,
};

use crate::{
    ebi_framework::{
        activity_key::{
            Activity, ActivityKey, ActivityKeyTranslator, HasActivityKey, TranslateActivityKey,
        },
        ebi_file_handler::EbiFileHandler,
        ebi_input::{self, EbiInput, EbiObjectImporter, EbiTraitImporter},
        ebi_object::EbiObject,
        ebi_output::{EbiObjectExporter, EbiOutput},
        ebi_trait::FromEbiTraitObject,
        exportable::Exportable,
        importable::Importable,
        infoable::Infoable,
        prom_link::JavaObjectHandler,
    },
    ebi_traits::{
        ebi_trait_activities,
        ebi_trait_event_log::{EbiTraitEventLog, IndexTrace},
        ebi_trait_finite_language::EbiTraitFiniteLanguage,
        ebi_trait_finite_stochastic_language::EbiTraitFiniteStochasticLanguage,
        ebi_trait_iterable_language::EbiTraitIterableLanguage,
        ebi_trait_iterable_stochastic_language::EbiTraitIterableStochasticLanguage,
        ebi_trait_queriable_stochastic_language::EbiTraitQueriableStochasticLanguage,
        ebi_trait_semantics::{EbiTraitSemantics, ToSemantics},
        ebi_trait_stochastic_deterministic_semantics::{
            EbiTraitStochasticDeterministicSemantics, ToStochasticDeterministicSemantics,
        },
        ebi_trait_stochastic_semantics::{EbiTraitStochasticSemantics, ToStochasticSemantics},
    },
    math::data_type::DataType,
};

use super::{
    finite_language::FiniteLanguage, finite_stochastic_language::FiniteStochasticLanguage,
    stochastic_deterministic_finite_automaton::StochasticDeterministicFiniteAutomaton,
};

pub const FORMAT_SPECIFICATION: &str =
    "An event log file follows the IEEE XES format~\\cite{DBLP:journals/cim/AcamporaVSAGV17}. 
Parsing is performed by the Rust4PM crate~\\cite{DBLP:conf/bpm/KustersA24}.
For instance:
    \\lstinputlisting[language=xml, style=boxed]{../testfiles/a-b.xes}";

pub const EBI_EVENT_LOG: EbiFileHandler = EbiFileHandler {
    name: "event log",
    article: "an",
    file_extension: "xes",
    is_binary: false,
    format_specification: &FORMAT_SPECIFICATION,
    validator: Some(ebi_input::validate::<EventLog>),
    trait_importers: &[
        EbiTraitImporter::Activities(ebi_trait_activities::import::<EventLog>),
        EbiTraitImporter::IterableLanguage(EventLog::read_as_iterable_language),
        EbiTraitImporter::FiniteLanguage(EventLog::read_as_finite_language),
        EbiTraitImporter::FiniteStochasticLanguage(EventLog::read_as_finite_stochastic_language),
        EbiTraitImporter::QueriableStochasticLanguage(
            EventLog::read_as_queriable_stochastic_language,
        ),
        EbiTraitImporter::IterableStochasticLanguage(
            EventLog::read_as_iterable_stochastic_language,
        ),
        EbiTraitImporter::EventLog(EventLog::read_as_event_log),
        EbiTraitImporter::StochasticSemantics(EventLog::import_as_stochastic_semantics),
        EbiTraitImporter::StochasticDeterministicSemantics(
            EventLog::import_as_stochastic_deterministic_semantics,
        ),
        EbiTraitImporter::StochasticSemantics(EventLog::import_as_stochastic_semantics),
        EbiTraitImporter::Semantics(EventLog::import_as_semantics),
    ],
    object_importers: &[
        EbiObjectImporter::EventLog(EventLog::import_as_object),
        EbiObjectImporter::FiniteStochasticLanguage(EventLog::import_as_finite_stochastic_language),
        EbiObjectImporter::StochasticDeterministicFiniteAutomaton(
            EventLog::import_as_stochastic_deterministic_finite_automaton,
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

#[derive(ActivityKey, Clone)]
pub struct EventLog {
    classifier: EventLogClassifier,
    pub(crate) activity_key: ActivityKey,
    pub(crate) traces: Vec<Vec<Activity>>,
    rust4pm_log: process_mining::EventLog,
}

impl EventLog {
    pub fn new(log: process_mining::EventLog, classifier: EventLogClassifier) -> Self {
        let mut result = Self {
            classifier: classifier,
            rust4pm_log: log,
            activity_key: ActivityKey::new(),
            traces: vec![],
        };

        for trace_index in 0..result.rust4pm_log.traces.len() {
            result.traces.push(
                result.rust4pm_log.traces[trace_index]
                    .events
                    .iter()
                    .map(|event| {
                        result
                            .activity_key
                            .process_activity(&result.classifier.get_class_identity(event))
                    })
                    .collect::<Vec<Activity>>(),
            );
        }

        result
    }

    pub fn read_as_finite_language(
        reader: &mut dyn BufRead,
    ) -> Result<Box<dyn EbiTraitFiniteLanguage>> {
        let event_log = EventLog::import(reader)?;
        Ok(Box::new(Into::<FiniteLanguage>::into(event_log)))
    }

    pub fn read_as_finite_stochastic_language(
        reader: &mut dyn BufRead,
    ) -> Result<Box<dyn EbiTraitFiniteStochasticLanguage>> {
        let event_log = EventLog::import(reader)?;
        Ok(Box::new(Into::<FiniteStochasticLanguage>::into(Into::<
            FiniteStochasticLanguage,
        >::into(
            event_log,
        ))))
    }

    pub fn read_as_queriable_stochastic_language(
        reader: &mut dyn BufRead,
    ) -> Result<Box<dyn EbiTraitQueriableStochasticLanguage>> {
        let event_log = EventLog::import(reader)?;
        Ok(Box::new(Into::<FiniteStochasticLanguage>::into(event_log)))
    }

    pub fn read_as_iterable_language(
        reader: &mut dyn BufRead,
    ) -> Result<Box<dyn EbiTraitIterableLanguage>> {
        let event_log = EventLog::import(reader)?;
        Ok(Box::new(Into::<FiniteLanguage>::into(event_log)))
    }

    pub fn read_as_iterable_stochastic_language(
        reader: &mut dyn BufRead,
    ) -> Result<Box<dyn EbiTraitIterableStochasticLanguage>> {
        let event_log = EventLog::import(reader)?;
        Ok(Box::new(Into::<FiniteStochasticLanguage>::into(Into::<
            FiniteStochasticLanguage,
        >::into(
            event_log,
        ))))
    }

    pub fn read_as_event_log(reader: &mut dyn BufRead) -> Result<Box<dyn EbiTraitEventLog>> {
        let event_log = EventLog::import(reader)?;
        Ok(Box::new(event_log))
    }

    pub fn import_as_finite_stochastic_language(reader: &mut dyn BufRead) -> Result<EbiObject> {
        let log = Self::import(reader)?;
        Ok(EbiObject::FiniteStochasticLanguage(Into::<
            FiniteStochasticLanguage,
        >::into(log)))
    }

    pub fn import_as_stochastic_deterministic_finite_automaton(
        reader: &mut dyn BufRead,
    ) -> Result<EbiObject> {
        let log = Self::import(reader)?;
        Ok(EbiObject::StochasticDeterministicFiniteAutomaton(Into::<
            StochasticDeterministicFiniteAutomaton,
        >::into(
            log
        )))
    }

    pub fn retain_traces_mut<F>(&mut self, f: &mut F)
    where
        F: FnMut((&Vec<Activity>, &Trace)) -> bool,
    {
        //get our hands free to change the traces without cloning
        let mut traces = vec![];
        let mut rust4pm_traces = vec![];
        std::mem::swap(&mut self.traces, &mut traces);
        std::mem::swap(&mut self.rust4pm_log.traces, &mut rust4pm_traces);

        (traces, rust4pm_traces) = traces
            .into_iter()
            .zip(rust4pm_traces.into_iter())
            .filter_map(|(mut trace, mut rust4pm_trace)| {
                if f((&mut trace, &mut rust4pm_trace)) {
                    Some((trace, rust4pm_trace))
                } else {
                    None
                }
            })
            .unzip();

        //swap the the traces back
        std::mem::swap(&mut self.traces, &mut traces);
        std::mem::swap(&mut self.rust4pm_log.traces, &mut rust4pm_traces);
    }
}

impl TranslateActivityKey for EventLog {
    fn translate_using_activity_key(&mut self, to_activity_key: &mut ActivityKey) {
        let translator = ActivityKeyTranslator::new(&self.activity_key, to_activity_key);
        self.traces
            .iter_mut()
            .for_each(|trace| translator.translate_trace_mut(trace));
        self.activity_key = to_activity_key.clone();
    }
}

impl ToSemantics for EventLog {
    fn to_semantics(self) -> EbiTraitSemantics {
        Into::<FiniteStochasticLanguage>::into(self).to_semantics()
    }
}

impl ToStochasticSemantics for EventLog {
    fn to_stochastic_semantics(self) -> EbiTraitStochasticSemantics {
        Into::<FiniteStochasticLanguage>::into(self).to_stochastic_semantics()
    }
}

impl ToStochasticDeterministicSemantics for EventLog {
    fn to_stochastic_deterministic_semantics(self) -> EbiTraitStochasticDeterministicSemantics {
        Into::<FiniteStochasticLanguage>::into(self).to_stochastic_deterministic_semantics()
    }
}

impl Importable for EventLog {
    fn import_as_object(reader: &mut dyn BufRead) -> Result<EbiObject> {
        Ok(EbiObject::EventLog(Self::import(reader)?))
    }

    fn import(reader: &mut dyn BufRead) -> anyhow::Result<Self>
    where
        Self: Sized,
    {
        let log =
            process_mining::event_log::import_xes::import_xes(reader, XESImportOptions::default());
        if log.is_err() {
            return Err(anyhow!("{}", log.err().unwrap()));
        }
        let log = log.unwrap();
        let classifier = EventLogClassifier {
            name: "concept:name".to_string(),
            keys: vec!["concept:name".to_string()],
        };
        Ok(EventLog::new(log, classifier))
    }
}

impl FromStr for EventLog {
    type Err = Error;

    fn from_str(s: &str) -> std::result::Result<Self, Self::Err> {
        let mut reader = io::Cursor::new(s);
        Self::import(&mut reader)
    }
}

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

impl Exportable for EventLog {
    fn export_from_object(object: EbiOutput, f: &mut dyn Write) -> Result<()> {
        match object {
            EbiOutput::Object(EbiObject::EventLog(log)) => log.export(f),
            _ => Err(anyhow!("Cannot export as event log.")),
        }
    }

    fn export(&self, f: &mut dyn std::io::Write) -> Result<()> {
        process_mining::event_log::export_xes::export_xes_event_log(f, &self.rust4pm_log)?;
        Ok(())
    }
}

impl fmt::Display for EventLog {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "event log with {} traces", self.len())
    }
}

impl Infoable for EventLog {
    fn info(&self, f: &mut impl std::io::Write) -> Result<()> {
        writeln!(f, "Number of traces\t{}", self.len())?;
        writeln!(
            f,
            "Number of events\t{}",
            self.traces.iter().map(|trace| trace.len()).sum::<usize>()
        )?;
        writeln!(
            f,
            "Number of activities\t{}",
            self.get_activity_key().get_number_of_activities()
        )?;

        writeln!(f, "")?;
        self.get_activity_key().info(f)?;

        let trace_atts = self.get_trace_attributes();
        let t: Vec<String> = trace_atts
            .iter()
            .map(|(att, data_type)| format!("{}\t{}", att, data_type))
            .collect();
        writeln!(f, "Trace attributes:")?;
        writeln!(f, "\t{}", t.join("\n\t"))?;

        Ok(write!(f, "")?)
    }
}

impl EbiTraitEventLog for EventLog {
    fn get_log(&self) -> &process_mining::EventLog {
        &self.rust4pm_log
    }

    fn get_trace_attributes(&self) -> HashMap<String, DataType> {
        let mut map: HashMap<String, DataType> = HashMap::new();
        for trace in &self.rust4pm_log.traces {
            for attribute in &trace.attributes {
                match map.entry(attribute.key.clone()) {
                    std::collections::hash_map::Entry::Occupied(mut e) => {
                        e.get_mut().update(&attribute.value);
                        ()
                    }
                    std::collections::hash_map::Entry::Vacant(e) => {
                        e.insert(DataType::init(&attribute.value));
                        ()
                    }
                }
            }
        }
        map
    }

    fn retain_traces<'a>(&'a mut self, f: Box<dyn Fn(&Vec<Activity>) -> bool + 'static>) {
        self.traces.retain(f);
    }
}

impl IndexTrace for EventLog {
    fn len(&self) -> usize {
        self.traces.len()
    }

    fn get_trace(&self, trace_index: usize) -> Option<&Vec<Activity>> {
        self.traces.get(trace_index)
    }
}

#[cfg(test)]
mod tests {
    use std::fs;

    use crate::{
        ebi_framework::activity_key::{ActivityKey, TranslateActivityKey},
        ebi_objects::finite_stochastic_language::FiniteStochasticLanguage,
        ebi_traits::{
            ebi_trait_event_log::{EbiTraitEventLog, IndexTrace},
            ebi_trait_semantics::{EbiTraitSemantics, ToSemantics},
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

        if let EbiTraitSemantics::Usize(semantics) = log.to_semantics() {
            assert!(semantics.get_initial_state().is_none());
        }
    }

    #[test]
    fn len_retain() {
        let fin = fs::read_to_string("testfiles/a-b.xes").unwrap();
        let mut log = fin.parse::<EventLog>().unwrap();

        assert_eq!(log.len(), 2);

        log.retain_traces(Box::new(|_| false));

        assert_eq!(log.len(), 0);
    }

    #[test]
    fn len_retain_mut() {
        let fin = fs::read_to_string("testfiles/a-b.xes").unwrap();
        let mut log = fin.parse::<EventLog>().unwrap();

        assert_eq!(log.len(), 2);
        assert_eq!(log.rust4pm_log.traces.len(), 2);

        log.retain_traces_mut(&mut |_| false);

        assert_eq!(log.len(), 0);
        assert_eq!(log.rust4pm_log.traces.len(), 0);
    }
}
