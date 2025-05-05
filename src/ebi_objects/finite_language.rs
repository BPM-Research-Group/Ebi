use anyhow::{Context, Error, Result, anyhow};
use fnv::FnvBuildHasher;
use std::{
    collections::HashSet,
    fmt::Display,
    io::{self, BufRead},
    str::FromStr,
};

use crate::{
    ebi_framework::{
        activity_key::{
            Activity, ActivityKey, ActivityKeyTranslator, HasActivityKey, TranslateActivityKey,
        }, ebi_file_handler::EbiFileHandler, ebi_input::{self, EbiInput, EbiObjectImporter, EbiTraitImporter}, ebi_object::EbiObject, ebi_output::{EbiObjectExporter, EbiOutput}, ebi_trait::FromEbiTraitObject, exportable::Exportable, importable::Importable, infoable::Infoable
    },
    ebi_traits::{
        ebi_trait_event_log::IndexTrace,
        ebi_trait_finite_language::{self, EbiTraitFiniteLanguage},
        ebi_trait_iterable_language::{self, EbiTraitIterableLanguage},
        ebi_trait_semantics::{EbiTraitSemantics, ToSemantics},
    },
    line_reader::LineReader,
};

use super::{
    deterministic_finite_automaton::DeterministicFiniteAutomaton, event_log::EventLog,
    finite_stochastic_language::FiniteStochasticLanguage,
};

pub const HEADER: &str = "finite language";

pub const FORMAT_SPECIFICATION: &str =
    "A finite language is a line-based structure. Lines starting with a \\# are ignored.
    This first line is exactly `finite language'.
    The second line is the number of traces in the language.
    For each trace, the first line contains the number of events in the trace.
    Then, each subsequent line contains the activity name of one event.
    
    For instance:
    \\lstinputlisting[language=ebilines, style=boxed]{../testfiles/aa-ab-ba.lang}";

pub const EBI_FINITE_LANGUAGE: EbiFileHandler = EbiFileHandler {
    name: "finite language",
    article: "a",
    file_extension: "lang",
    format_specification: &FORMAT_SPECIFICATION,
    validator: ebi_input::validate::<FiniteLanguage>,
    trait_importers: &[
        EbiTraitImporter::IterableLanguage(ebi_trait_iterable_language::import::<FiniteLanguage>),
        EbiTraitImporter::FiniteLanguage(ebi_trait_finite_language::import::<FiniteLanguage>),
        EbiTraitImporter::Semantics(FiniteLanguage::import_as_semantics),
    ],
    object_importers: &[
        EbiObjectImporter::FiniteLanguage(FiniteLanguage::import_as_object),
        EbiObjectImporter::DeterministicFiniteAutomaton(
            FiniteLanguage::import_as_deterministic_finite_automaton,
        ),
    ],
    object_exporters: &[
        EbiObjectExporter::EventLog(FiniteLanguage::export_from_object),
        EbiObjectExporter::FiniteLanguage(FiniteLanguage::export_from_object),
        EbiObjectExporter::FiniteStochasticLanguage(FiniteLanguage::export_from_object),
    ],
    java_object_handlers: &[],
};

#[derive(ActivityKey, Clone)]
pub struct FiniteLanguage {
    activity_key: ActivityKey,
    traces: HashSet<Vec<Activity>, FnvBuildHasher>,
}

impl FiniteLanguage {
    pub fn new_hashmap() -> HashSet<Vec<Activity>, FnvBuildHasher> {
        HashSet::<_, FnvBuildHasher>::default()
    }

    pub fn push_string(&mut self, trace: Vec<String>) {
        self.traces.insert(self.activity_key.process_trace(&trace));
    }

    pub fn push(&mut self, trace: Vec<Activity>) {
        self.traces.insert(trace);
    }

    pub fn import_as_deterministic_finite_automaton(reader: &mut dyn BufRead) -> Result<EbiObject> {
        let lang = Self::import(reader)?;
        Ok(EbiObject::DeterministicFiniteAutomaton(Into::<
            DeterministicFiniteAutomaton,
        >::into(
            lang
        )))
    }
}

impl TranslateActivityKey for FiniteLanguage {
    fn translate_using_activity_key(&mut self, to_activity_key: &mut ActivityKey) {
        let translator = ActivityKeyTranslator::new(&self.activity_key, to_activity_key);

        //a hashmap needs to be rebuilt, unfortunately
        let translated_traces: HashSet<Vec<Activity>, FnvBuildHasher> = self
            .traces
            .drain()
            .map(|trace| translator.translate_trace(&trace))
            .collect();

        // Update the traces in the language with the translated ones
        self.traces = translated_traces;

        self.activity_key = to_activity_key.clone();
    }
}

impl EbiTraitIterableLanguage for FiniteLanguage {
    fn iter(&self) -> Box<dyn Iterator<Item = &Vec<Activity>> + '_> {
        Box::new(self.traces.iter())
    }
}

impl EbiTraitFiniteLanguage for FiniteLanguage {}

impl IndexTrace for FiniteLanguage {
    fn len(&self) -> usize {
        self.traces.len()
    }

    fn get_trace(&self, trace_index: usize) -> Option<&Vec<Activity>> {
        self.traces.iter().nth(trace_index)
    }
}

impl FromEbiTraitObject for FiniteLanguage {
    fn from_trait_object(object: ebi_input::EbiInput) -> Result<Box<Self>> {
        match object {
            EbiInput::Object(EbiObject::FiniteLanguage(e), _) => Ok(Box::new(e)),
            _ => Err(anyhow!(
                "cannot read {} {} as a finite language",
                object.get_type().get_article(),
                object.get_type()
            )),
        }
    }
}

impl Exportable for FiniteLanguage {
    fn export_from_object(object: EbiOutput, f: &mut dyn std::io::Write) -> Result<()> {
        match object {
            EbiOutput::Object(EbiObject::EventLog(log)) => {
                <EventLog as Into<FiniteLanguage>>::into(log).export(f)
            }
            EbiOutput::Object(EbiObject::FiniteLanguage(slang)) => slang.export(f),
            EbiOutput::Object(EbiObject::FiniteStochasticLanguage(slang)) => {
                <FiniteStochasticLanguage as Into<FiniteLanguage>>::into(slang).export(f)
            }
            _ => Err(anyhow!("Cannote export as finite language.")),
        }
    }

    fn export(&self, f: &mut dyn std::io::Write) -> Result<()> {
        Ok(write!(f, "{}", self)?)
    }
}

impl Infoable for FiniteLanguage {
    fn info(&self, f: &mut impl std::io::Write) -> Result<()> {
        writeln!(f, "Number of traces\t{}", self.traces.len())?;
        writeln!(
            f,
            "Number of events\t{}",
            self.traces.iter().map(|t| t.len()).sum::<usize>()
        )?;
        writeln!(
            f,
            "Number of activities\t{}",
            self.get_activity_key().get_number_of_activities()
        )?;

        Ok(write!(f, "")?)
    }
}

impl Display for FiniteLanguage {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        writeln!(f, "{}", HEADER)?;
        writeln!(f, "# number of traces\n{}", self.len())?;

        for (pos, trace) in self.iter().enumerate() {
            writeln!(f, "# trace {}", pos)?;

            writeln!(f, "# number of events\n{}", trace.len())?;
            for activity in trace {
                writeln!(f, "{}", self.activity_key.get_activity_label(activity))?;
            }
        }

        write!(f, "")
    }
}

impl FromStr for FiniteLanguage {
    type Err = Error;

    fn from_str(s: &str) -> std::prelude::v1::Result<Self, Self::Err> {
        let mut reader = io::Cursor::new(s);
        Self::import(&mut reader)
    }
}

impl Importable for FiniteLanguage {
    fn import_as_object(reader: &mut dyn BufRead) -> Result<EbiObject> {
        Ok(EbiObject::FiniteLanguage(Self::import(reader)?))
    }

    fn import(reader: &mut dyn BufRead) -> Result<Self> {
        let mut lreader = LineReader::new(reader);

        let head = lreader
            .next_line_string()
            .with_context(|| format!("failed to read header, which should be `{}`", HEADER))?;
        if head != HEADER {
            return Err(anyhow!(
                "first line should be exactly `{}`, but found `{}`",
                HEADER,
                head
            ));
        }

        let number_of_traces = lreader
            .next_line_index()
            .context("failed to read number of places")?;

        let mut traces = HashSet::<Vec<Activity>, FnvBuildHasher>::default();
        let mut activity_key = ActivityKey::new();
        for trace_i in 0..number_of_traces {
            let number_of_events = lreader.next_line_index().with_context(|| {
                format!(
                    "failed to read number of events for trace {} at line {}",
                    trace_i,
                    lreader.get_last_line_number()
                )
            })?;

            let mut trace = vec![];
            trace.reserve_exact(number_of_events);

            for event_i in 0..number_of_events {
                let event = lreader.next_line_string().with_context(|| {
                    format!(
                        "failed to read event {} of trace {} at line {}",
                        event_i,
                        trace_i,
                        lreader.get_last_line_number()
                    )
                })?;
                trace.push(event);
            }

            let trace = activity_key.process_trace(&trace);

            if !traces.insert(trace) {
                return Err(anyhow!(
                    "trace {} ending at line {} appears twice in language",
                    trace_i,
                    lreader.get_last_line_number()
                ));
            }
        }

        Ok(Self {
            activity_key: activity_key,
            traces: traces,
        })
    }
}

impl From<HashSet<Vec<String>>> for FiniteLanguage {
    fn from(value: HashSet<Vec<String>>) -> Self {
        let mut activity_key = ActivityKey::new();
        let traces = value
            .into_iter()
            .map(|trace| activity_key.process_trace(&trace))
            .collect();
        Self {
            activity_key: activity_key,
            traces: traces,
        }
    }
}

impl From<(ActivityKey, HashSet<Vec<Activity>, FnvBuildHasher>)> for FiniteLanguage {
    fn from(value: (ActivityKey, HashSet<Vec<Activity>, FnvBuildHasher>)) -> Self {
        Self {
            activity_key: value.0,
            traces: value.1,
        }
    }
}

impl ToSemantics for FiniteLanguage {
    fn to_semantics(self) -> EbiTraitSemantics {
        Into::<DeterministicFiniteAutomaton>::into(self).to_semantics()
    }
}

#[cfg(test)]
mod tests {
    use std::fs;

    use crate::ebi_traits::ebi_trait_semantics::{EbiTraitSemantics, ToSemantics};

    use super::FiniteLanguage;

    #[test]
    fn lang_empty() {
        let fin = fs::read_to_string("testfiles/empty.lang").unwrap();
        let log = fin.parse::<FiniteLanguage>().unwrap();

        if let EbiTraitSemantics::Usize(semantics) = log.to_semantics() {
            assert!(semantics.get_initial_state().is_none());
        }
    }
}
