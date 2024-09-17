use crate::{activity_key::{self, Activity, ActivityKey}, ebi_commands::ebi_command_info::Infoable, ebi_traits::{ebi_trait_event_log::IndexTrace, ebi_trait_finite_language::EbiTraitFiniteLanguage, ebi_trait_iterable_language::EbiTraitIterableLanguage}, export::{EbiObjectExporter, EbiOutput, Exportable}, file_handler::EbiFileHandler, import::{self, EbiObjectImporter, EbiTraitImporter, Importable}, line_reader::LineReader, ActivityTrace, Trace};
use anyhow::{anyhow, Result, Context, Error};
use std::{collections::{hash_set::Iter, HashSet}, fmt::Display, io::{self, BufRead}, str::FromStr};

use super::ebi_object::EbiObject;

pub const HEADER: &str = "finite language";

pub const EBI_FINITE_LANGUAGE: EbiFileHandler = EbiFileHandler {
    name: "finite language",
    article: "a",
    file_extension: "lang",
    validator: import::validate::<FiniteLanguage>,
    trait_importers: &[
        EbiTraitImporter::FiniteLanguage(import::read_as_finite_language::<FiniteLanguage>),
    ],
    object_importers: &[
        EbiObjectImporter::FiniteLanguage(FiniteLanguage::import_as_object),
    ],
    object_exporters: &[
        EbiObjectExporter::FiniteLanguage(FiniteLanguage::export_from_object)
    ],
};

pub struct FiniteLanguage {
    activity_key: ActivityKey,
    traces: HashSet<ActivityTrace>
}

impl FiniteLanguage {
    pub fn push_string(&mut self, trace: Trace) {
        self.traces.insert(self.activity_key.process_trace(&trace));
    }

    pub fn push(&mut self, trace: ActivityTrace) {
        self.traces.insert(trace);
    }
}

impl EbiTraitIterableLanguage for FiniteLanguage {
    fn get_activity_key(&self) -> &ActivityKey {
        &self.activity_key
    }
    
    fn iter(&self) -> Box<dyn Iterator<Item = &Vec<Activity>> + '_> {
        Box::new(self.traces.iter())
    }
}

impl EbiTraitFiniteLanguage for FiniteLanguage {
    
}

impl IndexTrace for FiniteLanguage {
    fn len(&self) -> usize {
        self.traces.len()
    }
    
    fn get_trace(&self, trace_index: usize) -> Option<&Vec<Activity>> {
        self.traces.iter().nth(trace_index)
    }
}

impl Exportable for FiniteLanguage {
    fn export_from_object(object: EbiOutput, f: &mut dyn std::io::Write) -> Result<()> {
        match object {
            EbiOutput::Object(EbiObject::FiniteLanguage(slang)) => slang.export(f),
            _ => unreachable!()
        }
    }

    fn export(&self, f: &mut dyn std::io::Write) -> Result<()> {
        Ok(write!(f, "{}", self)?)
    }
}

impl Infoable for FiniteLanguage {
    fn info(&self, f: &mut impl std::io::Write) -> Result<()> {
        writeln!(f, "Number of traces\t{}", self.traces.len())?;
        writeln!(f, "Number of events\t{}", self.traces.iter().map(|t| t.len()).sum::<usize>())?;
        writeln!(f, "Number of activities\t{}", self.get_activity_key().get_number_of_activities())?;

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

        let head = lreader.next_line_string().with_context(|| format!("failed to read header, which should be `{}`", HEADER))?;
        if head != HEADER {
            return Err(anyhow!("first line should be exactly `{}`, but found `{}`", HEADER, head));
        }

        let number_of_traces = lreader.next_line_index().context("failed to read number of places")?;
        if number_of_traces == 0 {
            return Err(anyhow!("language is empty"));
        }

        let mut traces = HashSet::new();
        let mut activity_key = ActivityKey::new();
        for trace_i in 0 .. number_of_traces {
            let number_of_events = lreader.next_line_index().with_context(|| format!("failed to read number of events for trace {} at line {}", trace_i, lreader.get_last_line_number()))?;

            let mut trace = vec![];
            trace.reserve_exact(number_of_events);

            for event_i in 0 .. number_of_events {
                let event = lreader.next_line_string().with_context(|| format!("failed to read event {} of trace {} at line {}", event_i, trace_i, lreader.get_last_line_number()))?;
                trace.push(event);
            }

            let trace = activity_key.process_trace(&trace);

            if !traces.insert(trace) {
                return Err(anyhow!("trace {} ending at line {} appears twice in language", trace_i, lreader.get_last_line_number()));    
            }
        }

        Ok(Self {
            activity_key: activity_key,
            traces: traces
        })
    }
}

impl From<HashSet<Trace>> for FiniteLanguage {
    fn from(value: HashSet<Trace>) -> Self {
        let mut activity_key = ActivityKey::new();
        let traces = value.into_iter().map(|trace| activity_key.process_trace(&trace)).collect();
        Self { 
            activity_key: activity_key,
            traces: traces 
        }
    }
}

impl From<(ActivityKey, HashSet<ActivityTrace>)> for FiniteLanguage {
    fn from(value: (ActivityKey, HashSet<ActivityTrace>)) -> Self {
        Self{
            activity_key: value.0,
            traces: value.1
        }
    }
}