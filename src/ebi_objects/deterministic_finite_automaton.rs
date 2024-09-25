use std::{cmp::{max, Ordering}, fmt::{Display, Formatter}, io::{self, BufRead}, str::FromStr, sync::Arc};
use anyhow::{anyhow, Result, Error,Context};
use layout::topo::layout::VisualGraph;
use serde_json::Value;

use crate::{ebi_framework::{activity_key::{Activity, ActivityKey}, dottable::Dottable, ebi_file_handler::EbiFileHandler, ebi_input::{self, EbiObjectImporter, EbiTraitImporter}, ebi_object::EbiObject, ebi_output::{EbiObjectExporter, EbiOutput}, exportable::Exportable, importable::Importable, infoable::Infoable}, ebi_objects::deterministic_finite_automaton_semantics::DeterministicFiniteAutomatonSemantics, ebi_traits::ebi_trait_semantics::EbiTraitSemantics, json};


pub const EBI_DETERMINISTIC_FINITE_AUTOMATON: EbiFileHandler = EbiFileHandler {
    name: "deterministic finite automaton",
    article: "a",
    file_extension: "dfa",
    validator: ebi_input::validate::<DeterministicFiniteAutomaton>,
    trait_importers: &[
        EbiTraitImporter::Semantics(DeterministicFiniteAutomaton::import_as_semantics),
    ],
    object_importers: &[
        EbiObjectImporter::DeterministicFiniteAutomaton(DeterministicFiniteAutomaton::import_as_object),
    ],
    object_exporters: &[
        EbiObjectExporter::DeterministicFiniteAutomaton(DeterministicFiniteAutomaton::export_from_object)
    ]
};

#[derive(Debug)]
pub struct DeterministicFiniteAutomaton {
    pub(crate) activity_key: ActivityKey,
    pub(crate) initial_state: usize,
    pub(crate) max_state: usize,
    pub(crate) sources: Vec<usize>, //transition -> source of arc
    pub(crate) targets: Vec<usize>, //transition -> target of arc
    pub(crate) activities: Vec<Activity>, //transition -> activity of arc (every transition is labelled)
}

impl DeterministicFiniteAutomaton {

    pub fn new() -> Self {
        Self {
            activity_key: ActivityKey::new(),
            max_state: 0,
            initial_state: 0,
            sources: vec![],
            targets: vec![],
            activities: vec![],
        }
    }

    pub fn get_sources(&self) -> &Vec<usize> {
        &self.sources
    }

    pub fn get_targets(&self) -> &Vec<usize> {
        &self.targets
    }

    pub fn get_activities(&self) -> &Vec<Activity> {
        &self.activities
    }

    pub fn set_initial_state(&mut self, state: usize) {
        self.ensure_states(state);
        self.initial_state = state;
    }

    fn ensure_states(&mut self, new_max_state: usize) {
        if new_max_state > self.max_state {
            self.max_state = new_max_state;
        }
    }

    pub fn add_transition(&mut self, source: usize, activity: Activity, target: usize) -> Result<()> {
        self.ensure_states(max(source, target));

        let (found, from) = self.binary_search(source, self.activity_key.get_id_from_activity(activity));
        if found {
            //edge already present
            Err(anyhow!("tried to insert an edge that would violate the determinism of the SDFA"))
        } else {
            self.sources.insert(from, source);
            self.targets.insert(from, target);
            self.activities.insert(from, activity);

            Ok(())
        }
    }

    pub fn get_number_of_transitions(&self) -> usize {
        self.sources.len()
    }

    /**
     * Adds the probability to the transition. Returns the target state, which may be new.
     */
    pub fn take_or_add_transition(&mut self, source_state: usize, activity: Activity) -> usize {

        let (found, transition) = self.binary_search(source_state, self.activity_key.get_id_from_activity(activity));
        if found {
            return self.targets[transition];
        } else {
            let target = self.add_state();
            self.sources.insert(transition, source_state);
            self.targets.insert(transition, target);
            self.activities.insert(transition, activity);
            return target;
        }
    }

    pub fn get_initial_state(&self) -> usize {
        self.initial_state
    }

    pub fn add_state(&mut self) -> usize {
		self.max_state += 1;
		return self.max_state;
	}

    pub fn get_max_state(&self) -> usize {
        self.max_state
    }

    fn compare(source1: usize, activity1: usize, source2: usize, activity2: Activity) -> Ordering {
		if source1 < source2 {
			return Ordering::Greater;
		} else if source1 > source2 {
			return Ordering::Less;
		} else if activity2 > activity1 {
			return Ordering::Greater;
		} else if activity2 < activity1 {
			return Ordering::Less;
		} else {
			return Ordering::Equal;
		}
	}

    pub(crate) fn binary_search(&self, source: usize, activity: usize) -> (bool, usize) {
        if self.sources.is_empty() {
            return (false, 0);
        }


        let mut size = self.sources.len();
        let mut left = 0;
        let mut right = size;
        while left < right {
            let mid = left + size / 2;

            let cmp = Self::compare(source, activity, self.sources[mid], self.activities[mid]);

            left = if cmp == Ordering::Less { mid + 1 } else { left };
            right = if cmp == Ordering::Greater { mid } else { right };
            if cmp == Ordering::Equal {
                assert!(mid < self.sources.len());
                return (true, mid);
            }

            size = right - left;
        }

        assert!(left <= self.sources.len());
        (false, left)
	}

    pub fn import_as_semantics(reader: &mut dyn BufRead) -> Result<EbiTraitSemantics> {
        let dfa = Arc::new(Self::import(reader)?);
        Ok(Self::get_semantics(dfa))
    }

    pub fn get_semantics(dfa: Arc<Self>) -> EbiTraitSemantics {
        log::info!("convert DFA to semantics");
        EbiTraitSemantics::Usize(Box::new(DeterministicFiniteAutomatonSemantics::new(dfa)))
    }

    pub fn get_activity_key(&self) -> &ActivityKey {
        &self.activity_key
    }
    
    pub fn get_activity_key_mut(&mut self) -> &mut ActivityKey {
        &mut self.activity_key
    }
}

impl FromStr for DeterministicFiniteAutomaton {
    type Err = Error;

    fn from_str(s: &str) -> std::prelude::v1::Result<Self, Self::Err> {
        let mut reader = io::Cursor::new(s);
        Self::import(&mut reader)
    }
}

impl Importable for DeterministicFiniteAutomaton {
    fn import_as_object(reader: &mut dyn BufRead) -> Result<EbiObject> {
        Ok(EbiObject::DeterministicFiniteAutomaton(Self::import(reader)?))
    }

    fn import(reader: &mut dyn BufRead) -> Result<Self> where Self: Sized {
        let json: Value = serde_json::from_reader(reader)?;

        let mut result = DeterministicFiniteAutomaton::new();

        result.set_initial_state(json::read_number(&json, "initialState").context("failed to read initial state")?);
        let jtrans = json::read_list(&json, "transitions").context("failed to read list of transitions")?;
        for jtransition in jtrans {
            let from = json::read_number(jtransition, "from").context("could not read from")?;
            let to = json::read_number(jtransition, "to").context("could not read to")?;
            let label = json::read_string(jtransition, "label").context("could not read label")?;
            let activity = result.activity_key.process_activity(label.as_str());

            result.add_transition(from, activity, to)?;
        }

        return Ok(result);
    }
}

impl Exportable for DeterministicFiniteAutomaton {
    fn export_from_object(object: EbiOutput, f: &mut dyn std::io::Write) -> Result<()> {
        match object {
            EbiOutput::Object(EbiObject::DeterministicFiniteAutomaton(dfa)) => dfa.export(f),
            _ => unreachable!()
        }
    }

    fn export(&self, f: &mut dyn std::io::Write) -> Result<()> {
        Ok(write!(f, "{}", self)?)
    }
}

impl Infoable for DeterministicFiniteAutomaton {
    fn info(&self, f: &mut impl std::io::Write) -> Result<()> {
        writeln!(f, "Number of states\t{}", self.max_state)?;
        writeln!(f, "Number of transitions\t{}", self.sources.len())?;
        writeln!(f, "Number of activities\t{}", self.activity_key.get_number_of_activities())?;

        Ok(write!(f, "")?)
    }
}

impl Display for DeterministicFiniteAutomaton {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        writeln!(f, "{{")?;
        writeln!(f, "\"initialState\": {},", self.get_initial_state())?;
        writeln!(f, "\"transitions\": [")?;
        for pos in 0..self.sources.len() {
            write!(f, "{{\"from\":{},\"to\":{},\"label\":\"{}\"}}", 
                self.sources[pos], 
                self.targets[pos], 
                self.activity_key.get_activity_label(&self.activities[pos]))?;
            if pos + 1 == self.sources.len() {
                writeln!(f, "")?;
            } else {
                writeln!(f, ",")?;
            }
        }
        writeln!(f, "]}}")?;
        Ok(())
    }
}

impl Dottable for DeterministicFiniteAutomaton {
    fn to_dot(&self) -> layout::topo::layout::VisualGraph {
        let mut graph = VisualGraph::new(layout::core::base::Orientation::LeftToRight);

        let mut places = vec![];
        for state in 0 ..= self.max_state {
            places.push(<dyn Dottable>::create_place(&mut graph, &state.to_string()));
        }

        for pos in 0..self.sources.len() {
            let source = places[self.sources[pos]];
            let target = places[self.targets[pos]];
            let activity = self.activity_key.get_activity_label(&self.activities[pos]);
            
            <dyn Dottable>::create_edge(&mut graph, &source, &target, activity);
        }

        return graph;
    }
}