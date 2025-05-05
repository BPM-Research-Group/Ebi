use anyhow::{Context, Error, Result, anyhow};
use layout::topo::layout::VisualGraph;
use serde_json::Value;
use std::{
    cmp::{Ordering, max},
    fmt::{Display, Formatter},
    io::{self, BufRead},
    str::FromStr,
};

use crate::{
    ebi_framework::{
        activity_key::{Activity, ActivityKey, ActivityKeyTranslator, TranslateActivityKey},
        ebi_file_handler::EbiFileHandler,
        ebi_input::{self, EbiObjectImporter, EbiTraitImporter},
        ebi_object::EbiObject,
        ebi_output::{EbiObjectExporter, EbiOutput},
        exportable::Exportable,
        importable::Importable,
        infoable::Infoable,
    },
    ebi_traits::{
        ebi_trait_graphable::{self, EbiTraitGraphable},
        ebi_trait_semantics::{EbiTraitSemantics, Semantics, ToSemantics},
    },
    json,
};

pub const FORMAT_SPECIFICATION: &str = "A deterministic finite automaton is a JSON structure with the top level being an object.
    This object contains the following key-value pairs:
    \\begin{itemize}
    \\item \\texttt{initialState} being the index of the initial state. This field is optional: if omitted, the DFA has an empty language.
    \\item \\texttt{finalStates} being a list of indices of the final states.
    A final state is not necessarily a deadlock state.
    \\item \\texttt{transitions} being a list of transitions. 
    Each transition is an object with \\texttt{from} being the source state index of the transition, \\texttt{to} being the target state index of the transition, and \texttt{{label}} being the activity of the transition. 
    Silent transitions are not supported.
    The file format supports deadlocks and livelocks.
    \\end{itemize}
    For instance:
    \\lstinputlisting[language=json, style=boxed]{../testfiles/aa-ab-ba.dfa}";

pub const EBI_DETERMINISTIC_FINITE_AUTOMATON: EbiFileHandler = EbiFileHandler {
    name: "deterministic finite automaton",
    article: "a",
    file_extension: "dfa",
    format_specification: FORMAT_SPECIFICATION,
    validator: ebi_input::validate::<DeterministicFiniteAutomaton>,
    trait_importers: &[
        EbiTraitImporter::Semantics(DeterministicFiniteAutomaton::import_as_semantics),
        EbiTraitImporter::Graphable(ebi_trait_graphable::import::<DeterministicFiniteAutomaton>),
    ],
    object_importers: &[
        EbiObjectImporter::DeterministicFiniteAutomaton(
            DeterministicFiniteAutomaton::import_as_object,
        ),
        EbiObjectImporter::LabelledPetriNet(
            DeterministicFiniteAutomaton::import_as_labelled_petri_net,
        ),
    ],
    object_exporters: &[
        EbiObjectExporter::DeterministicFiniteAutomaton(
            DeterministicFiniteAutomaton::export_from_object,
        ),
        EbiObjectExporter::FiniteLanguage(DeterministicFiniteAutomaton::export_from_object),
        EbiObjectExporter::StochasticDeterministicFiniteAutomaton(
            DeterministicFiniteAutomaton::export_from_object,
        ),
        EbiObjectExporter::EventLog(DeterministicFiniteAutomaton::export_from_object),
        EbiObjectExporter::FiniteStochasticLanguage(
            DeterministicFiniteAutomaton::export_from_object,
        ),
    ],
    java_object_handlers: &[],
};

#[derive(Debug, ActivityKey, Clone)]
pub struct DeterministicFiniteAutomaton {
    pub(crate) activity_key: ActivityKey,
    pub(crate) initial_state: Option<usize>,
    pub(crate) max_state: usize,
    pub(crate) sources: Vec<usize>,       //transition -> source of arc
    pub(crate) targets: Vec<usize>,       //transition -> target of arc
    pub(crate) activities: Vec<Activity>, //transition -> activity of arc (every transition is labelled)
    pub(crate) final_states: Vec<bool>,
}

impl DeterministicFiniteAutomaton {
    /**
     * Creates a new DFA with an initial state that is not an explicit final state. As this state is a deadlock, it is an implicit final state anyway, until an outgoing transition is added.
     */
    pub fn new() -> Self {
        Self {
            activity_key: ActivityKey::new(),
            max_state: 0,
            initial_state: Some(0),
            sources: vec![],
            targets: vec![],
            activities: vec![],
            final_states: vec![false],
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

    pub fn set_initial_state(&mut self, state: Option<usize>) {
        if let Some(state) = state {
            self.ensure_states(state);
        }
        self.initial_state = state;
    }

    fn ensure_states(&mut self, new_max_state: usize) {
        while new_max_state > self.max_state {
            self.max_state += 1;
            self.final_states.push(false);
        }
    }

    pub fn add_transition(
        &mut self,
        source: usize,
        activity: Activity,
        target: usize,
    ) -> Result<()> {
        self.ensure_states(max(source, target));

        let (found, from) =
            self.binary_search(source, self.activity_key.get_id_from_activity(activity));
        if found {
            //edge already present
            Err(anyhow!(
                "tried to insert an edge that would violate the determinism of the SDFA"
            ))
        } else {
            self.sources.insert(from, source);
            self.targets.insert(from, target);
            self.activities.insert(from, activity);

            Ok(())
        }
    }

    /**
     * Adds the probability to the transition. Returns the target state, which may be new.
     */
    pub fn take_or_add_transition(&mut self, source_state: usize, activity: Activity) -> usize {
        let (found, transition) = self.binary_search(
            source_state,
            self.activity_key.get_id_from_activity(activity),
        );
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

    pub fn can_terminate_in_state(&self, state: usize) -> bool {
        self.final_states[state]
    }

    /**
     * Returns whether a transition is not permanently disabled.
     */
    pub fn can_execute_transition(&self, _transition: usize) -> bool {
        true
    }

    pub fn set_final_state(&mut self, state: usize, is_final: bool) {
        self.final_states[state] = is_final
    }

    pub fn add_state(&mut self) -> usize {
        self.max_state += 1;
        self.final_states.push(false);
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

    pub fn set_activity_key(&mut self, activity_key: ActivityKey) {
        self.activity_key = activity_key
    }

    pub fn import_as_labelled_petri_net(reader: &mut dyn BufRead) -> Result<EbiObject> {
        let dfm = Self::import(reader)?;
        Ok(EbiObject::LabelledPetriNet(dfm.into()))
    }
}

impl TranslateActivityKey for DeterministicFiniteAutomaton {
    fn translate_using_activity_key(&mut self, to_activity_key: &mut ActivityKey) {
        let translator = ActivityKeyTranslator::new(&self.activity_key, to_activity_key);
        self.activities
            .iter_mut()
            .for_each(|activity| *activity = translator.translate_activity(&activity));
        self.activity_key = to_activity_key.clone();
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
        Ok(EbiObject::DeterministicFiniteAutomaton(Self::import(
            reader,
        )?))
    }

    fn import(reader: &mut dyn BufRead) -> Result<Self>
    where
        Self: Sized,
    {
        let json: Value = serde_json::from_reader(reader)?;

        let mut result = DeterministicFiniteAutomaton::new();

        result.set_initial_state(json::read_field_number(&json, "initialState").ok());

        //read transitions
        let jtrans = json::read_field_list(&json, "transitions")
            .context("failed to read list of transitions")?;
        for jtransition in jtrans {
            let from =
                json::read_field_number(jtransition, "from").context("could not read from")?;
            let to = json::read_field_number(jtransition, "to").context("could not read to")?;
            let label =
                json::read_field_string(jtransition, "label").context("could not read label")?;
            let activity = result.activity_key.process_activity(label.as_str());

            result.ensure_states(from);
            result.ensure_states(to);

            result.add_transition(from, activity, to)?;
        }

        //read final states
        let jfinal_states = json::read_field_list(&json, "finalStates")
            .context("failed to read list of final states")?;
        for jfinal_state in jfinal_states {
            let state = json::read_number(jfinal_state).context("could not read final state")?;

            if state > result.max_state {
                result.ensure_states(state);
            }

            result.final_states[state] = true;
        }

        return Ok(result);
    }
}

impl Exportable for DeterministicFiniteAutomaton {
    fn export_from_object(object: EbiOutput, f: &mut dyn std::io::Write) -> Result<()> {
        match object {
            EbiOutput::Object(EbiObject::DeterministicFiniteAutomaton(dfa)) => dfa.export(f),
            EbiOutput::Object(EbiObject::FiniteLanguage(lang)) => {
                Into::<Self>::into(lang).export(f)
            }
            EbiOutput::Object(EbiObject::FiniteStochasticLanguage(slang)) => {
                Into::<Self>::into(slang).export(f)
            }
            EbiOutput::Object(EbiObject::EventLog(log)) => Into::<Self>::into(log).export(f),
            EbiOutput::Object(EbiObject::StochasticDeterministicFiniteAutomaton(sdfa)) => {
                Into::<Self>::into(sdfa).export(f)
            }
            _ => Err(anyhow!("Cannot export to DFA.")),
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
        writeln!(
            f,
            "Number of activities\t{}",
            self.activity_key.get_number_of_activities()
        )?;

        Ok(write!(f, "")?)
    }
}

impl Display for DeterministicFiniteAutomaton {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        writeln!(f, "{{")?;
        if let Some(state) = self.get_initial_state() {
            writeln!(f, "\"initialState\": {},", state)?;
        }
        writeln!(f, "\"transitions\": [")?;
        for pos in 0..self.sources.len() {
            write!(
                f,
                "{{\"from\":{},\"to\":{},\"label\":\"{}\"}}",
                self.sources[pos],
                self.targets[pos],
                self.activity_key.get_activity_label(&self.activities[pos])
            )?;
            if pos + 1 == self.sources.len() {
                writeln!(f, "")?;
            } else {
                writeln!(f, ",")?;
            }
        }
        writeln!(f, "], \"finalStates\": [")?;
        writeln!(
            f,
            "{}",
            self.final_states
                .iter()
                .enumerate()
                .filter_map(|(index, is)| if *is { Some(index.to_string()) } else { None })
                .collect::<Vec<_>>()
                .join(",")
        )?;
        writeln!(f, "]}}")?;
        Ok(())
    }
}

impl EbiTraitGraphable for DeterministicFiniteAutomaton {
    fn to_dot(&self) -> Result<layout::topo::layout::VisualGraph> {
        let mut graph = VisualGraph::new(layout::core::base::Orientation::LeftToRight);

        let mut places = vec![];
        for state in 0..=self.max_state {
            if self.can_terminate_in_state(state) {
                places.push(<dyn EbiTraitGraphable>::create_transition(
                    &mut graph,
                    &state.to_string(),
                    "",
                ));
            } else {
                places.push(<dyn EbiTraitGraphable>::create_place(
                    &mut graph,
                    &state.to_string(),
                ));
            }
        }

        for pos in 0..self.sources.len() {
            let source = places[self.sources[pos]];
            let target = places[self.targets[pos]];
            let activity = self.activity_key.get_activity_label(&self.activities[pos]);

            <dyn EbiTraitGraphable>::create_edge(&mut graph, &source, &target, activity);
        }

        Ok(graph)
    }
}

impl ToSemantics for DeterministicFiniteAutomaton {
    fn to_semantics(self) -> EbiTraitSemantics {
        EbiTraitSemantics::Usize(Box::new(self))
    }
}

#[cfg(test)]
mod tests {
    use std::fs;

    use crate::ebi_framework::activity_key::HasActivityKey;
    use crate::ebi_traits::ebi_trait_semantics::{EbiTraitSemantics, Semantics, ToSemantics};

    use super::DeterministicFiniteAutomaton;

    #[test]
    fn insert_wrong_edge() {
        let mut dfa = DeterministicFiniteAutomaton::new();
        let state = dfa.get_initial_state().unwrap();
        let activity = dfa.get_activity_key_mut().process_activity("a");

        dfa.get_sources();

        assert!(dfa.add_transition(state, activity, state).is_ok());
        assert!(dfa.add_transition(state, activity, state).is_err());
    }

    #[test]
    fn dfa_empty() {
        let fin = fs::read_to_string("testfiles/empty.dfa").unwrap();
        let dfa = fin.parse::<DeterministicFiniteAutomaton>().unwrap();

        if let EbiTraitSemantics::Usize(semantics) = dfa.to_semantics() {
            assert!(semantics.get_initial_state().is_none());
        } else {
            assert!(false);
        }
    }
}
