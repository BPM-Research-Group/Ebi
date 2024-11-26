use std::{cmp::{max, Ordering}, collections::HashMap, fmt, io::{self, BufRead}, str::FromStr};
use anyhow::{anyhow, Context, Result, Error};
use layout::topo::layout::VisualGraph;
use serde_json::Value;
use crate::{ebi_framework::{activity_key::{Activity, ActivityKey, ActivityKeyTranslator, HasActivityKey}, dottable::Dottable, ebi_file_handler::EbiFileHandler, ebi_input::{self, EbiObjectImporter, EbiTraitImporter}, ebi_object::EbiObject, ebi_output::{EbiObjectExporter, EbiOutput}, exportable::Exportable, importable::Importable, infoable::Infoable}, ebi_traits::{ebi_trait_queriable_stochastic_language::{self}, ebi_trait_semantics::{EbiTraitSemantics, ToSemantics}, ebi_trait_stochastic_deterministic_semantics::{EbiTraitStochasticDeterministicSemantics, ToStochasticDeterministicSemantics}, ebi_trait_stochastic_semantics::{EbiTraitStochasticSemantics, ToStochasticSemantics}}, json, math::fraction::Fraction};

use super::{labelled_petri_net::LabelledPetriNet, stochastic_labelled_petri_net::StochasticLabelledPetriNet};

pub const EBI_STOCHASTIC_DETERMINISTIC_FINITE_AUTOMATON: EbiFileHandler = EbiFileHandler {
    name: "stochastic deterministic finite automaton",
    article: "a",
    file_extension: "sdfa",
    validator: ebi_input::validate::<StochasticDeterministicFiniteAutomaton>,
    trait_importers: &[
        EbiTraitImporter::QueriableStochasticLanguage(ebi_trait_queriable_stochastic_language::import::<StochasticDeterministicFiniteAutomaton>),
        EbiTraitImporter::StochasticDeterministicSemantics(StochasticDeterministicFiniteAutomaton::import_as_stochastic_deterministic_semantics),
        EbiTraitImporter::StochasticSemantics(StochasticDeterministicFiniteAutomaton::import_as_stochastic_semantics),
        EbiTraitImporter::Semantics(StochasticDeterministicFiniteAutomaton::import_as_semantics),
    ],
    object_importers: &[
        EbiObjectImporter::StochasticDeterministicFiniteAutomaton(StochasticDeterministicFiniteAutomaton::import_as_object),
    ],
    object_exporters: &[
        EbiObjectExporter::StochasticDeterministicFiniteAutomaton(StochasticDeterministicFiniteAutomaton::export_from_object),
        EbiObjectExporter::FiniteStochasticLanguage(StochasticDeterministicFiniteAutomaton::export_from_finite_stochastic_language),
        EbiObjectExporter::EventLog(StochasticDeterministicFiniteAutomaton::export_from_event_log),
    ],
    java_object_handlers: &[],
};

#[derive(Debug,ActivityKey)]
pub struct StochasticDeterministicFiniteAutomaton {
    pub(crate) activity_key: ActivityKey,
    pub(crate) initial_state: usize,
    pub(crate) max_state: usize,
    pub(crate) sources: Vec<usize>, //transition -> source of arc
    pub(crate) targets: Vec<usize>, //transition -> target of arc
    pub(crate) activities: Vec<Activity>, //transition -> activity of arc (every transition is labelled)
    pub(crate) probabilities: Vec<Fraction>, //transition -> probability of arc
    pub(crate) terminating_probabilities: Vec<Fraction> //state -> termination probability
}

impl StochasticDeterministicFiniteAutomaton {

    pub fn new() -> Self {
        Self {
            activity_key: ActivityKey::new(),
            max_state: 0,
            initial_state: 0,
            sources: vec![],
            targets: vec![],
            activities: vec![],
            probabilities: vec![],
            terminating_probabilities: vec![Fraction::one()],
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

    pub fn get_probabilities(&self) -> &Vec<Fraction> {
        &self.probabilities
    }

    pub fn set_initial_state(&mut self, state: usize) {
        self.ensure_states(state);
        self.initial_state = state;
    }

    pub fn can_terminate_in_state(&self, state: usize) -> bool {
        self.get_termination_probability(state).is_positive()
    }

    fn ensure_states(&mut self, new_max_state: usize) {
        if new_max_state > self.max_state {
            self.terminating_probabilities.extend(vec![Fraction::one(); new_max_state - self.max_state]);
            self.max_state = new_max_state;

            assert!(self.terminating_probabilities.len() == self.max_state + 1)
        }
    }

    pub fn add_transition(&mut self, source: usize, activity: Activity, target: usize, probability: Fraction) -> Result<()> {
        self.ensure_states(max(source, target));

        let (found, from) = self.binary_search(source, self.activity_key.get_id_from_activity(activity));
        if found {
            //edge already present
            Err(anyhow!("tried to insert an edge that would violate the determinism of the SDFA"))
        } else {
            self.sources.insert(from, source);
            self.targets.insert(from, target);
            self.activities.insert(from, activity);
            self.terminating_probabilities[source] -= &probability;
            self.probabilities.insert(from, probability);

            if self.terminating_probabilities[source].is_negative() {
                Err(anyhow!("tried to insert an edge that brings the sum outgoing probability of the source state above 1"))
            } else {
                Ok(())
            }
        }
    }

    pub fn get_number_of_transitions(&self) -> usize {
        self.sources.len()
    }

    /**
     * Adds the probability to the transition. Returns the target state, which may be new.
     */
    pub fn take_or_add_transition(&mut self, source_state: usize, activity: Activity, probability: Fraction) -> usize {
        self.terminating_probabilities[source_state] -= &probability;

        let (found, transition) = self.binary_search(source_state, self.activity_key.get_id_from_activity(activity));
        if found {
            self.probabilities[transition] += &probability;
            return self.targets[transition];
        } else {
            let target = self.add_state();
            self.sources.insert(transition, source_state);
            self.targets.insert(transition, target);
            self.activities.insert(transition, activity);
            self.probabilities.insert(transition, probability);
            return target;
        }
    }

    /**
     * Scales the outgoing probabilities of the state.
     */
    pub fn scale_outgoing_probabilities(&mut self, state2scale: HashMap<usize, Fraction>) {
        let mut new_terminating_probabilities = vec![Fraction::one(); self.terminating_probabilities.len()];
        for (state, _, _, outgoing_probability) in &mut self.into_iter() {

            if let Some(factor) = state2scale.get(state) {
                *outgoing_probability /= factor;
            }
            new_terminating_probabilities[*state] -= outgoing_probability;
        }
        self.terminating_probabilities = new_terminating_probabilities;
    }

    pub fn get_initial_state(&self) -> usize {
        self.initial_state
    }

    pub fn get_termination_probability(&self, state: usize) -> &Fraction {
        &self.terminating_probabilities[state]
    }

    pub fn add_state(&mut self) -> usize {
		self.max_state += 1;
        self.terminating_probabilities.push(Fraction::one());
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
   
    pub fn get_stochastic_labelled_petri_net(&self) -> StochasticLabelledPetriNet {
        log::info!("convert SDFA to stochastic labelled Petri net");

        let mut result = LabelledPetriNet::new();
        let translator = ActivityKeyTranslator::new(self.get_activity_key(), result.get_activity_key_mut());
        let mut weights = vec![];

        let source = result.add_place();
        result.get_initial_marking_mut().increase(source, 1).unwrap();

        //add places
        let mut state2place = vec![];
        for state in 0..=self.max_state {
            let lpn_place = result.add_place();
            state2place.push(lpn_place);

            //add termination
            if self.get_termination_probability(state).is_positive() {
                let lpn_transition = result.add_transition(None);
                weights.push(self.get_termination_probability(state).clone());
                result.add_place_transition_arc(lpn_place, lpn_transition, 1).unwrap();
                result.add_transition_place_arc(lpn_transition, lpn_place, 1).unwrap();
            }
        }

        //add edges
        for (source, (target, (activity, probability))) in self.sources.iter().zip(self.targets.iter().zip(self.activities.iter().zip(self.probabilities.iter()))) {
            
            //add transition
            let lpn_activity = translator.translate_activity(activity);
            let lpn_transition = result.add_transition(Some(lpn_activity));
            let source_place = state2place[*source];
            let target_place = state2place[*target];
            result.add_place_transition_arc(source_place, lpn_transition, 1).unwrap();
            result.add_transition_place_arc(lpn_transition, target_place, 1).unwrap();

            weights.push(probability.clone());
        }

        StochasticLabelledPetriNet::from((result, weights))
    }

    pub fn set_activity_key(&mut self, activity_key: &ActivityKey) {
        self.activity_key = activity_key.clone();
    }

    pub fn export_from_finite_stochastic_language(object: EbiOutput, f: &mut dyn std::io::Write) -> Result<()> {
        match object {
            EbiOutput::Object(EbiObject::FiniteStochasticLanguage(slang)) => slang.get_stochastic_deterministic_finite_automaton().export(f),
            _ => unreachable!()
        }
    }

    pub fn export_from_event_log(object: EbiOutput, f: &mut dyn std::io::Write) -> Result<()> {
        match object {
            EbiOutput::Object(EbiObject::EventLog(log)) => log.to_stochastic_deterministic_finite_automaton().export(f),
            _ => unreachable!()
        }
    }
}

impl FromStr for StochasticDeterministicFiniteAutomaton {
    type Err = Error;

    fn from_str(s: &str) -> std::prelude::v1::Result<Self, Self::Err> {
        let mut reader = io::Cursor::new(s);
        Self::import(&mut reader)
    }
}

impl Importable for StochasticDeterministicFiniteAutomaton {

    fn import_as_object(reader: &mut dyn BufRead) -> Result<EbiObject> {
        Ok(EbiObject::StochasticDeterministicFiniteAutomaton(Self::import(reader)?))
    }

    fn import(reader: &mut dyn BufRead) -> Result<Self> where Self: Sized {
        let json: Value = serde_json::from_reader(reader)?;

        let mut result = StochasticDeterministicFiniteAutomaton::new();

        result.set_initial_state(json::read_field_number(&json, "initialState").context("failed to read initial state")?);
        let jtrans = json::read_field_list(&json, "transitions").context("failed to read list of transitions")?;
        for jtransition in jtrans {
            let from = json::read_field_number(jtransition, "from").context("could not read from")?;
            let to = json::read_field_number(jtransition, "to").context("could not read to")?;
            let label = json::read_field_string(jtransition, "label").context("could not read label")?;
            let activity = result.activity_key.process_activity(label.as_str());
            let probability = json::read_field_fraction(jtransition, "prob").context("could not read probability")?;

            result.add_transition(from, activity, to, probability)?;
        }

        return Ok(result);
    }
}

impl Exportable for StochasticDeterministicFiniteAutomaton {
    fn export_from_object(object: EbiOutput, f: &mut dyn std::io::Write) -> Result<()> {
        match object {
            EbiOutput::Object(EbiObject::StochasticDeterministicFiniteAutomaton(sdfa)) => sdfa.export(f),
            _ => unreachable!()
        }
    }

    fn export(&self, f: &mut dyn std::io::Write) -> Result<()> {
        Ok(write!(f, "{}", self)?)
    }
}

impl Infoable for StochasticDeterministicFiniteAutomaton {
    fn info(&self, f: &mut impl std::io::Write) -> Result<()> {
        writeln!(f, "Number of states\t{}", self.max_state)?;
        writeln!(f, "Number of transitions\t{}", self.sources.len())?;
        writeln!(f, "Number of activities\t{}", self.activity_key.get_number_of_activities())?;

        Ok(write!(f, "")?)
    }
}

impl fmt::Display for StochasticDeterministicFiniteAutomaton {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        writeln!(f, "{{")?;
        writeln!(f, "\"initialState\": {},", self.get_initial_state())?;
        writeln!(f, "\"transitions\": [")?;
        for pos in 0..self.sources.len() {
            write!(f, "{{\"from\":{},\"to\":{},\"label\":\"{}\",\"prob\":\"{}\"}}", 
                self.sources[pos], 
                self.targets[pos], 
                self.activity_key.get_activity_label(&self.activities[pos]),
                self.probabilities[pos])?;
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

impl Dottable for StochasticDeterministicFiniteAutomaton {
    fn to_dot(&self) -> layout::topo::layout::VisualGraph {
        log::info!("to_dot for StochasticDeterministicFiniteAutomaton");
        let mut graph = VisualGraph::new(layout::core::base::Orientation::LeftToRight);

        let mut places = vec![];
        for state in 0 ..= self.max_state {
            places.push(<dyn Dottable>::create_place(&mut graph, &format!("{}", self.terminating_probabilities[state])));
            // places.push(<dyn Dottable>::create_place(&mut graph, ""));
        }

        for pos in 0..self.sources.len() {
            let source = places[self.sources[pos]];
            let target = places[self.targets[pos]];
            let probability = &self.probabilities[pos];
            let activity = self.activity_key.get_activity_label(&self.activities[pos]);
            
            <dyn Dottable>::create_edge(&mut graph, &source, &target, &format!("{}, {}", activity, probability.to_string()));
        }

        return graph;
    }
}

impl<'a> IntoIterator for &'a StochasticDeterministicFiniteAutomaton {
    type Item = (&'a usize, &'a usize, &'a Activity, &'a Fraction);

    type IntoIter = StochasticDeterministicFiniteAutomatonIterator<'a>;

    fn into_iter(self) -> Self::IntoIter {
        Self::IntoIter {
            it_sources:  self.get_sources().iter(),
            it_targets:  self.get_targets().iter(),
            it_activities: self.get_activities().iter(),
            it_probabilities: self.get_probabilities().iter()
        }
    }
}

pub struct StochasticDeterministicFiniteAutomatonIterator<'a> {
    it_sources: std::slice::Iter<'a, usize>,
    it_targets: std::slice::Iter<'a, usize>,
    it_activities: std::slice::Iter<'a, Activity>,
    it_probabilities: std::slice::Iter<'a, Fraction>
}

impl<'a> Iterator for StochasticDeterministicFiniteAutomatonIterator<'a> {
    type Item = (&'a usize, &'a usize, &'a Activity, &'a Fraction);

    fn next(&mut self) -> Option<Self::Item> {
        if let Some(source) = self.it_sources.next() {
            let target = self.it_targets.next().unwrap();
            let activity = self.it_activities.next().unwrap();
            let probability = self.it_probabilities.next().unwrap();
            let result = Some((source, target, activity, probability));
            result
        } else {
            None
        }
    }
}

impl<'a> IntoIterator for &'a mut StochasticDeterministicFiniteAutomaton {
    type Item = (&'a usize, &'a usize, &'a Activity, &'a mut Fraction);

    type IntoIter = StochasticDeterministicFiniteAutomatonMutIterator<'a>;

    fn into_iter(self) -> Self::IntoIter {
        Self::IntoIter {
            it_sources:  self.sources.iter(),
            it_targets:  self.targets.iter(),
            it_activities: self.activities.iter(),
            it_probabilities: self.probabilities.iter_mut()
        }
    }
}

pub struct StochasticDeterministicFiniteAutomatonMutIterator<'a> {
    it_sources: std::slice::Iter<'a, usize>,
    it_targets: std::slice::Iter<'a, usize>,
    it_activities: std::slice::Iter<'a, Activity>,
    it_probabilities: std::slice::IterMut<'a, Fraction>
}

impl<'a> Iterator for StochasticDeterministicFiniteAutomatonMutIterator<'a> {
    type Item = (&'a usize, &'a usize, &'a Activity, &'a mut Fraction);

    fn next(&mut self) -> Option<Self::Item> {
        if let Some(source) = self.it_sources.next() {
            let target = self.it_targets.next().unwrap();
            let activity = self.it_activities.next().unwrap();
            let probability = self.it_probabilities.next().unwrap();
            let result = Some((source, target, activity, probability));
            result
        } else {
            None
        }
    }
}

impl ToSemantics for StochasticDeterministicFiniteAutomaton {
    fn to_semantics(self) -> EbiTraitSemantics {
        EbiTraitSemantics::Usize(Box::new(self))
    }
}

impl ToStochasticSemantics for StochasticDeterministicFiniteAutomaton {
    fn to_stochastic_semantics(self) -> EbiTraitStochasticSemantics {
        EbiTraitStochasticSemantics::Usize(Box::new(self))
    }
}

impl ToStochasticDeterministicSemantics for StochasticDeterministicFiniteAutomaton {
    fn to_stochastic_deterministic_semantics(self) -> EbiTraitStochasticDeterministicSemantics {
        EbiTraitStochasticDeterministicSemantics::Usize(Box::new(self))
    }
}