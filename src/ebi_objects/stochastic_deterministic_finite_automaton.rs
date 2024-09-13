use std::{cmp::{max, Ordering}, collections::{HashMap, HashSet}, fmt, io::{self, BufRead}, mem::zeroed, rc::Rc, slice::Iter, str::FromStr};
use anyhow::{anyhow, Context, Result, Error};
use num_traits::zero;
use rand::{thread_rng,Rng};
use fraction::{BigUint, GenericFraction, One, Zero};
use layout::topo::layout::VisualGraph;
use serde_json::Value;
use crate::{activity_key::{self, Activity, ActivityKey}, dottable::Dottable, ebi_commands::ebi_command_info::Infoable, ebi_traits::{ebi_semantics::{EbiTraitSemantics, Semantics}, ebi_trait_queriable_stochastic_language::EbiTraitQueriableStochasticLanguage, ebi_trait_stochastic_deterministic_semantics::{EbiTraitStochasticDeterministicSemantics, StochasticDeterministicSemantics}, ebi_trait_stochastic_semantics::{EbiTraitStochasticSemantics, StochasticSemantics, TransitionIndex}}, export::{EbiObjectExporter, EbiOutput, Exportable}, file_handler::EbiFileHandler, follower_semantics::FollowerSemantics, import::{self, EbiObjectImporter, EbiTraitImporter, Importable}, marking::Marking, math::fraction::Fraction, net::Transition, Trace};

use super::{ebi_object::EbiObject, finite_stochastic_language::FiniteStochasticLanguage, labelled_petri_net::LabelledPetriNet, stochastic_labelled_petri_net::StochasticLabelledPetriNet};

pub const EBI_STOCHASTIC_DETERMINISTIC_FINITE_AUTOMATON: EbiFileHandler = EbiFileHandler {
    name: "stochastic deterministic finite automaton",
    article: "an",
    file_extension: "sdfa",
    validator: import::validate::<StochasticDeterministicFiniteAutomaton>,
    trait_importers: &[
        EbiTraitImporter::QueriableStochasticLanguage(import::read_as_queriable_stochastic_language::<StochasticDeterministicFiniteAutomaton>),
        EbiTraitImporter::StochasticDeterministicSemantics(StochasticDeterministicFiniteAutomaton::import_as_stochastic_deterministic_semantics),
        EbiTraitImporter::StochasticSemantics(StochasticDeterministicFiniteAutomaton::import_as_stochastic_semantics),
        EbiTraitImporter::Semantics(StochasticDeterministicFiniteAutomaton::import_as_semantics),
    ],
    object_importers: &[
        EbiObjectImporter::StochasticDeterministicFiniteAutomaton(StochasticDeterministicFiniteAutomaton::import_as_object),
    ],
    object_exporters: &[
        EbiObjectExporter::StochasticDeterministicFiniteAutomaton(StochasticDeterministicFiniteAutomaton::export_from_object)
    ]
};

#[derive(Debug)]
pub struct StochasticDeterministicFiniteAutomaton {
    activity_key: ActivityKey,
    initial_state: usize,
    max_state: usize,
    sources: Vec<usize>, //transition -> source of arc
    targets: Vec<usize>, //transition -> target of arc
    activities: Vec<Activity>, //transition -> activity of arc (every transition is labelled)
    probabilities: Vec<Fraction>, //transition -> probability of arc
    terminating_probabilities: Vec<Fraction> //state -> termination probability
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

    pub fn get_activity_key(&mut self) -> &mut ActivityKey {
        &mut self.activity_key
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

    fn binary_search(&self, source: usize, activity: usize) -> (bool, usize) {
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

    fn read_number(json: &Value, field: &str) -> Result<usize> {
        match &json[field] {
            Value::Null => return Err(anyhow!("field not found")),
            Value::Bool(_) => return Err(anyhow!("field is a boolean, where number expected")),
            Value::Number(n) => {
                if !n.is_u64() {
                    return Err(anyhow!("number is not an integer"))
                }
                return Ok(usize::try_from(n.as_u64().unwrap())?);
            },
            Value::String(_) => return Err(anyhow!("field is a literal, where number expected")),
            Value::Array(_) => return Err(anyhow!("field is a list, where number expected")),
            Value::Object(_) => return Err(anyhow!("field is an object, where number expected")),
        }
    }

    fn read_fraction(json: &Value, field: &str) -> Result<Fraction> {
        match &json[field] {
            Value::Null => return Err(anyhow!("field not found")),
            Value::Bool(_) => return Err(anyhow!("field is a boolean, where fraction expected")),
            Value::Number(n) => return Ok(n.to_string().parse::<Fraction>()?),
            Value::String(s) => return Ok(s.parse::<Fraction>()?),
            Value::Array(_) => return Err(anyhow!("field is a list, where fraction expected")),
            Value::Object(_) => return Err(anyhow!("field is an object, where fraction expected")),
        }
    }

    fn read_list<'a>(json: &'a Value, field: &str) -> Result<&'a Vec<Value>> {
        match &json[field] {
            Value::Null => return Err(anyhow!("field not found")),
            Value::Bool(_) => return Err(anyhow!("field is a boolean, where list expected")),
            Value::Number(_) => return Err(anyhow!("field is a number, where list expected")),
            Value::String(_) => return Err(anyhow!("field is a literal, where list expected")),
            Value::Array(arr) => return Ok(&arr),
            Value::Object(_) => return Err(anyhow!("field is an object, where list expected")),
        }
    }

    fn read_string<'a>(json: &'a Value, field: &str) -> Result<String> {
        match &json[field] {
            Value::Null => return Err(anyhow!("field not found")),
            Value::Bool(_) => return Err(anyhow!("field is a boolean, where literal expected")),
            Value::Number(n) => return Ok(n.to_string()),
            Value::String(s) => return Ok(s.to_string()),
            Value::Array(_) => return Err(anyhow!("field is a list, where literal expected")),
            Value::Object(_) => return Err(anyhow!("field is an object, where literal expected")),
        }
    }

    pub fn get_deterministic_semantics(sdfa: Rc<Self>) -> Result<Box<dyn StochasticDeterministicSemantics<DState = usize>>> {
        Ok(Box::new(StochasticDeterministicFiniteAutomatonSemantics::new(sdfa)))
    }

    pub fn get_stochastic_semantics(sdfa: Rc<Self>) -> EbiTraitStochasticSemantics {
        EbiTraitStochasticSemantics::Usize(Box::new(StochasticDeterministicFiniteAutomatonSemantics::new(sdfa)))
    }

    pub fn get_semantics(sdfa: Rc<Self>) -> EbiTraitSemantics {
        EbiTraitSemantics::Usize(Box::new(StochasticDeterministicFiniteAutomatonSemantics::new(sdfa)))
    }

    pub fn import_as_stochastic_deterministic_semantics(reader: &mut dyn BufRead) -> Result<EbiTraitStochasticDeterministicSemantics> {
        log::info!("convert SDFA to stochastic deterministic semantics");
        let sdfa = StochasticDeterministicFiniteAutomaton::import(reader)?;
        let s = Rc::new(sdfa);
        Ok(EbiTraitStochasticDeterministicSemantics::Usize(Self::get_deterministic_semantics(s)?))
    }

    pub fn import_as_stochastic_semantics(reader: &mut dyn BufRead) -> Result<EbiTraitStochasticSemantics> {
        let sdfa = Rc::new(Self::import(reader)?);
        Ok(Self::get_stochastic_semantics(sdfa))
    }

    pub fn import_as_semantics(reader: &mut dyn BufRead) -> Result<EbiTraitSemantics> {
        let sdfa = Rc::new(Self::import(reader)?);
        Ok(Self::get_semantics(sdfa))
    }
    
    pub fn to_stochastic_labelled_petri_net(self) -> StochasticLabelledPetriNet {
        log::info!("convert SDFA to stochastic labelled Petri net");
        let activity_key = self.activity_key;
        let mut transitions = vec![];
        let mut weights = vec![];
        let mut places = 2 + self.max_state; //0 = source, 1 = sink
        let mut initial_marking = Marking::new(places);
        initial_marking.increase(0, 1);

        //add edges
        let mut source_probability = Fraction::one();
        if let Some(mut source_last) = self.sources.get(0) {
            for (source, (target, (activity, probability))) in self.sources.iter().zip(self.targets.iter().zip(self.activities.iter().zip(self.probabilities.iter()))) {
                //add finalisation transition
                if source_last != source {
                    if !source_probability.is_zero() {
                        let mut transition = Transition::new_silent(transitions.len());
                        transition.incoming.push(2 + source_last);
                        transition.outgoing.push(1);
                        transitions.push(transition);
                        weights.push(source_probability);
                    }

                    source_probability = Fraction::one();
                    source_last = source;
                }
                
                //add transition
                let mut transition = Transition::new_labelled(transitions.len(), *activity);
                transition.incoming.push(2 + source);
                transition.outgoing.push(2 + target);
            }
        }

        StochasticLabelledPetriNet::from_fields(activity_key, places, transitions, initial_marking, weights)
    }

    pub fn set_activity_key(&mut self, activity_key: &ActivityKey) {
        self.activity_key = activity_key.clone();
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

        result.set_initial_state(Self::read_number(&json, "initialState").context("failed to read initial state")?);
        let jtrans = Self::read_list(&json, "transitions").context("failed to read list of transitions")?;
        for jtransition in jtrans {
            let from = Self::read_number(jtransition, "from").context("could not read from")?;
            let to = Self::read_number(jtransition, "to").context("could not read to")?;
            let label = Self::read_string(jtransition, "label").context("could not read label")?;
            let activity = result.activity_key.process_activity(label.as_str());
            let probability = Self::read_fraction(jtransition, "prob").context("could not read probability")?;

            result.add_transition(from, activity, to, probability)?;
        }

        return Ok(result);
    }
}

impl EbiTraitQueriableStochasticLanguage for StochasticDeterministicFiniteAutomaton {

    fn get_probability(&mut self, follower: &FollowerSemantics) -> Result<Fraction> {
        match follower {
            FollowerSemantics::Trace(trace) => {
                let mut state = self.get_initial_state();
                let mut result = Fraction::one();

                for activity in trace.iter() {
                    let (found, pos) = self.binary_search(state, self.activity_key.get_id_from_activity(activity));
                    if !found {
                        return Ok(Fraction::zero());
                    }
                    state = self.targets[pos];
                    result *= &self.probabilities[pos];
                }

                result *= &self.terminating_probabilities[state];

                Ok(result)
            },
        }
    }
    
    fn get_activity_key(&self) -> &ActivityKey {
        &self.activity_key
    }
    
    fn get_activity_key_mut(&mut self) -> &mut ActivityKey {
        &mut self.activity_key
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

struct StochasticDeterministicFiniteAutomatonSemantics {
    sdfa: Rc<StochasticDeterministicFiniteAutomaton>
}

impl StochasticDeterministicFiniteAutomatonSemantics {
    pub fn new(sdfa: Rc<StochasticDeterministicFiniteAutomaton>) -> Self {
        Self {
            sdfa: sdfa
        }
    }
}

impl StochasticDeterministicSemantics for StochasticDeterministicFiniteAutomatonSemantics {
    type DState = usize;

    fn get_activity_key(&self) -> &ActivityKey {
        self.sdfa.get_activity_key()
    }

    fn get_initial_state(&self) -> Result<Self::DState> {
        Ok(self.sdfa.get_initial_state())
    }

    fn execute_activity(&self, state: &Self::DState, activity: Activity) -> Result<Self::DState> {
        let (found, i) = self.sdfa.binary_search(*state, self.sdfa.activity_key.get_id_from_activity(activity));
        if found{
            Ok(self.sdfa.targets[i])
        } else {
            Err(anyhow!("activity not enabled"))
        }
    }

    fn get_termination_probability(&self, state: &Self::DState) -> Fraction {
        self.sdfa.get_termination_probability(*state).clone()
    }

    fn get_activity_probability(&self, state: &Self::DState, activity: Activity) -> Fraction {
        let (found, i) = self.sdfa.binary_search(*state, self.sdfa.activity_key.get_id_from_activity(activity));
        match found {
            true => self.sdfa.probabilities[i].clone(),
            false => Fraction::zero(),
        }
    }

    fn get_enabled_activities(&self, state: &Self::DState) -> Vec<Activity> {
        let mut result = vec![];

        let (_, mut i) = self.sdfa.binary_search(*state, 0);
        while i < self.sdfa.activities.len() && self.sdfa.sources[i] == *state {
            result.push(self.sdfa.activities[i]);
            i += 1;
        }

        return result;
    }
}

impl Semantics for StochasticDeterministicFiniteAutomatonSemantics {
    type State = usize; //In semantics, the final state must be a deadlock state. Therefore, for every state in which we can terminate, we add a virtual silent transition to a virtual deadlock state.

    fn get_activity_key(&self) -> &ActivityKey {
        self.sdfa.get_activity_key()
    }

    fn get_initial_state(&self) -> Self::State {
        self.sdfa.get_initial_state()
    }

    fn execute_transition(&self, state: &mut Self::State, transition: TransitionIndex) -> Result<()> {
        if self.is_final_state(state) {
            return Err(anyhow!("Cannot execute a transition in a final state in these semantics."))
        }

        if self.is_transition_silent(transition) {
            //request for termination
            if self.sdfa.get_termination_probability(*state).is_positive() {
                //termination possible
                *state = self.sdfa.max_state + 1;
                return Ok(());
            } else {
                //termination not possible
                return Err(anyhow!("Cannot execute a terminating transition in this state in these semantics."))
            }
        }

        let (found, i) = self.sdfa.binary_search(*state, transition);
        if found{
            *state = self.sdfa.targets[i];
            return Ok(())
        } else {
            return Err(anyhow!("Transition cannot fire as it is not enabled."));
        }
    }

    fn is_final_state(&self, state: &Self::State) -> bool {
        state > &self.sdfa.max_state
    }

    fn is_transition_silent(&self, transition: TransitionIndex) -> bool {
        transition > self.sdfa.targets.len()
    }

    fn get_transition_activity(&self, transition: TransitionIndex) -> Option<Activity> {
        if self.is_transition_silent(transition) {
            None
        } else {
            Some(self.sdfa.activities[transition])
        }
    }

    fn get_enabled_transitions(&self, state: &Self::State) -> Vec<TransitionIndex> {
        let mut result = vec![];

        let (_, mut i) = self.sdfa.binary_search(*state, 0);
        while i < self.sdfa.activities.len() && self.sdfa.sources[i] == *state {
            result.push(i);
            i += 1;
        }

        if self.is_final_state(state) {
            result.push(self.sdfa.targets.len() + 1)
        }

        return result;
    }
}

impl StochasticSemantics for StochasticDeterministicFiniteAutomatonSemantics {
    fn get_transition_weight(&self, state: &Self::State, transition: TransitionIndex) -> &Fraction {
        if self.is_transition_silent(transition) {
            //terminating transition
            &self.sdfa.get_termination_probability(*state)
        } else {
            &self.sdfa.probabilities[transition]
        }
    }

    fn get_total_weight_of_enabled_transitions(&self, state: &Self::State) -> Result<Fraction> {
        Ok(Fraction::one())
    }
}

impl<'a> IntoIterator for &'a StochasticDeterministicFiniteAutomaton {
    type Item = (&'a usize, &'a usize, &'a Activity, &'a Fraction);

    type IntoIter = StochasticDeterministicFiniteAutomatonIterator<'a>;

    fn into_iter(self) -> Self::IntoIter {
        Self::IntoIter {
            it_sources:  self.sources.iter(),
            it_targets:  self.targets.iter(),
            it_activities: self.activities.iter(),
            it_probabilities: self.probabilities.iter()
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