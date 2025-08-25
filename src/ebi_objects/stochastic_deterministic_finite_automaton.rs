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
    },
    ebi_traits::{
        ebi_trait_activities,
        ebi_trait_graphable::{self, EbiTraitGraphable},
        ebi_trait_queriable_stochastic_language::{self},
        ebi_trait_semantics::{EbiTraitSemantics, ToSemantics},
        ebi_trait_stochastic_deterministic_semantics::{
            EbiTraitStochasticDeterministicSemantics, ToStochasticDeterministicSemantics,
        },
        ebi_trait_stochastic_semantics::{EbiTraitStochasticSemantics, ToStochasticSemantics},
    },
    json,
};
use anyhow::{Context, Error, Result, anyhow};
use ebi_arithmetic::{
    ebi_number::{One, Signed},
    fraction::Fraction,
};
use ebi_derive::ActivityKey;
use layout::topo::layout::VisualGraph;
use serde_json::Value;
use std::{
    cmp::{Ordering, max},
    collections::HashMap,
    fmt,
    io::{self, BufRead},
    str::FromStr,
};

use super::stochastic_labelled_petri_net::StochasticLabelledPetriNet;

pub const FORMAT_SPECIFICATION: &str = "A stochastic deterministic finite automaton is a JSON structure with the top level being an object.
    This object contains the following key-value pairs:
    \\begin{itemize}
    \\item \\texttt{initialState} being the index of the initial state. This field is optional: if omitted, the SDFA has an empty stochastic language.
    \\item \\texttt{transitions} being a list of transitions. 
    Each transition is an object with \\texttt{from} being the source state index of the transition, 
    \\texttt{to} being the target state index of the transition, 
    \\texttt{label} being the activity of the transition, and
    \\texttt{prob} being the probability of the transition (may be given as a fraction in a string or a float value. Must be $\\leq 1$). 
    Silent transitions are not supported.
    The file format supports deadlocks and livelocks.
    The probability that a trace terminates in a state is 1 - the sum probability of the outgoing transitions of the state.
    \\end{itemize}
    For instance:
    \\lstinputlisting[language=json, style=boxed]{../testfiles/aa-ab-ba.sdfa}";

pub const EBI_STOCHASTIC_DETERMINISTIC_FINITE_AUTOMATON: EbiFileHandler = EbiFileHandler {
    name: "stochastic deterministic finite automaton",
    article: "a",
    file_extension: "sdfa",
    is_binary: false,
    format_specification: &FORMAT_SPECIFICATION,
    validator: Some(ebi_input::validate::<StochasticDeterministicFiniteAutomaton>),
    trait_importers: &[
        EbiTraitImporter::Activities(
            ebi_trait_activities::import::<StochasticDeterministicFiniteAutomaton>,
        ),
        EbiTraitImporter::QueriableStochasticLanguage(
            ebi_trait_queriable_stochastic_language::import::<StochasticDeterministicFiniteAutomaton>,
        ),
        EbiTraitImporter::StochasticDeterministicSemantics(
            StochasticDeterministicFiniteAutomaton::import_as_stochastic_deterministic_semantics,
        ),
        EbiTraitImporter::StochasticSemantics(
            StochasticDeterministicFiniteAutomaton::import_as_stochastic_semantics,
        ),
        EbiTraitImporter::Semantics(StochasticDeterministicFiniteAutomaton::import_as_semantics),
        EbiTraitImporter::Graphable(
            ebi_trait_graphable::import::<StochasticDeterministicFiniteAutomaton>,
        ),
    ],
    object_importers: &[
        EbiObjectImporter::StochasticDeterministicFiniteAutomaton(
            StochasticDeterministicFiniteAutomaton::import_as_object,
        ),
        EbiObjectImporter::LabelledPetriNet(
            StochasticDeterministicFiniteAutomaton::import_as_labelled_petri_net,
        ),
    ],
    object_exporters: &[
        EbiObjectExporter::StochasticDeterministicFiniteAutomaton(
            StochasticDeterministicFiniteAutomaton::export_from_object,
        ),
        EbiObjectExporter::FiniteStochasticLanguage(
            StochasticDeterministicFiniteAutomaton::export_from_object,
        ),
        EbiObjectExporter::EventLog(StochasticDeterministicFiniteAutomaton::export_from_object),
    ],
    java_object_handlers: &[],
};

#[derive(Debug, ActivityKey, Clone)]
pub struct StochasticDeterministicFiniteAutomaton {
    pub(crate) activity_key: ActivityKey,
    pub(crate) initial_state: Option<usize>,
    pub(crate) max_state: usize,
    pub(crate) sources: Vec<usize>,       //transition -> source of arc
    pub(crate) targets: Vec<usize>,       //transition -> target of arc
    pub(crate) activities: Vec<Activity>, //transition -> activity of arc (every transition is labelled)
    pub(crate) probabilities: Vec<Fraction>, //transition -> probability of arc
    pub(crate) terminating_probabilities: Vec<Fraction>, //state -> termination probability
}

impl StochasticDeterministicFiniteAutomaton {
    /**
     * Creates a new SDFA with an initial state. That is, it will have the stochastic language with the empty trace.
     */
    pub fn new() -> Self {
        Self {
            activity_key: ActivityKey::new(),
            max_state: 0,
            initial_state: Some(0),
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

    pub fn set_initial_state(&mut self, state: Option<usize>) {
        if let Some(state) = state {
            self.ensure_states(state);
        }
        self.initial_state = state;
    }

    pub fn can_terminate_in_state(&self, state: usize) -> bool {
        self.get_termination_probability(state).is_positive()
    }

    /**
     * Returns whether a transition is not permanently disabled.
     */
    pub fn can_execute_transition(&self, transition: usize) -> bool {
        self.probabilities[transition].is_positive()
    }

    fn ensure_states(&mut self, new_max_state: usize) {
        if new_max_state > self.max_state {
            self.terminating_probabilities
                .extend(vec![Fraction::one(); new_max_state - self.max_state]);
            self.max_state = new_max_state;

            assert!(self.terminating_probabilities.len() == self.max_state + 1)
        }
    }

    pub fn add_transition(
        &mut self,
        source: usize,
        activity: Activity,
        target: usize,
        probability: Fraction,
    ) -> Result<()> {
        self.ensure_states(max(source, target));

        let (found, from) =
            self.binary_search(source, self.activity_key.get_id_from_activity(activity));
        if found {
            //edge already present
            Err(anyhow!(
                "tried to insert edge from {} to {}, which would violate the determinism of the SDFA",
                source,
                target
            ))
        } else {
            self.sources.insert(from, source);
            self.targets.insert(from, target);
            self.activities.insert(from, activity);
            self.terminating_probabilities[source] -= &probability;
            self.probabilities.insert(from, probability);

            if self.terminating_probabilities[source].is_negative() {
                Err(anyhow!(
                    "tried to insert edge from {} to {}, which brings the sum outgoing probability of the source state (1-{}) above 1",
                    source,
                    target,
                    self.terminating_probabilities[source]
                ))
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
    pub fn take_or_add_transition(
        &mut self,
        source_state: usize,
        activity: Activity,
        probability: Fraction,
    ) -> usize {
        self.terminating_probabilities[source_state] -= &probability;

        let (found, transition) = self.binary_search(
            source_state,
            self.activity_key.get_id_from_activity(activity),
        );
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
        let mut new_terminating_probabilities =
            vec![Fraction::one(); self.terminating_probabilities.len()];
        for (state, _, _, outgoing_probability) in &mut self.into_iter() {
            if let Some(factor) = state2scale.get(state) {
                *outgoing_probability /= factor;
            }
            new_terminating_probabilities[*state] -= outgoing_probability;
        }
        self.terminating_probabilities = new_terminating_probabilities;
    }

    pub fn get_initial_state(&self) -> Option<usize> {
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

    pub fn import_as_labelled_petri_net(reader: &mut dyn BufRead) -> Result<EbiObject> {
        let sdfa = Self::import(reader)?;
        Ok(EbiObject::LabelledPetriNet(
            Into::<StochasticLabelledPetriNet>::into(sdfa).into(),
        ))
    }

    pub fn set_activity_key(&mut self, activity_key: &ActivityKey) {
        self.activity_key = activity_key.clone();
    }
}

impl TranslateActivityKey for StochasticDeterministicFiniteAutomaton {
    fn translate_using_activity_key(&mut self, to_activity_key: &mut ActivityKey) {
        let translator = ActivityKeyTranslator::new(&self.activity_key, to_activity_key);
        self.activities
            .iter_mut()
            .for_each(|activity| *activity = translator.translate_activity(&activity));
        self.activity_key = to_activity_key.clone();
    }
}

impl FromEbiTraitObject for StochasticDeterministicFiniteAutomaton {
    fn from_trait_object(object: EbiInput) -> Result<Box<Self>> {
        match object {
            EbiInput::Object(EbiObject::StochasticDeterministicFiniteAutomaton(e), _) => {
                Ok(Box::new(e))
            }
            _ => Err(anyhow!(
                "cannot read {} {} as a stochastic deterministic finite automaton",
                object.get_type().get_article(),
                object.get_type()
            )),
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
        Ok(EbiObject::StochasticDeterministicFiniteAutomaton(
            Self::import(reader)?,
        ))
    }

    fn import(reader: &mut dyn BufRead) -> Result<Self>
    where
        Self: Sized,
    {
        let json: Value = serde_json::from_reader(reader)?;

        let mut result = StochasticDeterministicFiniteAutomaton::new();

        result.set_initial_state(json::read_field_number(&json, "initialState").ok());
        let jtrans = json::read_field_list(&json, "transitions")
            .context("failed to read list of transitions")?;
        for (i, jtransition) in jtrans.iter().enumerate() {
            let from = json::read_field_number(jtransition, "from")
                .with_context(|| format!("could not read source of transition {}", i))?;
            let to = json::read_field_number(jtransition, "to")
                .with_context(|| format!("could not read destination of transition {}", i))?;
            let label = json::read_field_string(jtransition, "label")
                .with_context(|| format!("could not read label of transition {}", i))?;
            let activity = result.activity_key.process_activity(label.as_str());
            let probability = json::read_field_fraction(jtransition, "prob")
                .with_context(|| format!("could not read probability on transition {}", i))?;

            result.add_transition(from, activity, to, probability)?;
        }

        return Ok(result);
    }
}

impl Exportable for StochasticDeterministicFiniteAutomaton {
    fn export_from_object(object: EbiOutput, f: &mut dyn std::io::Write) -> Result<()> {
        match object {
            EbiOutput::Object(EbiObject::StochasticDeterministicFiniteAutomaton(sdfa)) => {
                sdfa.export(f)
            }
            EbiOutput::Object(EbiObject::FiniteStochasticLanguage(slang)) => {
                Into::<Self>::into(slang).export(f)
            }
            EbiOutput::Object(EbiObject::EventLog(log)) => Into::<Self>::into(log).export(f),
            _ => Err(anyhow!("Cannot export as SDFA.")),
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
        writeln!(
            f,
            "Number of activities\t{}",
            self.activity_key.get_number_of_activities()
        )?;

        writeln!(f, "")?;
        self.get_activity_key().info(f)?;

        Ok(write!(f, "")?)
    }
}

impl fmt::Display for StochasticDeterministicFiniteAutomaton {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        writeln!(f, "{{")?;
        if let Some(state) = self.get_initial_state() {
            writeln!(f, "\"initialState\": {},", state)?;
        }
        writeln!(f, "\"transitions\": [")?;
        for pos in 0..self.sources.len() {
            write!(
                f,
                "{{\"from\":{},\"to\":{},\"label\":\"{}\",\"prob\":\"{}\"}}",
                self.sources[pos],
                self.targets[pos],
                self.activity_key.get_activity_label(&self.activities[pos]),
                self.probabilities[pos]
            )?;
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

impl EbiTraitGraphable for StochasticDeterministicFiniteAutomaton {
    fn to_dot(&self) -> Result<layout::topo::layout::VisualGraph> {
        log::info!("to_dot for StochasticDeterministicFiniteAutomaton");
        let mut graph = VisualGraph::new(layout::core::base::Orientation::LeftToRight);

        let mut places = vec![];
        for state in 0..=self.max_state {
            places.push(<dyn EbiTraitGraphable>::create_place(
                &mut graph,
                &format!("{}", self.terminating_probabilities[state]),
            ));
            // places.push(<dyn Dottable>::create_place(&mut graph, ""));
        }

        for pos in 0..self.sources.len() {
            let source = places[self.sources[pos]];
            let target = places[self.targets[pos]];
            let probability = &self.probabilities[pos];
            let activity = self.activity_key.get_activity_label(&self.activities[pos]);

            <dyn EbiTraitGraphable>::create_edge(
                &mut graph,
                &source,
                &target,
                &format!("{}, {}", activity, probability.to_string()),
            );
        }

        Ok(graph)
    }
}

impl<'a> IntoIterator for &'a StochasticDeterministicFiniteAutomaton {
    type Item = (&'a usize, &'a usize, &'a Activity, &'a Fraction);

    type IntoIter = StochasticDeterministicFiniteAutomatonIterator<'a>;

    fn into_iter(self) -> Self::IntoIter {
        Self::IntoIter {
            it_sources: self.get_sources().iter(),
            it_targets: self.get_targets().iter(),
            it_activities: self.get_activities().iter(),
            it_probabilities: self.get_probabilities().iter(),
        }
    }
}

pub struct StochasticDeterministicFiniteAutomatonIterator<'a> {
    it_sources: std::slice::Iter<'a, usize>,
    it_targets: std::slice::Iter<'a, usize>,
    it_activities: std::slice::Iter<'a, Activity>,
    it_probabilities: std::slice::Iter<'a, Fraction>,
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
            it_sources: self.sources.iter(),
            it_targets: self.targets.iter(),
            it_activities: self.activities.iter(),
            it_probabilities: self.probabilities.iter_mut(),
        }
    }
}

pub struct StochasticDeterministicFiniteAutomatonMutIterator<'a> {
    it_sources: std::slice::Iter<'a, usize>,
    it_targets: std::slice::Iter<'a, usize>,
    it_activities: std::slice::Iter<'a, Activity>,
    it_probabilities: std::slice::IterMut<'a, Fraction>,
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

#[cfg(test)]
mod tests {
    use std::fs;

    use crate::ebi_traits::ebi_trait_semantics::{EbiTraitSemantics, ToSemantics};

    use super::StochasticDeterministicFiniteAutomaton;

    #[test]
    fn sdfa_empty() {
        let fin = fs::read_to_string("testfiles/empty.sdfa").unwrap();
        let dfa = fin
            .parse::<StochasticDeterministicFiniteAutomaton>()
            .unwrap();

        if let EbiTraitSemantics::Usize(semantics) = dfa.to_semantics() {
            assert!(semantics.get_initial_state().is_none());
        } else {
            assert!(false);
        }
    }
}
