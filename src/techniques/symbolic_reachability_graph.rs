// use ebi_arithmetic::anyhow::Result;
// use rustc_hash::FxHashMap;
// use std::collections::VecDeque;

// use crate::{
//     Activity, ActivityKey, ActivityKeyTranslator, EbiObject, Exportable, Graphable, HasActivityKey,
//     Importable, Infoable, TranslateActivityKey, dfg_format_comparison,
//     line_reader::LineReader,
//     traits::{
//         graphable,
//         importable::{ImporterParameter, ImporterParameterValues, from_string},
//     },
// };
// #[cfg(any(test, feature = "testactivities"))]
// use ebi_activity_key::TestActivityKey;
// use ebi_arithmetic::anyhow::{Context, Result, anyhow, Error};
// use ebi_derive::ActivityKey;
// use layout::topo::layout::VisualGraph;
// use std::{
//     cmp::{Ordering, max},
//     fmt::Display,
// };

// pub const HEADER: &str = "stochastic non-deterministic finite automaton";

// // ---------------------------------------------------------------------------
// // Symbolic probability: weight(numerator) / sum( weight(d) for d in denominator )
// // ---------------------------------------------------------------------------

// /// A symbolic (parametric) probability expression.
// ///
// /// Represents the probability  `w_{numerator} / (w_{d_1} + w_{d_2} + …)`
// /// where `numerator` is the index of one transition in the labelled Petri net
// /// and `denominator` lists *all* transitions that are enabled in the same
// /// marking (including the numerator transition itself).
// #[derive(Clone, Debug, PartialEq, Eq, Hash)]
// pub struct SymbolicProbability {
//     /// Transition index whose weight appears in the numerator.
//     pub numerator: usize,
//     /// Transition indices of *all* enabled transitions in the source marking;
//     /// the sum of their weights forms the denominator.
//     pub denominator: Vec<usize>,
// }

// impl Display for SymbolicProbability {
//     fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
//         write!(f, "w_{}", self.numerator)?;
//         write!(f, " / (")?;
//         for (i, d) in self.denominator.iter().enumerate() {
//             if i > 0 {
//                 write!(f, " + ")?;
//             }
//             write!(f, "w_{}", d)?;
//         }
//         write!(f, ")")
//     }
// }

// // ---------------------------------------------------------------------------
// // Stochastic NFA with symbolic probabilities
// // ---------------------------------------------------------------------------

// #[derive(Clone, Debug, ActivityKey)]
// pub struct StochasticNondeterministicFiniteAutomaton {
//     pub activity_key: ActivityKey,

//     /// If the initial state is None, then the language is empty.
//     pub initial_state: Option<usize>,

//     pub sources: Vec<usize>,                          // transition -> source state
//     pub targets: Vec<usize>,                          // transition -> target state
//     pub activities: Vec<Option<Activity>>,             // transition -> activity label
//     pub probabilities: Vec<SymbolicProbability>,       // transition -> symbolic probability

//     /// Whether a state is a deadlock (no enabled transitions in the
//     /// corresponding Petri-net marking).  A deadlock state implicitly has
//     /// termination probability 1.
//     pub is_deadlock: Vec<bool>,
// }

// impl StochasticNondeterministicFiniteAutomaton {
//     /// Create an empty SNFA with a single initial / final (deadlock) state.
//     pub fn new() -> Self {
//         Self {
//             activity_key: ActivityKey::new(),
//             initial_state: Some(0),
//             sources: vec![],
//             targets: vec![],
//             activities: vec![],
//             probabilities: vec![],
//             is_deadlock: vec![true],
//         }
//     }

//     pub fn get_sources(&self) -> &Vec<usize> {
//         &self.sources
//     }

//     pub fn get_targets(&self) -> &Vec<usize> {
//         &self.targets
//     }

//     pub fn get_activities(&self) -> &Vec<Option<Activity>> {
//         &self.activities
//     }

//     pub fn get_probabilities(&self) -> &Vec<SymbolicProbability> {
//         &self.probabilities
//     }

//     pub fn set_initial_state(&mut self, state: Option<usize>) {
//         if let Some(state) = state {
//             self.ensure_states(state);
//         }
//         self.initial_state = state;
//     }

//     /// A state can terminate if it is a deadlock state (no outgoing transitions
//     /// in the Petri-net sense).
//     pub fn can_terminate_in_state(&self, state: usize) -> bool {
//         self.is_deadlock[state]
//     }

//     /// Ensures that a state with index `new_max_state` exists.
//     fn ensure_states(&mut self, new_max_state: usize) {
//         while self.number_of_states() <= new_max_state {
//             self.add_state();
//         }
//     }

//     /// Add a transition with a symbolic probability.
//     ///
//     /// Each call inserts a new edge even if the same `(source, activity,
//     /// target)` triple already exists, because symbolic probabilities from
//     /// different Petri-net transitions cannot be merged algebraically.
//     pub fn add_transition(
//         &mut self,
//         source: usize,
//         activity: Option<Activity>,
//         target: usize,
//         probability: SymbolicProbability,
//     ) {
//         self.ensure_states(max(source, target));

//         // The source state has at least one outgoing transition, so it is not
//         // a deadlock.
//         self.is_deadlock[source] = false;

//         // Find the insertion point that keeps edges sorted by
//         // (source, activity, target).
//         let (_, from) = self.binary_search(source, self.label_to_id(activity), target);

//         self.sources.insert(from, source);
//         self.targets.insert(from, target);
//         self.activities.insert(from, activity);
//         self.probabilities.insert(from, probability);
//     }

//     pub fn number_of_transitions(&self) -> usize {
//         self.sources.len()
//     }

//     /// Returns the number of states.
//     pub fn number_of_states(&self) -> usize {
//         self.is_deadlock.len()
//     }

//     pub fn get_initial_state(&self) -> Option<usize> {
//         self.initial_state
//     }

//     /// Adds a new state that is initially a deadlock (termination probability
//     /// 1).  Returns its index.
//     pub fn add_state(&mut self) -> usize {
//         let state = self.is_deadlock.len();
//         self.is_deadlock.push(true);
//         state
//     }

//     // -- ordering helpers (kept from original) --------------------------------

//     fn compare(
//         source1: usize,
//         activity1: usize,
//         target1: usize,
//         source2: usize,
//         activity2: usize,
//         target2: usize,
//     ) -> Ordering {
//         if source1 < source2 {
//             return Ordering::Greater;
//         } else if source1 > source2 {
//             return Ordering::Less;
//         } else if activity2 > activity1 {
//             return Ordering::Greater;
//         } else if activity2 < activity1 {
//             return Ordering::Less;
//         } else if target1 < target2 {
//             return Ordering::Greater;
//         } else if target1 > target2 {
//             return Ordering::Less;
//         } else {
//             return Ordering::Equal;
//         }
//     }

//     pub fn label_to_id(&self, label: Option<Activity>) -> usize {
//         if let Some(activity) = label {
//             1 + activity.id
//         } else {
//             0
//         }
//     }

//     pub fn binary_search(&self, source: usize, activity: usize, target: usize) -> (bool, usize) {
//         if self.sources.is_empty() {
//             return (false, 0);
//         }

//         let mut size = self.sources.len();
//         let mut left = 0;
//         let mut right = size;
//         while left < right {
//             let mid = left + size / 2;

//             let cmp = Self::compare(
//                 source,
//                 activity,
//                 target,
//                 self.sources[mid],
//                 self.label_to_id(self.activities[mid]),
//                 self.targets[mid],
//             );

//             left = if cmp == Ordering::Less { mid + 1 } else { left };
//             right = if cmp == Ordering::Greater { mid } else { right };
//             if cmp == Ordering::Equal {
//                 assert!(mid < self.sources.len());
//                 return (true, mid);
//             }

//             size = right - left;
//         }

//         assert!(left <= self.sources.len());
//         (false, left)
//     }

//     /// Returns an iterator over the outgoing edges of `source`.
//     /// That is, (source, target, label, symbolic_probability)
//     pub fn outgoing_edges(
//         &'_ self,
//         source: usize,
//     ) -> StochasticNondeterministicFiniteAutomatonIterator<'_> {
//         let (_, start) = self.binary_search(source, 0, 0);
//         let (_, end) = self.binary_search(source, usize::MAX, usize::MAX);
//         StochasticNondeterministicFiniteAutomatonIterator {
//             it_sources: self.get_sources()[start..end].iter(),
//             it_targets: self.get_targets()[start..end].iter(),
//             it_activities: self.get_activities()[start..end].iter(),
//             it_probabilities: self.get_probabilities()[start..end].iter(),
//         }
//     }
// }

// // ---------------------------------------------------------------------------
// // TranslateActivityKey
// // ---------------------------------------------------------------------------

// impl TranslateActivityKey for StochasticNondeterministicFiniteAutomaton {
//     fn translate_using_activity_key(&mut self, to_activity_key: &mut ActivityKey) {
//         let translator = ActivityKeyTranslator::new(&self.activity_key, to_activity_key);

//         let mut sources = Vec::with_capacity(self.number_of_transitions());
//         let mut targets = Vec::with_capacity(self.number_of_transitions());
//         let mut probabilities = Vec::with_capacity(self.number_of_transitions());
//         let mut activities = Vec::with_capacity(self.number_of_transitions());

//         std::mem::swap(&mut self.sources, &mut sources);
//         std::mem::swap(&mut self.targets, &mut targets);
//         std::mem::swap(&mut self.probabilities, &mut probabilities);
//         std::mem::swap(&mut self.activities, &mut activities);

//         for (source, (target, (probability, activity))) in sources.into_iter().zip(
//             targets
//                 .into_iter()
//                 .zip(probabilities.into_iter().zip(activities.into_iter())),
//         ) {
//             let activity = if let Some(activity) = activity {
//                 Some(translator.translate_activity(&activity))
//             } else {
//                 None
//             };
//             let (_, from) = self.binary_search(source, self.label_to_id(activity), target);

//             self.sources.insert(from, source);
//             self.targets.insert(from, target);
//             self.activities.insert(from, activity);
//             self.probabilities.insert(from, probability);
//         }
//         self.activity_key = to_activity_key.clone();
//     }
// }

// // ---------------------------------------------------------------------------
// // Display
// // ---------------------------------------------------------------------------

// impl Display for StochasticNondeterministicFiniteAutomaton {
//     fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
//         writeln!(f, "{}", HEADER)?;
//         if let Some(i) = self.initial_state {
//             writeln!(f, "# initial state\n{:?}", i)?;
//         } else {
//             writeln!(f, "#initial state\nnone")?;
//         }
//         writeln!(f, "# number of states\n{}", self.number_of_states())?;
//         writeln!(
//             f,
//             "# number of transitions\n{}",
//             self.number_of_transitions()
//         )?;

//         for transition in 0..self.number_of_transitions() {
//             writeln!(
//                 f,
//                 "# transition {}\n# source\n{}\n# target\n{}\n# probability\n{}",
//                 transition,
//                 self.sources[transition],
//                 self.targets[transition],
//                 self.probabilities[transition]
//             )?;
//             if let Some(act) = self.activities[transition] {
//                 writeln!(
//                     f,
//                     "# activity\nlabel {}",
//                     self.activity_key.get_activity_label(&act)
//                 )?;
//             } else {
//                 writeln!(f, "# activity\nsilent")?;
//             }
//         }
//         Ok(())
//     }
// }

// // ---------------------------------------------------------------------------
// // Infoable
// // ---------------------------------------------------------------------------

// impl Infoable for StochasticNondeterministicFiniteAutomaton {
//     fn info(&self, f: &mut impl std::io::Write) -> Result<()> {
//         writeln!(f, "Number of states\t{}", self.number_of_states())?;
//         writeln!(f, "Number of transitions\t{}", self.number_of_transitions())?;
//         writeln!(
//             f,
//             "Number of activities\t{}",
//             self.activity_key.get_number_of_activities()
//         )?;
//         writeln!(
//             f,
//             "Number of deadlock states\t{}",
//             self.is_deadlock.iter().filter(|&&d| d).count()
//         )?;

//         writeln!(f, "")?;
//         self.activity_key().info(f)?;

//         Ok(writeln!(f, "")?)
//     }
// }

// // ---------------------------------------------------------------------------
// // Graphable
// // ---------------------------------------------------------------------------

// impl Graphable for StochasticNondeterministicFiniteAutomaton {
//     fn to_dot(&self) -> Result<layout::topo::layout::VisualGraph> {
//         let mut graph = VisualGraph::new(layout::core::base::Orientation::LeftToRight);

//         let mut places = vec![];
//         for (state, &deadlock) in self.is_deadlock.iter().enumerate() {
//             let label = if deadlock {
//                 format!("s{} [dead]", state)
//             } else {
//                 format!("s{}", state)
//             };
//             places.push(graphable::create_place(&mut graph, &label));
//         }

//         for transition in 0..self.number_of_transitions() {
//             let source = places[self.sources[transition]];
//             let target = places[self.targets[transition]];
//             let probability = &self.probabilities[transition];
//             let activity = if let Some(activity) = self.activities[transition] {
//                 self.activity_key.get_activity_label(&activity)
//             } else {
//                 "τ"
//             };

//             graphable::create_edge(
//                 &mut graph,
//                 &source,
//                 &target,
//                 &format!("{}, {}", activity, probability),
//             );
//         }

//         Ok(graph)
//     }
// }

// // ---------------------------------------------------------------------------
// // Iterators
// // ---------------------------------------------------------------------------

// impl<'a> IntoIterator for &'a StochasticNondeterministicFiniteAutomaton {
//     type Item = (
//         &'a usize,
//         &'a usize,
//         &'a Option<Activity>,
//         &'a SymbolicProbability,
//     );

//     type IntoIter = StochasticNondeterministicFiniteAutomatonIterator<'a>;

//     fn into_iter(self) -> Self::IntoIter {
//         Self::IntoIter {
//             it_sources: self.get_sources().iter(),
//             it_targets: self.get_targets().iter(),
//             it_activities: self.get_activities().iter(),
//             it_probabilities: self.get_probabilities().iter(),
//         }
//     }
// }

// pub struct StochasticNondeterministicFiniteAutomatonIterator<'a> {
//     it_sources: std::slice::Iter<'a, usize>,
//     it_targets: std::slice::Iter<'a, usize>,
//     it_activities: std::slice::Iter<'a, Option<Activity>>,
//     it_probabilities: std::slice::Iter<'a, SymbolicProbability>,
// }

// impl<'a> Iterator for StochasticNondeterministicFiniteAutomatonIterator<'a> {
//     type Item = (
//         &'a usize,
//         &'a usize,
//         &'a Option<Activity>,
//         &'a SymbolicProbability,
//     );

//     fn next(&mut self) -> Option<Self::Item> {
//         if let Some(source) = self.it_sources.next() {
//             let target = self.it_targets.next().unwrap();
//             let activity = self.it_activities.next().unwrap();
//             let probability = self.it_probabilities.next().unwrap();
//             Some((source, target, activity, probability))
//         } else {
//             None
//         }
//     }

//     fn size_hint(&self) -> (usize, Option<usize>) {
//         self.it_sources.size_hint()
//     }
// }

// impl<'a> IntoIterator for &'a mut StochasticNondeterministicFiniteAutomaton {
//     type Item = (
//         &'a usize,
//         &'a usize,
//         &'a Option<Activity>,
//         &'a mut SymbolicProbability,
//     );

//     type IntoIter = StochasticNondeterministicFiniteAutomatonMutIterator<'a>;

//     fn into_iter(self) -> Self::IntoIter {
//         Self::IntoIter {
//             it_sources: self.sources.iter(),
//             it_targets: self.targets.iter(),
//             it_activities: self.activities.iter(),
//             it_probabilities: self.probabilities.iter_mut(),
//         }
//     }
// }

// pub struct StochasticNondeterministicFiniteAutomatonMutIterator<'a> {
//     it_sources: std::slice::Iter<'a, usize>,
//     it_targets: std::slice::Iter<'a, usize>,
//     it_activities: std::slice::Iter<'a, Option<Activity>>,
//     it_probabilities: std::slice::IterMut<'a, SymbolicProbability>,
// }

// impl<'a> Iterator for StochasticNondeterministicFiniteAutomatonMutIterator<'a> {
//     type Item = (
//         &'a usize,
//         &'a usize,
//         &'a Option<Activity>,
//         &'a mut SymbolicProbability,
//     );

//     fn next(&mut self) -> Option<Self::Item> {
//         if let Some(source) = self.it_sources.next() {
//             let target = self.it_targets.next().unwrap();
//             let activity = self.it_activities.next().unwrap();
//             let probability = self.it_probabilities.next().unwrap();
//             Some((source, target, activity, probability))
//         } else {
//             None
//         }
//     }

//     fn size_hint(&self) -> (usize, Option<usize>) {
//         self.it_sources.size_hint()
//     }
// }

// /// Build the parametrized (symbolic) reachability graph of a labelled Petri net.
// ///
// /// Every transition in the Petri net – including silent ones – is assumed to
// /// carry a symbolic weight.  The resulting SNFA encodes each edge probability
// /// as a [`SymbolicProbability`] whose `numerator` is the index of the fired
// /// transition and whose `denominator` lists **all** transitions that are
// /// enabled in the source marking.
// pub fn build_stochastic_reachability_graph(
//     net: &LabelledPetriNet,
// ) -> Result<StochasticNondeterministicFiniteAutomaton> {
//     // Create a new SNFA without the default initial state
//     let mut snfa = StochasticNondeterministicFiniteAutomaton::new();
//     snfa.activity_key = net.activity_key().clone();

//     // Reachability exploration queue
//     let mut state2snfa_state: FxHashMap<Marking, usize> = FxHashMap::default();
//     let mut queue: VecDeque<Marking> = VecDeque::new();

//     // Insert the initial state of the Petri net
//     let initial_state = if let Some(initial) = net.get_initial_state() {
//         initial
//     } else {
//         return Ok(snfa);
//     };
//     state2snfa_state.insert(initial_state.clone(), 0);
//     queue.push_back(initial_state);

//     while let Some(state) = queue.pop_front() {
//         let snfa_state = *state2snfa_state.get(&state).unwrap();

//         // Collect enabled transitions in this marking
//         let enabled_transitions = net.get_enabled_transitions(&state);
//         if enabled_transitions.is_empty() {
//             // Deadlock state -> already marked with is_deadlock = true
//             continue;
//         }

//         for &transition in &enabled_transitions {
//             // Build the symbolic probability for this transition:
//             //   numerator   = transition (the one being fired)
//             //   denominator = all enabled transitions in this marking
//             let prob = SymbolicProbability {
//                 numerator: transition,
//                 denominator: enabled_transitions.clone(),
//             };

//             // Fire transition (creates a successor marking)
//             let mut next_state = state.clone();
//             net.execute_transition(&mut next_state, transition)?;

//             // Map / enqueue successor
//             let snfa_target = *state2snfa_state
//                 .entry(next_state.clone())
//                 .or_insert_with(|| {
//                     let new_state = snfa.add_state();
//                     queue.push_back(next_state);
//                     new_state
//                 });

//             // Transition label (None for silent transitions)
//             let label = net.get_transition_label(transition);

//             snfa.add_transition(snfa_state, label, snfa_target, prob);
//         }
//     }

//     // The first discovered marking is the initial state of the SNFA
//     snfa.initial_state = Some(0);
//     Ok(snfa)
// }