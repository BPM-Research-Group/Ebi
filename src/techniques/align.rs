use anyhow::{Context, Error, Result, anyhow};
use ebi_objects::{
    Activity, ActivityKeyTranslator, DeterministicFiniteAutomaton, DirectlyFollowsGraph,
    DirectlyFollowsModel, LabelledPetriNet, LanguageOfAlignments, ProcessTree,
    StochasticDeterministicFiniteAutomaton, StochasticDirectlyFollowsModel,
    StochasticLabelledPetriNet, StochasticLanguageOfAlignments, StochasticProcessTree,
    ebi_objects::{labelled_petri_net::TransitionIndex, language_of_alignments::Move},
};
use rayon::iter::{IntoParallelIterator, ParallelIterator};
use std::{
    fmt::{Debug, Display},
    hash::Hash,
    sync::{Arc, Mutex},
};

use crate::{
    ebi_framework::{displayable::Displayable, ebi_command::EbiCommand},
    ebi_traits::{
        ebi_trait_finite_language::EbiTraitFiniteLanguage,
        ebi_trait_finite_stochastic_language::EbiTraitFiniteStochasticLanguage,
        ebi_trait_semantics::EbiTraitSemantics,
    },
    semantics::{
        finite_stochastic_language_semantics::FiniteStochasticLanguageSemantics,
        labelled_petri_net_semantics::LPNMarking, process_tree_semantics::NodeStates,
        semantics::Semantics,
    },
};

pub trait Align {
    fn align_language(
        &mut self,
        log: Box<dyn EbiTraitFiniteLanguage>,
    ) -> Result<LanguageOfAlignments>;

    fn align_stochastic_language(
        &mut self,
        log: Box<dyn EbiTraitFiniteStochasticLanguage>,
    ) -> Result<StochasticLanguageOfAlignments>;

    /**
     * Please note to ensure the trace and the semantics use the same ActivityKey, or they have been translated
     */
    fn align_trace(&self, trace: &Vec<Activity>) -> Result<(Vec<Move>, usize)>;
}

impl Align for EbiTraitSemantics {
    fn align_language(
        &mut self,
        log: Box<dyn EbiTraitFiniteLanguage>,
    ) -> Result<LanguageOfAlignments> {
        match self {
            EbiTraitSemantics::Usize(sem) => sem.align_language(log),
            EbiTraitSemantics::Marking(sem) => sem.align_language(log),
            EbiTraitSemantics::NodeStates(sem) => sem.align_language(log),
        }
    }

    fn align_stochastic_language(
        &mut self,
        log: Box<dyn EbiTraitFiniteStochasticLanguage>,
    ) -> Result<StochasticLanguageOfAlignments> {
        match self {
            EbiTraitSemantics::Usize(sem) => sem.align_stochastic_language(log),
            EbiTraitSemantics::Marking(sem) => sem.align_stochastic_language(log),
            EbiTraitSemantics::NodeStates(sem) => sem.align_stochastic_language(log),
        }
    }

    fn align_trace(&self, trace: &Vec<Activity>) -> Result<(Vec<Move>, usize)> {
        match self {
            EbiTraitSemantics::Usize(sem) => sem.align_trace(trace),
            EbiTraitSemantics::Marking(sem) => sem.align_trace(trace),
            EbiTraitSemantics::NodeStates(sem) => sem.align_trace(trace),
        }
    }
}

impl<T, State> Align for T
where
    T: Semantics<SemState = State> + Send + Sync + ?Sized,
    State: Displayable,
{
    fn align_language(
        &mut self,
        log: Box<dyn EbiTraitFiniteLanguage>,
    ) -> Result<LanguageOfAlignments> {
        let mut activity_key = self.activity_key().clone();
        let translator = Arc::new(ActivityKeyTranslator::new(
            log.activity_key(),
            &mut activity_key,
        ));
        let log = Arc::new(log);
        let error: Arc<Mutex<Option<Error>>> = Arc::new(Mutex::new(None));

        log::info!("Compute alignments");
        let progress_bar = EbiCommand::get_progress_bar_ticks(log.number_of_traces());

        //compute alignments multi-threadedly
        let mut aligned_traces = (0..log.number_of_traces())
            .into_par_iter()
            .filter_map(|trace_index| {
                let log = Arc::clone(&log);
                let translator = Arc::clone(&translator);
                let self2 = Arc::from(&self);

                let trace = log.get_trace(trace_index).unwrap();
                let trace_translated = translator.translate_trace(trace);

                let result = self2
                    .align_trace(&trace_translated)
                    .with_context(|| format!("Aligning trace {:?}", trace));
                progress_bar.inc(1);

                match result {
                    Ok((aligned_trace, _)) => Some(aligned_trace),
                    Err(err) => {
                        let error = Arc::clone(&error);
                        *error.lock().unwrap() = Some(err);
                        None
                    }
                }
            })
            .collect::<Vec<_>>(); // end parallel execution

        if aligned_traces.len() != log.number_of_traces() {
            //something went wrong
            if let Ok(mutex) = Arc::try_unwrap(error) {
                if let Ok(err) = mutex.into_inner() {
                    if let Some(err) = err {
                        return Err(err);
                    }
                }
            }
            return Err(anyhow!("Something went wrong when computing alignments."));
        }

        progress_bar.finish_and_clear();

        let mut result = LanguageOfAlignments::new(activity_key);
        result.append(&mut aligned_traces);
        Ok(result)
    }

    fn align_stochastic_language(
        &mut self,
        log: Box<dyn EbiTraitFiniteStochasticLanguage>,
    ) -> Result<StochasticLanguageOfAlignments> {
        let mut activity_key = self.activity_key().clone();
        let translator = Arc::new(ActivityKeyTranslator::new(
            log.activity_key(),
            &mut activity_key,
        ));
        let log = Arc::new(log);
        let error: Arc<Mutex<Option<Error>>> = Arc::new(Mutex::new(None));

        log::info!("Compute alignments");
        let progress_bar = EbiCommand::get_progress_bar_ticks(log.number_of_traces());

        //compute alignments multi-threadedly
        let aligned_traces = (0..log.number_of_traces())
            .into_par_iter()
            .filter_map(|trace_index| {
                let log = Arc::clone(&log);
                let translator = Arc::clone(&translator);
                let self2 = Arc::from(&self);

                let trace = log.get_trace(trace_index).unwrap();
                let trace_translated = translator.translate_trace(trace);
                let probability = log.get_trace_probability(trace_index).unwrap().clone();

                // log::debug!("align trace {:?}", trace);
                let result = self2
                    .align_trace(&trace_translated)
                    .with_context(|| format!("Aligning trace {:?}", trace));
                progress_bar.inc(1);

                match result {
                    Ok((aligned_trace, _)) => Some((aligned_trace, probability)),
                    Err(err) => {
                        let error = Arc::clone(&error);
                        *error.lock().unwrap() = Some(err);
                        None
                    }
                }
            })
            .collect::<Vec<_>>(); // end parallel execution

        if aligned_traces.len() != log.number_of_traces() {
            //something went wrong
            if let Ok(mutex) = Arc::try_unwrap(error) {
                if let Ok(err) = mutex.into_inner() {
                    if let Some(err) = err {
                        return Err(err);
                    }
                }
            }
            return Err(anyhow!("Something went wrong when computing alignments."));
        }

        progress_bar.finish_and_clear();

        let mut result = StochasticLanguageOfAlignments::new(activity_key);
        result.append(aligned_traces);
        Ok(result)
    }

    /**
     * Please note to ensure the trace and the semantics use the same ActivityKey, or they have been translated
     */
    fn align_trace(&self, trace: &Vec<Activity>) -> Result<(Vec<Move>, usize)> {
        if let Some((states, cost)) = align_astar(self, trace) {
            Ok((transform_alignment(self, trace, states)?, cost))
        } else {
            Err(anyhow!(
                "The synchronous product has no way to terminate: it is impossible to reach a deadlock."
            ))
        }
    }
}

pub fn align_astar<T, State>(
    semantics: &T,
    trace: &Vec<Activity>,
) -> Option<(Vec<(usize, State)>, usize)>
where
    T: Semantics<SemState = State> + AlignmentHeuristics<AliState = State> + ?Sized,
    State: Displayable,
{
    // log::debug!("activity key {:?}", semantics.get_activity_key());
    // log::debug!("align trace {:?}", trace);

    let cache = semantics.initialise_alignment_heuristic_cache();

    if let Some(start) = semantics.get_initial_state() {
        let start = (0, start);
        let successors = |(trace_index, state): &(usize, State)| {
            let mut result = vec![];

            // log::debug!("successors of log {} model {}", trace_index, state);

            if trace_index < &trace.len() {
                //we can do a log move
                let new_trace_index = trace_index + 1;
                let new_state = state.clone();
                // log::debug!("\tlog move {} to {} {}", trace[*trace_index], new_trace_index, new_state);
                result.push(((new_trace_index, new_state), 10000));
            }

            //walk through the enabled transitions in the model
            for transition in semantics.get_enabled_transitions(&state) {
                let mut new_state = state.clone();
                let _ = semantics.execute_transition(&mut new_state, transition);

                if let Some(activity) = semantics.get_transition_activity(transition) {
                    //non-silent model move
                    result.push(((*trace_index, new_state.clone()), 10000));
                    // log::debug!("\tmodel move t{} {} to {} {}", transition, activity, trace_index, new_state);

                    //which may also be a synchronous move
                    if trace_index < &trace.len() && activity == trace[*trace_index] {
                        //synchronous move
                        let new_trace_index = trace_index + 1;
                        // log::debug!("\tsynchronous move t{} {} to {} {}", transition, activity, new_trace_index, new_state);
                        result.push(((new_trace_index, new_state), 0));
                    }
                } else {
                    //silent move
                    // log::debug!("\tsilent move t{} to {} {}", transition, trace_index, new_state);
                    result.push(((*trace_index, new_state), 1));
                }
            }

            // log::debug!("successors of {} {}: {:?}", trace_index, state, result);
            result
        };

        //function that returns a heuristic on how far we are still at least from a final state
        let heuristic = |(trace_index, state): &(usize, State)| {
            semantics.underestimate_cost_to_final_synchronous_state(
                trace,
                trace_index,
                state,
                &cache,
            )
        };

        //function that returns whether we are in a final synchronous product state
        let success = |(trace_index, state): &(usize, State)| {
            let result = trace_index == &trace.len() && semantics.is_final_state(&state);
            // log::debug!("state {} {} is final: {}", trace_index, state, result);
            result
        };

        pathfinding::prelude::astar(&start, successors, heuristic, success)
    } else {
        None
    }
}

/**
 * The a* function we use returns a sequence of states, while we need a sequence of moves.
 * This function transforms the sequence of states into a sequence of moves.
 *
 * This function assumes equal costs (of 10000) for the log and model moves, and 1 for the silent moves.
 */
pub fn transform_alignment<T, State>(
    semantics: &T,
    trace: &Vec<Activity>,
    states: Vec<(usize, State)>,
) -> Result<Vec<Move>>
where
    T: Semantics<SemState = State> + ?Sized,
    State: Display + Debug + Clone + Hash + Eq,
{
    // log::debug!("transform alignment {:?}", states);
    let mut alignment = vec![];

    let mut it = states.into_iter();

    let (mut previous_trace_index, mut previous_state) = it.next().unwrap();

    for (trace_index, state) in it {
        // log::debug!("transform {} from {} to {}", trace_index, previous_state, state);
        if trace_index != previous_trace_index {
            //we did a move on the log
            let activity = trace[previous_trace_index];

            if previous_state == state {
                //we did not move on the model => log move
                alignment.push(Move::LogMove(activity));
            } else {
                //we moved on the model => synchronous move
                let transition =
                    find_transition_with_label(semantics, &previous_state, &state, activity)
                        .with_context(|| {
                            format!(
                                "Map synchronous move from {} {} to {} {} with label {}",
                                previous_trace_index, previous_state, trace_index, state, activity
                            )
                        })?;
                alignment.push(Move::SynchronousMove(activity, transition));
            }
        } else {
            //we did not do a move on the log

            if let Some(transition) =
                is_there_a_silent_transition_enabled(semantics, &previous_state, &state)
            {
                //there is a silent transition enabled, which is the cheapest
                alignment.push(Move::SilentMove(transition));
            } else {
                //otherwise, we take an arbitrary labelled model move
                let transition = find_labelled_transition(semantics, &previous_state, &state)?;
                alignment.push(Move::ModelMove(
                    semantics.get_transition_activity(transition).unwrap(),
                    transition,
                ));
            }
        }

        previous_trace_index = trace_index;
        previous_state = state;

        // log::debug!("prefix: {:?}", alignment);
    }

    Ok(alignment)
}

pub fn is_there_a_silent_transition_enabled<T, FS>(
    semantics: &T,
    from: &FS,
    to: &FS,
) -> Option<TransitionIndex>
where
    T: Semantics<SemState = FS> + ?Sized,
    FS: Display + Debug + Clone + Hash + Eq,
{
    // log::debug!("is there a silent transition enabled from {} to {}", from, to);
    // log::debug!("enabled transitions {:?}", semantics.get_enabled_transitions(from));
    for transition in semantics.get_enabled_transitions(from) {
        if semantics.is_transition_silent(transition) {
            let mut from = from.clone();
            let _ = semantics.execute_transition(&mut from, transition);
            if &from == to {
                // log::debug!("yes");
                return Some(transition);
            }
        }
    }
    // log::debug!("no");
    None
}

pub fn find_transition_with_label<T, FS>(
    semantics: &T,
    from: &FS,
    to: &FS,
    label: Activity,
) -> Result<TransitionIndex>
where
    T: Semantics<SemState = FS> + ?Sized,
    FS: Display + Debug + Clone + Hash + Eq,
{
    // log::debug!("find transition with label {}", label);
    for transition in semantics.get_enabled_transitions(from) {
        // log::debug!("transition {} is enabled", transition);
        if semantics.get_transition_activity(transition) == Some(label) {
            let mut from = from.clone();
            semantics.execute_transition(&mut from, transition)?;
            if &from == to {
                return Ok(transition);
            }
        }
    }
    Err(anyhow!(
        "There is no transition with activity {} that brings the model from {} to {}",
        label,
        from,
        to
    ))
}

pub fn find_labelled_transition<T, FS>(semantics: &T, from: &FS, to: &FS) -> Result<TransitionIndex>
where
    T: Semantics<SemState = FS> + ?Sized,
    FS: Display + Debug + Clone + Hash + Eq,
{
    for transition in semantics.get_enabled_transitions(from) {
        if !semantics.is_transition_silent(transition) {
            let mut from = from.clone();
            semantics.execute_transition(&mut from, transition)?;
            if &from == to {
                return Ok(transition);
            }
        }
    }
    Err(anyhow!(
        "There is no transition with any activity enabled that brings the model from {} to {}",
        from,
        to
    ))
}

pub trait AlignmentHeuristics {
    type AliState;

    fn initialise_alignment_heuristic_cache(&self) -> Vec<Vec<usize>>;

    /**
     * Return a lower bound on the cost to reach a final state in the synchronous product.
     * If not sure: 0 is a valid return value, though better bounds will make searches more efficient.
     */
    fn underestimate_cost_to_final_synchronous_state(
        &self,
        trace: &Vec<Activity>,
        trace_index: &usize,
        state: &Self::AliState,
        cache: &Vec<Vec<usize>>,
    ) -> usize;
}

impl AlignmentHeuristics for DeterministicFiniteAutomaton {
    type AliState = usize;

    fn initialise_alignment_heuristic_cache(&self) -> Vec<Vec<usize>> {
        vec![]
    }

    fn underestimate_cost_to_final_synchronous_state(
        &self,
        _: &Vec<Activity>,
        _: &usize,
        _: &usize,
        _: &Vec<Vec<usize>>,
    ) -> usize {
        0
    }
}

impl AlignmentHeuristics for FiniteStochasticLanguageSemantics {
    type AliState = usize;

    fn initialise_alignment_heuristic_cache(&self) -> Vec<Vec<usize>> {
        vec![]
    }

    fn underestimate_cost_to_final_synchronous_state(
        &self,
        _: &Vec<Activity>,
        _: &usize,
        _: &usize,
        _: &Vec<Vec<usize>>,
    ) -> usize {
        0
    }
}

impl AlignmentHeuristics for LabelledPetriNet {
    type AliState = LPNMarking;

    fn initialise_alignment_heuristic_cache(&self) -> Vec<Vec<usize>> {
        vec![]
    }

    fn underestimate_cost_to_final_synchronous_state(
        &self,
        _trace: &Vec<Activity>,
        _trace_index: &usize,
        _state: &LPNMarking,
        _cache: &Vec<Vec<usize>>,
    ) -> usize {
        0
    }
}

impl AlignmentHeuristics for StochasticLabelledPetriNet {
    type AliState = LPNMarking;

    fn initialise_alignment_heuristic_cache(&self) -> Vec<Vec<usize>> {
        vec![]
    }

    fn underestimate_cost_to_final_synchronous_state(
        &self,
        _trace: &Vec<Activity>,
        _trace_index: &usize,
        _state: &LPNMarking,
        _cache: &Vec<Vec<usize>>,
    ) -> usize {
        0
    }
}

macro_rules! usize {
    ($t:ident) => {
        impl AlignmentHeuristics for $t {
            type AliState = usize;

            fn initialise_alignment_heuristic_cache(&self) -> Vec<Vec<usize>> {
                vec![]
            }

            fn underestimate_cost_to_final_synchronous_state(
                &self,
                _: &Vec<Activity>,
                _: &usize,
                _: &usize,
                _: &Vec<Vec<usize>>,
            ) -> usize {
                0
            }
        }
    };
}

macro_rules! nodestates {
    ($t:ident) => {
        impl AlignmentHeuristics for $t {
            type AliState = NodeStates;

            fn initialise_alignment_heuristic_cache(&self) -> Vec<Vec<usize>> {
                vec![]
            }

            fn underestimate_cost_to_final_synchronous_state(
                &self,
                _: &Vec<Activity>,
                _: &usize,
                _: &NodeStates,
                _: &Vec<Vec<usize>>,
            ) -> usize {
                0
            }
        }
    };
}
usize!(StochasticDeterministicFiniteAutomaton);
nodestates!(ProcessTree);
nodestates!(StochasticProcessTree);
usize!(DirectlyFollowsGraph);
usize!(DirectlyFollowsModel);
usize!(StochasticDirectlyFollowsModel);

#[cfg(test)]
mod tests {
    use std::fs;

    use ebi_objects::{
        DeterministicFiniteAutomaton, FiniteLanguage, FiniteStochasticLanguage, HasActivityKey,
        StochasticDeterministicFiniteAutomaton, TranslateActivityKey,
        ebi_objects::language_of_alignments::Move,
    };

    use crate::{ebi_traits::ebi_trait_semantics::ToSemantics, techniques::align::Align};

    #[test]
    fn align_sdfa_trace() {
        let fin = fs::read_to_string("testfiles/aa-ab-ba.sdfa").unwrap();
        let sdfa = fin
            .parse::<StochasticDeterministicFiniteAutomaton>()
            .unwrap();
        let mut semantics = sdfa.to_semantics();

        let a = semantics.activity_key_mut().process_activity("a");
        let b = semantics.activity_key_mut().process_activity("b");

        let trace = vec![b, b];

        let (alignment, _) = semantics.align_trace(&trace).unwrap();

        let correct_1 = vec![
            Move::SynchronousMove(b, 1),
            Move::ModelMove(a, 4),
            Move::SilentMove(5),
            Move::LogMove(b),
        ];
        let correct_2 = vec![
            Move::SynchronousMove(b, 1),
            Move::LogMove(b),
            Move::ModelMove(a, 4),
            Move::SilentMove(5),
        ];

        assert!(alignment == correct_1 || alignment == correct_2);
    }

    #[test]
    fn align_sdfa_lang() {
        let fin1 = fs::read_to_string("testfiles/aa-ab-ba.sdfa").unwrap();
        let sdfa = fin1
            .parse::<StochasticDeterministicFiniteAutomaton>()
            .unwrap();
        let mut semantics = sdfa.to_semantics();

        let fin2 = fs::read_to_string("testfiles/bb.lang").unwrap();
        let lang = Box::new(fin2.parse::<FiniteLanguage>().unwrap());

        let a = semantics.activity_key_mut().process_activity("a");
        let b = semantics.activity_key_mut().process_activity("b");

        let alignment = semantics.align_language(lang).unwrap();

        let correct_1 = vec![
            Move::SynchronousMove(b, 1),
            Move::ModelMove(a, 4),
            Move::SilentMove(5),
            Move::LogMove(b),
        ];
        let correct_2 = vec![
            Move::SynchronousMove(b, 1),
            Move::LogMove(b),
            Move::ModelMove(a, 4),
            Move::SilentMove(5),
        ];

        assert!(alignment.get(0) == Some(&correct_1) || alignment.get(0) == Some(&correct_2));
    }

    #[test]
    fn align_slang_lang() {
        let fin1 = fs::read_to_string("testfiles/aa-ab-ba.slang").unwrap();
        let sdfa = fin1.parse::<FiniteStochasticLanguage>().unwrap();
        let mut semantics = sdfa.to_semantics();

        let fin2 = fs::read_to_string("testfiles/bb.lang").unwrap();
        let lang = Box::new(fin2.parse::<FiniteLanguage>().unwrap());

        let a = semantics.activity_key_mut().process_activity("a");
        let b = semantics.activity_key_mut().process_activity("b");

        let alignment = semantics.align_language(lang).unwrap();

        let correct_1 = vec![
            Move::ModelMove(a, 1),
            Move::SynchronousMove(b, 2),
            Move::SilentMove(0),
            Move::LogMove(b),
        ]; //other options may be valid, please check semantically when this fails
        let correct_2 = vec![
            Move::SynchronousMove(b, 2),
            Move::ModelMove(a, 1),
            Move::SilentMove(0),
            Move::LogMove(b),
        ]; //other options may be valid, please check semantically when this fails
        assert!(*alignment.get(0).unwrap() == correct_1 || *alignment.get(0).unwrap() == correct_2);
    }

    #[test]
    fn align_lang_lang() {
        let fin1 = fs::read_to_string("testfiles/aa-ab-ba.lang").unwrap();
        let sdfa = fin1.parse::<FiniteLanguage>().unwrap();
        let mut semantics = sdfa.to_semantics();

        let fin2 = fs::read_to_string("testfiles/bb.lang").unwrap();
        let lang = Box::new(fin2.parse::<FiniteLanguage>().unwrap());

        let a = semantics.activity_key_mut().process_activity("a");
        let b = semantics.activity_key_mut().process_activity("b");

        let alignment = semantics.align_language(lang).unwrap();

        let correct_1 = vec![
            Move::SynchronousMove(b, 1),
            Move::ModelMove(a, 2),
            Move::SilentMove(5),
            Move::LogMove(b),
        ]; //other options may be valid, please check semantically when this fails

        assert_eq!(*alignment.get(0).unwrap(), correct_1);
        // assert!(*alignment.get(0).unwrap() == correct_1 || *alignment.get(0).unwrap() == correct_2);
    }

    #[test]
    fn align_dfa_lang() {
        let fin1 = fs::read_to_string("testfiles/aa-ab-ba.dfa").unwrap();
        let dfa = fin1.parse::<DeterministicFiniteAutomaton>().unwrap();
        let mut semantics = dfa.to_semantics();

        let fin2 = fs::read_to_string("testfiles/bb.lang").unwrap();
        let lang = Box::new(fin2.parse::<FiniteLanguage>().unwrap());

        let a = semantics.activity_key_mut().process_activity("a");
        let b = semantics.activity_key_mut().process_activity("b");

        let alignment = semantics.align_language(lang).unwrap();

        let correct_1 = vec![
            Move::SynchronousMove(b, 1),
            Move::ModelMove(a, 4),
            Move::SilentMove(5),
            Move::LogMove(b),
        ]; //other options may be valid, please check semantically when this fails
        assert_eq!(*alignment.get(0).unwrap(), correct_1);
    }

    #[test]
    fn align_dfa_lang_translate() {
        let fin2 = fs::read_to_string("testfiles/bb.lang").unwrap();
        let mut lang = Box::new(fin2.parse::<FiniteLanguage>().unwrap());

        let fin1 = fs::read_to_string("testfiles/aa-ab-ba.dfa").unwrap();
        let mut dfa = fin1.parse::<DeterministicFiniteAutomaton>().unwrap();
        dfa.translate_using_activity_key(lang.activity_key_mut());
        let mut semantics = dfa.to_semantics();

        let alignment = semantics.align_language(lang).unwrap();

        let a = semantics.activity_key_mut().process_activity("a");
        let b = semantics.activity_key_mut().process_activity("b");

        let correct_1 = vec![
            Move::SynchronousMove(b, 1),
            Move::ModelMove(a, 4),
            Move::SilentMove(5),
            Move::LogMove(b),
        ]; //other options may be valid, please check semantically when this fails
        assert_eq!(*alignment.get(0).unwrap(), correct_1);
    }
}
