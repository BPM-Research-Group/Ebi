use std::{fmt::{Debug, Display}, hash::Hash, sync::{Arc, Mutex}};
use rayon::iter::{IntoParallelIterator, ParallelIterator};
use anyhow::{anyhow, Context, Error, Result};

use crate::{ebi_framework::{activity_key::{Activity, ActivityKeyTranslator}, ebi_command::EbiCommand}, ebi_objects::alignments::{Alignments, Move}, ebi_traits::{ebi_trait_finite_language::EbiTraitFiniteLanguage, ebi_trait_semantics::{EbiTraitSemantics, Semantics}, ebi_trait_stochastic_semantics::TransitionIndex}};

pub trait Align {
    fn align_language(&mut self, log: Box<dyn EbiTraitFiniteLanguage>) -> Result<Alignments>;

    /**
	 * Please note to ensure the trace and the semantics use the same ActivityKey, or they have been translated
	 */
    fn align_trace(&self, trace: &Vec<Activity>) -> Result<(Vec<Move>, usize)>;
}

impl Align for EbiTraitSemantics {
    fn align_language(&mut self, log: Box<dyn EbiTraitFiniteLanguage>) -> Result<Alignments> {
		match self {
			EbiTraitSemantics::Usize(sem) => sem.align_language(log),
			EbiTraitSemantics::Marking(sem) => sem.align_language(log),
		}
	}

	fn align_trace(&self, trace: &Vec<Activity>) -> Result<(Vec<Move>, usize)> {
		match self {
			EbiTraitSemantics::Usize(sem) => sem.align_trace(trace),
			EbiTraitSemantics::Marking(sem) => sem.align_trace(trace),
		}
	}
}

impl <T, FS> Align for T where T: Semantics<State = FS> + Send + Sync + ?Sized, FS: Display + Debug + Clone + Hash + Eq {
    
    fn align_language(&mut self, log: Box<dyn EbiTraitFiniteLanguage>) -> Result<Alignments> {
        let mut activity_key = self.get_activity_key().clone();
        let translator = Arc::new(ActivityKeyTranslator::new(log.get_activity_key(), &mut activity_key));
        let log = Arc::new(log);
        let error: Arc<Mutex<Option<Error>>> = Arc::new(Mutex::new(None));

        log::info!("Compute alignments");
        let progress_bar = EbiCommand::get_progress_bar(log.len());

        //compute alignments multi-threadedly
        let mut aligned_traces = (0..log.len()).into_par_iter().filter_map(|trace_index| {
            let log = Arc::clone(&log);
            let translator = Arc::clone(&translator);
            let self2 = Arc::from(&self);

            let trace = log.get_trace(trace_index).unwrap();
            let trace_translated = translator.translate_trace(trace);

            progress_bar.inc(1);

            match self2.align_trace(&trace_translated).with_context(|| format!("Aligning trace {:?}", trace)) {
                Ok((aligned_trace, _)) => Some(aligned_trace),
                Err(err) => {
                    let error = Arc::clone(&error);
                *error.lock().unwrap() = Some(err);
                None
                },
            }
        }).collect::<Vec<_>>();// end parallel execution

        if aligned_traces.len() != log.len() {
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

        let mut result = Alignments::new(activity_key);
        result.append(&mut aligned_traces);
        Ok(result)
    }

    /**
	 * Please note to ensure the trace and the semantics use the same ActivityKey, or they have been translated
	 */
    fn align_trace(&self, trace: &Vec<Activity>) -> Result<(Vec<Move>, usize)> {
        if let Some((states, cost)) = align_astar(self, trace) {
            Ok((transform_alignment(self, trace, states)?, cost))
        } else {
            Err(anyhow!("The synchronous product has no way to terminate: it is impossible to reach a deadlock."))
        }
    }

}

pub fn align_astar<T, FS>(semantics: &T, trace: &Vec<Activity>) -> Option<(Vec<(usize, FS)>, usize)> where T: Semantics<State = FS> + ?Sized, FS: Display + Debug + Clone + Hash + Eq {
    log::debug!("activity key {}", semantics.get_activity_key());
    log::debug!("align trace {:?}", trace);

    let start = (0, semantics.get_initial_state());
    let successors = |(trace_index, state) : &(usize, FS)| {

        let mut result = vec![];

        log::debug!("successors of log {} model {}", trace_index, state);
        
        if trace_index < &trace.len() {
            //we can do a log move
            // log::debug!("\tlog move {}", trace[*trace_index]);
            result.push(((trace_index + 1, state.clone()), 10000));
        }

        //walk through the enabled transitions in the model
        for transition in semantics.get_enabled_transitions(&state) {

            let mut new_state = state.clone();
            log::debug!("\t\tnew state before {}", new_state);
            let _ = semantics.execute_transition(&mut new_state, transition);
            log::debug!("\t\tnew state after {}", new_state);

            if let Some(activity) = semantics.get_transition_activity(transition) {
                //non-silent model move
                result.push(((*trace_index, new_state.clone()), 10000));
                log::debug!("\tmodel move t{} {} to {}", transition, activity, new_state);

                //which may also be a synchronous move
                if trace_index < &trace.len() && activity == trace[*trace_index] {
                    //synchronous move
                    log::debug!("\tsynchronous move t{} {} to {}", transition, activity, new_state);
                    result.push(((trace_index + 1, new_state), 0));
                }
            } else {
                //silent move
                result.push(((*trace_index, new_state), 1));
            }
        }

        log::debug!("successors of {} {}: {:?}", trace_index, state, result);
        result
    };

    //function that returns a heuristic on how far we are still minimally from a final state
    let heuristic = |_ : &(usize, FS)| {
        0
    };

    //function that returns whether we are in a final synchronous product state
    let success = |(trace_index, state): &(usize, FS)| {
        trace_index == &trace.len() && semantics.is_final_state(&state)
    };

    pathfinding::prelude::astar(&start, successors, heuristic, success)
}

/**
 * The a* function we use returns a sequence of states, while we need a sequence of moves.
 * This function transforms the sequence of states into a sequence of moves.
 * 
 * This function assumes equal costs (of 10000) for the log and model moves, and 1 for the silent moves.
 */
pub fn transform_alignment<T, FS>(semantics: &T, trace: &Vec<Activity>, states: Vec<(usize, FS)>) -> Result<Vec<Move>> where T: Semantics<State = FS> + ?Sized, FS: Display + Debug + Clone + Hash + Eq {
    let mut alignment = vec![];

    let mut it = states.into_iter();
    
    let (mut previous_trace_index, mut previous_state) = it.next().unwrap();
    
    for (trace_index, state) in it {

        if trace_index != previous_trace_index {
            //we did a move on the log
            let activity = trace[previous_trace_index];

            if previous_state == state {
                //we did not move on the model => log move
                alignment.push(Move::LogMove(activity));
            } else {
                //we moved on the model => synchronous move
                let transition = find_transition_with_label(semantics, &previous_state, &state, activity).with_context(|| format!("Map synchronous move from {} {} to {} {} with label {}", previous_trace_index, previous_state, trace_index, state, activity))?;
                alignment.push(Move::SynchronousMove(activity, transition));
            }

        } else {
            //we did not do a move on the log

            if let Some(transition) = is_there_a_silent_transition_enabled(semantics, &previous_state, &state) {
                //there is a silent transition enabled, which is the cheapest
                alignment.push(Move::SilentMove(transition));
            } else {
                //otherwise, we take an arbitrary labelled model move
                let transition = find_labelled_transition(semantics, &previous_state, &state)?;
                alignment.push(Move::ModelMove(semantics.get_transition_activity(transition).unwrap(), transition));
            }
        }

        previous_trace_index = trace_index;
        previous_state = state;

        // log::debug!("prefix: {:?}", alignment);
    }

    Ok(alignment)
}

pub fn is_there_a_silent_transition_enabled<T, FS>(semantics: &T, from: &FS, to: &FS) -> Option<TransitionIndex> where T: Semantics<State = FS> + ?Sized, FS: Display + Debug + Clone + Hash + Eq {
    for transition in semantics.get_enabled_transitions(from) {
        if semantics.is_transition_silent(transition) {
            let mut from = from.clone();
            let _ = semantics.execute_transition(&mut from, transition);
            if &from == to {
                return Some(transition);
            }
        }
    }
    None
}

pub fn find_transition_with_label<T, FS>(semantics: &T, from: &FS, to: &FS, label: Activity) -> Result<TransitionIndex> where T: Semantics<State = FS> + ?Sized, FS: Display + Debug + Clone + Hash + Eq {
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
    Err(anyhow!("There is no transition with activity {} that brings the model from {} to {}", label, from, to))
}

pub fn find_labelled_transition<T, FS>(semantics: &T, from: &FS, to: &FS) -> Result<TransitionIndex> where T: Semantics<State = FS> + ?Sized, FS: Display + Debug + Clone + Hash + Eq {
    for transition in semantics.get_enabled_transitions(from) {
        if !semantics.is_transition_silent(transition) {
            let mut from = from.clone();
            semantics.execute_transition(&mut from, transition)?;
            if &from == to {
                return Ok(transition);
            }
        }
    }
    Err(anyhow!("There is no transition with any activity enabled that brings the model from {} to {}", from, to))
}