use std::{fmt::{Debug, Display}, hash::Hash, sync::{Arc, Mutex}};

use anyhow::{Error, Ok, Result};
use chrono::{DateTime, FixedOffset};
use rayon::iter::{IntoParallelIterator, ParallelIterator};

use crate::{ebi_framework::{displayable::Displayable, ebi_command::EbiCommand}, ebi_objects::{alignments::Move, executions::{Execution, Executions}}, ebi_traits::{ebi_trait_event_log::{EbiTraitEventLog, ATTRIBUTE_TIME}, ebi_trait_semantics::{EbiTraitSemantics, Semantics}, ebi_trait_stochastic_semantics::TransitionIndex}, techniques::align::Align};

pub trait FindExecutions {
    fn find_executions(&self, log: Box<dyn EbiTraitEventLog>) -> Result<Executions>;
}

impl FindExecutions for EbiTraitSemantics {
    fn find_executions(&self, log: Box<dyn EbiTraitEventLog>) -> Result<Executions> {
		match self {
			EbiTraitSemantics::Usize(sem) => sem.find_executions(log),
			EbiTraitSemantics::Marking(sem) => sem.find_executions(log),
		}
	}
}

impl <T, State> FindExecutions for T where T: Semantics<SemState = State, AliState = State> + Send + Sync + ?Sized, State: Displayable {
    fn find_executions(&self, log: Box<dyn EbiTraitEventLog>) -> Result<Executions> {

        let error: Arc<Mutex<Option<Error>>> = Arc::new(Mutex::new(None));
        log::info!("Compute alignments");
        let progress_bar = EbiCommand::get_progress_bar(log.len());
        
        let result = (0..log.len()).into_par_iter().filter_map(|trace_index| {
            let trace = log.read_trace_with_activity_key(&mut self.get_activity_key().clone(), &trace_index);

            //align the trace
            let alignment = self.align_trace(&trace);

            progress_bar.inc(1);

            match alignment {
                Result::Ok((aligned_trace, _)) => {
                    //process the moves of this trace
                    let c = C::new(trace_index, aligned_trace);
                    match c.process_alignment(self, &log) {
                        Result::Ok(c) => Some(c),
                        Err(err) => {
                            let error = Arc::clone(&error);
                            *error.lock().unwrap() = Some(err);
                            None
                        }
                    }
                },
                Err(err) => { //in case of an alignment, pass the error to the main thread
                    let error = Arc::clone(&error);
                    *error.lock().unwrap() = Some(err);
                    None
                },
            }
        }).flatten().collect::<Vec<_>>();
        
        //see whether an error was reported
        if let Result::Ok(mutex) = Arc::try_unwrap(error) {
            if let Result::Ok(err) = mutex.into_inner() {
                if let Some(err) = err {
                    return Err(err);
                }
            }
        }

        Ok(result.into())
    }
}

struct C {
    trace_index: usize,
    moves: Vec<Move>,
}

impl C {
    fn new(trace_index: usize, moves: Vec<Move>) -> Self {
        Self {
            trace_index: trace_index,
            moves: moves,
        }
    }

    fn process_alignment<T, FS>(&self, semantics: &T, log: &Box<dyn EbiTraitEventLog>) -> Result<Vec<Execution>> where T: Semantics<SemState = FS> + Send + Sync + ?Sized, FS: Display + Debug + Clone + Hash + Eq {
        let mut state = semantics.get_initial_state();
        let mut executions = vec![];

        for move_index in 0..self.moves.len() {
            match self.moves[move_index] {
                Move::LogMove(_) => {
                    //logmoves are not linked to transitions and have no executions
                },
                Move::ModelMove(_, transition) => {
                    semantics.execute_transition(&mut state, transition)?;
                },
                Move::SynchronousMove(_, transition) => {
                    let enabling_move_index = self.get_enabling_move(transition, semantics);

                    executions.push(Execution{
                        transition: transition,
                        enabled_transitions_at_enablement: self.get_enabled_transitions(enabling_move_index, semantics)?,
                        time_of_enablement: self.get_time(enabling_move_index, log),
                        time_of_execution: self.get_time(Some(move_index), log),
                        features_at_enablement: None,
                    });

                    semantics.execute_transition(&mut state, transition)?;
                },
                Move::SilentMove(transition) => {
                    let enabling_move_index = self.get_enabling_move(transition, semantics);

                    executions.push(Execution{
                        transition: transition,
                        enabled_transitions_at_enablement: self.get_enabled_transitions(enabling_move_index, semantics)?,
                        time_of_enablement: self.get_time(enabling_move_index, log),
                        time_of_execution: None,
                        features_at_enablement: None,
                    });

                    semantics.execute_transition(&mut state, transition)?;
                },
            }
        }

        Ok(executions)
    }

    fn get_time(&self, move_index: Option<usize>, log: &Box<dyn EbiTraitEventLog>) -> Option<DateTime<FixedOffset>> {
        let event_index = self.get_event_index(move_index?);

        log.get_event_attribute_time(self.trace_index, event_index, &ATTRIBUTE_TIME.to_string())
    }

    fn get_enabled_transitions<T, FS>(&self, move_index: Option<usize>, semantics: &T) -> Result<Option<Vec<TransitionIndex>>> where T: Semantics<SemState = FS> + Send + Sync + ?Sized, FS: Display + Debug + Clone + Hash + Eq {
        let mut state = semantics.get_initial_state();
        if let Some(mi) = move_index {
            for movee in self.moves.iter().take(mi) {
                match movee {
                    Move::ModelMove(_, transition) | Move::SynchronousMove(_, transition) | Move::SilentMove(transition) => semantics.execute_transition(&mut state, *transition)?,
                    _ => ()
                };
            }
        }
        Ok(Some(semantics.get_enabled_transitions(&state)))
    }

    fn get_event_index(&self, move_index: usize) -> usize {
        let mut event_index = 0;
        for movee in self.moves.iter().take(move_index) {
            match movee {
                Move::LogMove(_) | Move::SynchronousMove(_, _) => event_index += 1,
                _ => {}
            }
        }

        event_index
    }

    /**
     * Get the last non-silent move that enabled the move at the given index
     */
    fn get_enabling_move<T, FS>(&self, move_index: usize, semantics: &T) -> Option<usize> where T: Semantics<SemState = FS> + Send + Sync + ?Sized, FS: Display + Debug + Clone + Hash + Eq {
        let transition = self.moves.get(move_index)?.get_transition()?;

        //first, figure out when this move's transition was last enabled
        let mut last_enabled_before_move = None;
        {
            let mut state = semantics.get_initial_state();
            for (move_index2, move2) in self.moves.iter().take(move_index).enumerate() {
                if let Some(transition2) = move2.get_transition() {

                    if semantics.get_enabled_transitions(&state).contains(&transition) {
                        if last_enabled_before_move.is_none() {
                            //transition is now enabled and was not before
                            last_enabled_before_move = Some(move_index2);
                        } else {
                            //transition was already enabled and is still enabled
                        }
                    } else {
                        //transition is not enabled
                        last_enabled_before_move = None;
                    }
                    let _ = semantics.execute_transition(&mut state, transition2);
                }
            }
        }

        if let Some(Move::SilentMove(_)) = self.moves.get(last_enabled_before_move?) {
            self.get_enabling_move(last_enabled_before_move?, semantics)
        } else {
            last_enabled_before_move
        }
    }
}