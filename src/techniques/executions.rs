use crate::{
    ebi_framework::{displayable::Displayable, ebi_command::EbiCommand},
    ebi_traits::{
        ebi_trait_event_log_trace_attributes::ATTRIBUTE_TIME,
        ebi_trait_semantics::EbiTraitSemantics,
    },
    semantics::semantics::Semantics,
    techniques::align::Align,
};
use anyhow::{Context, Error, Ok, Result};
use chrono::{DateTime, FixedOffset};
use ebi_objects::{
    EventLogXes, Executions, HasActivityKey, IntoTraceIterator, NumberOfTraces,
    ebi_objects::{
        executions::Execution, labelled_petri_net::TransitionIndex, language_of_alignments::Move,
    },
};
use process_mining::core::event_data::case_centric::{AttributeValue, XESEditableAttribute};
use rayon::iter::{IndexedParallelIterator, ParallelIterator};
use std::{
    fmt::{Debug, Display},
    hash::Hash,
    sync::{Arc, Mutex},
};

pub trait FindExecutions {
    fn find_executions(&mut self, log: &mut EventLogXes) -> Result<Executions>;
}

impl FindExecutions for EbiTraitSemantics {
    fn find_executions(&mut self, log: &mut EventLogXes) -> Result<Executions> {
        match self {
            EbiTraitSemantics::Usize(sem) => sem.find_executions(log),
            EbiTraitSemantics::Marking(sem) => sem.find_executions(log),
            EbiTraitSemantics::TreeMarking(sem) => sem.find_executions(log),
        }
    }
}

impl<T, State> FindExecutions for T
where
    T: Semantics<SemState = State, AliState = State> + Send + Sync + ?Sized,
    State: Displayable,
{
    fn find_executions(&mut self, log: &mut EventLogXes) -> Result<Executions> {
        log::info!("Compute alignments");
        let progress_bar = EbiCommand::get_progress_bar_ticks(log.number_of_traces());
        let error: Arc<Mutex<Option<Error>>> = Arc::new(Mutex::new(None));

        self.translate_using_activity_key(log.activity_key_mut());

        let result = log
            .par_iter_traces()
            .enumerate()
            .filter_map(|(trace_index, trace)| {
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
                    }
                    Err(err) => {
                        //in case of an alignment, pass the error to the main thread
                        let error = Arc::clone(&error);
                        *error.lock().unwrap() = Some(err);
                        None
                    }
                }
            })
            .flatten()
            .collect::<Vec<_>>();

        progress_bar.finish_and_clear();

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

    fn process_alignment<T, FS>(&self, semantics: &T, log: &EventLogXes) -> Result<Vec<Execution>>
    where
        T: Semantics<SemState = FS> + Send + Sync + ?Sized,
        FS: Display + Debug + Clone + Hash + Eq,
    {
        let mut state = semantics
            .get_initial_state()
            .context("Got an unexpected emtpy state.")?;
        let mut executions = vec![];

        for move_index in 0..self.moves.len() {
            match self.moves[move_index] {
                Move::LogMove(_) => {
                    //logmoves are not linked to transitions and have no executions
                }
                Move::ModelMove(_, transition) => {
                    semantics.execute_transition(&mut state, transition)?;
                }
                Move::SynchronousMove(_, transition) => {
                    let enabling_move_index = self.get_enabling_move(transition, semantics);

                    executions.push(Execution {
                        transition: transition,
                        enabled_transitions_at_enablement: self
                            .get_enabled_transitions(enabling_move_index, semantics)?,
                        time_of_enablement: self.get_time(enabling_move_index, log),
                        time_of_execution: self.get_time(Some(move_index), log),
                        features_at_enablement: None,
                    });

                    semantics.execute_transition(&mut state, transition)?;
                }
                Move::SilentMove(transition) => {
                    let enabling_move_index = self.get_enabling_move(transition, semantics);

                    executions.push(Execution {
                        transition: transition,
                        enabled_transitions_at_enablement: self
                            .get_enabled_transitions(enabling_move_index, semantics)?,
                        time_of_enablement: self.get_time(enabling_move_index, log),
                        time_of_execution: None,
                        features_at_enablement: None,
                    });

                    semantics.execute_transition(&mut state, transition)?;
                }
            }
        }

        Ok(executions)
    }

    fn get_time(
        &self,
        move_index: Option<usize>,
        log: &EventLogXes,
    ) -> Option<DateTime<FixedOffset>> {
        let event_index = self.get_event_index(move_index?);

        Self::get_event_attribute_time(
            log,
            self.trace_index,
            event_index,
            &ATTRIBUTE_TIME.to_string(),
        )
    }

    fn get_event_attribute_time(
        log: &EventLogXes,
        trace_index: usize,
        event_index: usize,
        case_attribute: &String,
    ) -> Option<DateTime<FixedOffset>> {
        if let Some(attribute) = log
            .rust4pm_log
            .traces
            .get(trace_index)?
            .events
            .get(event_index)?
            .attributes
            .get_by_key(case_attribute)
        {
            match &attribute.value {
                AttributeValue::String(x) => x.parse::<DateTime<FixedOffset>>().ok(),
                AttributeValue::Date(x) => Some(*x),
                _ => None,
            }
        } else {
            None
        }
    }

    fn get_enabled_transitions<T, FS>(
        &self,
        move_index: Option<usize>,
        semantics: &T,
    ) -> Result<Option<Vec<TransitionIndex>>>
    where
        T: Semantics<SemState = FS> + Send + Sync + ?Sized,
        FS: Display + Debug + Clone + Hash + Eq,
    {
        if let Some(mut state) = semantics.get_initial_state() {
            if let Some(mi) = move_index {
                for movee in self.moves.iter().take(mi) {
                    match movee {
                        Move::ModelMove(_, transition)
                        | Move::SynchronousMove(_, transition)
                        | Move::SilentMove(transition) => {
                            semantics.execute_transition(&mut state, *transition)?
                        }
                        _ => (),
                    };
                }
            }
            Ok(Some(semantics.get_enabled_transitions(&state)))
        } else {
            Ok(None)
        }
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
    fn get_enabling_move<T, FS>(&self, move_index: usize, semantics: &T) -> Option<usize>
    where
        T: Semantics<SemState = FS> + Send + Sync + ?Sized,
        FS: Display + Debug + Clone + Hash + Eq,
    {
        let transition = self.moves.get(move_index)?.get_transition()?;

        //first, figure out when this move's transition was last enabled
        let mut last_enabled_before_move = None;
        {
            let mut state = semantics.get_initial_state()?;
            for (move_index2, move2) in self.moves.iter().take(move_index).enumerate() {
                if let Some(transition2) = move2.get_transition() {
                    if semantics
                        .get_enabled_transitions(&state)
                        .contains(&transition)
                    {
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

#[cfg(test)]
mod tests {
    use std::fs;

    use ebi_objects::{
        EventLogXes, StochasticDeterministicFiniteAutomaton,
        StochasticNondeterministicFiniteAutomaton,
    };

    use crate::techniques::executions::FindExecutions;

    #[test]
    fn executions() {
        let fin = fs::read_to_string("testfiles/a-b.xes").unwrap();
        let log = fin.parse::<EventLogXes>().unwrap();

        let fin2 = fs::read_to_string("testfiles/a-b-c-livelock.sdfa").unwrap();
        let mut model = fin2
            .parse::<StochasticDeterministicFiniteAutomaton>()
            .unwrap();

        let out = fs::read_to_string("testfiles/a-b.exs").unwrap();

        let x = model.find_executions(&mut Box::new(log)).unwrap();

        assert_eq!(out, x.to_string());
    }

    #[test]
    fn snfa_executions() {
        let fin = fs::read_to_string("testfiles/simple_log_markovian_abstraction.xes").unwrap();
        let log = fin.parse::<EventLogXes>().unwrap();

        let fin2 = fs::read_to_string("testfiles/aa-ab-ba.snfa").unwrap();
        let mut model = fin2
            .parse::<StochasticNondeterministicFiniteAutomaton>()
            .unwrap();

        model.find_executions(&mut Box::new(log)).unwrap();
    }
}
