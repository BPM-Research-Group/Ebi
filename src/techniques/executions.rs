use crate::{
    ebi_framework::{displayable::Displayable, ebi_command::EbiCommand},
    ebi_traits::{
        ebi_trait_event_log_event_attributes::EbiTraitEventLogEventAttributes,
        ebi_trait_semantics::EbiTraitSemantics,
    },
    semantics::semantics::Semantics,
    techniques::{align::Align, resource_utilisation::set_resource_utilisations},
};
use chrono::{DateTime, FixedOffset};
use ebi_objects::{
    Activity, ActivityKey, Executions,
    anyhow::{Context, Error, Ok, Result},
    ebi_objects::{
        executions::Execution, labelled_petri_net::TransitionIndex, language_of_alignments::Move,
    },
};
use rayon::iter::{IndexedParallelIterator, ParallelIterator};
use std::{
    collections::VecDeque,
    fmt::{Debug, Display},
    hash::Hash,
    sync::{Arc, Mutex},
};

pub trait FindExecutions {
    fn find_executions(
        &mut self,
        log: &mut Box<dyn EbiTraitEventLogEventAttributes>,
    ) -> Result<Executions>;
}

impl FindExecutions for EbiTraitSemantics {
    fn find_executions(
        &mut self,
        log: &mut Box<dyn EbiTraitEventLogEventAttributes>,
    ) -> Result<Executions> {
        match self {
            EbiTraitSemantics::Usize(sem) => sem.find_executions(log),
            EbiTraitSemantics::Marking(sem) => sem.find_executions(log),
            EbiTraitSemantics::TreeMarking(sem) => sem.find_executions(log),
            EbiTraitSemantics::BPMNMarking(sem) => sem.find_executions(log),
            EbiTraitSemantics::FspolangMarking(sem) => sem.find_executions(log),
        }
    }
}

impl<T, State> FindExecutions for T
where
    T: Semantics<SemState = State, AliState = State> + Send + Sync + ?Sized,
    State: Displayable,
{
    fn find_executions(
        &mut self,
        log: &mut Box<dyn EbiTraitEventLogEventAttributes>,
    ) -> Result<Executions> {
        log::info!("Compute alignments");
        let progress_bar = EbiCommand::get_progress_bar_ticks(log.number_of_traces());
        let error: Arc<Mutex<Option<Error>>> = Arc::new(Mutex::new(None));
        let resource_key = Arc::new(Mutex::new(ActivityKey::new()));

        self.translate_using_activity_key(log.activity_key_mut());

        let trace_executions = log
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
                        match c.alignment_to_executions(self, &log, &resource_key) {
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

        //merge sort the executions
        let execution_list = ExecutionsSorter::merge_sort(trace_executions);

        //create the result object
        let mut executions = (
            log.activity_key().clone(),
            Arc::try_unwrap(resource_key).unwrap().into_inner().unwrap(),
            execution_list,
        )
            .into();

        //set resource utilisations
        set_resource_utilisations(&mut executions)?;

        Ok(executions)
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

    fn alignment_to_executions<T, FS>(
        &self,
        semantics: &T,
        log: &Box<dyn EbiTraitEventLogEventAttributes>,
        resource_key: &Arc<Mutex<ActivityKey>>,
    ) -> Result<VecDeque<Execution>>
    where
        T: Semantics<SemState = FS> + Send + Sync + ?Sized,
        FS: Display + Debug + Clone + Hash + Eq,
    {
        let mut state = semantics
            .get_initial_state()
            .context("The model does not have an initial state.")?;
        let mut executions = VecDeque::with_capacity(self.moves.len());

        for move_index in 0..self.moves.len() {
            match self.moves[move_index] {
                Move::LogMove(_) => {
                    //logmoves are not linked to transitions and have no executions
                }
                Move::ModelMove(activity, transition) => {
                    let move_index_of_enablement = self.get_enabling_move(move_index, semantics);
                    let mut other_enabled_transitions = semantics.get_enabled_transitions(&state);
                    other_enabled_transitions.retain(|t| *t != transition);

                    executions.push_back(Execution {
                        trace: self.trace_index,
                        move_index,
                        activity: Some(activity),
                        also_in_log: false,
                        fired_transition: transition,
                        other_enabled_transitions,
                        move_index_of_enablement,
                        time_of_execution: None,
                        resource: None,
                        resource_utilisation: None,
                    });

                    semantics.execute_transition(&mut state, transition)?;
                }
                Move::SynchronousMove(activity, transition) => {
                    let move_index_of_enablement = self.get_enabling_move(move_index, semantics);
                    let mut other_enabled_transitions = semantics.get_enabled_transitions(&state);
                    other_enabled_transitions.retain(|t| *t != transition);

                    executions.push_back(Execution {
                        trace: self.trace_index,
                        move_index,
                        activity: Some(activity),
                        also_in_log: true,
                        fired_transition: transition,
                        other_enabled_transitions,
                        move_index_of_enablement,
                        time_of_execution: self.get_time(Some(move_index), log).cloned(),
                        resource: self.get_resource(Some(move_index), log, resource_key),
                        resource_utilisation: None,
                    });

                    semantics.execute_transition(&mut state, transition)?;
                }
                Move::SilentMove(transition) => {
                    let move_index_of_enablement = self.get_enabling_move(move_index, semantics);
                    let mut other_enabled_transitions = semantics.get_enabled_transitions(&state);
                    other_enabled_transitions.retain(|t| *t != transition);

                    executions.push_back(Execution {
                        trace: self.trace_index,
                        move_index,
                        activity: None,
                        also_in_log: false,
                        fired_transition: transition,
                        other_enabled_transitions,
                        move_index_of_enablement,
                        time_of_execution: None,
                        resource: None,
                        resource_utilisation: None,
                    });

                    semantics.execute_transition(&mut state, transition)?;
                }
            }
        }

        Ok(executions)
    }

    fn get_time<'a>(
        &self,
        move_index: Option<usize>,
        log: &'a Box<dyn EbiTraitEventLogEventAttributes>,
    ) -> Option<&'a DateTime<FixedOffset>> {
        let event_index = self.get_event_index(move_index?);
        log.get_event_time(self.trace_index, event_index)
    }

    fn get_resource<'a>(
        &self,
        move_index: Option<usize>,
        log: &'a Box<dyn EbiTraitEventLogEventAttributes>,
        resource_key: &Arc<Mutex<ActivityKey>>,
    ) -> Option<Activity> {
        let event_index = self.get_event_index(move_index?);
        let resource_string = log.get_event_resource(self.trace_index, event_index)?;

        Some(
            resource_key
                .lock()
                .as_mut()
                .unwrap()
                .process_activity(resource_string),
        )
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
     * Get the last move that enabled the move at the given index.
     */
    fn get_enabling_move<T, FS>(&self, move_index: usize, semantics: &T) -> Option<usize>
    where
        T: Semantics<SemState = FS> + Send + Sync + ?Sized,
        FS: Display + Debug + Clone + Hash + Eq,
    {
        let transition_that_may_get_enabled = self.moves[move_index].get_transition().unwrap();

        //first, figure out when this move's transition was last enabled
        let (mut result, mut state) =
            MoveEnabled::start(semantics, transition_that_may_get_enabled)?;
        for (move_index2, move2) in self.moves.iter().take(move_index).enumerate() {
            if let Some(transition2) = move2.get_transition() {
                //sync, model or silent move
                result.execute_transition(
                    semantics,
                    &mut state,
                    transition_that_may_get_enabled,
                    transition2,
                    move_index2,
                );
            } else {
                //skip log move
            }
        }
        result.finalise()
    }
}

#[derive(Debug, Copy, Clone)]
enum MoveEnabled {
    FromStartOfTrace,
    AsResultOfMove(usize),
    NotEnabled,
}

impl MoveEnabled {
    fn start<T, FS>(
        semantics: &T,
        transition_that_may_get_enabled: TransitionIndex,
    ) -> Option<(Self, FS)>
    where
        T: Semantics<SemState = FS> + Send + Sync + ?Sized,
        FS: Display + Debug + Clone + Hash + Eq,
    {
        let state = semantics
            .get_initial_state()
            .expect("there is no initial state");

        if semantics
            .get_enabled_transitions(&state)
            .contains(&transition_that_may_get_enabled)
        {
            Some((Self::FromStartOfTrace, state))
        } else {
            Some((Self::NotEnabled, state))
        }
    }

    fn execute_transition<T, FS>(
        &mut self,
        semantics: &T,
        state: &mut FS,
        transition_that_may_get_enabled: TransitionIndex,
        transition: TransitionIndex,
        move_index: usize,
    ) where
        T: Semantics<SemState = FS> + Send + Sync + ?Sized,
        FS: Display + Debug + Clone + Hash + Eq,
    {
        semantics
            .execute_transition(state, transition)
            .expect("transition was not enabled and nevertheless fired");

        let now_enabled = semantics
            .get_enabled_transitions(&state)
            .contains(&transition_that_may_get_enabled);
        *self = match (now_enabled, &self) {
            (true, MoveEnabled::FromStartOfTrace) => MoveEnabled::FromStartOfTrace,
            (true, MoveEnabled::AsResultOfMove(x)) => MoveEnabled::AsResultOfMove(*x),
            (true, MoveEnabled::NotEnabled) => MoveEnabled::AsResultOfMove(move_index),
            (false, MoveEnabled::FromStartOfTrace) => MoveEnabled::NotEnabled,
            (false, MoveEnabled::AsResultOfMove(_)) => MoveEnabled::NotEnabled,
            (false, MoveEnabled::NotEnabled) => MoveEnabled::NotEnabled,
        };
    }

    fn finalise(self) -> Option<usize> {
        match self {
            MoveEnabled::FromStartOfTrace => None,
            MoveEnabled::AsResultOfMove(move_index) => Some(move_index),
            MoveEnabled::NotEnabled => {
                panic!("transition was not enabled and it nevertheless fired")
            }
        }
    }
}

struct ExecutionsSorter {}

impl ExecutionsSorter {
    fn merge_sort(mut traces: Vec<VecDeque<Execution>>) -> Vec<Execution> {
        let number_of_executions = traces.iter().map(|t| t.len()).sum();
        let mut result = Vec::with_capacity(number_of_executions);

        //initialise first-timestamps
        let mut first_timestamps = (0..traces.len())
            .map(|trace_index| Self::get_first_timestamp(&traces[trace_index]).cloned())
            .collect::<Vec<_>>();

        loop {
            if let Some((trace_index, _)) = first_timestamps
                .iter()
                .enumerate()
                .filter_map(|(trace_index, first)| {
                    if let Some(first) = first {
                        Some((trace_index, first))
                    } else {
                        None
                    }
                })
                .min_by(|a, b| a.cmp(b))
            {
                //process the trace with the lowest timestamp

                //first, add all executions that do not have a timestamp
                while let Some(execution) = traces[trace_index].pop_front() {
                    let has_timestamp = execution.time_of_execution.is_none();
                    result.push(execution);

                    if has_timestamp {
                        break;
                    }
                }

                //second, add the execution that has a timestamp (if it exists)
                if let Some(execution) = traces[trace_index].pop_front() {
                    result.push(execution);
                }

                first_timestamps[trace_index] =
                    Self::get_first_timestamp(&traces[trace_index]).cloned();

                //check whether the trace still has timestamps
                if first_timestamps[trace_index].is_none() {
                    //This trace does not have timestamps anymore; finish it first to avoid postponing it until the end of the log.
                    let mut swap = VecDeque::new();
                    std::mem::swap(&mut swap, &mut traces[trace_index]);
                    result.extend(swap.into_iter());
                }
            } else {
                //no more timestamps, just append the result
                result.extend(traces.into_iter().flatten());
                return result;
            }
        }
    }

    fn get_first_timestamp(trace: &VecDeque<Execution>) -> Option<&DateTime<FixedOffset>> {
        for execution in trace {
            if execution.time_of_execution.is_some() {
                return execution.time_of_execution.as_ref();
            }
        }
        None
    }
}

#[cfg(test)]
mod tests {
    use std::fs;

    use ebi_objects::{
        StochasticDeterministicFiniteAutomaton, StochasticNondeterministicFiniteAutomaton,
        ebi_objects::event_log_event_attributes::EventLogEventAttributes,
    };

    use crate::{
        ebi_traits::ebi_trait_event_log_event_attributes::EbiTraitEventLogEventAttributes,
        techniques::executions::FindExecutions,
    };

    #[test]
    fn executions() {
        let fin = fs::read_to_string("testfiles/a-b.xes").unwrap();
        let log = fin.parse::<EventLogEventAttributes>().unwrap();

        let fin2 = fs::read_to_string("testfiles/a-b-c-livelock.sdfa").unwrap();
        let mut model = fin2
            .parse::<StochasticDeterministicFiniteAutomaton>()
            .unwrap();

        let out = fs::read_to_string("testfiles/a-b.exs").unwrap();

        let mut log2: Box<dyn EbiTraitEventLogEventAttributes> = Box::new(log);
        let x = model.find_executions(&mut log2).unwrap();

        assert_eq!(out, x.to_string() + "\n");
    }

    #[test]
    fn snfa_executions() {
        let fin = fs::read_to_string("testfiles/simple_log_markovian_abstraction.xes").unwrap();
        let log = fin.parse::<EventLogEventAttributes>().unwrap();

        let fin2 = fs::read_to_string("testfiles/aa-ab-ba.snfa").unwrap();
        let mut model = fin2
            .parse::<StochasticNondeterministicFiniteAutomaton>()
            .unwrap();

        let mut log2: Box<dyn EbiTraitEventLogEventAttributes> = Box::new(log);
        model.find_executions(&mut log2).unwrap();
    }
}
