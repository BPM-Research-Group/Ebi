use crate::{ebi_framework::displayable::Displayable, semantics::semantics::Semantics};
use bitvec::{bitvec, prelude::Lsb0, vec::BitVec};
use ebi_objects::{
    Activity, FiniteStochasticPartiallyOrderedLanguage, anyhow::Result,
    ebi_objects::labelled_petri_net::TransitionIndex,
};
use std::fmt::{Debug, Display};

impl Semantics for FiniteStochasticPartiallyOrderedLanguage {
    type SemState = FspolangMarking;

    fn get_initial_state(&self) -> Option<<Self as Semantics>::SemState> {
        if self.number_of_traces() > 0 {
            Some(FspolangMarking {
                trace: None,
                event_2_executed: bitvec![],
            })
        } else {
            None
        }
    }

    fn execute_transition(
        &self,
        state: &mut <Self as Semantics>::SemState,
        transition: TransitionIndex,
    ) -> Result<()> {
        //transition = event
        if state.trace.is_some() {
            state.event_2_executed.set(transition, true);
        } else {
            state.trace = Some(transition);
            state.event_2_executed = bitvec!(0; self.traces[transition].number_of_events());
        }
        Ok(())
    }

    fn is_final_state(&self, state: &<Self as Semantics>::SemState) -> bool {
        if let Some(_) = state.trace {
            state.event_2_executed.all()
        } else {
            true
        }
    }

    fn is_transition_silent(
        &self,
        transition: TransitionIndex,
        state: &<Self as Semantics>::SemState,
    ) -> bool {
        self.get_transition_activity(transition, state).is_none()
    }

    fn get_transition_activity(
        &self,
        transition: TransitionIndex,
        state: &<Self as Semantics>::SemState,
    ) -> Option<Activity> {
        if let Some(trace) = state.trace {
            Some(self.traces[trace].event_2_activity[transition])
        } else {
            None
        }
    }

    fn get_enabled_transitions(
        &self,
        state: &<Self as Semantics>::SemState,
    ) -> Vec<TransitionIndex> {
        if let Some(trace) = state.trace {
            let trace = &self.traces[trace];
            let mut result = vec![];
            'outer: for event in 0..trace.number_of_events() {
                if !state.event_2_executed[event] {
                    for predecessor in &trace.event_2_predecessors[event] {
                        if !state.event_2_executed[*predecessor] {
                            continue 'outer;
                        }
                    }
                    result.push(event);
                }
            }
            result
        } else {
            (0..self.number_of_traces()).collect()
        }
    }

    fn number_of_transitions(&self, state: &<Self as Semantics>::SemState) -> usize {
        if let Some(trace) = state.trace {
            self.traces[trace].number_of_edges()
        } else {
            self.number_of_traces()
        }
    }
}

#[derive(Clone, Eq, PartialEq, Hash, Debug)]
pub struct FspolangMarking {
    trace: Option<usize>,
    event_2_executed: BitVec,
}

impl Display for FspolangMarking {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "trace: {:?}, events: {:?}",
            self.trace, self.event_2_executed
        )
    }
}

impl Displayable for FspolangMarking {}
