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
                states: bitvec![],
                edges: bitvec![],
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
        if let Some(trace) = state.trace {
            let trace = &self.traces[trace];
            for output in &trace.edge_2_outputs[transition] {
                state.states.set(*output, true);
            }
            state.edges.set(transition, true);
        } else {
            state.trace = Some(transition);
            state.states.clear();
            state.states = bitvec!(0; self.traces[transition].number_of_states());
            state.states.fill(false);
        }
        Ok(())
    }

    fn is_final_state(&self, state: &<Self as Semantics>::SemState) -> bool {
        if let Some(_) = state.trace {
            state.edges.all()
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
            self.traces[trace].edge_2_activity[transition]
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
            'outer: for edge in 0..trace.number_of_edges() {
                if !state.edges[edge] {
                    for input in &trace.edge_2_inputs[edge] {
                        if !state.states[*input] {
                            continue 'outer;
                        }
                    }
                    result.push(edge);
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
    states: BitVec,
    edges: BitVec,
}

impl Display for FspolangMarking {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "trace: {:?}, states: {:?}, edges: {:?}",
            self.trace, self.states, self.edges
        )
    }
}

impl Displayable for FspolangMarking {}
