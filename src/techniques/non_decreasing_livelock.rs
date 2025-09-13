use anyhow::Result;
use ebi_objects::{
    DeterministicFiniteAutomaton, DirectlyFollowsModel, LabelledPetriNet, ProcessTree,
    StochasticDeterministicFiniteAutomaton, StochasticDirectlyFollowsModel,
    StochasticLabelledPetriNet, StochasticProcessTree,
};

use crate::{
    ebi_framework::displayable::Displayable,
    semantics::{
        labelled_petri_net_semantics::LPNMarking, process_tree_semantics::NodeStates,
        semantics::Semantics,
    },
};

pub trait NonDecreasingLivelock {
    type LivState: Displayable;

    /**
     * Detects livelocks in which there is no choice: at each step, only one transition is enabled.
     */
    fn is_part_of_non_decreasing_livelock(&self, state: &mut Self::LivState) -> Result<bool>;
}

macro_rules! tree {
    ($t:ident) => {
        impl NonDecreasingLivelock for $t {
            type LivState = NodeStates;

            fn is_part_of_non_decreasing_livelock(
                &self,
                _state: &mut Self::LivState,
            ) -> Result<bool> {
                Ok(false)
            }
        }
    };
}

tree!(ProcessTree);
tree!(StochasticProcessTree);

macro_rules! lpn {
    ($t:ident, $v:expr) => {
        impl NonDecreasingLivelock for $t {
            type LivState = LPNMarking;

            fn is_part_of_non_decreasing_livelock(
                &self,
                state: &mut Self::LivState,
            ) -> Result<bool> {
                let mut trace = vec![];

                while !self.is_final_state(state) {
                    let enabled = self.get_enabled_transitions(state);
                    if enabled.len() != 1 {
                        return Ok(false);
                    }

                    let transition = enabled.into_iter().next().unwrap();

                    if let Some(pos) = trace.iter().position(|t| t == &transition) {
                        //we are in a loop

                        //create incidence vector
                        let incidence = (pos..trace.len()).into_iter().fold(
                            vec![0; self.get_number_of_places()],
                            |mut vec1, trace_index| {
                                let transition2 = trace[trace_index];
                                let vec2 = self.incidence_vector(transition2);
                                for (zref, aval) in vec1.iter_mut().zip(&vec2) {
                                    *zref += aval;
                                }
                                vec1
                            },
                        );

                        //if the incidence vector is negative somewhere, then the loop cannot be repeated forever
                        if incidence.iter().any(|x| x.is_negative()) {
                            return Ok(false);
                        }

                        //check that even if we would execute the loop an unbounded number of times, no other transition would become enabled.
                        {
                            //first, "execute" the loop a sufficient number of times
                            let omega = self.max_transition_input_arc_cardinality() + 1;
                            state.marking *= omega;

                            //technicality: after touching the state, we need to re-compute the enabled transitions
                            ($v)(self, state);

                            //then, walk through the loop again
                            for transition in trace.iter().skip(pos) {
                                if self.get_enabled_transitions(state).len() != 1 {
                                    return Ok(false);
                                }
                                self.execute_transition(state, *transition)?;
                            }
                        }

                        return Ok(true);
                    } else {
                        trace.push(transition);
                    }

                    self.execute_transition(state, transition)?;
                }

                Ok(false)
            }
        }
    };
}

macro_rules! is_non_decreasing_livelock_dfm {
    ($t:ident) => {
        impl NonDecreasingLivelock for $t {
            type LivState = usize;

            fn is_part_of_non_decreasing_livelock(&self, state: &mut usize) -> Result<bool> {
                let mut trace = vec![*state];

                while !self.is_final_state(state) && self.get_enabled_transitions(state).len() == 1
                {
                    let transition = self
                        .get_enabled_transitions(state)
                        .into_iter()
                        .next()
                        .unwrap();
                    self.execute_transition(state, transition)?;

                    if trace.contains(state) {
                        //we are in a loop, and for DFM like models, that means it's a non-decreasing livelock
                        return Ok(true);
                    } else {
                        trace.push(*state);
                    }
                }

                Ok(false)
            }
        }
    };
}

lpn!(
    LabelledPetriNet,
    crate::semantics::labelled_petri_net_semantics::compute_enabled_transitions
);
lpn!(
    StochasticLabelledPetriNet,
    crate::semantics::stochastic_labelled_petri_net_semantics::compute_enabled_transitions
);
is_non_decreasing_livelock_dfm!(DirectlyFollowsModel);
is_non_decreasing_livelock_dfm!(StochasticDirectlyFollowsModel);
is_non_decreasing_livelock_dfm!(DeterministicFiniteAutomaton);
is_non_decreasing_livelock_dfm!(StochasticDeterministicFiniteAutomaton);
