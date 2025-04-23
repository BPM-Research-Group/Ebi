use crate::{
    ebi_framework::displayable::Displayable,
    ebi_objects::{
        deterministic_finite_automaton::DeterministicFiniteAutomaton,
        labelled_petri_net::{LPNMarking, LabelledPetriNet},
        process_tree::ProcessTree,
        process_tree_semantics::NodeStates,
        stochastic_deterministic_finite_automaton::StochasticDeterministicFiniteAutomaton,
        stochastic_labelled_petri_net::StochasticLabelledPetriNet,
    },
    ebi_traits::ebi_trait_semantics::Semantics,
};
use anyhow::Result;
use scc::HashSet;

pub trait IsPartOfLivelock {
    type LivState: Displayable;

    fn is_state_part_of_livelock(&self, state: &Self::LivState) -> Result<bool>;
}

impl IsPartOfLivelock for ProcessTree {
    type LivState = NodeStates;

    fn is_state_part_of_livelock(&self, _: &Self::LivState) -> Result<bool> {
        Ok(false)
    }
}

macro_rules! lpn {
    ($t:ident) => {
        impl IsPartOfLivelock for $t {
            type LivState = LPNMarking;

            fn is_state_part_of_livelock(&self, state: &Self::LivState) -> Result<bool> {
                //if there is a transition without an input place, the net is always livelocked.
                if self
                    .transition2input_places
                    .iter()
                    .any(|input_places| input_places.is_empty())
                {
                    return Ok(true);
                }

                //for now, the following only works if the model is bounded
                let mut queue = vec![];
                queue.push(state.clone());
                let visited = HashSet::new();
                let _ = visited.insert(state.clone());

                while let Some(state) = queue.pop() {
                    if self.is_final_state(&state) {
                        return Ok(false);
                    }

                    for transition in self.get_enabled_transitions(&state) {
                        let mut child_state = state.clone();
                        self.execute_transition(&mut child_state, transition)?;

                        if visited.insert(child_state.clone()).is_ok() {
                            queue.push(child_state);
                        }
                    }
                }

                return Ok(true);
            }
        }
    };
}

macro_rules! dfm {
    ($t:ident) => {
        impl IsPartOfLivelock for $t {
            type LivState = usize;

            fn is_state_part_of_livelock(&self, state: &Self::LivState) -> Result<bool> {
                let mut queue = vec![];
                queue.push(state.clone());
                let visited = HashSet::new();
                let _ = visited.insert(state.clone());

                while let Some(state) = queue.pop() {
                    if self.is_final_state(&state) {
                        return Ok(false);
                    }

                    for transition in self.get_enabled_transitions(&state) {
                        let mut child_state = state.clone();
                        self.execute_transition(&mut child_state, transition)?;

                        if visited.insert(child_state.clone()).is_ok() {
                            queue.push(child_state);
                        }
                    }
                }

                return Ok(true);
            }
        }
    };
}

lpn!(LabelledPetriNet);
lpn!(StochasticLabelledPetriNet);
dfm!(DeterministicFiniteAutomaton);
dfm!(StochasticDeterministicFiniteAutomaton);
