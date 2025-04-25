use std::usize;

use crate::{
    ebi_framework::displayable::Displayable,
    ebi_objects::{
        deterministic_finite_automaton::DeterministicFiniteAutomaton,
        event_log::EventLog,
        finite_language::FiniteLanguage,
        finite_stochastic_language::FiniteStochasticLanguage,
        labelled_petri_net::{LPNMarking, LabelledPetriNet},
        process_tree::{Node, Operator, ProcessTree},
        process_tree_semantics::NodeStates,
        stochastic_deterministic_finite_automaton::StochasticDeterministicFiniteAutomaton,
    },
    ebi_traits::ebi_trait_semantics::Semantics,
    techniques::livelock::IsPartOfLivelock,
};
use anyhow::Result;

pub trait InfinitelyManyTraces {
    type LivState: Displayable;

    /**
     * Returns whether the model has infinitely many traces.
     */
    fn infinitely_many_traces(&self) -> Result<bool>;
}

impl InfinitelyManyTraces for ProcessTree {
    type LivState = NodeStates;

    fn infinitely_many_traces(&self) -> Result<bool> {
        for node in 0..self.get_number_of_nodes() {
            if let Some(Node::Operator(Operator::Loop, _)) = self.get_node(node) {
                //see whether at least one leaf in the loop is an activity
                for child in self.get_descendants(node) {
                    if let Node::Activity(_) = child {
                        return Ok(true);
                    }
                }
            }
        }
        return Ok(false);
    }
}

macro_rules! dfa {
    ($t:ident) => {
        impl InfinitelyManyTraces for $t {
            type LivState = usize;

            fn infinitely_many_traces(&self) -> Result<bool> {
                //in a DFA-like model, we must find a loop that has an activity on it and that has a state that is not in a livelock.
                let mut queue = vec![];
                let mut distance_from_initial = vec![usize::MAX; self.get_max_state() + 2];
                if let Some(initial_state) = self.get_initial_state() {
                    queue.push(initial_state);
                    distance_from_initial[initial_state] = 0;
                } else {
                    //a DFA without initial state has no traces.
                    return Ok(false);
                }

                // log::debug!("queue {:?}", queue);
                // log::debug!("distances {:?}", distance_from_initial);

                while let Some(state) = queue.pop() {
                    let state_distance = distance_from_initial[state];

                    // log::debug!("state {}, distance from initial {}", state, state_distance);

                    for transition in self.get_enabled_transitions(&state) {
                        let mut child_state = state;
                        self.execute_transition(&mut child_state, transition)?;

                        let child_distance = distance_from_initial[child_state];
                        if state_distance + 1 > child_distance {
                            //loopback edge detected

                            //in a DFA, every transition is labelled, so the loop causes infinitely many paths.
                            // log::debug!("loop detected with node {}", child_state);

                            //however, there must be a path to a final state for these paths to be traces.
                            if !self.is_state_part_of_livelock(&state)? {
                                return Ok(true);
                            }
                        } else if child_distance == usize::MAX {
                            //child hit for the first time
                            distance_from_initial[child_state] = state_distance + 1;
                            queue.push(child_state);
                        }
                    }
                }
                Ok(false)
            }
        }
    };
}

macro_rules! lang {
    ($t:ident) => {
        impl InfinitelyManyTraces for $t {
            type LivState = usize;

            fn infinitely_many_traces(&self) -> Result<bool> {
                Ok(false)
            }
        }
    };
}

dfa!(DeterministicFiniteAutomaton);
dfa!(StochasticDeterministicFiniteAutomaton);
lang!(EventLog);
lang!(FiniteLanguage);
lang!(FiniteStochasticLanguage);
