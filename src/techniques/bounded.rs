use std::collections::{HashMap, hash_map::Entry};

use crate::{
    ebi_framework::displayable::Displayable,
    semantics::{
        labelled_petri_net_semantics::LPNMarking, process_tree_semantics::NodeStates,
        semantics::Semantics,
    },
};
use anyhow::Result;
use ebi_objects::{
    DeterministicFiniteAutomaton, DirectlyFollowsGraph, DirectlyFollowsModel, EventLog,
    FiniteLanguage, FiniteStochasticLanguage, LabelledPetriNet, ProcessTree,
    StochasticDeterministicFiniteAutomaton, StochasticDirectlyFollowsModel,
    StochasticLabelledPetriNet, StochasticProcessTree,
};

pub trait Bounded {
    type LivState: Displayable;

    /**
     * Returns whether the model has a bounded state space.
     */
    fn bounded(&self) -> Result<bool>;
}

impl Bounded for ProcessTree {
    type LivState = NodeStates;

    fn bounded(&self) -> Result<bool> {
        Ok(true)
    }
}

impl Bounded for StochasticProcessTree {
    type LivState = NodeStates;

    fn bounded(&self) -> Result<bool> {
        Ok(true)
    }
}

macro_rules! LPNMarking {
    ($t:ident) => {
        impl Bounded for $t {
            type LivState = LPNMarking;

            fn bounded(&self) -> Result<bool> {
                let mut graph = CoverabilityGraph::new();
                let mut queue;
                if let Some(initial_state) = self.get_initial_state() {
                    graph.add_state(&initial_state);
                    queue = vec![0usize];
                } else {
                    //a model without an initial marking has a bounded state space
                    return Ok(true);
                }

                while let Some(state_index) = queue.pop() {
                    let state = graph.get_state(state_index).clone();
                    let enabled_transitions = self.get_enabled_transitions(&state);

                    for transition in enabled_transitions {
                        let mut child_state = state.clone();
                        self.execute_transition(&mut child_state, transition)?;

                        if let Some(child_state_index) = graph.add_state(&child_state) {
                            graph.add_edge(state_index, child_state_index);
                            queue.push(child_state_index);

                            if graph.is_a_larger_marking_a_predecessor(child_state_index) {
                                return Ok(false);
                            }
                        } else {
                            //state was already present; do not proceed further with this branch
                        }
                    }
                }
                Ok(true)
            }
        }
    };
}

struct CoverabilityGraph {
    //edges are reversed to allow easy backward search
    index2state: Vec<LPNMarking>,
    state2index: HashMap<LPNMarking, usize>,
    edges: Vec<Vec<usize>>, //target, vec<source>
}

impl CoverabilityGraph {
    fn new() -> Self {
        Self {
            index2state: vec![],
            state2index: HashMap::new(),
            edges: vec![],
        }
    }

    /**
     * Attempts to add a state and returns its index if it is new.
     */
    fn add_state(&mut self, marking: &LPNMarking) -> Option<usize> {
        match self.state2index.entry(marking.clone()) {
            Entry::Occupied(_) => None,
            Entry::Vacant(vacant_entry) => {
                let index = self.index2state.len();
                vacant_entry.insert(self.index2state.len());
                self.index2state.push(marking.clone());
                self.edges.push(vec![]);
                Some(index)
            }
        }
    }

    fn get_state(&self, state_index: usize) -> &LPNMarking {
        &self.index2state[state_index]
    }

    /**
     * Adds an edge to the graph.
     */
    fn add_edge(&mut self, source: usize, target: usize) {
        self.edges[target].push(source);
    }

    /**
     * Returns whether a state has a predecessor that is strictly larger.
     */
    fn is_a_larger_marking_a_predecessor(&self, state: usize) -> bool {
        let mut reached = vec![false; self.index2state.len()];
        reached[state] = true;
        let mut queue = vec![];
        queue.push(state);

        while let Some(state) = queue.pop() {
            for predecessor in self.edges[state].iter() {
                if self.index2state[*predecessor].is_larger_than(&self.index2state[state]) {
                    return true;
                }
            }
        }

        false
    }
}

macro_rules! usize {
    ($t:ident) => {
        impl Bounded for $t {
            type LivState = usize;

            fn bounded(&self) -> Result<bool> {
                Ok(true)
            }
        }
    };
}

usize!(StochasticDeterministicFiniteAutomaton);
usize!(DeterministicFiniteAutomaton);
usize!(DirectlyFollowsModel);
usize!(StochasticDirectlyFollowsModel);
usize!(FiniteLanguage);
usize!(EventLog);
usize!(FiniteStochasticLanguage);
usize!(DirectlyFollowsGraph);
LPNMarking!(LabelledPetriNet);
LPNMarking!(StochasticLabelledPetriNet);

#[cfg(test)]
mod tests {
    use std::fs;

    use ebi_objects::{
        DeterministicFiniteAutomaton, FiniteStochasticLanguage, LabelledPetriNet, ProcessTree,
        StochasticDeterministicFiniteAutomaton, StochasticLabelledPetriNet,
    };

    use crate::techniques::bounded::Bounded;

    #[test]
    fn bounded_tree() {
        let fin = fs::read_to_string("testfiles/empty.ptree").unwrap();
        let tree = fin.parse::<ProcessTree>().unwrap();

        assert!(tree.bounded().unwrap());
    }

    #[test]
    fn bounded_lpn() {
        let fin = fs::read_to_string("testfiles/a-a-livelock.lpn").unwrap();
        let lpn = fin.parse::<LabelledPetriNet>().unwrap();

        assert!(lpn.bounded().unwrap());
    }

    #[test]
    fn bounded_slpn() {
        let fin = fs::read_to_string("testfiles/unbounded_empty.slpn").unwrap();
        let slpn = fin.parse::<StochasticLabelledPetriNet>().unwrap();

        assert!(!slpn.bounded().unwrap());
    }

    #[test]
    fn bounded_dfa() {
        let fin = fs::read_to_string("testfiles/empty.dfa").unwrap();
        let dfa = fin.parse::<DeterministicFiniteAutomaton>().unwrap();

        assert!(dfa.bounded().unwrap());
    }

    #[test]
    fn bounded_sdfa() {
        let fin = fs::read_to_string("testfiles/empty.sdfa").unwrap();
        let sdfa = fin
            .parse::<StochasticDeterministicFiniteAutomaton>()
            .unwrap();

        assert!(sdfa.bounded().unwrap());
    }

    #[test]
    fn bounded_slang() {
        let fin = fs::read_to_string("testfiles/empty.slang").unwrap();
        let slang = fin.parse::<FiniteStochasticLanguage>().unwrap();

        assert!(slang.bounded().unwrap());
    }
}
