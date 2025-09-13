use std::{
    collections::{HashMap, hash_map::Entry},
    fmt::Display,
};

use crate::{
    ebi_framework::displayable::Displayable,
    semantics::{
        labelled_petri_net_semantics::LPNMarking, process_tree_semantics::NodeStates,
        semantics::Semantics,
    },
    techniques::livelock::IsPartOfLivelock,
};
use anyhow::Result;
use ebi_objects::{
    DeterministicFiniteAutomaton, DirectlyFollowsGraph, DirectlyFollowsModel, EventLog,
    FiniteLanguage, FiniteStochasticLanguage, LabelledPetriNet, ProcessTree,
    StochasticDeterministicFiniteAutomaton, StochasticDirectlyFollowsModel,
    StochasticLabelledPetriNet, StochasticProcessTree,
    ebi_objects::process_tree::{Node, Operator},
};

pub trait InfinitelyManyTraces {
    type LivState: Displayable;

    /**
     * Returns whether the model has infinitely many traces.
     */
    fn infinitely_many_traces(&self) -> Result<bool>;
}

macro_rules! tree {
    ($t:ident) => {
        impl InfinitelyManyTraces for $t {
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
    };
}
tree!(ProcessTree);
tree!(StochasticProcessTree);

macro_rules! lpn {
    ($t:ident) => {
        //TODO: livelock check
        //TODO: unbounded models
        //TODO: silent loops
        impl InfinitelyManyTraces for $t {
            type LivState = LPNMarking;

            fn infinitely_many_traces(&self) -> Result<bool> {
                let mut graph = CycleGraph::new();
                let mut queue;
                if let Some(initial_state) = self.get_initial_state() {
                    graph.add_state(&initial_state);
                    queue = vec![0usize];
                } else {
                    //a model without an initial marking does not have infinitely many traces.
                    return Ok(false);
                }

                while let Some(state_index) = queue.pop() {
                    let state = graph.get_state(state_index).clone();

                    // log::debug!("marking {}, state_index {}", state, state_index);

                    let enabled_transitions = self.get_enabled_transitions(&state);

                    for transition in enabled_transitions {
                        let mut child_state = state.clone();
                        self.execute_transition(&mut child_state, transition)?;

                        let (child_state_index, new) = graph.add_state(&child_state);
                        if new {
                            queue.push(child_state_index);

                            // log::debug!(
                            //     "\tnew child\tmarking {}, state_index {}",
                            //     child_state, child_state_index
                            // );
                        }

                        graph.add_edge(
                            state_index,
                            child_state_index,
                            self.is_transition_silent(transition),
                        );

                        // log::debug!("\tgraph {}", graph);

                        if graph.search_for_non_silent_cycle(child_state_index) {
                            return Ok(true);
                        }
                    }
                }

                Ok(false)
            }
        }
    };
}

struct CycleGraph {
    //edges are stored reversely to allow easy backward search
    index2state: Vec<LPNMarking>,
    state2index: HashMap<LPNMarking, usize>,
    edges: Vec<Vec<usize>>,         //target, vec<source>
    is_edge_silent: Vec<Vec<bool>>, //target, vec<source>
}

impl CycleGraph {
    fn new() -> Self {
        Self {
            index2state: vec![],
            state2index: HashMap::new(),
            edges: vec![],
            is_edge_silent: vec![],
        }
    }

    /**
     * Attempts to add a state and returns its index and whether it is new.
     */
    fn add_state(&mut self, marking: &LPNMarking) -> (usize, bool) {
        match self.state2index.entry(marking.clone()) {
            Entry::Occupied(occupied_entry) => (*occupied_entry.get(), false),
            Entry::Vacant(vacant_entry) => {
                let index = self.index2state.len();
                vacant_entry.insert(self.index2state.len());
                self.index2state.push(marking.clone());
                self.edges.push(vec![]);
                self.is_edge_silent.push(vec![]);
                (index, true)
            }
        }
    }

    fn get_state(&self, state_index: usize) -> &LPNMarking {
        &self.index2state[state_index]
    }

    /**
     * Adds an edge to the graph.
     */
    fn add_edge(&mut self, source: usize, target: usize, is_silent: bool) {
        self.edges[target].push(source);
        self.is_edge_silent[target].push(is_silent);
    }

    /**
     * Returns whether there exists a predecessor state that:
     * - has a predecessor that is equal-or-larger;
     * - has a non-silent transition on a path from that predecessor;
     * - TODO: is not a livelock
     */
    fn search_for_non_silent_cycle(&self, state: usize) -> bool {
        let mut reached = vec![false; self.index2state.len()]; //all the states we reach at least one step before the given state
        let mut reached_with_non_silent_transition = vec![false; self.index2state.len()];
        let mut queue = vec![];
        queue.push(state);

        // log::debug!("\tsearch for silent cycle state index {}", state);

        while let Some(state) = queue.pop() {
            for (predecessor, edge_silent) in self.edges[state]
                .iter()
                .zip(self.is_edge_silent[state].iter())
            {
                reached_with_non_silent_transition[*predecessor] |= !edge_silent;
                if !reached[*predecessor] {
                    reached[*predecessor] = true;
                    queue.push(*predecessor);
                }

                if self.index2state[*predecessor]
                    .is_larger_than_or_equal_to(&self.index2state[state])
                    && reached_with_non_silent_transition[*predecessor]
                {
                    return true;
                }
            }
        }

        // log::debug!("\t\t reached {:?}", reached);
        // log::debug!("\t\tr_silent {:?}", reached_with_non_silent_transition);

        false
    }
}

impl Display for CycleGraph {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        writeln!(
            f,
            "nodes: {:?}, rev_edges {:?}, silent {:?}",
            self.index2state, self.edges, self.is_edge_silent
        )
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
                                log::debug!("witness of infinite traces: state {}", state);
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
dfa!(DirectlyFollowsModel);
dfa!(StochasticDirectlyFollowsModel);
dfa!(DirectlyFollowsGraph);
lpn!(LabelledPetriNet);
lpn!(StochasticLabelledPetriNet);
lang!(EventLog);
lang!(FiniteLanguage);
lang!(FiniteStochasticLanguage);

#[cfg(test)]
mod tests {
    use std::fs;

    use ebi_objects::{
        DeterministicFiniteAutomaton, LabelledPetriNet, ProcessTree,
        StochasticDeterministicFiniteAutomaton, StochasticLabelledPetriNet,
    };

    use crate::techniques::infinitely_many_traces::InfinitelyManyTraces;

    #[test]
    fn infinitely_many_traces_lpn() {
        let fin = fs::read_to_string("testfiles/a-loop.lpn").unwrap();
        let lpn = fin.parse::<LabelledPetriNet>().unwrap();

        assert!(lpn.infinitely_many_traces().unwrap());
    }

    #[test]
    fn infinitely_many_traces_slpn() {
        let fin = fs::read_to_string("testfiles/empty_net.slpn").unwrap();
        let slpn = fin.parse::<StochasticLabelledPetriNet>().unwrap();

        assert!(!slpn.infinitely_many_traces().unwrap());
    }

    #[test]
    fn infinitely_many_traces_dfa() {
        let fin = fs::read_to_string("testfiles/a-loop.dfa").unwrap();
        let lpn = fin.parse::<DeterministicFiniteAutomaton>().unwrap();

        assert!(lpn.infinitely_many_traces().unwrap());
    }

    #[test]
    fn infinitely_many_traces_sdfa() {
        let fin = fs::read_to_string("testfiles/a-livelock-zeroweight.sdfa").unwrap();
        let lpn = fin
            .parse::<StochasticDeterministicFiniteAutomaton>()
            .unwrap();

        assert!(!lpn.infinitely_many_traces().unwrap());
    }

    #[test]
    fn infinitely_many_traces_tree() {
        let fin = fs::read_to_string("testfiles/all_operators.ptree").unwrap();
        let lpn = fin.parse::<ProcessTree>().unwrap();

        assert!(lpn.infinitely_many_traces().unwrap());
    }
}
