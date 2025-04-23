use anyhow::Result;

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
    techniques::livelock::IsPartOfLivelock,
};

pub trait HasTraces {
    type LivState: Displayable;

    /**
     * Returns whether the object has any traces.
     */
    fn has_traces(&self) -> Result<bool>;
}

impl HasTraces for ProcessTree {
    type LivState = NodeStates;

    fn has_traces(&self) -> Result<bool> {
        Ok(self.get_initial_state().is_none()) //an empty tree has no traces, otherwise a tree has traces
    }
}

macro_rules! lpn {
    ($t:ident) => {
        impl HasTraces for $t {
            type LivState = LPNMarking;

            fn has_traces(&self) -> Result<bool> {
                if let Some(initial_state) = self.get_initial_state() {
                    //If the initial state is not a livelock, the model has traces
                    return Ok(!self.is_state_part_of_livelock(&initial_state)?);
                } else {
                    //if there is no initial state, then the model has no traces.
                    return Ok(false);
                }
            }
        }
    };
}

macro_rules! dfm {
    ($t:ident) => {
        impl HasTraces for $t {
            type LivState = usize;

            fn has_traces(&self) -> Result<bool> {
                if let Some(initial_state) = self.get_initial_state() {
                    //If the initial state is not a livelock, the model has traces.
                    return Ok(!self.is_state_part_of_livelock(&initial_state)?);
                } else {
                    //if there is no initial state, then the model has no traces.
                    return Ok(false);
                }
            }
        }
    };
}

lpn!(LabelledPetriNet);
lpn!(StochasticLabelledPetriNet);
dfm!(DeterministicFiniteAutomaton);
dfm!(StochasticDeterministicFiniteAutomaton);

#[cfg(test)]
mod tests {
    use std::fs;

    use crate::{
        ebi_objects::{
            deterministic_finite_automaton::DeterministicFiniteAutomaton, labelled_petri_net::LabelledPetriNet, process_tree::ProcessTree, stochastic_deterministic_finite_automaton::StochasticDeterministicFiniteAutomaton, stochastic_labelled_petri_net::StochasticLabelledPetriNet
        },
        techniques::has_traces::HasTraces,
    };

    #[test]
    fn has_traces_ptree() {
        let fin = fs::read_to_string("testfiles/empty.ptree").unwrap();
        let tree = fin.parse::<ProcessTree>().unwrap();

        assert!(tree.has_traces().unwrap());
    }

    #[test]
    fn has_traces_lpn() {
        let fin = fs::read_to_string("testfiles/a-a-livelock.lpn").unwrap();
        let tree = fin.parse::<LabelledPetriNet>().unwrap();

        assert!(!tree.has_traces().unwrap());
    }

    #[test]
    fn has_traces_slpn() {
        let fin = fs::read_to_string("testfiles/infinite_bs.slpn").unwrap();
        let tree = fin.parse::<StochasticLabelledPetriNet>().unwrap();

        assert!(tree.has_traces().unwrap());
    }

    #[test]
    fn has_traces_dfa() {
        let fin = fs::read_to_string("testfiles/empty.dfa").unwrap();
        let tree = fin.parse::<DeterministicFiniteAutomaton>().unwrap();

        assert!(!tree.has_traces().unwrap());
    }

    #[test]
    fn has_traces_sdfa() {
        let fin = fs::read_to_string("testfiles/empty.sdfa").unwrap();
        let tree = fin.parse::<StochasticDeterministicFiniteAutomaton>().unwrap();

        assert!(!tree.has_traces().unwrap());
    }
}
