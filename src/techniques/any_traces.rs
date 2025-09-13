use anyhow::Result;
use ebi_arithmetic::ebi_number::Zero;
use ebi_objects::{
    DeterministicFiniteAutomaton, DirectlyFollowsGraph, DirectlyFollowsModel, EventLog,
    FiniteLanguage, FiniteStochasticLanguage, IndexTrace, LabelledPetriNet, ProcessTree,
    StochasticDeterministicFiniteAutomaton, StochasticDirectlyFollowsModel,
    StochasticLabelledPetriNet, StochasticProcessTree,
};

use crate::{
    ebi_framework::displayable::Displayable,
    semantics::{
        labelled_petri_net_semantics::LPNMarking, process_tree_semantics::NodeStates,
        semantics::Semantics,
    },
    techniques::livelock::IsPartOfLivelock,
};

pub trait AnyTraces {
    type LivState: Displayable;

    /**
     * Returns whether the object has any traces.
     */
    fn any_traces(&self) -> Result<bool>;
}

impl AnyTraces for ProcessTree {
    type LivState = NodeStates;

    fn any_traces(&self) -> Result<bool> {
        Ok(self.get_initial_state().is_none()) //an empty tree has no traces, otherwise a tree has traces
    }
}

impl AnyTraces for StochasticProcessTree {
    type LivState = NodeStates;

    fn any_traces(&self) -> Result<bool> {
        Ok(self.get_initial_state().is_none()) //an empty tree has no traces, otherwise a tree has traces
    }
}

macro_rules! lang {
    ($t:ident) => {
        impl AnyTraces for $t {
            type LivState = usize;

            fn any_traces(&self) -> Result<bool> {
                Ok(!self.number_of_traces().is_zero())
            }
        }
    };
}

macro_rules! lpn {
    ($t:ident) => {
        impl AnyTraces for $t {
            type LivState = LPNMarking;

            fn any_traces(&self) -> Result<bool> {
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
        impl AnyTraces for $t {
            type LivState = usize;

            fn any_traces(&self) -> Result<bool> {
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

lang!(FiniteLanguage);
lang!(FiniteStochasticLanguage);
lang!(EventLog);
lpn!(LabelledPetriNet);
lpn!(StochasticLabelledPetriNet);
dfm!(DeterministicFiniteAutomaton);
dfm!(StochasticDeterministicFiniteAutomaton);
dfm!(DirectlyFollowsModel);
dfm!(StochasticDirectlyFollowsModel);
dfm!(DirectlyFollowsGraph);

#[cfg(test)]
mod tests {
    use std::fs;

    use ebi_objects::{
        DeterministicFiniteAutomaton, FiniteStochasticLanguage, LabelledPetriNet, ProcessTree,
        StochasticDeterministicFiniteAutomaton, StochasticLabelledPetriNet,
    };

    use crate::techniques::any_traces::AnyTraces;

    #[test]
    fn any_traces_tree() {
        let fin = fs::read_to_string("testfiles/empty.ptree").unwrap();
        let tree = fin.parse::<ProcessTree>().unwrap();

        assert!(tree.any_traces().unwrap());
    }

    #[test]
    fn any_traces_lpn() {
        let fin = fs::read_to_string("testfiles/a-a-livelock.lpn").unwrap();
        let lpn = fin.parse::<LabelledPetriNet>().unwrap();

        assert!(!lpn.any_traces().unwrap());
    }

    #[test]
    fn any_traces_slpn() {
        let fin = fs::read_to_string("testfiles/infinite_bs.slpn").unwrap();
        let slpn = fin.parse::<StochasticLabelledPetriNet>().unwrap();

        assert!(slpn.any_traces().unwrap());
    }

    #[test]
    fn any_traces_dfa() {
        let fin = fs::read_to_string("testfiles/empty.dfa").unwrap();
        let dfa = fin.parse::<DeterministicFiniteAutomaton>().unwrap();

        assert!(!dfa.any_traces().unwrap());
    }

    #[test]
    fn any_traces_sdfa() {
        let fin = fs::read_to_string("testfiles/empty.sdfa").unwrap();
        let sdfa = fin
            .parse::<StochasticDeterministicFiniteAutomaton>()
            .unwrap();

        assert!(!sdfa.any_traces().unwrap());
    }

    #[test]
    fn any_traces_slang() {
        let fin = fs::read_to_string("testfiles/empty.slang").unwrap();
        let slang = fin.parse::<FiniteStochasticLanguage>().unwrap();

        assert!(!slang.any_traces().unwrap());
    }
}
