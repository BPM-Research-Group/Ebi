use anyhow::Result;

use crate::{
    ebi_framework::displayable::Displayable,
    ebi_objects::{
        deterministic_finite_automaton::DeterministicFiniteAutomaton,
        directly_follows_model::DirectlyFollowsModel,
        event_log::EventLog,
        finite_language::FiniteLanguage,
        finite_stochastic_language::FiniteStochasticLanguage,
        labelled_petri_net::{LPNMarking, LabelledPetriNet},
        process_tree::ProcessTree,
        stochastic_process_tree_semantics::NodeStates,
        stochastic_deterministic_finite_automaton::StochasticDeterministicFiniteAutomaton,
        stochastic_labelled_petri_net::StochasticLabelledPetriNet, stochastic_process_tree::StochasticProcessTree,
    },
    ebi_traits::{ebi_trait_event_log::IndexTrace, ebi_trait_semantics::Semantics},
    math::traits::Zero,
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
                Ok(!self.len().is_zero())
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

#[cfg(test)]
mod tests {
    use std::fs;

    use crate::{
        ebi_objects::{
            deterministic_finite_automaton::DeterministicFiniteAutomaton,
            finite_stochastic_language::FiniteStochasticLanguage,
            labelled_petri_net::LabelledPetriNet, process_tree::ProcessTree,
            stochastic_deterministic_finite_automaton::StochasticDeterministicFiniteAutomaton,
            stochastic_labelled_petri_net::StochasticLabelledPetriNet,
        },
        techniques::any_traces::AnyTraces,
    };

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
