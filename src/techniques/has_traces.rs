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
        process_tree_semantics::NodeStates,
        stochastic_deterministic_finite_automaton::StochasticDeterministicFiniteAutomaton,
        stochastic_labelled_petri_net::StochasticLabelledPetriNet,
    },
    ebi_traits::{ebi_trait_event_log::IndexTrace, ebi_trait_semantics::Semantics},
    math::traits::Zero,
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

macro_rules! lang {
    ($t:ident) => {
        impl HasTraces for $t {
            type LivState = usize;

            fn has_traces(&self) -> Result<bool> {
                Ok(!self.len().is_zero())
            }
        }
    };
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
        techniques::has_traces::HasTraces,
    };

    #[test]
    fn has_traces_tree() {
        let fin = fs::read_to_string("testfiles/empty.ptree").unwrap();
        let tree = fin.parse::<ProcessTree>().unwrap();

        assert!(tree.has_traces().unwrap());
    }

    #[test]
    fn has_traces_lpn() {
        let fin = fs::read_to_string("testfiles/a-a-livelock.lpn").unwrap();
        let lpn = fin.parse::<LabelledPetriNet>().unwrap();

        assert!(!lpn.has_traces().unwrap());
    }

    #[test]
    fn has_traces_slpn() {
        let fin = fs::read_to_string("testfiles/infinite_bs.slpn").unwrap();
        let slpn = fin.parse::<StochasticLabelledPetriNet>().unwrap();

        assert!(slpn.has_traces().unwrap());
    }

    #[test]
    fn has_traces_dfa() {
        let fin = fs::read_to_string("testfiles/empty.dfa").unwrap();
        let dfa = fin.parse::<DeterministicFiniteAutomaton>().unwrap();

        assert!(!dfa.has_traces().unwrap());
    }

    #[test]
    fn has_traces_sdfa() {
        let fin = fs::read_to_string("testfiles/empty.sdfa").unwrap();
        let sdfa = fin
            .parse::<StochasticDeterministicFiniteAutomaton>()
            .unwrap();

        assert!(!sdfa.has_traces().unwrap());
    }

    #[test]
    fn has_traces_slang() {
        let fin = fs::read_to_string("testfiles/empty.slang").unwrap();
        let slang = fin.parse::<FiniteStochasticLanguage>().unwrap();

        assert!(!slang.has_traces().unwrap());
    }
}
