use anyhow::Result;

use crate::{
    ebi_framework::displayable::Displayable,
    ebi_objects::{
        labelled_petri_net::{LPNMarking, LabelledPetriNet},
        process_tree::ProcessTree,
        process_tree_semantics::NodeStates,
        stochastic_labelled_petri_net::StochasticLabelledPetriNet,
    },
    ebi_traits::ebi_trait_semantics::Semantics,
    techniques::livelock::HasLiveLock,
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
        Ok(self.get_initial_state().is_none()) //an empty tree is in a livelock, as it cannot finish
    }
}

macro_rules! lpn {
    ($t:ident) => {
        impl HasTraces for $t {
            type LivState = LPNMarking;

            fn has_traces(&self) -> Result<bool> {
                if let Some(initial_state) = self.get_initial_state() {
                    //otherwise, the model has traces if the initial state is not a livelock
                    return self.is_state_part_of_livelock(&initial_state);
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

#[cfg(test)]
mod tests {
    use std::fs;

    use crate::{
        ebi_objects::{
            labelled_petri_net::LabelledPetriNet, process_tree::ProcessTree,
            stochastic_labelled_petri_net::StochasticLabelledPetriNet,
        },
        techniques::has_traces::HasTraces,
    };

    #[test]
    fn livelock_ptree() {
        let fin = fs::read_to_string("testfiles/empty.ptree").unwrap();
        let tree = fin.parse::<ProcessTree>().unwrap();

        assert!(tree.has_traces().unwrap());
    }

    #[test]
    fn livelock_lpn() {
        let fin = fs::read_to_string("testfiles/a-a-livelock.lpn").unwrap();
        let tree = fin.parse::<LabelledPetriNet>().unwrap();

        assert!(tree.has_traces().unwrap());
    }

    #[test]
    fn livelock_slpn() {
        let fin = fs::read_to_string("testfiles/infinite_bs.slpn").unwrap();
        let tree = fin.parse::<StochasticLabelledPetriNet>().unwrap();

        assert!(!tree.has_traces().unwrap());
    }
}
