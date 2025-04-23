use crate::{
    ebi_framework::displayable::Displayable,
    ebi_objects::{
        labelled_petri_net::{LPNMarking, LabelledPetriNet},
        process_tree::ProcessTree, process_tree_semantics::NodeStates,
    },
    ebi_traits::ebi_trait_semantics::Semantics,
};
use anyhow::Result;
use scc::HashSet;

pub trait Livelock {
    type LivState: Displayable;

    /**
     * Testing for livelocks requires two calls: first, check for livelocks using this function.
     * Second, check for the particular state using the is_state_in_livelock method.
     */
    fn is_livelock_in_model_regardless_of_state(&self) -> Result<bool>;

    /**
     * Testing for livelocks requires two calls: first, check for livelocks for a particular state using this function.
     * Second, check for other types of livelocks using the is_livelock_in_model_regardless_of_state method.
     */
    fn is_state_in_livelock(&self, state: &mut Self::LivState) -> Result<bool>;
}

impl Livelock for ProcessTree {
    type LivState = NodeStates;

    fn is_livelock_in_model_regardless_of_state(&self) -> Result<bool> {
        Ok(false)
    }

    fn is_state_in_livelock(&self, _: &mut Self::LivState) -> Result<bool> {
        Ok(false)
    }
}

impl Livelock for LabelledPetriNet {
    type LivState = LPNMarking;

    fn is_livelock_in_model_regardless_of_state(&self) -> Result<bool> {
        //if there is a transition without an input place, the net is always livelocked.
        if self
            .transition2input_places
            .iter()
            .any(|input_places| input_places.is_empty())
        {
            return Ok(true);
        }

        //if there is no initial state, then the model is livelocked.
        if self.get_initial_state().is_none() {
            return Ok(true);
        }

        return Ok(false);
    }

    fn is_state_in_livelock(&self, state: &mut Self::LivState) -> Result<bool> {
        //for now, only works if the model is bounded
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
