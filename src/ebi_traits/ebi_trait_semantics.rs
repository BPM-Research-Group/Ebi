use std::{fmt::{Debug, Display}, hash::Hash};
use anyhow::{anyhow, Result};
use crate::{ebi_framework::{activity_key::{Activity, ActivityKey}, ebi_input::EbiInput, ebi_object::EbiTraitObject, ebi_trait::FromEbiTraitObject}, ebi_objects::labelled_petri_net::LPNMarking};

use super::ebi_trait_stochastic_semantics::TransitionIndex;

pub enum EbiTraitSemantics {
	Marking(Box<dyn Semantics<State = LPNMarking>>),
	Usize(Box<dyn Semantics<State = usize>>)
}

impl FromEbiTraitObject for EbiTraitSemantics {
    fn from_trait_object(object: EbiInput) -> Result<Box<Self>> {
        match object {
            EbiInput::Trait(EbiTraitObject::Semantics(e), _) => Ok(Box::new(e)),
            _ => Err(anyhow!("Cannot read {} {} as a semantics.", object.get_type().get_article(), object.get_type()))
        }
    }
}

impl EbiTraitSemantics {
	pub fn get_activity_key(&self) -> &ActivityKey {
		match self {
			EbiTraitSemantics::Marking(semantics) => semantics.get_activity_key(),
			EbiTraitSemantics::Usize(semantics) => semantics.get_activity_key(),
		}
	}

	pub fn get_activity_key_mut(&mut self) -> &mut ActivityKey {
		match self {
			EbiTraitSemantics::Marking(semantics) => semantics.get_activity_key_mut(),
			EbiTraitSemantics::Usize(semantics) => semantics.get_activity_key_mut(),
		}
	}
}

pub trait Semantics : Debug {
	type State: Eq + Hash + Clone + Display;

	fn get_activity_key(&self) -> &ActivityKey;

	fn get_activity_key_mut(&mut self) -> &mut ActivityKey;

    /**
	 * (Re)set the semantics to the initial state.
	 */
    fn get_initial_state(&self) -> Self::State;


    /**
	 * Update the state to reflect execution of the transition. This alters the state to avoid repeated memory allocations in simple walkthroughs.
	 * Will return an error when the transition is not enabled, or when the marking cannot be represented (unbounded).
	 * 
	 * @param transition
	 */
    fn execute_transition(&self, state: &mut Self::State, transition: TransitionIndex) -> Result<()>;

    /**
	 * 
	 * @return whether the current state is a final state. In a final state, no other transitions may be enabled.
	 */
    fn is_final_state(&self, state: &Self::State) -> bool;

	fn is_transition_silent(&self, transition: TransitionIndex) -> bool;

	fn get_transition_activity(&self, transition: TransitionIndex) -> Option<Activity>;

	fn get_enabled_transitions(&self, state: &Self::State) -> Vec<TransitionIndex>;

}