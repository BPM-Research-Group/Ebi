use std::{fmt::Debug, io::BufRead};
use anyhow::{anyhow, Result};
use crate::{ebi_framework::{activity_key::{Activity, ActivityKey, HasActivityKey}, displayable::Displayable, ebi_input::EbiInput, ebi_object::EbiTraitObject, ebi_trait::FromEbiTraitObject, importable::Importable}, ebi_objects::{labelled_petri_net::LPNMarking, stochastic_process_tree_semantics::NodeStates}, techniques::align::AlignmentHeuristics};

use super::ebi_trait_stochastic_semantics::TransitionIndex;

pub enum EbiTraitSemantics {
	Usize(Box<dyn Semantics<SemState = usize, AliState = usize>>),
	Marking(Box<dyn Semantics<SemState = LPNMarking, AliState = LPNMarking>>),
	NodeStates(Box<dyn Semantics<SemState = NodeStates, AliState = NodeStates>>),
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
			EbiTraitSemantics::NodeStates(semantics) => semantics.get_activity_key(),
		}
	}

	pub fn get_activity_key_mut(&mut self) -> &mut ActivityKey {
		match self {
			EbiTraitSemantics::Marking(semantics) => semantics.get_activity_key_mut(),
			EbiTraitSemantics::Usize(semantics) => semantics.get_activity_key_mut(),
			EbiTraitSemantics::NodeStates(semantics) => semantics.get_activity_key_mut(),
		}
	}

}

pub trait Semantics : Debug + Send + Sync + AlignmentHeuristics<AliState = Self::SemState> + HasActivityKey {
	type SemState: Displayable;

    /**
	 * Get the initial state.
	 * If it does not exist, then the language is empty.
	 */
    fn get_initial_state(&self) -> Option<<Self as Semantics>::SemState>;


    /**
	 * Update the state to reflect execution of the transition. This alters the state to avoid repeated memory allocations in simple walkthroughs.
	 * May return an error when the transition is not enabled, or when the marking cannot be represented (unbounded).
	 * 
	 * @param transition
	 */
    fn execute_transition(&self, state: &mut <Self as Semantics>::SemState, transition: TransitionIndex) -> Result<()>;

    /**
	 * 
	 * @return whether the current state is a final state. In a final state, no other transitions may be enabled.
	 */
    fn is_final_state(&self, state: &<Self as Semantics>::SemState) -> bool;

	fn is_transition_silent(&self, transition: TransitionIndex) -> bool;

	fn get_transition_activity(&self, transition: TransitionIndex) -> Option<Activity>;

	fn get_enabled_transitions(&self, state: &<Self as Semantics>::SemState) -> Vec<TransitionIndex>;

	fn get_number_of_transitions(&self) -> usize;

}

pub trait ToSemantics: Importable + Sized {
	fn to_semantics(self) -> EbiTraitSemantics;

	fn import_as_semantics(reader: &mut dyn BufRead) -> Result<EbiTraitSemantics> {
		Ok(Self::import(reader)?.to_semantics())
	}
}