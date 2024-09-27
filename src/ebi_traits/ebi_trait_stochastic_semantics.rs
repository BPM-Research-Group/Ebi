use std::{fmt::Display, hash::Hash, io::BufRead};
use anyhow::{anyhow, Result};

use crate::{ebi_framework::{activity_key::ActivityKey, ebi_input::EbiInput, ebi_object::EbiTraitObject, ebi_trait::FromEbiTraitObject}, ebi_objects::labelled_petri_net::LPNMarking, math::fraction::Fraction};

use super::ebi_trait_semantics::Semantics;

pub enum EbiTraitStochasticSemantics {
	Marking(Box<dyn StochasticSemantics<State = LPNMarking, AState = LPNMarking>>),
	Usize(Box<dyn StochasticSemantics<State = usize, AState = usize>>)
}

impl FromEbiTraitObject for EbiTraitStochasticSemantics {
    fn from_trait_object(object: EbiInput) -> Result<Box<Self>> {
        match object {
            EbiInput::Trait(EbiTraitObject::StochasticSemantics(e), _) => Ok(Box::new(e)),
            _ => Err(anyhow!("cannot read {} {} as a stochastic semantics", object.get_type().get_article(), object.get_type()))
        }
    }
}

impl EbiTraitStochasticSemantics {
	pub fn get_activity_key(&self) -> &ActivityKey {
		match self {
			EbiTraitStochasticSemantics::Marking(sem) => sem.get_activity_key(),
			EbiTraitStochasticSemantics::Usize(sem) => sem.get_activity_key(),
		}
	}

	pub fn get_activity_key_mut(&mut self) -> &mut ActivityKey {
		match self {
			EbiTraitStochasticSemantics::Marking(sem) => sem.get_activity_key_mut(),
			EbiTraitStochasticSemantics::Usize(sem) => sem.get_activity_key_mut(),
		}
	}
}

pub trait StochasticSemantics: Semantics {
	
    /**
	 * 
	 * @param transition
	 * @return the weight of the transition. This might depend on the state.
	 */
    fn get_transition_weight(&self, state: &Self::State, transition: TransitionIndex) -> &Fraction;
	
    /**
	 * 
	 * @param enabledTransitions
	 * @return the sum of the weight of the enabled transitions
	 */
    fn get_total_weight_of_enabled_transitions(&self, state: &Self::State) -> anyhow::Result<Fraction>;

}

pub type TransitionIndex = usize;

pub trait ToStochasticSemantics {
	type State: Eq + Hash + Clone + Display;
	fn get_stochastic_semantics(&self) -> Box<dyn StochasticSemantics<State = Self::State, AState = Self::State>>;

	fn import_as_stochastic_semantics(reader: &mut dyn BufRead) -> Result<EbiTraitStochasticSemantics>;
}