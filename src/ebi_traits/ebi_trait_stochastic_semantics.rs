use std::{fmt::Display, hash::Hash, io::BufRead, rc::Rc};
use anyhow::{anyhow, Result};

use crate::{activity_key::Activity, ebi_input_output::EbiInput, ebi_objects::{ebi_object::EbiTraitObject, labelled_petri_net::LPNMarking}, math::fraction::Fraction};

use super::{ebi_semantics::Semantics, ebi_trait::FromEbiTraitObject};

pub enum EbiTraitStochasticSemantics {
	Marking(Box<dyn StochasticSemantics<State = LPNMarking>>),
	Usize(Box<dyn StochasticSemantics<State = usize>>)
}

impl FromEbiTraitObject for EbiTraitStochasticSemantics {
    fn from_trait_object(object: EbiInput) -> Result<Box<Self>> {
        match object {
            EbiInput::Trait(EbiTraitObject::StochasticSemantics(e), _) => Ok(Box::new(e)),
            _ => Err(anyhow!("cannot read {} {} as a stochastic semantics", object.get_type().get_article(), object.get_type()))
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
	fn get_stochastic_semantics(net: Rc<Self>) -> Box<dyn StochasticSemantics<State = Self::State>>;

	fn import_as_stochastic_semantics(reader: &mut dyn BufRead) -> Result<EbiTraitStochasticSemantics>;
}