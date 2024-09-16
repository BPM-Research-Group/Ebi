use std::{fmt::{Alignment, Display, Debug}, hash::Hash, io::BufRead, rc::Rc};
use anyhow::{anyhow, Result};

use crate::{ebi_traits::{ebi_trait::FromEbiTraitObject,ebi_trait_semantics::Semantics}, activity_key::{Activity, ActivityKey}, ebi_input_output::EbiInput, ebi_objects::{alignments::Alignments, ebi_object::EbiTraitObject, labelled_petri_net::LPNMarking}, math::fraction::Fraction};

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

    pub fn explain_trace(&self, balance: &Fraction, trace: &Vec<Activity>) -> Result<Alignments> {
        match self {
            EbiTraitStochasticSemantics::Usize(s) => {
                let semantics = s.as_ref();
                semantics.explain_trace(trace, balance)
            },
            EbiTraitStochasticSemantics::Marking(s) => {
                let semantics = s.as_ref();
                semantics.explain_trace(trace, balance)
            },
        }
    }
}

impl <FS: Hash + Display + Debug + Clone + Eq> dyn StochasticSemantics<State = FS> {

    pub fn explain_trace(&self, trace: &Vec<Activity>, balance: &Fraction) -> Result<Alignments> {
        todo!()
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