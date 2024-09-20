use anyhow::{anyhow, Result};

use crate::{deterministic_semantics_for_stochastic_semantics::PMarking, ebi_framework::{activity_key::{Activity, ActivityKey}, ebi_input::EbiInput, ebi_object::EbiTraitObject, ebi_trait::FromEbiTraitObject}, ebi_objects::labelled_petri_net::LPNMarking, math::fraction::Fraction};


pub enum EbiTraitStochasticDeterministicSemantics {
	Usize(Box<dyn StochasticDeterministicSemantics<DState = usize>>),
    PMarking(Box<dyn StochasticDeterministicSemantics<DState = PMarking<LPNMarking>>>)
}

impl FromEbiTraitObject for EbiTraitStochasticDeterministicSemantics {
    fn from_trait_object(object: EbiInput) -> Result<Box<Self>> {
        match object {
            EbiInput::Trait(EbiTraitObject::StochasticDeterministicSemantics(e), _) => Ok(Box::new(e)),
            _ => Err(anyhow!("cannot read {} {} as a stochastic deterministic semantics", object.get_type().get_article(), object.get_type()))
        }
    }
}

pub trait StochasticDeterministicSemantics {
    type DState;

    fn get_activity_key(&self) -> &ActivityKey;

    /**
	 * (Re)set the semantics to the initial state.
	 */
    fn get_initial_state(&self) -> Result<Self::DState>;


    /**
     * Get the state that results from executing the activity. 
     * This method should not be called on activities that are not enabled or have a zero proability in this state.
    */
    fn execute_activity(&self, state: &Self::DState, activity: Activity) -> Result<Self::DState>;

    /**
     * 
    * @return whether the current state is a final state.
    */
    fn get_termination_probability(&self, state: &Self::DState) -> Fraction;

    /**
     * 
    * @param activity
    * @return the probability of the activity in the given state.
    */
    fn get_activity_probability(&self, state: &Self::DState, activity: Activity) -> Fraction;

    fn get_enabled_activities(&self, state: &Self::DState) -> Vec<Activity>;

}