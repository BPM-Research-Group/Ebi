use crate::{activity_key::ActivityKey, ebi_input_output::EbiInput, ebi_objects::{ebi_object::EbiTraitObject, finite_stochastic_language::FiniteStochasticLanguage}, follower_semantics::FollowerSemantics, math::fraction::Fraction};
use anyhow::{anyhow, Result};

use super::ebi_trait::FromEbiTraitObject;

pub trait EbiTraitQueriableStochasticLanguage {
    fn get_activity_key(&self) -> &ActivityKey;

    fn get_activity_key_mut(&mut self) -> &mut ActivityKey;

    fn get_probability(&self, follower: &FollowerSemantics) -> Result<Fraction>;
}

impl FromEbiTraitObject for dyn EbiTraitQueriableStochasticLanguage {
    fn from_trait_object(object: EbiInput) -> Result<Box<Self>> {
        match object {
            EbiInput::Trait(EbiTraitObject::QueriableStochasticLanguage(e), _) => Ok(e),
            _ => Err(anyhow!("cannot read {} {} as a queriable stochastic language", object.get_type().get_article(), object.get_type()))
        }
    }
}