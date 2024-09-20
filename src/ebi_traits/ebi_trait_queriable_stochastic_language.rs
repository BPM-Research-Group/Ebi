use std::io::BufRead;
use anyhow::{anyhow, Result};

use crate::{ebi_framework::{activity_key::ActivityKey, ebi_input::EbiInput, ebi_object::EbiTraitObject, ebi_trait::FromEbiTraitObject, importable::Importable}, follower_semantics::FollowerSemantics, math::fraction::Fraction};

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

pub fn import<X: 'static + Importable + EbiTraitQueriableStochasticLanguage> (reader: &mut dyn BufRead) -> Result<Box<dyn EbiTraitQueriableStochasticLanguage>> {
    match X::import(reader) {
        Ok(x) => Ok(Box::new(x)),
        Err(x) => Err(x),
    }
}