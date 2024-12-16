use std::io::BufRead;
use anyhow::{anyhow, Result};

use crate::{ebi_framework::{activity_key::HasActivityKey, ebi_input::EbiInput, ebi_object::EbiTraitObject, ebi_trait::FromEbiTraitObject, importable::Importable}, follower_semantics::FollowerSemantics, math::fraction::Fraction};

pub trait EbiTraitQueriableStochasticLanguage: HasActivityKey + Sync {
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