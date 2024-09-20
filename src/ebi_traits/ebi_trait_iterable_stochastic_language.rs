use std::io::BufRead;
use anyhow::{anyhow, Result};

use crate::{ebi_framework::{activity_key::Activity, ebi_input::EbiInput, ebi_object::EbiTraitObject, ebi_trait::FromEbiTraitObject, importable::Importable}, math::fraction::Fraction};


pub trait EbiTraitIterableStochasticLanguage { //an iterable language is not necessarily finite
    fn iter_trace_probability(&self) -> Box<dyn Iterator<Item = (&Vec<Activity>, &Fraction)> + '_>;

    fn get_probability(&self, trace_index: usize) -> Option<&Fraction>;
}

impl FromEbiTraitObject for dyn EbiTraitIterableStochasticLanguage {
    fn from_trait_object(object: EbiInput) -> Result<Box<Self>> {
        match object {
            EbiInput::Trait(EbiTraitObject::IterableStochasticLanguage(e), _) => Ok(e),
            _ => Err(anyhow!("Cannot read {} {} as an iterable stochastic language.", object.get_type().get_article(), object.get_type()))
        }
    }
}

pub fn import<X: 'static + Importable + EbiTraitIterableStochasticLanguage> (reader: &mut dyn BufRead) -> Result<Box<dyn EbiTraitIterableStochasticLanguage>> {
    match X::import(reader) {
        Ok(x) => Ok(Box::new(x)),
        Err(x) => Err(x),
    }
}