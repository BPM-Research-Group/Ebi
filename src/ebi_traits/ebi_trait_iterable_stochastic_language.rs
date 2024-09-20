use std::{collections::hash_map::Iter, io::BufRead};

use anyhow::{anyhow, Result};

use crate::{activity_key::{Activity, ActivityKey}, ebi_input_output::EbiInput, ebi_objects::ebi_object::EbiTraitObject, import::Importable, math::fraction::Fraction, ActivityTrace, Trace};

use super::ebi_trait::FromEbiTraitObject;

pub trait EbiTraitIterableStochasticLanguage { //an iterable language is not necessarily finite
    fn iter_trace_probability(&self) -> Box<dyn Iterator<Item = (&ActivityTrace, &Fraction)> + '_>;

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