use anyhow::{anyhow, Result};
use std::io::BufRead;

use crate::{
    ebi_framework::{
        ebi_input::EbiInput, ebi_object::EbiTraitObject, ebi_trait::FromEbiTraitObject,
        importable::Importable,
    },
    ebi_objects::finite_stochastic_language::FiniteStochasticLanguage,
    math::fraction::Fraction,
};

use super::{
    ebi_trait_finite_language::EbiTraitFiniteLanguage,
    ebi_trait_iterable_stochastic_language::EbiTraitIterableStochasticLanguage,
};

pub trait EbiTraitFiniteStochasticLanguage:
    EbiTraitIterableStochasticLanguage + EbiTraitFiniteLanguage + Sync
{
    fn get_trace_probability(&self, trace_index: usize) -> Option<&Fraction>;

    fn to_finite_stochastic_language(&self) -> FiniteStochasticLanguage;

    fn get_probability_sum(&self) -> Fraction;
}

impl FromEbiTraitObject for dyn EbiTraitFiniteStochasticLanguage {
    fn from_trait_object(object: EbiInput) -> Result<Box<Self>> {
        match object {
            EbiInput::Trait(EbiTraitObject::FiniteStochasticLanguage(e), _) => Ok(e),
            _ => Err(anyhow!(
                "cannot read {} {} as a finite stochastic language",
                object.get_type().get_article(),
                object.get_type()
            )),
        }
    }
}

pub fn import<X: 'static + Importable + EbiTraitFiniteStochasticLanguage>(
    reader: &mut dyn BufRead,
) -> Result<Box<dyn EbiTraitFiniteStochasticLanguage>> {
    match X::import(reader) {
        Ok(x) => Ok(Box::new(x)),
        Err(x) => Err(x),
    }
}
