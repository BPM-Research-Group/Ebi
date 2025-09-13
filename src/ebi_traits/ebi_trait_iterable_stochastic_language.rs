use anyhow::{Result, anyhow};
use ebi_arithmetic::Fraction;
use ebi_objects::{Activity, FiniteStochasticLanguage, Importable};
use std::io::BufRead;

use crate::ebi_framework::{
    ebi_input::EbiInput, ebi_trait::FromEbiTraitObject, ebi_trait_object::EbiTraitObject,
};

pub trait EbiTraitIterableStochasticLanguage {
    //an iterable language is not necessarily finite
    fn iter_trace_probability(&self) -> Box<dyn Iterator<Item = (&Vec<Activity>, &Fraction)> + '_>;

    fn get_trace_probability(&self, trace_index: usize) -> Option<&Fraction>;
}

impl FromEbiTraitObject for dyn EbiTraitIterableStochasticLanguage {
    fn from_trait_object(object: EbiInput) -> Result<Box<Self>> {
        match object {
            EbiInput::Trait(EbiTraitObject::IterableStochasticLanguage(e), _) => Ok(e),
            _ => Err(anyhow!(
                "cannot read {} {} as an iterable stochastic language",
                object.get_type().get_article(),
                object.get_type()
            )),
        }
    }
}

impl EbiTraitIterableStochasticLanguage for FiniteStochasticLanguage {
    fn iter_trace_probability(&self) -> Box<dyn Iterator<Item = (&Vec<Activity>, &Fraction)> + '_> {
        Box::new(self.traces.iter())
    }

    fn get_trace_probability(&self, trace_index: usize) -> Option<&Fraction> {
        Some(self.traces.iter().nth(trace_index)?.1)
    }
}

pub trait ToIterableStochasticLanguage: Importable {
    fn to_iterable_stochastic_language(self) -> Box<dyn EbiTraitIterableStochasticLanguage>;

    fn import_as_iterable_stochastic_language(
        reader: &mut dyn BufRead,
    ) -> Result<Box<dyn EbiTraitIterableStochasticLanguage>>
    where
        Self: Sized,
    {
        Ok(Self::import(reader)?.to_iterable_stochastic_language())
    }
}

impl<T> ToIterableStochasticLanguage for T
where
    T: Into<FiniteStochasticLanguage> + Importable,
{
    fn to_iterable_stochastic_language(self) -> Box<dyn EbiTraitIterableStochasticLanguage> {
        Box::new(Into::<FiniteStochasticLanguage>::into(self))
    }
}
