use anyhow::{Result, anyhow};
use ebi_objects::{
    FiniteStochasticLanguage, Importable, IntoRefProbabilityIterator, IntoRefTraceIterator,
    IntoRefTraceProbabilityIterator, traits::importable::ImporterParameterValues,
};
use std::io::BufRead;

use crate::ebi_framework::{
    ebi_input::EbiInput, ebi_trait::FromEbiTraitObject, ebi_trait_object::EbiTraitObject,
};

pub trait EbiTraitIterableStochasticLanguage:
    IntoRefTraceIterator + IntoRefProbabilityIterator + IntoRefTraceProbabilityIterator
{
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

impl EbiTraitIterableStochasticLanguage for FiniteStochasticLanguage {}

pub trait ToIterableStochasticLanguage: Importable {
    fn to_iterable_stochastic_language(self) -> Box<dyn EbiTraitIterableStochasticLanguage>;

    fn import_as_iterable_stochastic_language(
        reader: &mut dyn BufRead,
        parameter_values: &ImporterParameterValues,
    ) -> Result<Box<dyn EbiTraitIterableStochasticLanguage>>
    where
        Self: Sized,
    {
        Ok(Self::import(reader, parameter_values)?.to_iterable_stochastic_language())
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
