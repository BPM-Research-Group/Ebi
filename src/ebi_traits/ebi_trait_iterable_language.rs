use std::io::BufRead;

use anyhow::{Result, anyhow};
use ebi_objects::{
    FiniteLanguage, FiniteStochasticLanguage, HasActivityKey, Importable, IntoRefTraceIterator,
};

use crate::ebi_framework::{
    ebi_input::EbiInput, ebi_trait::FromEbiTraitObject, ebi_trait_object::EbiTraitObject,
};

pub trait EbiTraitIterableLanguage: HasActivityKey + IntoRefTraceIterator {}

impl FromEbiTraitObject for dyn EbiTraitIterableLanguage {
    fn from_trait_object(object: EbiInput) -> Result<Box<Self>> {
        match object {
            EbiInput::Trait(EbiTraitObject::IterableLanguage(e), _) => Ok(e),
            _ => Err(anyhow!(
                "Cannot read {} {} as an iterable stochastic language.",
                object.get_type().get_article(),
                object.get_type()
            )),
        }
    }
}

impl EbiTraitIterableLanguage for FiniteLanguage {}

impl EbiTraitIterableLanguage for FiniteStochasticLanguage {}

pub trait ToIterableLanguage: Importable {
    fn to_iterable_language(self) -> Box<dyn EbiTraitIterableLanguage>;

    fn import_as_iterable_language(
        reader: &mut dyn BufRead,
    ) -> Result<Box<dyn EbiTraitIterableLanguage>>
    where
        Self: Sized,
    {
        Ok(Self::import(reader)?.to_iterable_language())
    }
}

impl<T> ToIterableLanguage for T
where
    T: Into<FiniteLanguage> + Importable,
{
    fn to_iterable_language(self) -> Box<dyn EbiTraitIterableLanguage> {
        Box::new(Into::<FiniteLanguage>::into(self))
    }
}
