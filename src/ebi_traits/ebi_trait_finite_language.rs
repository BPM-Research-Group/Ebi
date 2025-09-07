use anyhow::{Result, anyhow};
use ebi_objects::{FiniteLanguage, FiniteStochasticLanguage, Importable, IndexTrace};
use std::{fmt::Display, io::BufRead};

use crate::ebi_framework::{
    ebi_input::EbiInput, ebi_trait::FromEbiTraitObject, ebi_trait_object::EbiTraitObject,
};

use super::ebi_trait_iterable_language::EbiTraitIterableLanguage;

pub trait EbiTraitFiniteLanguage:
    IndexTrace + Display + EbiTraitIterableLanguage + Send + Sync
{
}

impl FromEbiTraitObject for dyn EbiTraitFiniteLanguage {
    fn from_trait_object(object: EbiInput) -> Result<Box<Self>> {
        match object {
            EbiInput::Trait(EbiTraitObject::FiniteLanguage(e), _) => Ok(e),
            _ => Err(anyhow!(
                "cannot read {} {} as a finite language",
                object.get_type().get_article(),
                object.get_type()
            )),
        }
    }
}

impl EbiTraitFiniteLanguage for FiniteLanguage {}

impl EbiTraitFiniteLanguage for FiniteStochasticLanguage {}

pub trait ToFiniteLanguage: Importable {
    fn to_finite_language(self) -> Box<dyn EbiTraitFiniteLanguage>;

    fn import_as_finite_language(
        reader: &mut dyn BufRead,
    ) -> Result<Box<dyn EbiTraitFiniteLanguage>>
    where
        Self: Sized,
    {
        Ok(Self::import(reader)?.to_finite_language())
    }
}

impl<T> ToFiniteLanguage for T
where
    T: Into<FiniteLanguage> + Importable,
{
    fn to_finite_language(self) -> Box<dyn EbiTraitFiniteLanguage> {
        let x = Into::<FiniteLanguage>::into(self);
        Box::new(x)
    }
}
