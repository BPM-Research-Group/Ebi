use super::ebi_trait_iterable_language::EbiTraitIterableLanguage;
use crate::ebi_framework::{
    ebi_input::EbiInput, ebi_trait::FromEbiTraitObject, ebi_trait_object::EbiTraitObject,
    trait_importers::ToFiniteLanguageTrait,
};
use ebi_objects::{
    FiniteLanguage, FiniteStochasticLanguage, Importable, NumberOfTraces,
    anyhow::{Result, anyhow},
};
use std::fmt::Display;

pub trait EbiTraitFiniteLanguage:
    Display + EbiTraitIterableLanguage + NumberOfTraces + Send + Sync
{
    fn to_finite_language(&self) -> FiniteLanguage;
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

impl EbiTraitFiniteLanguage for FiniteLanguage {
    fn to_finite_language(&self) -> FiniteLanguage {
        self.clone()
    }
}

impl EbiTraitFiniteLanguage for FiniteStochasticLanguage {
    fn to_finite_language(&self) -> FiniteLanguage {
        self.clone().into()
    }
}

impl<T> ToFiniteLanguageTrait for T
where
    T: Into<FiniteLanguage> + Importable,
{
    fn to_finite_language_trait(self) -> Box<dyn EbiTraitFiniteLanguage> {
        let x = Into::<FiniteLanguage>::into(self);
        Box::new(x)
    }
}
