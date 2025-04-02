use anyhow::{anyhow, Result};
use std::{fmt::Display, io::BufRead};

use crate::ebi_framework::{
    ebi_input::EbiInput, ebi_object::EbiTraitObject,
    ebi_trait::FromEbiTraitObject, importable::Importable,
};

use super::{
    ebi_trait_event_log::IndexTrace, ebi_trait_iterable_language::EbiTraitIterableLanguage,
};

pub trait EbiTraitFiniteLanguage: IndexTrace + Display + EbiTraitIterableLanguage + Send + Sync {}

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

pub fn import<X: 'static + Importable + EbiTraitFiniteLanguage>(
    reader: &mut dyn BufRead,
) -> Result<Box<dyn EbiTraitFiniteLanguage>> {
    match X::import(reader) {
        Ok(x) => Ok(Box::new(x)),
        Err(x) => Err(x),
    }
}
