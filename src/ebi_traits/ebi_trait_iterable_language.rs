use anyhow::{anyhow, Result};
use crate::{activity_key::{Activity, ActivityKey}, ebi_input_output::EbiInput, ebi_objects::ebi_object::EbiTraitObject, ActivityTrace};

use super::ebi_trait::FromEbiTraitObject;

pub trait EbiTraitIterableLanguage {
    fn get_activity_key(&self) -> &ActivityKey;

    fn iter(&self) -> Box<dyn Iterator<Item = &ActivityTrace> + '_>;
}

impl FromEbiTraitObject for dyn EbiTraitIterableLanguage {
    fn from_trait_object(object: EbiInput) -> Result<Box<Self>> {
        match object {
            EbiInput::Trait(EbiTraitObject::IterableLanguage(e), _) => Ok(e),
            _ => Err(anyhow!("Cannot read {} {} as an iterable stochastic language.", object.get_type().get_article(), object.get_type()))
        }
    }
}