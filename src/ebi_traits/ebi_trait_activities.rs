use std::io::BufRead;

use crate::ebi_framework::{
    activity_key::HasActivityKey, ebi_input::EbiInput, ebi_object::EbiTraitObject,
    ebi_trait::FromEbiTraitObject, importable::Importable,
};
use anyhow::{Result, anyhow};

pub trait EbiTraitActivities: HasActivityKey {}

impl<X> EbiTraitActivities for X where X: HasActivityKey {} //blanket implementation

impl FromEbiTraitObject for dyn EbiTraitActivities {
    fn from_trait_object(object: EbiInput) -> Result<Box<Self>> {
        match object {
            EbiInput::Trait(EbiTraitObject::Activities(e), _) => Ok(e),
            _ => Err(anyhow!(
                "cannot read {} {} as activities",
                object.get_type().get_article(),
                object.get_type()
            )),
        }
    }
}

pub fn import<X: 'static + Importable + EbiTraitActivities>(
    reader: &mut dyn BufRead,
) -> Result<Box<dyn EbiTraitActivities>> {
    match X::import(reader) {
        Ok(x) => Ok(Box::new(x)),
        Err(x) => Err(x),
    }
}
