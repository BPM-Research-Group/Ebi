use crate::ebi_framework::{
    ebi_input::EbiInput, ebi_trait::FromEbiTraitObject, ebi_trait_object::EbiTraitObject,
    trait_importers::ToActivitiesTrait,
};
use anyhow::{Result, anyhow};
use ebi_objects::{HasActivityKey, Importable};

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

impl<T> ToActivitiesTrait for T
where
    T: HasActivityKey + Importable + 'static,
{
    fn to_activities_trait(self) -> Box<dyn EbiTraitActivities> {
        Box::new(self)
    }
}

#[cfg(test)]
pub mod tests {

    use crate::ebi_framework::{ebi_input::EbiInput, ebi_trait_object::EbiTraitObject};

    #[test]
    fn all_activities() {
        for (input, _, _, _) in crate::tests::get_all_test_files() {
            if let EbiInput::Trait(object, _) = input {
                if let EbiTraitObject::Activities(object) = object {
                    object.activity_key();
                }
            }
        }
    }
}
