use crate::ebi_framework::{
    ebi_input::EbiInput, ebi_trait::FromEbiTraitObject, ebi_trait_object::EbiTraitObject,
    trait_importers::ToActivitiesTrait,
};
use ebi_objects::{
    anyhow::{anyhow, Result},
    HasActivityKey, Importable,
};

/// Allows for access to activities.
///
/// For documentation on Ebi traits and mathematical definitions, please refer to the [manual](https://github.com/BPM-Research-Group/Ebi/releases/latest/download/manual.pdf).
pub trait EbiTraitActivities: HasActivityKey {
    fn trait_explanation() -> &'static str
    where
        Self: Sized,
    {
        "The trait ``activities'' provides access to activities."
    }
}

impl<X> EbiTraitActivities for X where X: HasActivityKey {} //blanket implementation

impl FromEbiTraitObject for dyn EbiTraitActivities {
    fn from_trait_object(object: EbiInput) -> Result<Box<Self>> {
        match object {
            EbiInput::Trait(EbiTraitObject::Activities(e), _) => Ok(e),
            _ => Err(anyhow!(
                "Cannot read {} {} as activities.",
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
        for (input, _, _, _) in crate::tests::tests::get_all_test_files() {
            if let EbiInput::Trait(object, _) = input {
                if let EbiTraitObject::Activities(object) = object {
                    object.activity_key();
                }
            }
        }
    }
}
