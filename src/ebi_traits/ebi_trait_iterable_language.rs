use crate::ebi_framework::{
    ebi_input::EbiInput, ebi_trait::FromEbiTraitObject, ebi_trait_object::EbiTraitObject,
    trait_importers::ToIterableLanguageTrait,
};
use ebi_objects::{
    anyhow::{anyhow, Result},
    FiniteLanguage, FiniteStochasticLanguage, HasActivityKey, Importable, IntoRefTraceIterator,
};

/// Allows for iteration over traces (which may not be finite).
///
/// For documentation on Ebi traits and mathematical definitions, please refer to the [manual](https://github.com/BPM-Research-Group/Ebi/releases/latest/download/manual.pdf).
pub trait EbiTraitIterableLanguage: HasActivityKey + IntoRefTraceIterator {
    fn trait_explanation() -> &'static str
    where
        Self: Sized,
    {
        "An iterable language allows to walk over its traces. May iterate over infinitely many traces and not terminate."
    }
}

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

impl<T> ToIterableLanguageTrait for T
where
    T: Into<FiniteLanguage> + Importable,
{
    fn to_iterable_language_trait(self) -> Box<dyn EbiTraitIterableLanguage> {
        Box::new(Into::<FiniteLanguage>::into(self))
    }
}
