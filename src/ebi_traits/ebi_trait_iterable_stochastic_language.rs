use crate::ebi_framework::{
    ebi_input::EbiInput, ebi_trait::FromEbiTraitObject, ebi_trait_object::EbiTraitObject,
    trait_importers::ToIterableStochasticLanguageTrait,
};
use ebi_objects::{
    anyhow::{anyhow, Result},
    FiniteStochasticLanguage, Importable, IntoRefProbabilityIterator, IntoRefTraceIterator,
    IntoRefTraceProbabilityIterator,
};

/// Allows for iteration over traces and their probabilities (which may not be finite).
///
/// For documentation on Ebi traits and mathematical definitions, please refer to the [manual](https://github.com/BPM-Research-Group/Ebi/releases/latest/download/manual.pdf).
pub trait EbiTraitIterableStochasticLanguage:
    IntoRefTraceIterator + IntoRefProbabilityIterator + IntoRefTraceProbabilityIterator
{
    fn trait_explanation() -> &'static str
    where
        Self: Sized,
    {
        "An iterable stochastic language allows to walk over its traces and their probabilities. May iterate over infinitely many traces and not terminate."
    }
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

impl<T> ToIterableStochasticLanguageTrait for T
where
    T: Into<FiniteStochasticLanguage> + Importable,
{
    fn to_iterable_stochastic_language_trait(self) -> Box<dyn EbiTraitIterableStochasticLanguage> {
        Box::new(Into::<FiniteStochasticLanguage>::into(self))
    }
}
