use crate::ebi_framework::{
    ebi_input::EbiInput, ebi_trait::FromEbiTraitObject, ebi_trait_object::EbiTraitObject,
    trait_importers::ToIterableStochasticLanguageTrait,
};
use anyhow::{Result, anyhow};
use ebi_objects::{
    FiniteStochasticLanguage, Importable, IntoRefProbabilityIterator, IntoRefTraceIterator,
    IntoRefTraceProbabilityIterator,
};

pub trait EbiTraitIterableStochasticLanguage:
    IntoRefTraceIterator + IntoRefProbabilityIterator + IntoRefTraceProbabilityIterator
{
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
