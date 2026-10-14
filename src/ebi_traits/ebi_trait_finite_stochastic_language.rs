use super::{
    ebi_trait_finite_language::EbiTraitFiniteLanguage,
    ebi_trait_iterable_stochastic_language::EbiTraitIterableStochasticLanguage,
};
use crate::ebi_framework::{
    ebi_input::EbiInput, ebi_trait::FromEbiTraitObject, ebi_trait_object::EbiTraitObject,
    trait_importers::ToFiniteStochasticLanguageTrait,
};
use ebi_objects::{
    Activity, Importable, TranslateActivityKey,
    anyhow::{Result, anyhow},
    ebi_arithmetic::{Fraction, Zero},
    ebi_objects::finite_stochastic_language::FiniteStochasticLanguage,
};
use std::fmt::Debug;

pub trait EbiTraitFiniteStochasticLanguage:
    EbiTraitIterableStochasticLanguage + EbiTraitFiniteLanguage + Sync + Debug + TranslateActivityKey
{
    fn get_probability_sum(&self) -> Fraction;

    fn to_finite_stochastic_language(&self) -> FiniteStochasticLanguage;

    /**
     * Remove traces for which the function returns false.
     * Normalisation is applied afterwards, that is, after the call, the sum of probabilities is 1, unless the language is empty.
     *
     * Note to callers: please put the closure definition inside the Box::new in the call of retain_traces.
     * Otherwise, Rust may give weird compile errors.
     */
    fn retain_traces_probabilities<'a>(
        &'a mut self,
        f: Box<dyn Fn(&Vec<Activity>, &Fraction) -> bool + 'static>,
    );

    /**
     * Remove traces for which the function returns false.
     * It is the responsibility of the caller to ensure normalisation. That is, probabilities will not be re-scaled.
     *
     * Note to callers: please put the closure definition inside the Box::new in the call of retain_traces.
     * Otherwise, Rust may give weird compile errors.
     */
    fn retain_traces_probabilities_mut<'a>(
        &'a mut self,
        f: Box<dyn Fn(&Vec<Activity>, &mut Fraction) -> bool + 'static>,
    );
}

impl FromEbiTraitObject for dyn EbiTraitFiniteStochasticLanguage {
    fn from_trait_object(object: EbiInput) -> Result<Box<Self>> {
        match object {
            EbiInput::Trait(EbiTraitObject::FiniteStochasticLanguage(e), _) => Ok(e),
            _ => Err(anyhow!(
                "cannot read {} {} as a finite stochastic language",
                object.get_type().get_article(),
                object.get_type()
            )),
        }
    }
}

impl EbiTraitFiniteStochasticLanguage for FiniteStochasticLanguage {
    fn to_finite_stochastic_language(&self) -> FiniteStochasticLanguage {
        self.clone()
    }

    fn get_probability_sum(&self) -> Fraction {
        self.traces.values().fold(Fraction::zero(), |mut x, y| {
            x += y;
            x
        })
    }

    fn retain_traces_probabilities<'a>(
        &'a mut self,
        f: Box<dyn Fn(&Vec<Activity>, &Fraction) -> bool + 'static>,
    )
    {
        self.traces.retain(|trace, probability| f(trace, probability) );
        self.normalise();
    }

    fn retain_traces_probabilities_mut<'a>(
        &'a mut self,
        f: Box<dyn Fn(&Vec<Activity>, &mut Fraction) -> bool + 'static>,
    ) {
        self.traces.retain(f);
    }
}

impl<T> ToFiniteStochasticLanguageTrait for T
where
    T: Into<FiniteStochasticLanguage> + Importable,
{
    fn to_finite_stochastic_language_trait(self) -> Box<dyn EbiTraitFiniteStochasticLanguage> {
        Box::new(Into::<FiniteStochasticLanguage>::into(self))
    }
}
