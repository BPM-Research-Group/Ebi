use anyhow::{Result, anyhow};
use ebi_arithmetic::fraction::Fraction;
use std::{fmt::Debug, io::BufRead};

use crate::{
    ebi_framework::{
        activity_key::{Activity, ActivityKey, TranslateActivityKey},
        ebi_input::EbiInput,
        ebi_object::EbiTraitObject,
        ebi_trait::FromEbiTraitObject,
        importable::Importable,
    },
    ebi_objects::finite_stochastic_language::FiniteStochasticLanguage,
};

use super::{
    ebi_trait_finite_language::EbiTraitFiniteLanguage,
    ebi_trait_iterable_stochastic_language::EbiTraitIterableStochasticLanguage,
};

pub trait EbiTraitFiniteStochasticLanguage:
    EbiTraitIterableStochasticLanguage + EbiTraitFiniteLanguage + Sync + Debug + TranslateActivityKey
{
    fn get_probability_sum(&self) -> Fraction;

    // necessary for translations where order traces must be maintained
    fn translate(&mut self, target_activity_key: &mut ActivityKey);

    fn to_finite_stochastic_language(&self) -> FiniteStochasticLanguage;

    /**
     * Remove traces for which the function returns false.
     *
     * Note to callers: please put the closure definition inside the Box::new in the call of retain_traces.
     * Otherwise, Rust may give weird compile errors.
     */
    fn retain_traces<'a>(
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

pub fn import<X: 'static + Importable + EbiTraitFiniteStochasticLanguage>(
    reader: &mut dyn BufRead,
) -> Result<Box<dyn EbiTraitFiniteStochasticLanguage>> {
    match X::import(reader) {
        Ok(x) => Ok(Box::new(x)),
        Err(x) => Err(x),
    }
}
