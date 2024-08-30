use anyhow::{anyhow, Result};

use crate::{activity_key::Activity, ebi_input_output::EbiInput, ebi_objects::{ebi_object::EbiTraitObject, finite_stochastic_language::FiniteStochasticLanguage}, math::fraction::Fraction, Trace};

use super::{ebi_trait::FromEbiTraitObject, ebi_trait_finite_language::EbiTraitFiniteLanguage, ebi_trait_iterable_stochastic_language::EbiTraitIterableStochasticLanguage};

pub trait EbiTraitFiniteStochasticLanguage : EbiTraitIterableStochasticLanguage + EbiTraitFiniteLanguage + Sync {
    fn get_trace_proability(&self, trace_index: usize) -> Option<&Fraction>;

    fn to_finite_stochastic_language(&self) -> FiniteStochasticLanguage;
}

impl FromEbiTraitObject for dyn EbiTraitFiniteStochasticLanguage {
    fn from_trait_object(object: EbiInput) -> Result<Box<Self>> {
        match object {
            EbiInput::Trait(EbiTraitObject::FiniteStochasticLanguage(e), _) => Ok(e),
            _ => Err(anyhow!("cannot read {} {} as a finite stochastic language", object.get_type().get_article(), object.get_type()))
        }
    }
}