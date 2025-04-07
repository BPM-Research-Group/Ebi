use crate::{
    ebi_traits::ebi_trait_finite_stochastic_language::EbiTraitFiniteStochasticLanguage,
    math::fraction::Fraction,
};
use anyhow::Result;

pub trait EarthMoversStochasticConformance {
    fn earth_movers_stochastic_conformance(
        &mut self,
        lang_b: &mut dyn EbiTraitFiniteStochasticLanguage,
    ) -> Result<Fraction>;
}
