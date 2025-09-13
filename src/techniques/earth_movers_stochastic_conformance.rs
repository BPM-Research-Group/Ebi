use crate::{
    ebi_traits::ebi_trait_finite_stochastic_language::EbiTraitFiniteStochasticLanguage,
    math::{distances::WeightedDistances, distances_matrix::WeightedDistanceMatrix},
};
use anyhow::Result;
use ebi_arithmetic::Fraction;

pub trait EarthMoversStochasticConformance {
    fn earth_movers_stochastic_conformance(
        &mut self,
        lang_b: &mut dyn EbiTraitFiniteStochasticLanguage,
    ) -> Result<Fraction>;
}

impl EarthMoversStochasticConformance for dyn EbiTraitFiniteStochasticLanguage {
    /// Calculate the Earth Movers Stochastic Conformance (EMSC) between two finite stochastic languages.
    /// Return its value as a Fraction within the range [0, 1].
    ///
    /// # Description
    /// Function is called on the first language, the second language is passed as an argument.
    /// Note that the second language is mutable, as the it is necessary to translate it to match the first language.
    ///
    /// # Example
    /// ```ignore
    /// lang_a.earth_movers_stochastic_conformance(lang_b.as_mut())
    /// ```
    fn earth_movers_stochastic_conformance(
        &mut self,
        lang_b: &mut dyn EbiTraitFiniteStochasticLanguage,
    ) -> Result<Fraction> {
        lang_b.translate_using_activity_key(self.activity_key_mut());
        let distances = WeightedDistanceMatrix::new(self, lang_b);
        let distances: Box<dyn WeightedDistances> = Box::new(distances);
        distances.earth_movers_stochastic_conformance()
    }
}
