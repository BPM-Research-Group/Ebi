use crate::{ebi_traits::ebi_trait_finite_stochastic_language::EbiTraitFiniteStochasticLanguage, math::{distances::WeightedDistances, distances_matrix::WeightedDistanceMatrix}};
use ebi_objects::{
    FiniteStochasticPartiallyOrderedLanguage, HasActivityKey, TranslateActivityKey, anyhow::Result,
    ebi_arithmetic::Fraction,
};

pub trait PartiallyOrderedEarthMoversStochasticConformance {
    fn partially_ordered_earth_movers_stochastic_conformance(
        &mut self,
        lang_b: &mut FiniteStochasticPartiallyOrderedLanguage,
    ) -> Result<Fraction>;
}

impl PartiallyOrderedEarthMoversStochasticConformance for FiniteStochasticPartiallyOrderedLanguage {
    /// Calculate the Earth Movers Stochastic Conformance (EMSC) between two finite stochastic partially ordered languages.
    /// Return its value as a Fraction within the range [0, 1].
    fn partially_ordered_earth_movers_stochastic_conformance(
        &mut self,
        lang_b: &mut FiniteStochasticPartiallyOrderedLanguage,
    ) -> Result<Fraction> {
        lang_b.translate_using_activity_key(self.activity_key_mut());
        let distances = WeightedDistanceMatrix::from_partially_ordered_languages(self, lang_b);
        let distances: Box<dyn WeightedDistances> = Box::new(distances);
        distances.earth_movers_stochastic_conformance()
    }
}

impl PartiallyOrderedEarthMoversStochasticConformance for dyn EbiTraitFiniteStochasticLanguage {
    /// Calculate the Earth Movers Stochastic Conformance (EMSC) between two finite (stochastic partially) ordered languages.
    /// Return its value as a Fraction within the range [0, 1].
    fn partially_ordered_earth_movers_stochastic_conformance(
        &mut self,
        lang_b: &mut FiniteStochasticPartiallyOrderedLanguage,
    ) -> Result<Fraction> {
        lang_b.translate_using_activity_key(self.activity_key_mut());
        let distances = WeightedDistanceMatrix::from_total_and_partially_ordered_languages(self, lang_b);
        let distances: Box<dyn WeightedDistances> = Box::new(distances);
        distances.earth_movers_stochastic_conformance()
    }
}