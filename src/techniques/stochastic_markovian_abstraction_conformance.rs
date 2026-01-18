use crate::{
    ebi_framework::{ebi_input::EbiInput, ebi_trait::FromEbiTraitObject},
    ebi_traits::{
        ebi_trait_finite_stochastic_language::EbiTraitFiniteStochasticLanguage,
        ebi_trait_queriable_stochastic_language::EbiTraitQueriableStochasticLanguage,
    },
    techniques::{
        chi_square_stochastic_conformance::ChiSquareStochasticConformance,
        hellinger_stochastic_conformance::HellingerStochasticConformance,
        stochastic_markovian_abstraction::MarkovianAbstraction,
        unit_earth_movers_stochastic_conformance::UnitEarthMoversStochasticConformance,
    },
};
use anyhow::{Result, anyhow};
use ebi_derive::EbiInputEnum;
use ebi_objects::{FiniteStochasticLanguage, ebi_arithmetic::Fraction};

/// Supported distance metrics for Markovian abstraction comparison.
#[derive(Clone, Copy, Debug, EbiInputEnum)]
pub enum DistanceMeasure {
    CSSC,
    HSC,
    UEMSC,
}

impl DistanceMeasure {
    /// Applies the distance measure to two finite stochastic languages.
    /// The caller must ensure that the activity keys match.
    pub fn apply(
        &self,
        language1: Box<dyn EbiTraitFiniteStochasticLanguage>,
        language2: Box<dyn EbiTraitQueriableStochasticLanguage>,
    ) -> Result<Fraction> {
        match self {
            DistanceMeasure::CSSC => language1.chi_square_stochastic_conformance(language2),
            DistanceMeasure::HSC => language1.hellinger_stochastic_conformance(language2),
            DistanceMeasure::UEMSC => language1.unit_earth_movers_stochastic_conformance(language2),
        }
    }
}

pub trait StochasticMarkovianConformance {
    /// Compare `self` with a stochastic labelled Petri net using the selected
    /// distance measure over their k-th order Markovian abstractions, and
    /// return a conformance score in the range [0,1] where 1 means perfect
    /// match.
    fn markovian_conformance(
        self,
        other: MarkovianAbstraction,
        measure: DistanceMeasure,
    ) -> Result<Fraction>;
}

impl StochasticMarkovianConformance for MarkovianAbstraction {
    fn markovian_conformance(
        self,
        other: MarkovianAbstraction,
        measure: DistanceMeasure,
    ) -> Result<Fraction> {
        if self.order != other.order {
            return Err(anyhow!("order of to-be compared abstractions must match"));
        }

        // Check the value of k
        if self.order < 1 {
            return Err(anyhow::anyhow!("order must be at least 1"));
        }

        let abslang1 = Box::new(FiniteStochasticLanguage::from(self));
        let abslang2 = Box::new(FiniteStochasticLanguage::from(other));

        return measure.apply(abslang1, abslang2);
    }
}