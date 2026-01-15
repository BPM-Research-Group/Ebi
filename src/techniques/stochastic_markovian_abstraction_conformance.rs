use crate::techniques::stochastic_markovian_abstraction::{DistanceMeasure, MarkovianAbstraction};
use anyhow::{Result, anyhow};
use ebi_objects::{FiniteStochasticLanguage, ebi_arithmetic::Fraction};

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
            return Err(anyhow!("order of to be compared abstractions must match"));
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
