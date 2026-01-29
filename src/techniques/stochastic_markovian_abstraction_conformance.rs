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
        mut self,
        mut other: MarkovianAbstraction,
        measure: DistanceMeasure,
    ) -> Result<Fraction> {
        // Check that the orders match
        if self.order != other.order {
            return Err(anyhow!("order of to-be compared abstractions must match"));
        }

        // Check the value of k
        if self.order < 1 {
            return Err(anyhow::anyhow!("order must be at least 1"));
        }

        // Both markovian abstractions have had artificial start and end activities inserted, though these may have different names.
        // Harmonise them.
        self.harmonise_start_end(&mut other);

        //compute stochastic languages
        let abslang1 = Box::new(FiniteStochasticLanguage::from(self));
        let abslang2 = Box::new(FiniteStochasticLanguage::from(other));

        return measure.apply(abslang1, abslang2);
    }
}

#[cfg(test)]
mod tests {
    use crate::{
        ebi_traits::ebi_trait_finite_stochastic_language::EbiTraitFiniteStochasticLanguage,
        techniques::{stochastic_markovian_abstraction::AbstractMarkovian, stochastic_markovian_abstraction_conformance::{DistanceMeasure, StochasticMarkovianConformance}},
    };
    use ebi_objects::{FiniteStochasticLanguage, StochasticLabelledPetriNet, ebi_arithmetic::Fraction};
    use std::fs;

    #[test]
    fn eduardo_paper_test() {
        let fin3 = fs::read_to_string("testfiles/seq(a,xor(seq(f,and(c,b)),seq(f,loop(d,e))).slpn")
            .unwrap();
        let mut slpn = fin3.parse::<StochasticLabelledPetriNet>().unwrap();

        let slpn_abstraction = slpn.abstract_markovian(2).unwrap();
        println!("{}", slpn_abstraction);

        let fin4 = fs::read_to_string("testfiles/acb-abc-ad-aded-adeded-adededed.slang").unwrap();
        let mut slang: Box<dyn EbiTraitFiniteStochasticLanguage> =
            Box::new(fin4.parse::<FiniteStochasticLanguage>().unwrap());

        let slang_abstraction = slang.abstract_markovian(2).unwrap();
        println!("{}", slang_abstraction);

        let result = slpn_abstraction.markovian_conformance(slang_abstraction, DistanceMeasure::UEMSC).unwrap();
        assert_eq!(result, Fraction::from((6383, 10005)));
    }
}
