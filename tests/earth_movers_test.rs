use ebi::ebi_objects::FiniteStochasticLanguage;
use ebi::{
    ebi_traits::ebi_trait_finite_stochastic_language::EbiTraitFiniteStochasticLanguage,
    techniques::earth_movers_stochastic_conformance::EarthMoversStochasticConformance,
};
use ebi_objects::ebi_arithmetic::{Fraction, One};
use std::fs;

#[test]
fn earth_movers_test() {
    // Read the first file and box it
    let fin1 = fs::read_to_string("testfiles/empty_trace.slang").unwrap();
    let slang1 = fin1.parse::<FiniteStochasticLanguage>().unwrap();
    let mut object1: Box<dyn EbiTraitFiniteStochasticLanguage> = Box::new(slang1);
    // Read the second file and box it
    let fin2 = fs::read_to_string("testfiles/empty_trace.slang").unwrap();
    let slang2 = fin2.parse::<FiniteStochasticLanguage>().unwrap();
    let mut object2: Box<dyn EbiTraitFiniteStochasticLanguage> = Box::new(slang2);
    // Compute EMSC
    let emsc = object1
        .earth_movers_stochastic_conformance(object2.as_mut())
        .unwrap();
    assert_eq!(emsc, Fraction::one());
}
