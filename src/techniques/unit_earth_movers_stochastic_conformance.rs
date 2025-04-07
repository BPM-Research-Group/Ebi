use std::{ops::Neg, sync::{Arc, Mutex}};

use anyhow::Result;
use rayon::iter::{IntoParallelIterator, ParallelIterator};

use crate::{ebi_framework::activity_key::ActivityKeyTranslator, ebi_traits::{ebi_trait_finite_stochastic_language::EbiTraitFiniteStochasticLanguage, ebi_trait_queriable_stochastic_language::EbiTraitQueriableStochasticLanguage}, follower_semantics::FollowerSemantics, math::fraction::Fraction};

pub trait UnitEarthMoversStochasticConformance {
    fn unit_earth_movers_stochastic_conformance(&self, language2: Box<dyn EbiTraitQueriableStochasticLanguage>) -> Result<Fraction>;
}

impl UnitEarthMoversStochasticConformance for dyn EbiTraitFiniteStochasticLanguage {
    fn unit_earth_movers_stochastic_conformance(&self, mut language2: Box<dyn EbiTraitQueriableStochasticLanguage>) -> Result<Fraction> {   
        let translator = ActivityKeyTranslator::new(self.get_activity_key(), language2.get_activity_key_mut());
        let error = Arc::new(Mutex::new(None));

        let mut sum = (0..self.len()).into_par_iter().filter_map(|trace_index| {
            let trace = self.get_trace(trace_index).unwrap();
            let probability = self.get_probability(trace_index).unwrap();

            match language2.get_probability(&FollowerSemantics::Trace(&translator.translate_trace(trace))) {
                Ok(probability2) => if *probability > probability2 {
                    let mut probability2 = probability2.neg();
                    probability2 += probability;
                    Some(probability2)
                } else {
                    None
                },
                Err(err) => {
                    *Arc::clone(&error).lock().unwrap() = Some(err);
                    None
                },
            }
        }).sum::<Fraction>();

        //something went wrong
        if let Ok(mutex) = Arc::try_unwrap(error) {
            if let Ok(err) = mutex.into_inner() {
                if let Some(err) = err {
                    return Err(err);
                }
            }
        }

        sum = sum.one_minus();
        return Ok(sum);
    }
}

#[cfg(test)]
mod tests {
    use std::fs;

    use crate::{ebi_objects::finite_stochastic_language::FiniteStochasticLanguage, ebi_traits::ebi_trait_finite_stochastic_language::EbiTraitFiniteStochasticLanguage, math::{fraction::Fraction, traits::One}, techniques::unit_earth_movers_stochastic_conformance::UnitEarthMoversStochasticConformance};

    
    #[test]
    fn uemsc_activity_key() {
        let fin1 = fs::read_to_string("testfiles/aa-ab-ba.slang").unwrap();
        let slang1 = fin1.parse::<FiniteStochasticLanguage>().unwrap();

        let fin2 = fs::read_to_string("testfiles/ba-aa-ab.slang").unwrap();
        let slang2 = fin2.parse::<FiniteStochasticLanguage>().unwrap();

        let slang1: Box<dyn EbiTraitFiniteStochasticLanguage> = Box::new(slang1);

        let uemsc = slang1
            .unit_earth_movers_stochastic_conformance(Box::new(slang2))
            .unwrap();
        assert_eq!(uemsc, Fraction::one())
    }
}