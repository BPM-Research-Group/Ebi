use crate::{
    ebi_traits::{
        ebi_trait_finite_stochastic_language::EbiTraitFiniteStochasticLanguage,
        ebi_trait_queriable_stochastic_language::EbiTraitQueriableStochasticLanguage,
    },
    semantics::follower_semantics::FollowerSemantics,
};
use ebi_objects::{
    anyhow::Result,
    ebi_arithmetic::{Fraction, One, OneMinus, Sqrt, Zero},
    ActivityKeyTranslator,
};
use rayon::iter::ParallelIterator;
use std::sync::{Arc, Mutex};

pub trait HellingerStochasticConformance {
    fn hellinger_stochastic_conformance(
        &self,
        language2: Box<dyn EbiTraitQueriableStochasticLanguage>,
    ) -> Result<Fraction>;
}

impl HellingerStochasticConformance for dyn EbiTraitFiniteStochasticLanguage {
    fn hellinger_stochastic_conformance(
        &self,
        mut language2: Box<dyn EbiTraitQueriableStochasticLanguage>,
    ) -> Result<Fraction> {
        let translator =
            ActivityKeyTranslator::new(self.activity_key(), language2.activity_key_mut());
        let error = Arc::new(Mutex::new(None));

        let mut sum = self
            .par_iter_traces_probabilities()
            .filter_map(|(trace, probability)| {
                match language2.get_probability(&FollowerSemantics::Trace(
                    &translator.translate_trace(trace),
                )) {
                    Ok(probability2) => {
                        let mut probability2 = probability2.approx_abs_sqrt(10);
                        probability2 -= probability.clone().approx_abs_sqrt(10);
                        Some(&probability2 * &probability2)
                    }
                    Err(err) => {
                        *Arc::clone(&error).lock().unwrap() = Some(err);
                        None
                    }
                }
            })
            .sum::<Fraction>();

        //something went wrong
        if let Ok(mutex) = Arc::try_unwrap(error)
            && let Ok(err) = mutex.into_inner()
            && let Some(err) = err
        {
            return Err(err);
        }
        sum /= 2usize;
        if sum.is_zero() {
            return Ok(Fraction::one());
        }
        sum = sum.approx_abs_sqrt(10);
        sum = sum.one_minus();
        Ok(sum)
    }
}

#[cfg(test)]
mod tests {
    use crate::{
        ebi_traits::ebi_trait_finite_stochastic_language::EbiTraitFiniteStochasticLanguage,
        techniques::hellinger_stochastic_conformance::HellingerStochasticConformance,
    };
    use ebi_objects::{
        ebi_arithmetic::{Fraction, One},
        FiniteStochasticLanguage,
    };
    use std::fs;

    #[test]
    fn uemsc_activity_key() {
        let fin1 = fs::read_to_string("testfiles/aa-ab-ba.slang").unwrap();
        let slang1 = fin1.parse::<FiniteStochasticLanguage>().unwrap();

        let fin2 = fs::read_to_string("testfiles/ba-aa-ab.slang").unwrap();
        let slang2 = fin2.parse::<FiniteStochasticLanguage>().unwrap();

        let slang1: Box<dyn EbiTraitFiniteStochasticLanguage> = Box::new(slang1);

        let uemsc = slang1
            .hellinger_stochastic_conformance(Box::new(slang2))
            .unwrap();
        assert_eq!(uemsc, Fraction::one())
    }
}
