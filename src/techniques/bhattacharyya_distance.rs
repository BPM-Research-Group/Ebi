use anyhow::Result;

use crate::{
    ebi_framework::activity_key::ActivityKeyTranslator,
    ebi_traits::{
        ebi_trait_finite_stochastic_language::EbiTraitFiniteStochasticLanguage,
        ebi_trait_queriable_stochastic_language::EbiTraitQueriableStochasticLanguage,
    },
    follower_semantics::FollowerSemantics,
    math::{
        fraction::{Fraction, MaybeExact}, fraction_enum::FractionEnum, traits::{Signed, Zero}
    },
};

pub trait BhattacharyyaDistance {
    fn bd_log2log(
        &self,
        event_log2: Box<dyn EbiTraitFiniteStochasticLanguage>,
    ) -> Result<Fraction>;

    fn bd_log2model(
        &self,
        logmodel2: Box<dyn EbiTraitQueriableStochasticLanguage>,
    ) -> Result<Fraction>;
}

impl BhattacharyyaDistance for dyn EbiTraitFiniteStochasticLanguage {
    fn bd_log2log(
        &self,
        event_log2: Box<dyn EbiTraitFiniteStochasticLanguage>,
    ) -> Result<Fraction> {
        let mut activity_key1 = self.get_activity_key().clone();
        let translator =
            ActivityKeyTranslator::new(&event_log2.get_activity_key(), &mut activity_key1);
        let mut sum = Fraction::zero();
        for (trace1, probability1) in self.iter_trace_probability() {
            for (trace2, probability2) in event_log2.iter_trace_probability() {
                if trace1 == &translator.translate_trace(trace2) {
                    let prob: crate::math::fraction_enum::FractionEnum = probability1 * probability2;
                    sum += prob.sqrt_abs(10);
                }
            }
        }
        println!("sum:{:?}", sum);
        return Ok(FractionEnum::Approx(-sum.extract_approx().unwrap().ln()));
    }

    fn bd_log2model(
        &self,
        mut language2: Box<dyn EbiTraitQueriableStochasticLanguage>,
    ) -> Result<Fraction> {
        let mut sum = Fraction::zero();
        let translator =
            ActivityKeyTranslator::new(self.get_activity_key(), language2.get_activity_key_mut());

        for (trace, probability1) in self.iter_trace_probability() {
            let probability2 = language2.get_probability(&FollowerSemantics::Trace(
                &translator.translate_trace(trace),
            ))?;
            if probability2.is_positive() {
                let prob: crate::math::fraction_enum::FractionEnum = probability1 * &probability2;
                sum += prob.sqrt_abs(10);
            }
        }
        return Ok(FractionEnum::Approx(-sum.extract_approx().unwrap().ln()));
    }
}