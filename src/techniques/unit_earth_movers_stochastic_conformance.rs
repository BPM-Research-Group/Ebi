use anyhow::Result;

use crate::{ebi_framework::activity_key::ActivityKeyTranslator, ebi_traits::{ebi_trait_finite_stochastic_language::EbiTraitFiniteStochasticLanguage, ebi_trait_queriable_stochastic_language::EbiTraitQueriableStochasticLanguage}, follower_semantics::FollowerSemantics, math::fraction::Fraction};

pub trait UnitEarthMoversStochasticConformance {
    fn unit_earth_movers_stochastic_conformance(&self, language2: Box<dyn EbiTraitQueriableStochasticLanguage>) -> Result<Fraction>;
}

impl UnitEarthMoversStochasticConformance for dyn EbiTraitFiniteStochasticLanguage {
    fn unit_earth_movers_stochastic_conformance(&self, mut language2: Box<dyn EbiTraitQueriableStochasticLanguage>) -> Result<Fraction> {
        let mut sum = Fraction::zero();

        for (trace, probability1) in self.iter_trace_probability() {
            let translator = ActivityKeyTranslator::new(self.get_activity_key(), language2.get_activity_key_mut());
            let probability2 = language2.get_probability(&FollowerSemantics::Trace(&translator.translate_trace(trace)))?;

            if *probability1 > probability2 {
                sum += probability1;
                sum -= probability2;
            }
        }

        sum = sum.one_minus();
        return Ok(sum);
    }
}