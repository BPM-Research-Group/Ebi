use anyhow::Result;

use crate::{
    ebi_framework::activity_key::ActivityKeyTranslator,
    ebi_traits::{
        ebi_trait_finite_stochastic_language::EbiTraitFiniteStochasticLanguage,
        ebi_trait_queriable_stochastic_language::EbiTraitQueriableStochasticLanguage,
    },
    follower_semantics::FollowerSemantics,
    math::{
        fraction::Fraction, traits::{Signed, Zero}
    },
};

pub trait HellingerStochasticConformance {
    fn hsc_log2log(
        &self,
        event_log2: Box<dyn EbiTraitFiniteStochasticLanguage>,
    ) -> Result<Fraction>;

    fn hsc_log2model(
        &self,
        logmodel2: Box<dyn EbiTraitQueriableStochasticLanguage>,
    ) -> Result<Fraction>;
}

impl HellingerStochasticConformance for dyn EbiTraitFiniteStochasticLanguage {
    fn hsc_log2log(
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
                    let mut prob = probability1.sqrt_abs(10);
                    prob -=  probability2.sqrt_abs(10);
                    prob *= prob.clone();
                    sum += prob;
                }
            }
        }
        sum /= 2usize;
        println!("Sum: {:?}", sum);
        return Ok(sum.sqrt_abs(10).one_minus());
    }

    fn hsc_log2model(
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
                let mut prob = probability1.sqrt_abs(10);
                prob -=  probability2.sqrt_abs(10);
                prob *= prob.clone();
                sum += prob;
            }
        }
        sum /= 2usize;
        return Ok(sum.sqrt_abs(10).one_minus());
    }
}

#[cfg(test)]
mod tests {
    use std::fs;

    use crate::{
        ebi_objects::{
            event_log::EventLog, finite_stochastic_language::FiniteStochasticLanguage,
            stochastic_labelled_petri_net::StochasticLabelledPetriNet,
        },
        ebi_traits::ebi_trait_finite_stochastic_language::EbiTraitFiniteStochasticLanguage,
        math::{fraction::Fraction, traits::Zero},
        techniques::hellinger_stochastic_conformance::HellingerStochasticConformance,
    };

    #[test]
    fn hsc() {
        let fin1 = fs::read_to_string("testfiles/aa-ab-ba_ali.slpn").unwrap();
        let slpn = fin1.parse::<StochasticLabelledPetriNet>().unwrap();

        let fin2 = fs::read_to_string("testfiles/a-b.xes").unwrap();
        let log = fin2.parse::<EventLog>().unwrap();

        let slang: Box<dyn EbiTraitFiniteStochasticLanguage> =
            Box::new(Into::<FiniteStochasticLanguage>::into(log));

        assert_eq!(slang.hsc_log2model(Box::new(slpn)).unwrap(), Fraction::zero().one_minus());
    }

    #[test]
    fn hsc_activity_key() {
        let fin1 = fs::read_to_string("testfiles/aa.slang").unwrap();
        let slang1 = fin1.parse::<FiniteStochasticLanguage>().unwrap();

        let fin2 = fs::read_to_string("testfiles/aa.slang").unwrap();
        let slang2 = fin2.parse::<FiniteStochasticLanguage>().unwrap();

        let slang1: Box<dyn EbiTraitFiniteStochasticLanguage> = Box::new(slang1);

        let hsc = slang1.hsc_log2log(Box::new(slang2)).unwrap();

        assert_eq!(hsc, Fraction::zero().one_minus())
    }
}
