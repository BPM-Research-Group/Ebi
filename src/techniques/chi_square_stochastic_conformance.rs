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

pub trait ChiSquareStochasticConformance {
    fn cssc_log2log(
        &self,
        event_log2: Box<dyn EbiTraitFiniteStochasticLanguage>,
    ) -> Result<Fraction>;

    fn cssc_log2model(
        &self,
        logmodel2: Box<dyn EbiTraitQueriableStochasticLanguage>,
    ) -> Result<Fraction>;
}

impl ChiSquareStochasticConformance for dyn EbiTraitFiniteStochasticLanguage {
    fn cssc_log2log(
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
                    let mut prob = probability1.clone();
                    prob -= probability2.clone();
                    prob *= prob.clone();
                    let mut prob1 = probability1.clone();
                    prob1 += probability2.clone();
                    prob /= prob1;
                    sum += prob;
                }
            }
        }
        sum /= 2usize;
        return Ok(sum.one_minus());
    }

    fn cssc_log2model(
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
                    let mut prob = probability1.clone();
                    prob -= probability2.clone();
                    prob *= prob.clone();
                    let mut prob1 = probability1.clone();
                    prob1 += probability2.clone();
                    prob /= prob1;
                    sum += prob;
            }
        }
        sum /= 2usize;
        return Ok(sum.one_minus());
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
        math::{fraction::Fraction, log_div::LogDiv, traits::Zero},
        techniques::chi_square_stochastic_conformance::ChiSquareStochasticConformance,
    };

    #[test]
    fn cssc() {
        let fin1 = fs::read_to_string("testfiles/aa-ab-ba_ali.slpn").unwrap();
        let slpn = fin1.parse::<StochasticLabelledPetriNet>().unwrap();

        let fin2 = fs::read_to_string("testfiles/a-b.xes").unwrap();
        let log = fin2.parse::<EventLog>().unwrap();

        let slang: Box<dyn EbiTraitFiniteStochasticLanguage> =
            Box::new(Into::<FiniteStochasticLanguage>::into(log));

        let mut x = LogDiv::from(Fraction::from(2));
        x /= 2;

        assert_eq!(slang.cssc_log2model(Box::new(slpn)).unwrap(), Fraction::zero().one_minus());
    }

    #[test]
    fn cssc_activity_key() {
        let fin1 = fs::read_to_string("testfiles/aa.slang").unwrap();
        let slang1 = fin1.parse::<FiniteStochasticLanguage>().unwrap();

        let fin2 = fs::read_to_string("testfiles/ba.slang").unwrap();
        let slang2 = fin2.parse::<FiniteStochasticLanguage>().unwrap();

        let slang1: Box<dyn EbiTraitFiniteStochasticLanguage> = Box::new(slang1);

        let cssc = slang1.cssc_log2log(Box::new(slang2)).unwrap();

        assert_eq!(cssc, Fraction::zero().one_minus())
    }
}
