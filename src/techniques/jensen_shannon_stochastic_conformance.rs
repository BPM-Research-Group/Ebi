use anyhow::Result;
use ebi_arithmetic::{Fraction, OneMinus, Signed, Zero};
use ebi_objects::ActivityKeyTranslator;

use crate::{
    ebi_traits::{
        ebi_trait_finite_stochastic_language::EbiTraitFiniteStochasticLanguage,
        ebi_trait_queriable_stochastic_language::EbiTraitQueriableStochasticLanguage,
    },
    follower_semantics::FollowerSemantics,
    math::{log_div::LogDiv, root_log_div::RootLogDiv},
};

pub trait JensenShannonStochasticConformance {
    fn jssc_log2log(
        &self,
        event_log2: Box<dyn EbiTraitFiniteStochasticLanguage>,
    ) -> Result<RootLogDiv>;

    fn jssc_log2model(
        &self,
        logmodel2: Box<dyn EbiTraitQueriableStochasticLanguage>,
    ) -> Result<RootLogDiv>;
}

impl JensenShannonStochasticConformance for dyn EbiTraitFiniteStochasticLanguage {
    fn jssc_log2log(
        &self,
        event_log2: Box<dyn EbiTraitFiniteStochasticLanguage>,
    ) -> Result<RootLogDiv> {
        let mut activity_key1 = self.activity_key().clone();
        let translator =
            ActivityKeyTranslator::new(&event_log2.activity_key(), &mut activity_key1);

        let mut sum = LogDiv::zero();

        let mut log1_prob_intersection_sum = Fraction::zero();
        let mut log2_prob_intersection_sum = Fraction::zero();

        for (trace1, probability1) in self.iter_trace_probability() {
            for (trace2, probability2) in event_log2.iter_trace_probability() {
                if trace1 == &translator.translate_trace(trace2) {
                    log1_prob_intersection_sum += probability1;
                    log2_prob_intersection_sum += probability2;
                    sum += LogDiv::n_log_n(&probability1)?;
                    sum += LogDiv::n_log_n(&probability2)?;
                    sum -= LogDiv::n_log_n(&(probability1 + probability2))?;
                    sum += LogDiv::try_from(probability1 + probability2)?; // move it out of the loop and handle at the end
                }
            }
        }

        log1_prob_intersection_sum = log1_prob_intersection_sum.one_minus();
        log1_prob_intersection_sum += log2_prob_intersection_sum.one_minus();
        sum += LogDiv::try_from(log1_prob_intersection_sum)?;

        sum /= 2usize;

        return Ok(RootLogDiv::sqrt(sum).one_minus());
    }

    fn jssc_log2model(
        &self,
        mut language2: Box<dyn EbiTraitQueriableStochasticLanguage>,
    ) -> Result<RootLogDiv> {
        let mut sum = LogDiv::zero();
        let mut sum4model = Fraction::zero();
        let mut sum4log = Fraction::zero();
        let translator =
            ActivityKeyTranslator::new(self.activity_key(), language2.activity_key_mut());

        for (trace, probability1) in self.iter_trace_probability() {
            let probability2 = language2.get_probability(&FollowerSemantics::Trace(
                &translator.translate_trace(trace),
            ))?;
            if probability2.is_positive() {
                sum4log += probability1;
                sum4model += &probability2;
                sum += LogDiv::n_log_n(&probability1)?;
                sum += LogDiv::n_log_n(&probability2)?;
                sum -= LogDiv::n_log_n(&(probability1 + &probability2))?;
                sum += LogDiv::try_from(probability1 + &probability2)?;
            }
        }
        sum4log = sum4log.one_minus();
        sum4log += sum4model.one_minus();
        sum += LogDiv::try_from(sum4log)?;
        sum /= 2usize;
        return Ok(RootLogDiv::sqrt(sum).one_minus());
    }
}

#[cfg(test)]
mod tests {
    use std::fs;

    use ebi_arithmetic::{Fraction, ebi_number::Zero, f};
    use ebi_objects::{EventLog, FiniteStochasticLanguage, StochasticLabelledPetriNet};

    use crate::{
        ebi_traits::ebi_trait_finite_stochastic_language::EbiTraitFiniteStochasticLanguage,
        math::{log_div::LogDiv, root_log_div::RootLogDiv},
        techniques::jensen_shannon_stochastic_conformance::JensenShannonStochasticConformance,
    };

    #[test]
    fn jssc() {
        let fin1 = fs::read_to_string("testfiles/aa-ab-ba_ali.slpn").unwrap();
        let slpn = fin1.parse::<StochasticLabelledPetriNet>().unwrap();

        let fin2 = fs::read_to_string("testfiles/a-b.xes").unwrap();
        let log = fin2.parse::<EventLog>().unwrap();

        let slang: Box<dyn EbiTraitFiniteStochasticLanguage> =
            Box::new(Into::<FiniteStochasticLanguage>::into(log));

        let mut x = LogDiv::try_from(f!(2)).unwrap();
        x /= 2;
        let answer = RootLogDiv::sqrt(x).one_minus();

        assert_eq!(slang.jssc_log2model(Box::new(slpn)).unwrap(), answer);
    }

    #[test]
    fn jssc_activity_key() {
        let fin1 = fs::read_to_string("testfiles/aa.slang").unwrap();
        let slang1 = fin1.parse::<FiniteStochasticLanguage>().unwrap();

        let fin2 = fs::read_to_string("testfiles/aa.slang").unwrap();
        let slang2 = fin2.parse::<FiniteStochasticLanguage>().unwrap();

        let slang1: Box<dyn EbiTraitFiniteStochasticLanguage> = Box::new(slang1);

        let jssc = slang1.jssc_log2log(Box::new(slang2)).unwrap();

        let right_side = LogDiv::zero();
        assert_eq!(jssc, RootLogDiv::sqrt(right_side).one_minus())
    }
}
