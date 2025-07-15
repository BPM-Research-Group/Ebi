use anyhow::Result;

use crate::{
    ebi_framework::activity_key::ActivityKeyTranslator,
    ebi_traits::{
        ebi_trait_finite_stochastic_language::EbiTraitFiniteStochasticLanguage,
        ebi_trait_queriable_stochastic_language::EbiTraitQueriableStochasticLanguage,
    },
    follower_semantics::FollowerSemantics,
    math::{
       log_div::LogDiv, traits::{Signed, Zero}
    },
};

pub trait KullbackLeiblerDivergence {
    fn kld_log2log(
        &self,
        event_log2: Box<dyn EbiTraitFiniteStochasticLanguage>,
    ) -> Result<LogDiv>;

    fn kld_log2model(
        &self,
        logmodel2: Box<dyn EbiTraitQueriableStochasticLanguage>,
    ) -> Result<LogDiv>;
}

impl KullbackLeiblerDivergence for dyn EbiTraitFiniteStochasticLanguage {
    fn kld_log2log(
        &self,
        event_log2: Box<dyn EbiTraitFiniteStochasticLanguage>,
    ) -> Result<LogDiv> {
        let mut activity_key1 = self.get_activity_key().clone();
        let translator =
            ActivityKeyTranslator::new(&event_log2.get_activity_key(), &mut activity_key1);
        let mut sum = LogDiv::zero();
        for (trace1, probability1) in self.iter_trace_probability() {
            let mut flag = false;
            for (trace2, probability2) in event_log2.iter_trace_probability() {
                if trace1 == &translator.translate_trace(trace2) {
                    let prob1 = LogDiv::n_log_n(&probability1);
                    let mut prob2 = LogDiv::n_log_n(&probability2);
                    prob2 /= probability2.clone();
                    prob2 *= probability1.clone();
                    sum += prob1;
                    sum -= prob2;
                    flag = true;
                    break;
                }
            }
            if flag == false {
                return Err(anyhow::anyhow!("Trace {:?} not found in the second language.", trace1));
            }
        }
        return Ok(sum);
    }

    fn kld_log2model(
        &self,
        mut language2: Box<dyn EbiTraitQueriableStochasticLanguage>,
    ) -> Result<LogDiv> {
        let mut sum = LogDiv::zero();
        let translator =
            ActivityKeyTranslator::new(self.get_activity_key(), language2.get_activity_key_mut());

        for (trace, probability1) in self.iter_trace_probability() {
            let probability2 = language2.get_probability(&FollowerSemantics::Trace(
                &translator.translate_trace(trace),
            ))?;
            if probability2.is_positive() {
                let prob1 = LogDiv::n_log_n(&probability1);
                let mut prob2 = LogDiv::n_log_n(&probability2);
                prob2 /= probability2.clone();
                prob2 *= probability1.clone();
                sum += prob1;
                sum -= prob2;
            }
            else {
                return Err(anyhow::anyhow!("Probability of trace {:?} is zero in the second language.", trace));
            }
        }
        return Ok(sum);
    }
}

#[cfg(test)]
mod tests {
    use std::fs;

    use crate::{
        ebi_objects::{
            finite_stochastic_language::FiniteStochasticLanguage, stochastic_deterministic_finite_automaton::StochasticDeterministicFiniteAutomaton, 
        },
        ebi_traits::ebi_trait_finite_stochastic_language::EbiTraitFiniteStochasticLanguage,
        math::{fraction::Fraction, log_div::LogDiv},
        techniques::kullback_leibler_divergence::KullbackLeiblerDivergence,
    };

    #[test]
    fn kld_log_model() {
        let fin1 = fs::read_to_string("testfiles/a-b.sdfa").unwrap();
        let sdfa = fin1.parse::<StochasticDeterministicFiniteAutomaton>().unwrap();

        let fin2 = fs::read_to_string("testfiles/a.slang").unwrap();
        let slang = fin2.parse::<FiniteStochasticLanguage>().unwrap();

        let slang1: Box<dyn EbiTraitFiniteStochasticLanguage> = Box::new(slang);
        let kld = slang1.kld_log2model(Box::new(sdfa)).unwrap();
        let mut x = LogDiv::from(Fraction::from(2));
        x /= 2;
        assert_eq!(kld, x);
    }

    #[test]
    fn kld_log_log() {
        let fin1 = fs::read_to_string("testfiles/aa.slang").unwrap();
        let slang1 = fin1.parse::<FiniteStochasticLanguage>().unwrap();

        let fin2 = fs::read_to_string("testfiles/aa.slang").unwrap();
        let slang2 = fin2.parse::<FiniteStochasticLanguage>().unwrap();

        let slang1: Box<dyn EbiTraitFiniteStochasticLanguage> = Box::new(slang1);

        let kld = slang1.kld_log2log(Box::new(slang2)).unwrap();
        let x = LogDiv::from(Fraction::from(0));
        assert_eq!(kld, x);
    }
}
