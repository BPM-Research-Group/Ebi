use crate::{
    ebi_traits::{
        ebi_trait_finite_stochastic_language::EbiTraitFiniteStochasticLanguage,
        ebi_trait_queriable_stochastic_language::EbiTraitQueriableStochasticLanguage,
    },
    follower_semantics::FollowerSemantics,
};
use ebi_objects::{
    Activity, ActivityKeyTranslator,
    anyhow::{Context, Result},
    ebi_arithmetic::{
        Fraction, Log, One, Signed, Zero, log_polynomial::log_polynomial::LogPolynomial,
    },
};
use std::collections::HashSet;

pub trait EntropicRelvance {
    fn entropic_relevance(
        &self,
        model: Box<dyn EbiTraitQueriableStochasticLanguage>,
    ) -> Result<LogPolynomial>;
}

impl EntropicRelvance for dyn EbiTraitFiniteStochasticLanguage {
    fn entropic_relevance(
        &self,
        mut model: Box<dyn EbiTraitQueriableStochasticLanguage>,
    ) -> Result<LogPolynomial> {
        let mut rho = Fraction::zero(); // the overall probability that a trace in the event log E is possible in the stochastic language of model A
        let mut sum = LogPolynomial::zero();

        let number_of_activities_in_log = {
            let mut activities = HashSet::new();
            for trace in self.iter_traces() {
                for activity in trace {
                    activities.insert(activity);
                }
            }
            activities.len()
        };

        let translator = ActivityKeyTranslator::new(self.activity_key(), model.activity_key_mut());
        let sum_j = self.j(&mut model, number_of_activities_in_log, &translator)?;

        for (trace, log_probability) in self.iter_traces_probabilities() {
            let translated_trace = translator.translate_trace(trace);
            let follower = FollowerSemantics::Trace(&translated_trace);
            let model_probability = model.get_probability(&follower).with_context(|| {
                format!("could not compute the probability of trace `{:?}`", trace)
            })?;
            if model_probability.is_positive() {
                rho += log_probability;
            }

            if !model_probability.is_zero() {
                sum -= model_probability.log()?;
            } else {
                sum += bits(trace, number_of_activities_in_log)?;
            }
        }

        let mut result = sum_j;
        result -= h(&rho)?;
        return Ok(result);
    }
}

impl dyn EbiTraitFiniteStochasticLanguage {
    fn j(
        &self,
        model: &mut Box<dyn EbiTraitQueriableStochasticLanguage>,
        number_of_activities_in_log: usize,
        translator: &ActivityKeyTranslator,
    ) -> Result<LogPolynomial> {
        let mut sum_j = LogPolynomial::zero();

        for (trace, probability_log) in self.iter_traces_probabilities() {
            let translated_trace = translator.translate_trace(&trace);
            let probability_model =
                model.get_probability(&FollowerSemantics::Trace(&translated_trace))?;
            if probability_model.is_zero() {
                //trace not in model
                let l = 1 + number_of_activities_in_log;
                let mut log_div = l.log()?;

                log_div *= trace.len() + 1;

                log_div *= probability_log;

                sum_j += log_div;
            } else {
                //trace in model

                //log(pm^a) / b
                // = a log(pm) / b
                // = a/b log(pm)
                let mut logdiv = probability_model.log()?;
                logdiv *= probability_log;

                // let log_of = LogDiv::power_f_u(&probability_model, &a_log);
                // let logdiv = LogDiv::log2_div(log_of, b_log);
                sum_j -= logdiv;
            }
        }

        Ok(sum_j)
    }
}

fn bits(trace: &Vec<Activity>, number_of_activities_in_log: usize) -> Result<LogPolynomial> {
    let mut result = (number_of_activities_in_log + 1).log()?;
    result *= trace.len() + 1;
    Ok(result)
}

fn h(x: &Fraction) -> Result<LogPolynomial> {
    if x.is_zero() || x.is_one() {
        return Ok(LogPolynomial::zero());
    }

    let xlogx = x.n_log_n()?;
    let mut n = Fraction::one();
    n -= x;
    let nlogn = n.n_log_n()?;

    let mut result = xlogx;
    result += nlogn;
    return Ok(result);
}

#[cfg(test)]
mod tests {
    use crate::{
        ebi_traits::{
            ebi_trait_finite_stochastic_language::EbiTraitFiniteStochasticLanguage,
            ebi_trait_queriable_stochastic_language::EbiTraitQueriableStochasticLanguage,
        },
        techniques::entropic_relevance::EntropicRelvance,
    };
    use ebi_objects::{EventLog, FiniteStochasticLanguage, ebi_arithmetic::ebi_number::One};
    use std::fs;

    #[test]
    fn entropic_relevance() {
        let fin = fs::read_to_string("testfiles/a-b.xes").unwrap();
        let log = fin.parse::<EventLog>().unwrap();

        let fin2 = fs::read_to_string("testfiles/a-b.xes").unwrap();
        let log2 = fin2.parse::<EventLog>().unwrap();

        let lang: Box<dyn EbiTraitFiniteStochasticLanguage> =
            Box::new(Into::<FiniteStochasticLanguage>::into(log));
        let lang2: Box<dyn EbiTraitQueriableStochasticLanguage> =
            Box::new(Into::<FiniteStochasticLanguage>::into(log2));
        let er = lang.entropic_relevance(lang2).unwrap();

        assert!(er.is_one());
    }
}
