use core::num;
use std::{collections::HashSet, str::FromStr, borrow::Borrow};

use anyhow::{Result, Context, anyhow};
use num_bigint::{ToBigUint,ToBigInt};
use num_traits::{One, ToPrimitive, Zero};
use crate::{ebi_traits::{ebi_trait_finite_stochastic_language::EbiTraitFiniteStochasticLanguage, ebi_trait_queriable_stochastic_language::EbiTraitQueriableStochasticLanguage}, follower_semantics::FollowerSemantics, math::{fraction::Fraction, log_div::LogDiv}, ActivityTrace};

pub fn er(mut log: Box<dyn EbiTraitFiniteStochasticLanguage>, mut model: Box<dyn EbiTraitQueriableStochasticLanguage>) -> Result<LogDiv> {
    log::info!("compute entropic relevance");
    
    let sum_j = j(&mut log, &mut model)?;

    let mut rho = Fraction::zero(); // the overall probability that a trace in the event log E is possible in the stochastic language of model A
    let mut sum = LogDiv::zero();

    let number_of_activities_in_log = get_number_of_activities_in_log(&mut log);

    for (trace, log_probability) in log.iter_trace_probability() {
        let follower = FollowerSemantics::Trace(trace);
        let model_probability = model.get_probability(&follower).with_context(|| format!("could not compute the probability of trace `{:?}`", trace))?;
        if model_probability > Fraction::zero() {
            rho += log_probability;
        }

        if !model_probability.is_zero() {
            sum -= LogDiv::log2(model_probability);
        } else {
            sum += bits(trace, number_of_activities_in_log);
        }
    }
    
    return Ok(sum_j - h(&rho));
}

fn get_number_of_activities_in_log(log: &mut Box<dyn EbiTraitFiniteStochasticLanguage>) -> usize {
    let mut activities = HashSet::new();
    for (trace, _) in log.iter_trace_probability() {
        for activity in trace {
            activities.insert(activity);
        }
    }
    return activities.len();
}

pub fn bits(trace: &ActivityTrace, number_of_activities_in_log: usize) -> LogDiv {
    let mut result = LogDiv::log2(Fraction::from(number_of_activities_in_log + 1));
    result *= trace.len() + 1;
    result
}

pub fn h(x: &Fraction) -> LogDiv {
    if x.is_zero() || x.is_one() {
        return LogDiv::zero();
    }

    let xlogx = LogDiv::n_log_n(x);
    let mut n = Fraction::one();
    n -= x;
    let nlogn = LogDiv::n_log_n(&n);

    let result = xlogx + nlogn;
    return result;
}

pub fn j(log: &mut Box<dyn EbiTraitFiniteStochasticLanguage>, model: &mut Box<dyn EbiTraitQueriableStochasticLanguage>) -> Result<LogDiv> {
    let mut sum_j = LogDiv::zero();
    let number_of_activities_in_log = get_number_of_activities_in_log(log);

    for (trace, probability_log) in log.iter_trace_probability() {
        let probability_model = model.get_probability(&FollowerSemantics::Trace(trace))?;
        if probability_model.is_zero() {
            //trace not in model
            let l = 1 + number_of_activities_in_log;
            let mut log_div = LogDiv::log2(l.into());

            log_div *= trace.len() + 1;
            
            log_div *= probability_log;

            sum_j += log_div;
        } else {
            //trace in model

            //log(pm^a) / b
            // = a log(pm) / b
            // = a/b log(pm)
            let mut logdiv = LogDiv::log2(probability_model);
            logdiv *= probability_log;


            // let log_of = LogDiv::power_f_u(&probability_model, &a_log);
            // let logdiv = LogDiv::log2_div(log_of, b_log);
            sum_j -= logdiv;
        }
    };

    Ok(sum_j)
}