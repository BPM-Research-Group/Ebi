use anyhow::Result;
use fraction::Zero;

use crate::{ebi_traits::{ebi_trait_finite_stochastic_language::EbiTraitFiniteStochasticLanguage, ebi_trait_queriable_stochastic_language::EbiTraitQueriableStochasticLanguage}, follower_semantics::FollowerSemantics, math::{fraction::Fraction, log_div::LogDiv, root_log_div::RootLogDiv}};

pub trait JensenShannonStochasticConformance {
    fn jssc_log2log(&self, event_log2: Box<dyn EbiTraitFiniteStochasticLanguage>) -> Result<RootLogDiv>;

    fn jssc_log2model(&self, logmodel2: Box<dyn EbiTraitQueriableStochasticLanguage>) -> Result<RootLogDiv>;
}

impl JensenShannonStochasticConformance for dyn EbiTraitFiniteStochasticLanguage {
    fn jssc_log2log(&self, event_log2: Box<dyn EbiTraitFiniteStochasticLanguage>) -> Result<RootLogDiv> {
        let mut sum = LogDiv::zero();
    
        let mut log1_prob_intersection_sum = Fraction::zero();
        let mut log2_prob_intersection_sum = Fraction::zero();
    
        for (trace1, probability1) in self.iter_trace_probability() {
            for (trace2, probability2) in event_log2.iter_trace_probability() {
                if trace1 == trace2{
                    log1_prob_intersection_sum += probability1;
                    log2_prob_intersection_sum += probability2;
                    sum += LogDiv::n_log_n(&probability1);
                    sum += LogDiv::n_log_n(&probability2);
                    sum -= LogDiv::n_log_n(&(probability1 + probability2));
                    sum += LogDiv::from(probability1 + probability2); // move it out of the loop and handle at the end
                }
            }
        }
    
        //sum += LogDiv::from((Fraction::one() - log1_prob_intersection_sum) + (Fraction::one() - log2_prob_intersection_sum));
        log1_prob_intersection_sum = log1_prob_intersection_sum.one_minus();
        log1_prob_intersection_sum += log2_prob_intersection_sum.one_minus();
        sum += LogDiv::from(log1_prob_intersection_sum);
    
        sum /= 2usize;
    
        return Ok(RootLogDiv::sqrt(sum).one_minus());
    }

    fn jssc_log2model(&self, mut logmodel2: Box<dyn EbiTraitQueriableStochasticLanguage>) -> Result<RootLogDiv> {
        let mut sum = LogDiv::zero();
        let mut sum4model = Fraction::zero();
        let mut sum4log = Fraction::zero();
    
        for (trace, probability1) in self.iter_trace_probability() {
            let probability2 = logmodel2.get_probability(&FollowerSemantics::Trace(trace))?;
            if probability2>Fraction::zero() {
                sum4log += probability1;
                sum4model += &probability2;
    
                sum += LogDiv::n_log_n(&probability1);
                sum += LogDiv::n_log_n(&probability2);
                sum -= LogDiv::n_log_n(&(probability1 + &probability2));
                sum += LogDiv::from(probability1 + &probability2);
            }
        }
        sum /= 2usize;
    
        return Ok(RootLogDiv::sqrt(sum).one_minus());
    }
}