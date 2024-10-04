use anyhow::Result;
use crate::{ebi_traits::{ebi_trait_finite_stochastic_language::EbiTraitFiniteStochasticLanguage, ebi_trait_queriable_stochastic_language::EbiTraitQueriableStochasticLanguage}, follower_semantics::FollowerSemantics, math::fraction::Fraction};

pub trait HellingerStochasticConformance {
    fn hsc_log2log(&self, event_log2: Box<dyn EbiTraitFiniteStochasticLanguage>) -> Result<Fraction>;

    fn hsc_log2model(&self, logmodel2: Box<dyn EbiTraitQueriableStochasticLanguage>) -> Result<Fraction>;
}

impl HellingerStochasticConformance for dyn EbiTraitFiniteStochasticLanguage {
    fn hsc_log2log(&self, event_log2: Box<dyn EbiTraitFiniteStochasticLanguage>) -> Result<Fraction> {    
        let mut sum = Fraction::zero();
        let mut probability_sum1 = Fraction::one();
        let mut probability_sum2 = Fraction::one();
    
        for (trace1, probability1) in self.iter_trace_probability() {
            for (trace2, probability2) in event_log2.iter_trace_probability() {
                if trace1 == trace2{
                    probability_sum1 -= probability1;
                    probability_sum2 -= probability2;  

                    let mut term = probability1.sqrt_abs(20);
                    term -= probability2.sqrt_abs(20);
                    term *= term.clone();
                    sum += term;
                }
            }
        }

        sum += probability_sum1;
        sum += probability_sum2; 
        sum = sum.sqrt_abs(20);
        sum /= Fraction::from(2).sqrt_abs(20);
    
        return Ok(sum.one_minus());
    }

    fn hsc_log2model(&self, logmodel2: Box<dyn EbiTraitQueriableStochasticLanguage>) -> Result<Fraction> {
        let mut sum = Fraction::zero();
        let mut probability_sum1 = Fraction::one();
        let mut probability_sum2 = Fraction::one();

        for (trace, probability1) in self.iter_trace_probability() {
            let probability2 = logmodel2.get_probability(&FollowerSemantics::Trace(trace))?;
            if probability2>Fraction::zero() {
                println!("trace: {:?}", trace);

                probability_sum1 -= probability1.clone();
                probability_sum2 -= probability2.clone();  

                let mut term = probability1.sqrt_abs(20);
                term -= probability2.sqrt_abs(20);
                term *= term.clone();
                sum += term;
            }
        }
        sum += probability_sum1.sqrt_abs(20);
        sum += probability_sum2.sqrt_abs(20); 
        sum = sum.sqrt_abs(20);
        sum /= Fraction::from(2).sqrt_abs(20);

        return Ok(sum.one_minus());
    }
}