use anyhow::Result;
use fraction::{One, Zero};

use crate::{ebi_traits::{ebi_trait_finite_stochastic_language::EbiTraitFiniteStochasticLanguage, ebi_trait_queriable_stochastic_language::EbiTraitQueriableStochasticLanguage}, follower_semantics::FollowerSemantics, math::fraction::Fraction};

pub fn uemsc(log1: Box<dyn EbiTraitFiniteStochasticLanguage>, mut language2: Box<dyn EbiTraitQueriableStochasticLanguage>) -> Result<Fraction> {
    let mut sum = Fraction::zero();

    for (trace, probability1) in log1.iter_trace_probability() {
        let probability2 = language2.get_probability(&FollowerSemantics::Trace(&trace))?;

        if *probability1 > probability2 {
            sum += probability1;
            sum -= probability2;
        }
    }

    sum = sum.one_minus();
    return Ok(sum);
}