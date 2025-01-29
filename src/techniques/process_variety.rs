use rayon::iter::{IntoParallelIterator, ParallelIterator};

use crate::{ebi_traits::ebi_trait_finite_stochastic_language::EbiTraitFiniteStochasticLanguage, math::{fraction::Fraction, levenshtein}};

pub trait ProcessVariety {
    fn rao_stirling_diversity(&self) -> Fraction;

    fn diversity_gini(&self) -> Fraction;
}

impl ProcessVariety for dyn EbiTraitFiniteStochasticLanguage {
    fn rao_stirling_diversity(&self) -> Fraction {
        (0..self.len()).into_par_iter().map(|i| {
            let trace_i = self.get_trace(i).unwrap();
            let probability_i = self.get_trace_proability(i).unwrap();
            (i+1..self.len()).into_par_iter().map(|j| {
                let trace_j = self.get_trace(j).unwrap();
                let probability_j = self.get_trace_proability(j).unwrap();
                let mut d = Fraction::from(levenshtein::distance(trace_i, trace_j));
                d *= probability_i;
                d *= probability_j;
                d
            }).sum::<Fraction>()
        }).sum()
    }

    fn diversity_gini(&self) -> Fraction {
        let n_c = self.len();
        Fraction::from(n_c)
    }
}