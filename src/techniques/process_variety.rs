use ebi_arithmetic::Fraction;
use rayon::iter::{IntoParallelIterator, ParallelIterator};

use crate::{
    ebi_traits::ebi_trait_finite_stochastic_language::EbiTraitFiniteStochasticLanguage,
    math::levenshtein,
};

pub trait ProcessVariety {
    fn rao_stirling_diversity(&self) -> Fraction;
}

impl ProcessVariety for dyn EbiTraitFiniteStochasticLanguage {
    fn rao_stirling_diversity(&self) -> Fraction {
        (0..self.number_of_traces())
            .into_par_iter()
            .map(|i| {
                let trace_i = self.get_trace(i).unwrap();
                let probability_i = self.get_trace_probability(i).unwrap();
                (i + 1..self.number_of_traces())
                    .into_par_iter()
                    .map(|j| {
                        let trace_j = self.get_trace(j).unwrap();
                        let probability_j = self.get_trace_probability(j).unwrap();
                        let mut d = Fraction::from(levenshtein::distance(trace_i, trace_j));
                        d *= probability_i;
                        d *= probability_j;
                        d
                    })
                    .sum::<Fraction>()
            })
            .sum()
    }
}

#[cfg(test)]
mod tests {
    use std::fs;

    use ebi_arithmetic::Fraction;
    use ebi_objects::FiniteStochasticLanguage;

    use crate::{
        ebi_traits::ebi_trait_finite_stochastic_language::EbiTraitFiniteStochasticLanguage,
        techniques::process_variety::ProcessVariety,
    };

    #[test]
    fn variety() {
        let fin = fs::read_to_string("testfiles/aa-ab-ba.slang").unwrap();
        let slang: Box<dyn EbiTraitFiniteStochasticLanguage> =
            Box::new(fin.parse::<FiniteStochasticLanguage>().unwrap());

        assert_eq!(slang.rao_stirling_diversity(), Fraction::from((2, 5)));
    }
}
