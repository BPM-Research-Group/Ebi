use crate::{
    ebi_traits::ebi_trait_finite_stochastic_language::EbiTraitFiniteStochasticLanguage,
    math::levenshtein,
};
use ebi_objects::ebi_arithmetic::Fraction;
use rayon::iter::{IndexedParallelIterator, ParallelIterator};

pub trait ProcessVariety {
    fn rao_stirling_diversity(&self) -> Fraction;
}

impl ProcessVariety for dyn EbiTraitFiniteStochasticLanguage {
    fn rao_stirling_diversity(&self) -> Fraction {
        self.par_iter_traces_probabilities()
            .enumerate()
            .map(|(i, (trace_i, probability_i))| {
                self.par_iter_traces_probabilities()
                    .skip(i)
                    .map(|(trace_j, probability_j)| {
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
    use crate::{
        ebi_traits::ebi_trait_finite_stochastic_language::EbiTraitFiniteStochasticLanguage,
        techniques::process_variety::ProcessVariety,
    };
    use ebi_objects::{FiniteStochasticLanguage, ebi_arithmetic::Fraction};
    use std::fs;

    #[test]
    fn variety() {
        let fin = fs::read_to_string("testfiles/aa-ab-ba.slang").unwrap();
        let slang: Box<dyn EbiTraitFiniteStochasticLanguage> =
            Box::new(fin.parse::<FiniteStochasticLanguage>().unwrap());

        assert_eq!(slang.rao_stirling_diversity(), Fraction::from((2, 5)));
    }
}
