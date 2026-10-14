use crate::{
    ebi_traits::ebi_trait_finite_stochastic_language::EbiTraitFiniteStochasticLanguage,
    techniques::filter_trace::Operator,
};
use ebi_objects::{FiniteStochasticLanguage, anyhow::Result, ebi_arithmetic::Fraction};

pub trait FilterStochasticTraces {
    /// Keep all traces that have a given probability.
    fn remove_traces_probability(&mut self, operator: Operator, threshold: Fraction);

    /// Keep the `x` most frequent trace variants, and remove all the others. Ties are decided arbitrarily.
    fn keep_most_frequent_traces(&mut self, number_of_traces_to_keep: usize) -> Result<()>;

    /// Remove the `x` least frequent trace variants, and remove all the others. Ties are decided arbitrarily.
    fn remove_least_frequent_traces(&mut self, number_of_traces_to_remove: usize) -> Result<()>;
}

impl FilterStochasticTraces for FiniteStochasticLanguage {
    fn remove_traces_probability(&mut self, operator: Operator, threshold: Fraction) {
        self.retain_traces_probabilities(Box::new(move |_, probability| {
            !operator.apply(probability, &threshold)
        }));
    }

    fn keep_most_frequent_traces(&mut self, number_of_traces_to_keep: usize) -> Result<()> {
        
    }

    fn remove_least_frequent_traces(&mut self, number_of_traces_to_remove: usize) -> Result<()> {
        todo!()
    }
}
