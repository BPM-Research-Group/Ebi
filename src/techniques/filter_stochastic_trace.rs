use crate::{
    ebi_traits::ebi_trait_finite_stochastic_language::EbiTraitFiniteStochasticLanguage,
    techniques::filter_trace::Operator,
};
use ebi_objects::{FiniteStochasticLanguage, NumberOfTraces, ebi_arithmetic::Fraction};
use fnv::FnvBuildHasher;
use std::collections::HashMap;

pub trait FilterStochasticTraces {
    /// Keep all traces that have a given probability.
    fn remove_traces_probability(&mut self, operator: Operator, threshold: Fraction);

    /// Keep the `x` most likely trace variants, and remove all the others. Ties are decided arbitrarily.
    fn remove_most_likely_traces(&mut self, number_of_traces_to_keep: usize);

    /// Remove the `x` least likely trace variants, and remove all the others. Ties are decided arbitrarily.
    fn remove_least_likely_traces(&mut self, number_of_traces_to_keep: usize);
}

impl FilterStochasticTraces for FiniteStochasticLanguage {
    fn remove_traces_probability(&mut self, operator: Operator, threshold: Fraction) {
        self.retain_traces_probabilities(Box::new(move |_, probability| {
            !operator.apply(probability, &threshold)
        }));
    }

    fn remove_most_likely_traces(&mut self, number_of_traces_to_keep: usize) {
        if number_of_traces_to_keep >= self.number_of_traces() {
            return;
        }
        if number_of_traces_to_keep == 0 {
            self.traces.clear();
            return;
        }

        let mut traces = HashMap::default();
        std::mem::swap(&mut traces, &mut self.traces);

        let mut traces = traces.into_iter().collect::<Vec<_>>();
        let (a, _b, _c) = traces.select_nth_unstable_by(
            number_of_traces_to_keep,
            |(_, probability_a), (_, probability_b)| probability_b.cmp(probability_a),
        );

        let mut x = a
            .into_iter()
            .map(|(trace, probability)| (trace.clone(), probability.clone()))
            .collect::<HashMap<_, _, FnvBuildHasher>>();

        std::mem::swap(&mut x, &mut self.traces);

        self.normalise();
    }

    fn remove_least_likely_traces(&mut self, number_of_traces_to_keep: usize) {
        if number_of_traces_to_keep >= self.number_of_traces() {
            return;
        }
        if number_of_traces_to_keep == 0 {
            self.traces.clear();
            return;
        }

        let mut traces = HashMap::default();
        std::mem::swap(&mut traces, &mut self.traces);

        let mut traces = traces.into_iter().collect::<Vec<_>>();
        let (a, _b, _c) = traces.select_nth_unstable_by(
            number_of_traces_to_keep,
            |(_, probability_a), (_, probability_b)| probability_a.cmp(probability_b),
        );

        let mut x = a
            .into_iter()
            .map(|(trace, probability)| (trace.clone(), probability.clone()))
            .collect::<HashMap<_, _, FnvBuildHasher>>();

        std::mem::swap(&mut x, &mut self.traces);

        self.normalise();
    }
}

impl FilterStochasticTraces for dyn EbiTraitFiniteStochasticLanguage {
    fn remove_traces_probability(&mut self, operator: Operator, threshold: Fraction) {
        self.retain_traces_probabilities(Box::new(move |_, probability| {
            !operator.apply(probability, &threshold)
        }));
    }

    fn remove_most_likely_traces(&mut self, number_of_traces_to_keep: usize) {
        if number_of_traces_to_keep >= self.number_of_traces() {
            return;
        }
        if number_of_traces_to_keep == 0 {
            self.clear();
            return;
        }

        let mut traces = self.iter_traces_probabilities().collect::<Vec<_>>();
        let (a, _b, _c) = traces.select_nth_unstable_by(
            number_of_traces_to_keep,
            |(_, probability_a), (_, probability_b)| probability_b.cmp(probability_a),
        );

        let mut x = a
            .into_iter()
            .map(|(trace, probability)| (trace.clone(), probability.clone()))
            .collect::<HashMap<_, _, FnvBuildHasher>>();

        self.retain_traces_probabilities_mut(Box::new(move |trace, probability| {
            match x.get_mut(trace) {
                Some(p) => {
                    *p = probability.clone();
                    true
                }
                None => false,
            }
        }));

        self.normalise();
    }

    fn remove_least_likely_traces(&mut self, number_of_traces_to_keep: usize) {
        if number_of_traces_to_keep >= self.number_of_traces() {
            return;
        }
        if number_of_traces_to_keep == 0 {
            self.clear();
            return;
        }

        let mut traces = self.iter_traces_probabilities().collect::<Vec<_>>();
        let (a, _b, _c) = traces.select_nth_unstable_by(
            number_of_traces_to_keep,
            |(_, probability_a), (_, probability_b)| probability_a.cmp(probability_b),
        );

        let mut x = a
            .into_iter()
            .map(|(trace, probability)| (trace.clone(), probability.clone()))
            .collect::<HashMap<_, _, FnvBuildHasher>>();

        self.retain_traces_probabilities_mut(Box::new(move |trace, probability| {
            match x.get_mut(trace) {
                Some(p) => {
                    *p = probability.clone();
                    true
                }
                None => false,
            }
        }));

        self.normalise();
    }
}

#[cfg(test)]
mod tests {
    use crate::techniques::filter_stochastic_trace::FilterStochasticTraces;
    use ebi_objects::{FiniteStochasticLanguage, NumberOfTraces};
    use std::fs;

    #[test]
    fn least_frequent_traces() {
        let sin = fs::read_to_string("testfiles/acb-abc-ad-aded-adeded-adededed.slang").unwrap();
        let mut slang = sin.parse::<FiniteStochasticLanguage>().unwrap();

        assert_eq!(slang.number_of_traces(), 6);
        slang.remove_least_likely_traces(2);
        assert_eq!(slang.number_of_traces(), 2);
    }

    #[test]
    fn most_frequent_traces() {
        let sin = fs::read_to_string("testfiles/acb-abc-ad-aded-adeded-adededed.slang").unwrap();
        let mut slang = sin.parse::<FiniteStochasticLanguage>().unwrap();

        assert_eq!(slang.number_of_traces(), 6);
        slang.remove_most_likely_traces(2);
        assert_eq!(slang.number_of_traces(), 2);
    }
}
