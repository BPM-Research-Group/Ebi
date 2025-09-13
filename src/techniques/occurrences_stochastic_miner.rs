use std::collections::HashMap;

use ebi_arithmetic::{Fraction, One, Zero};
use ebi_objects::{
    ebi_objects::process_tree::Node, ActivityKeyTranslator, HasActivityKey, LabelledPetriNet, ProcessTree, StochasticLabelledPetriNet, StochasticProcessTree
};

use crate::{
    ebi_traits::ebi_trait_finite_stochastic_language::EbiTraitFiniteStochasticLanguage,
    semantics::semantics::Semantics,
};

pub trait OccurrencesStochasticMinerLPN {
    fn mine_occurrences_stochastic_lpn(
        self,
        language: Box<dyn EbiTraitFiniteStochasticLanguage>,
    ) -> StochasticLabelledPetriNet;
}

pub trait OccurrencesStochasticMinerTree {
    fn mine_occurrences_stochastic_tree(
        self,
        language: Box<dyn EbiTraitFiniteStochasticLanguage>,
    ) -> StochasticProcessTree;
}

impl OccurrencesStochasticMinerLPN for LabelledPetriNet {
    fn mine_occurrences_stochastic_lpn(
        mut self,
        language: Box<dyn EbiTraitFiniteStochasticLanguage>,
    ) -> StochasticLabelledPetriNet {
        let translator =
            ActivityKeyTranslator::new(language.activity_key(), self.activity_key_mut());

        let mut model_activity2frequency = HashMap::new();
        for activity in self.activity_key().get_activities() {
            model_activity2frequency.insert(*activity, Fraction::zero());
        }
        for (trace, probability) in language.iter_trace_probability() {
            for log_activity in trace {
                let model_activity = translator.translate_activity(log_activity);
                model_activity2frequency
                    .entry(model_activity)
                    .and_modify(|f: &mut Fraction| *f += probability);
            }
        }

        let mut weights: Vec<Fraction> = vec![];
        for transition in 0..self.get_number_of_transitions() {
            if let Some(model_activity) = self.get_transition_label(transition) {
                //labelled transition
                weights.push(
                    model_activity2frequency
                        .get(&model_activity)
                        .unwrap()
                        .clone(),
                );
            } else {
                //silent transition
                weights.push(Fraction::one());
            }
        }

        (self, weights).into()
    }
}

impl OccurrencesStochasticMinerTree for ProcessTree {
    fn mine_occurrences_stochastic_tree(
        mut self,
        language: Box<dyn EbiTraitFiniteStochasticLanguage>,
    ) -> StochasticProcessTree {
        let translator =
            ActivityKeyTranslator::new(language.activity_key(), self.activity_key_mut());

        let mut model_activity2frequency = HashMap::new();
        for activity in self.activity_key().get_activities() {
            model_activity2frequency.insert(*activity, Fraction::zero());
        }
        for (trace, probability) in language.iter_trace_probability() {
            for log_activity in trace {
                let model_activity = translator.translate_activity(log_activity);
                model_activity2frequency
                    .entry(model_activity)
                    .and_modify(|f: &mut Fraction| *f += probability);
            }
        }

        let mut weights: Vec<Fraction> = vec![];
        for node in &self.tree {
            match node {
                Node::Activity(model_activity) => {
                    //labelled transition
                    weights.push(
                        model_activity2frequency
                            .get(&model_activity)
                            .unwrap()
                            .clone(),
                    );
                }
                Node::Tau => {
                    //silent transition
                    weights.push(Fraction::one());
                }
                _ => {}
            }
        }

        (self, weights, Fraction::one()).try_into().unwrap()
    }
}

#[cfg(test)]
mod tests {
    use std::fs;

    use ebi_arithmetic::exact::is_exact_globally;
    use ebi_objects::{FiniteStochasticLanguage, LabelledPetriNet};

    use super::OccurrencesStochasticMinerLPN;

    #[test]
    fn lpn_occurrence() {
        let fin1 = fs::read_to_string("testfiles/aa-ab-ba.lpn").unwrap();
        let lpn = fin1.parse::<LabelledPetriNet>().unwrap();
        let fin2 = fs::read_to_string("testfiles/aa-ab-ba.slang").unwrap();
        let slang = fin2.parse::<FiniteStochasticLanguage>().unwrap();
        let slpn = lpn.mine_occurrences_stochastic_lpn(Box::new(slang));
        if is_exact_globally() {
            //with approximate arithmetic, this test is too fragile
            let fout = fs::read_to_string("testfiles/aa-ab-ba_occ.slpn").unwrap();
            assert_eq!(fout, slpn.to_string())
        };
    }
}
