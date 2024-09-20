use std::collections::HashMap;

use anyhow::Result;
use fraction::{One, Zero};
use crate::{activity_key::{Activity, ActivityKeyTranslator}, ebi_objects::{labelled_petri_net::LabelledPetriNet, stochastic_labelled_petri_net::StochasticLabelledPetriNet}, ebi_traits::{ebi_trait_finite_stochastic_language::EbiTraitFiniteStochasticLanguage, ebi_trait_iterable_stochastic_language::EbiTraitIterableStochasticLanguage}, math::fraction::Fraction};

pub trait OccurrencesStochasticMiner {
    fn mine_occurrences_stochastic(self, language: Box<dyn EbiTraitFiniteStochasticLanguage>) -> StochasticLabelledPetriNet;
}

impl OccurrencesStochasticMiner for LabelledPetriNet {
    fn mine_occurrences_stochastic(mut self, language: Box<dyn EbiTraitFiniteStochasticLanguage>) -> StochasticLabelledPetriNet {
        let translator = ActivityKeyTranslator::new(language.get_activity_key(), self.get_activity_key_mut());
        
        let mut model_activity2frequency = HashMap::new();
        for (trace, probability) in language.iter_trace_probability() {
            for log_activity in trace {
                let model_activity = translator.translate_activity(log_activity);
                *model_activity2frequency.entry(model_activity).or_insert(Fraction::zero()) += probability;
            }
        }

        let mut weights: Vec<Fraction> = vec![];
        for transition in 0..self.get_number_of_transitions() {

            if let Some(model_activity) = self.get_transition_label(transition) {
                //labelled transition
                weights.push(model_activity2frequency.get(&model_activity).unwrap().clone());
            } else {
                //silent transition
                weights.push(Fraction::one());
            }
        }

        (self, weights).into()
    }
}