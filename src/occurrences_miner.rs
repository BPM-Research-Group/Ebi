use std::collections::HashMap;

use anyhow::Result;
use fraction::{One, Zero};
use crate::{activity_key::Activity, ebi_objects::{labelled_petri_net::LabelledPetriNet, stochastic_labelled_petri_net::StochasticLabelledPetriNet}, ebi_traits::{ebi_trait_finite_stochastic_language::EbiTraitFiniteStochasticLanguage, ebi_trait_iterable_stochastic_language::EbiTraitIterableStochasticLanguage, ebi_trait_labelled_petri_net::EbiTraitLabelledPetriNet}, math::fraction::Fraction};

pub fn mine(mut net: Box<dyn EbiTraitLabelledPetriNet>, language: Box<dyn EbiTraitFiniteStochasticLanguage>) -> StochasticLabelledPetriNet {
    let mut log_activity2frequency: HashMap<Activity, Fraction> = HashMap::new();

    for (trace, probability) in language.iter_trace_probability() {
        for activity in trace {
            *log_activity2frequency.entry(*activity).or_insert(Fraction::zero()) += probability.clone();
        }
    }

    let mut log_activity_key = language.get_activity_key().clone();

    let mut weights: Vec<Fraction> = vec![];
    for transition in net.get_transitions() {

        if transition.is_silent() {
            weights.push(Fraction::one());
        } else {
            let net_activity = transition.get_label().unwrap();
            let string_activity = net.get_activity_key().deprocess_activity(&net_activity);
            let log_activity = log_activity_key.process_activity(string_activity);

            weights.push(
                log_activity2frequency.get(&log_activity).unwrap().clone()
            );
        }
    }

    (net, weights).into()
}