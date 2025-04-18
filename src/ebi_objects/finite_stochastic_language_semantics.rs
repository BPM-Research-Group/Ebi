use anyhow::{Result, anyhow};
use std::collections::HashMap;

use crate::{
    ebi_framework::{
        activity_key::{
            Activity, ActivityKey, ActivityKeyTranslator, HasActivityKey, TranslateActivityKey,
        },
        infoable::Infoable,
    },
    ebi_traits::{
        ebi_trait_event_log::IndexTrace,
        ebi_trait_iterable_stochastic_language::EbiTraitIterableStochasticLanguage,
        ebi_trait_semantics::Semantics,
        ebi_trait_stochastic_semantics::{StochasticSemantics, TransitionIndex},
    },
    math::{fraction::Fraction, traits::Zero},
};

use super::finite_stochastic_language::FiniteStochasticLanguage;

#[derive(Debug, ActivityKey)]
pub struct FiniteStochasticLanguageSemantics {
    activity_key: ActivityKey,
    nodes: Vec<HashMap<Option<Activity>, (usize, Fraction)>>, //state -> activity or silent -> (state, probability)
}

impl FiniteStochasticLanguageSemantics {
    pub fn from_language(lang: &FiniteStochasticLanguage) -> Self {
        let activity_key = lang.get_activity_key().clone();

        let mut nodes: Vec<HashMap<Option<Activity>, (usize, Fraction)>> = vec![];
        if lang.len().is_zero() {
            //empty language
        } else {
            nodes.push(HashMap::new()); //0: root

            for (trace, trace_probability) in lang.iter_trace_probability() {
                let mut node_index = 0usize;

                for activity in trace {
                    let mut new_probability: Fraction;
                    let child_index;
                    if let Some((ci, old_probability)) = nodes[node_index].get(&Some(*activity)) {
                        child_index = *ci;
                        new_probability = old_probability.to_owned();
                        new_probability += trace_probability.to_owned();
                    } else {
                        child_index = nodes.len();
                        nodes.push(HashMap::new());
                        new_probability = trace_probability.to_owned();
                    }
                    nodes[node_index].insert(Some(*activity), (child_index, new_probability));
                    node_index = child_index;
                }

                //add a silent transition to the dead state
                {
                    let activity = None;
                    let new_probability;
                    let child_index;
                    if let Some((ci, old_probability)) = nodes[node_index].get(&activity) {
                        child_index = *ci;
                        new_probability = old_probability + trace_probability;
                    } else {
                        child_index = nodes.len();
                        nodes.push(HashMap::new());
                        new_probability = trace_probability.clone();
                    }
                    nodes[node_index].insert(activity, (child_index, new_probability));
                }
            }
        }

        Self {
            activity_key: activity_key,
            nodes: nodes,
        }
    }

    fn transition_index_to_activity(&self, transition: TransitionIndex) -> Option<Activity> {
        if transition == 0 {
            None
        } else {
            Some(self.activity_key.get_activity_by_id(transition - 1))
        }
    }

    fn activity_to_transition_index(&self, activity: &Option<Activity>) -> TransitionIndex {
        match activity {
            Some(ai) => 1 + self.activity_key.get_id_from_activity(ai),
            None => 0,
        }
    }
}

impl TranslateActivityKey for FiniteStochasticLanguageSemantics {
    fn translate_using_activity_key(&mut self, to_activity_key: &mut ActivityKey) {
        let translator = ActivityKeyTranslator::new(&self.activity_key, to_activity_key);

        self.nodes.iter_mut().for_each(|map| {
            *map = map
                .drain()
                .map(|(activity, x)| {
                    (
                        if let Some(a) = activity {
                            Some(translator.translate_activity(&a))
                        } else {
                            activity
                        },
                        x,
                    )
                })
                .collect()
        });

        self.activity_key = to_activity_key.clone();
    }
}

impl Semantics for FiniteStochasticLanguageSemantics {
    type SemState = usize;

    fn get_initial_state(&self) -> Option<usize> {
        if self.nodes.len().is_zero() {
            None
        } else {
            Some(0)
        }
    }

    fn execute_transition(&self, state: &mut usize, transition: TransitionIndex) -> Result<()> {
        let activity = self.transition_index_to_activity(transition);

        if let Some((new_state, _)) = self.nodes[*state].get(&activity) {
            *state = *new_state;
            return Ok(());
        }
        return Err(anyhow!(
            "transition cannot be executed as it is not enabled in state {}",
            state
        ));
    }

    fn is_final_state(&self, state: &usize) -> bool {
        self.nodes[*state].is_empty()
    }

    fn is_transition_silent(&self, transition: TransitionIndex) -> bool {
        transition == 0
    }

    fn get_transition_activity(&self, transition: TransitionIndex) -> Option<Activity> {
        self.transition_index_to_activity(transition)
    }

    fn get_enabled_transitions(&self, state: &usize) -> Vec<TransitionIndex> {
        let mut result = vec![];
        for (activity, _) in &self.nodes[*state] {
            result.push(self.activity_to_transition_index(activity));
        }
        return result;
    }

    fn get_number_of_transitions(&self) -> usize {
        self.activity_key.get_number_of_activities()
    }
}

impl StochasticSemantics for FiniteStochasticLanguageSemantics {
    type StoSemState = usize;

    fn get_transition_weight(&self, state: &usize, transition: TransitionIndex) -> &Fraction {
        let activity = self.transition_index_to_activity(transition);

        &self.nodes[*state].get(&activity).unwrap().1
    }

    fn get_total_weight_of_enabled_transitions(&self, state: &usize) -> Result<Fraction> {
        let mut sum = Fraction::zero();
        for (_, (_, probability)) in &self.nodes[*state] {
            sum += probability;
        }
        if sum == Fraction::zero() {
            return Err(anyhow!("total enabled weight is 0"));
        }
        Ok(sum)
    }
}

impl Infoable for FiniteStochasticLanguageSemantics {
    fn info(&self, f: &mut impl std::io::Write) -> Result<()> {
        writeln!(f, "Number of states\t{}", self.nodes.len())?;

        Ok(write!(f, "")?)
    }
}

#[cfg(test)]
mod tests {
    use std::fs;

    use crate::{
        ebi_objects::finite_stochastic_language::FiniteStochasticLanguage,
        ebi_traits::ebi_trait_stochastic_semantics::{
            EbiTraitStochasticSemantics, ToStochasticSemantics,
        },
    };

    #[test]
    fn slang_empty() {
        let fin = fs::read_to_string("testfiles/empty.slang").unwrap();
        let slpn = fin.parse::<FiniteStochasticLanguage>().unwrap();

        if let EbiTraitStochasticSemantics::Usize(semantics) = slpn.to_stochastic_semantics() {
            assert!(semantics.get_initial_state().is_none())
        } else {
            assert!(false)
        }
    }
}
