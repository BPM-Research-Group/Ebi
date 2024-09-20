use std::{collections::HashMap, rc::Rc};
use anyhow::{Result, anyhow};

use crate::{ebi_framework::{activity_key::{Activity, ActivityKey}, infoable::Infoable}, ebi_traits::{ebi_trait_iterable_stochastic_language::EbiTraitIterableStochasticLanguage, ebi_trait_semantics::Semantics, ebi_trait_stochastic_semantics::{StochasticSemantics, TransitionIndex}}, math::fraction::Fraction};

use super::finite_stochastic_language::FiniteStochasticLanguage;

#[derive(Debug)]
pub struct FiniteStochasticLanguageSemantics {
    activity_key: ActivityKey,
    nodes: Vec<HashMap<Option<Activity>, (usize, Fraction)>> //state -> activity or silent -> (state, probability)
}

impl FiniteStochasticLanguageSemantics {
    pub fn from_language(lang: Rc<FiniteStochasticLanguage>) -> Self {
        let mut activity_key = ActivityKey::new();
        let mut nodes: Vec<HashMap<Option<Activity>, (usize, Fraction)>> = vec![];

        nodes.push(HashMap::new()); //0: root
        nodes.push(HashMap::new()); //1: dead
        
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

        Self {
            activity_key: activity_key,
            nodes: nodes
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

impl Semantics for FiniteStochasticLanguageSemantics {
    type State = usize;

    fn get_activity_key(&self) -> &ActivityKey {
        &self.activity_key
    }

    fn get_activity_key_mut(&mut self) -> &mut ActivityKey {
        &mut self.activity_key
    }

    fn get_initial_state(&self) -> Self::State {
        0
    }

    fn execute_transition(&self, state: &mut Self::State, transition: TransitionIndex) -> Result<()> {
        let activity = self.transition_index_to_activity(transition);
        
        if let Some((new_state, _)) = self.nodes[*state].get(&activity) {
            *state = *new_state; 
            return Ok(());
        }
        return Err(anyhow!("transition cannot be executed as it is not enabled in state {}", state));
    }

    fn is_final_state(&self, state: &Self::State) -> bool {
        *state == 1usize
    }

    fn is_transition_silent(&self, transition: TransitionIndex) -> bool {
        transition == 0
    }

    fn get_transition_activity(&self, transition: TransitionIndex) -> Option<Activity> {
        self.transition_index_to_activity(transition)
    }

    fn get_enabled_transitions(&self, state: &Self::State) -> Vec<TransitionIndex> {
        let mut result = vec![];
        for (activity, _) in &self.nodes[*state] {
            result.push(self.activity_to_transition_index(activity));
        }
        return result;
    }

}

impl StochasticSemantics for FiniteStochasticLanguageSemantics {
    fn get_transition_weight(&self, state: &Self::State, transition: TransitionIndex) -> &Fraction {
        let activity = self.transition_index_to_activity(transition);

        &self.nodes[*state].get(&activity).unwrap().1
    }

    fn get_total_weight_of_enabled_transitions(&self, state: &Self::State) -> Result<Fraction> {
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