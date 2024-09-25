use std::collections::HashMap;
use anyhow::{Result, anyhow};

use crate::{ebi_framework::{activity_key::{Activity, ActivityKey}, infoable::Infoable}, ebi_traits::{ebi_trait_iterable_language::EbiTraitIterableLanguage, ebi_trait_semantics::Semantics, ebi_trait_stochastic_semantics::TransitionIndex}};

use super::finite_language::FiniteLanguage;

#[derive(Debug)]
pub struct FiniteLanguageSemantics {
    activity_key: ActivityKey,
    nodes: Vec<HashMap<Option<Activity>, usize>> //state -> activity or silent -> state
}

impl FiniteLanguageSemantics {
    pub fn from_language(lang: &FiniteLanguage) -> Self {
        let activity_key = ActivityKey::new();
        let mut nodes: Vec<HashMap<Option<Activity>, usize>> = vec![];

        nodes.push(HashMap::new()); //0: root
        nodes.push(HashMap::new()); //1: dead
        
        for trace in lang.iter() {

            let mut node_index = 0usize;

            for activity in trace {
                let child_index;
                if let Some(ci) = nodes[node_index].get(&Some(*activity)) {
                    child_index = *ci;
                } else {
                    child_index = nodes.len();
                    nodes.push(HashMap::new());
                }
                nodes[node_index].insert(Some(*activity), child_index);
                node_index = child_index;
            }

            //add a silent transition to the dead state
            {
                let activity = None;
                let child_index;
                if let Some(ci) = nodes[node_index].get(&activity) {
                    child_index = *ci;
                } else {
                    child_index = nodes.len();
                    nodes.push(HashMap::new());
                }
                nodes[node_index].insert(activity, child_index);
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

impl Semantics for FiniteLanguageSemantics {
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
        
        if let Some(new_state) = self.nodes[*state].get(&activity) {
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

impl Infoable for FiniteLanguageSemantics {
    fn info(&self, f: &mut impl std::io::Write) -> Result<()> {
        writeln!(f, "Number of states\t{}", self.nodes.len())?;

        Ok(write!(f, "")?)
    }
}