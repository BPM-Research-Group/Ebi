use std::collections::HashSet;

use anyhow::{Result, anyhow};
use ebi_objects::{
    DeterministicFiniteAutomaton, HasActivityKey, ProcessTree,
    ebi_objects::process_tree::{Node, Operator},
};

use crate::{ebi_traits::{
    ebi_trait_finite_language::EbiTraitFiniteLanguage, ebi_trait_semantics::EbiTraitSemantics,
}, semantics::semantics::Semantics};

pub trait FlowerMinerTree {
    fn mine_flower_tree(&self) -> ProcessTree;
}

pub trait FlowerMinerDFA {
    fn mine_flower_dfa(&self) -> Result<DeterministicFiniteAutomaton>;
}

impl FlowerMinerTree for dyn EbiTraitFiniteLanguage {
    fn mine_flower_tree(&self) -> ProcessTree {
        //gather activities
        let mut activities = HashSet::new();
        for trace in self.iter() {
            for activity in trace {
                activities.insert(activity);
            }
        }

        let mut tree = vec![
            Node::Operator(Operator::Loop, activities.len() + 1),
            Node::Tau,
        ];
        tree.append(
            &mut activities
                .iter()
                .map(|a| Node::Activity(**a))
                .collect::<Vec<_>>(),
        );

        (self.activity_key().clone(), tree).into()
    }
}

impl FlowerMinerDFA for dyn EbiTraitFiniteLanguage {
    fn mine_flower_dfa(&self) -> Result<DeterministicFiniteAutomaton> {
        //gather activities
        let mut activities = HashSet::new();
        for trace in self.iter() {
            for activity in trace {
                activities.insert(activity);
            }
        }

        let mut result = DeterministicFiniteAutomaton::new();
        result.set_activity_key(self.activity_key().clone());
        let state = result
            .get_initial_state()
            .ok_or_else(|| anyhow!("no initial state found"))?;
        result.set_final_state(state, true);

        for activity in activities {
            result.add_transition(state, *activity, state)?;
        }

        Ok(result)
    }
}

impl FlowerMinerDFA for EbiTraitSemantics {
    ///This method relies on the activityKey being set properly. That is, a flower model will be returned that contains all -declared- activities, even if they are not used in the model, or not reachable, or .. .
    fn mine_flower_dfa(&self) -> Result<DeterministicFiniteAutomaton> {
        let mut result = DeterministicFiniteAutomaton::new();
        result.set_activity_key(self.activity_key().clone());
        let state = result
            .get_initial_state()
            .ok_or_else(|| anyhow!("no initial state found"))?;
        result.set_final_state(state, true);

        let activities = result
            .activity_key()
            .get_activities()
            .into_iter()
            .cloned()
            .collect::<Vec<_>>();

        for activity in activities {
            result.add_transition(state, activity, state)?;
        }

        Ok(result)
    }
}

impl FlowerMinerTree for EbiTraitSemantics {
    ///This method relies on the activityKey being set properly. That is, a flower model will be returned that contains all -declared- activities, even if they are not used in the model, or not reachable, or .. .
    fn mine_flower_tree(&self) -> ProcessTree {
        let activity_key = self.activity_key().clone();
        let activities = activity_key
            .get_activities()
            .into_iter()
            .cloned()
            .collect::<Vec<_>>();

        let mut tree = vec![
            Node::Operator(Operator::Loop, activities.len() + 1),
            Node::Tau,
        ];
        tree.append(
            &mut activities
                .iter()
                .map(|a| Node::Activity(*a))
                .collect::<Vec<_>>(),
        );

        (activity_key, tree).into()
    }
}

impl FlowerMinerDFA for dyn HasActivityKey {
    ///This method relies on the activityKey being set properly. That is, a flower model will be returned that contains all -declared- activities, even if they are not used in the model, or not reachable, or .. .
    fn mine_flower_dfa(&self) -> Result<DeterministicFiniteAutomaton> {
        let mut result = DeterministicFiniteAutomaton::new();
        result.set_activity_key(self.activity_key().clone());
        let state = result
            .get_initial_state()
            .ok_or_else(|| anyhow!("no initial state found"))?;
        result.set_final_state(state, true);

        let activities = result
            .activity_key()
            .get_activities()
            .into_iter()
            .cloned()
            .collect::<Vec<_>>();

        for activity in activities {
            result.add_transition(state, activity, state)?;
        }

        Ok(result)
    }
}

impl FlowerMinerTree for dyn HasActivityKey {
    ///This method relies on the activityKey being set properly. That is, a flower model will be returned that contains all -declared- activities, even if they are not used in the model, or not reachable, or .. .
    fn mine_flower_tree(&self) -> ProcessTree {
        let activity_key = self.activity_key().clone();
        let activities = activity_key
            .get_activities()
            .into_iter()
            .cloned()
            .collect::<Vec<_>>();

        let mut tree = vec![
            Node::Operator(Operator::Loop, activities.len() + 1),
            Node::Tau,
        ];
        tree.append(
            &mut activities
                .iter()
                .map(|a| Node::Activity(*a))
                .collect::<Vec<_>>(),
        );

        (activity_key, tree).into()
    }
}
