use crate::{
    ebi_traits::{EbiTraitFiniteLanguage, EbiTraitSemantics},
    semantics::semantics_trait::Semantics,
};
use ebi_objects::{
    anyhow::{anyhow, Result},
    tau, DeterministicFiniteAutomaton, HasActivityKey, ProcessTree, TreeNode, TreeOperator,
};
use std::collections::HashSet;

pub trait FlowerMinerTree {
    fn mine_flower_tree(&self) -> ProcessTree;
}

pub trait FlowerMinerDFA {
    fn mine_flower_dfa(&self) -> Result<DeterministicFiniteAutomaton>;
}

impl FlowerMinerTree for dyn EbiTraitFiniteLanguage {
    fn mine_flower_tree(&self) -> ProcessTree {
        if self.number_of_traces() == 0 {
            //empty language
            return ProcessTree::default();
        }

        //gather activities
        let mut activities = HashSet::new();
        for trace in self.iter_traces() {
            for activity in trace {
                activities.insert(activity);
            }
        }

        if activities.is_empty() {
            // only empty traces
            return tau!();
        }

        let mut tree = vec![
            TreeNode::Operator(TreeOperator::Loop, activities.len() + 1),
            TreeNode::Tau,
        ];
        tree.append(
            &mut activities
                .iter()
                .map(|a| TreeNode::Activity(**a))
                .collect::<Vec<_>>(),
        );

        ProcessTree::new_unchecked(self.activity_key().clone(), tree)
    }
}

impl FlowerMinerDFA for dyn EbiTraitFiniteLanguage {
    fn mine_flower_dfa(&self) -> Result<DeterministicFiniteAutomaton> {
        //gather activities
        let mut activities = HashSet::new();
        for trace in self.iter_traces() {
            for activity in trace {
                activities.insert(activity);
            }
        }

        let mut result = DeterministicFiniteAutomaton::default();
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
        let mut result = DeterministicFiniteAutomaton::default();
        result.set_activity_key(self.activity_key().clone());
        let state = result
            .get_initial_state()
            .ok_or_else(|| anyhow!("no initial state found"))?;
        result.set_final_state(state, true);

        let activities = result
            .activity_key()
            .activities()
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
            .activities()
            .into_iter()
            .cloned()
            .collect::<Vec<_>>();

        if activities.is_empty() {
            return tau!();
        }

        let mut tree = vec![
            TreeNode::Operator(TreeOperator::Loop, activities.len() + 1),
            TreeNode::Tau,
        ];
        tree.append(
            &mut activities
                .iter()
                .map(|a| TreeNode::Activity(*a))
                .collect::<Vec<_>>(),
        );

        ProcessTree::new_unchecked(activity_key, tree)
    }
}

impl FlowerMinerDFA for dyn HasActivityKey {
    ///This method relies on the activityKey being set properly. That is, a flower model will be returned that contains all -declared- activities, even if they are not used in the model, or not reachable, or .. .
    fn mine_flower_dfa(&self) -> Result<DeterministicFiniteAutomaton> {
        let mut result = DeterministicFiniteAutomaton::default();
        result.set_activity_key(self.activity_key().clone());
        let state = result
            .get_initial_state()
            .ok_or_else(|| anyhow!("no initial state found"))?;
        result.set_final_state(state, true);

        let activities = result
            .activity_key()
            .activities()
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
            .activities()
            .into_iter()
            .cloned()
            .collect::<Vec<_>>();

        if activities.is_empty() {
            return tau!();
        }

        let mut tree = vec![
            TreeNode::Operator(TreeOperator::Loop, activities.len() + 1),
            TreeNode::Tau,
        ];
        tree.append(
            &mut activities
                .iter()
                .map(|a| TreeNode::Activity(*a))
                .collect::<Vec<_>>(),
        );

        ProcessTree::new_unchecked(activity_key, tree)
    }
}
