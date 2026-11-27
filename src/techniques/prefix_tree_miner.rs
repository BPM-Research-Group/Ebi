use crate::{ebi_traits::EbiTraitFiniteLanguage, semantics::semantics_trait::Semantics};
use ebi_objects::{
    ebi_arithmetic::ebi_number::Zero, DeterministicFiniteAutomaton, FiniteLanguage, NumberOfTraces,
    ProcessTree, TreeNode, TreeOperator,
};
use fnv::FnvBuildHasher;
use std::collections::HashMap;

pub trait PrefixTreeMinerDFA {
    fn mine_prefix_tree_dfa(&self) -> DeterministicFiniteAutomaton;
}

pub trait PrefixTreeMinerTree {
    fn mine_prefix_tree_tree(&self) -> ProcessTree;
}

impl PrefixTreeMinerDFA for dyn EbiTraitFiniteLanguage {
    fn mine_prefix_tree_dfa(&self) -> DeterministicFiniteAutomaton {
        let mut result = DeterministicFiniteAutomaton::default();
        result.set_activity_key(self.activity_key().clone());

        if self.number_of_traces().is_zero() {
            result.set_initial_state(None);
        } else {
            for trace in self.iter_traces() {
                let mut state = result.get_initial_state().unwrap();

                for activity in trace {
                    state = result.take_or_add_transition(state, *activity);
                }

                result.set_final_state(state, true);
            }
        }

        result
    }
}

impl PrefixTreeMinerTree for dyn EbiTraitFiniteLanguage {
    fn mine_prefix_tree_tree(&self) -> ProcessTree {
        if self.number_of_traces().is_zero() {
            //empty language; return empty tree
            return ProcessTree::default();
        }

        //clone to be able to recurse
        let lang = self.to_finite_language();

        let mut tree = vec![];
        node_2_tree(lang, &mut tree);

        ProcessTree::new_unchecked(self.activity_key().clone(), tree)
    }
}

pub fn node_2_tree(lang: FiniteLanguage, tree: &mut Vec<TreeNode>) {
    //split traces
    let mut activity_2_lang = HashMap::<_, _, FnvBuildHasher>::default();
    let mut empty_traces = false;
    for mut trace in lang {
        if trace.is_empty() {
            empty_traces = true;
        } else {
            let first_activity = trace.remove(0);
            activity_2_lang
                .entry(first_activity)
                .or_insert_with(FiniteLanguage::default)
                .push(trace);
        }
    }

    if empty_traces && activity_2_lang.is_empty() {
        tree.push(TreeNode::Tau);
        return;
    }

    if activity_2_lang.len() == 1 {
        //do nothing
    } else if empty_traces {
        tree.push(TreeNode::Operator(
            TreeOperator::Xor,
            activity_2_lang.len() + 1,
        ));
        tree.push(TreeNode::Tau);
    } else {
        tree.push(TreeNode::Operator(TreeOperator::Xor, activity_2_lang.len()));
    }

    for (activity, sub_lang) in activity_2_lang {
        if sub_lang.number_of_events() == 0 {
            //only empty traces left
            tree.push(TreeNode::Activity(activity));
        } else {
            tree.push(TreeNode::Operator(TreeOperator::Sequence, 2));
            tree.push(TreeNode::Activity(activity));
            //recurse
            node_2_tree(sub_lang, tree);
        }
    }
}
