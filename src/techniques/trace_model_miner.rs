use ebi_objects::{ebi_arithmetic::Zero, ProcessTree, TreeNode, TreeOperator};

use crate::ebi_traits::ebi_trait_finite_language::EbiTraitFiniteLanguage;

pub trait TraceModelMinerTree {
    fn mine_trace_model_tree(&self) -> ProcessTree;
}

impl TraceModelMinerTree for dyn EbiTraitFiniteLanguage {
    fn mine_trace_model_tree(&self) -> ProcessTree {
        if self.number_of_traces().is_zero() {
            //empty language; empty tree
            (self.activity_key().clone(), vec![]).into()
        } else {
            let mut tree = vec![TreeNode::Operator(
                TreeOperator::Xor,
                self.number_of_traces(),
            )];

            for trace in self.iter_traces() {
                if trace.len().is_zero() {
                    //empty trace
                    tree.push(TreeNode::Tau);
                } else {
                    //non-empty trace
                    tree.push(TreeNode::Operator(TreeOperator::Sequence, trace.len()));
                    tree.append(
                        &mut trace
                            .iter()
                            .map(|a| TreeNode::Activity(*a))
                            .collect::<Vec<_>>(),
                    );
                }
            }

            (self.activity_key().clone(), tree).into()
        }
    }
}
