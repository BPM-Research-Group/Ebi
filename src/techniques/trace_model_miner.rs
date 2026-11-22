use ebi_objects::{
    ebi_arithmetic::Zero,
    ebi_objects::process_tree::{Node, Operator},
    ProcessTree,
};

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
            let mut tree = vec![Node::Operator(Operator::Xor, self.number_of_traces())];

            for trace in self.iter_traces() {
                if trace.len().is_zero() {
                    //empty trace
                    tree.push(Node::Tau);
                } else {
                    //non-empty trace
                    tree.push(Node::Operator(Operator::Sequence, trace.len()));
                    tree.append(&mut trace.iter().map(|a| Node::Activity(*a)).collect::<Vec<_>>());
                }
            }

            (self.activity_key().clone(), tree).into()
        }
    }
}
