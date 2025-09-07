use ebi_arithmetic::ebi_number::Zero;
use ebi_objects::{
    DeterministicFiniteAutomaton, ProcessTree,
    ebi_objects::process_tree::{Node, Operator},
};

use crate::{
    ebi_traits::ebi_trait_finite_language::EbiTraitFiniteLanguage, semantics::semantics::Semantics,
};

pub trait PrefixTreeMinerDFA {
    fn mine_prefix_tree_dfa(&self) -> DeterministicFiniteAutomaton;
}

pub trait PrefixTreeMinerTree {
    fn mine_prefix_tree_tree(&self) -> ProcessTree;
}

impl PrefixTreeMinerDFA for dyn EbiTraitFiniteLanguage {
    fn mine_prefix_tree_dfa(&self) -> DeterministicFiniteAutomaton {
        let mut result = DeterministicFiniteAutomaton::new();
        result.set_activity_key(self.activity_key().clone());

        if self.number_of_traces().is_zero() {
            result.set_initial_state(None);
        } else {
            for trace in self.iter() {
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
            //empty language; empty tree
            return (self.activity_key().clone(), vec![]).into();
        } else {
            let mut tree = vec![Node::Operator(Operator::Xor, self.number_of_traces())];

            for trace in self.iter() {
                if trace.len().is_zero() {
                    //empty trace
                    tree.push(Node::Tau);
                } else {
                    //non-empty trace
                    tree.push(Node::Operator(Operator::Sequence, trace.len()));
                    tree.append(
                        &mut trace
                            .iter()
                            .map(|a| Node::Activity(a.clone()))
                            .collect::<Vec<_>>(),
                    );
                }
            }

            return (self.activity_key().clone(), tree).into();
        }
    }
}
