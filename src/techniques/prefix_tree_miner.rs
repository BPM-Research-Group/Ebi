use crate::{
    ebi_objects::{
        deterministic_finite_automaton::DeterministicFiniteAutomaton,
        process_tree::{Node, Operator, ProcessTree},
    },
    ebi_traits::{
        ebi_trait_finite_language::EbiTraitFiniteLanguage, ebi_trait_semantics::Semantics,
    },
    math::traits::Zero,
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
        result.set_activity_key(self.get_activity_key().clone());

        if self.len().is_zero() {
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
        if self.len().is_zero() {
            //empty language; empty tree
            return (self.get_activity_key().clone(), vec![]).into();
        } else {
            let mut tree = vec![Node::Operator(Operator::Xor, self.len())];

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

            return (self.get_activity_key().clone(), tree).into();
        }
    }
}
