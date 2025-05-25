use std::collections::HashSet;

use anyhow::{Result, anyhow};

use crate::{
    ebi_objects::{
        deterministic_finite_automaton::DeterministicFiniteAutomaton,
        process_tree::{Node, Operator, ProcessTree},
    },
    ebi_traits::{
        ebi_trait_finite_language::EbiTraitFiniteLanguage, ebi_trait_semantics::Semantics,
    },
};

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

        (self.get_activity_key().clone(), tree).into()
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
        result.set_activity_key(self.get_activity_key().clone());
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
