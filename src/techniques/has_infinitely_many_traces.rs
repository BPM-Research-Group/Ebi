use crate::{
    ebi_framework::displayable::Displayable,
    ebi_objects::{
        deterministic_finite_automaton::DeterministicFiniteAutomaton,
        directly_follows_model::DirectlyFollowsModel,
        process_tree::{Node, Operator, ProcessTree},
        process_tree_semantics::NodeStates,
    },
};
use anyhow::Result;

pub trait HasInfinitelyManyTraces {
    type LivState: Displayable;

    fn has_infinitely_many_traces(&self) -> Result<bool>;
}

impl HasInfinitelyManyTraces for ProcessTree {
    type LivState = NodeStates;

    fn has_infinitely_many_traces(&self) -> Result<bool> {
        for node in 0..self.get_number_of_nodes() {
            if let Some(Node::Operator(Operator::Loop, _)) = self.get_node(node) {
                //see whether at least one leaf in the loop is an activity
                for child in self.get_descendants(node) {
                    if let Node::Activity(_) = child {
                        return Ok(true);
                    }
                }
            }
        }
        return Ok(false);
    }
}

impl HasInfinitelyManyTraces for DeterministicFiniteAutomaton {
    type LivState = usize;

    fn has_infinitely_many_traces(&self) -> Result<bool> {
        todo!()
    }
}
