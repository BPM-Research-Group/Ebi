use crate::ebi_objects::{process_tree::ProcessTree, stochastic_process_tree::StochasticProcessTree};

impl From<StochasticProcessTree> for ProcessTree {
    fn from(value: StochasticProcessTree) -> Self {
        Self {
            activity_key: value.activity_key,
            tree: value.tree,
            transition2node: value.transition2node,
        }
    }
}