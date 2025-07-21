use crate::ebi_objects::{
    process_tree::ProcessTree, process_tree_markup_language::ProcessTreeMarkupLanguage,
    stochastic_process_tree::StochasticProcessTree,
};

impl From<ProcessTreeMarkupLanguage> for ProcessTree {
    fn from(value: ProcessTreeMarkupLanguage) -> Self {
        value.tree
    }
}

impl From<StochasticProcessTree> for ProcessTree {
    fn from(value: StochasticProcessTree) -> Self {
        Self {
            activity_key: value.activity_key,
            tree: value.tree,
            transition2node: value.transition2node,
        }
    }
}
