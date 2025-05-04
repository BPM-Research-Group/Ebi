use std::{fmt::Display, io::BufRead};

use crate::{
    ebi_framework::{
        activity_key::ActivityKey,
        ebi_file_handler::EbiFileHandler,
        ebi_input::{self, EbiInput, EbiObjectImporter, EbiTraitImporter},
        ebi_object::EbiObject,
        ebi_output::{EbiObjectExporter, EbiOutput},
        ebi_trait::FromEbiTraitObject,
        exportable::Exportable,
        importable::Importable,
    },
    ebi_traits::{
        ebi_trait_graphable::{self, EbiTraitGraphable},
        ebi_trait_queriable_stochastic_language,
        ebi_trait_semantics::{Semantics, ToSemantics},
        ebi_trait_stochastic_deterministic_semantics::{
            EbiTraitStochasticDeterministicSemantics, ToStochasticDeterministicSemantics,
        },
        ebi_trait_stochastic_semantics::{EbiTraitStochasticSemantics, ToStochasticSemantics},
    },
    line_reader::LineReader,
    math::{fraction::Fraction, traits::Signed},
};

use anyhow::{Context, Result, anyhow};
use layout::{adt::dag::NodeHandle, topo::layout::VisualGraph};

use super::process_tree::{Node, Operator, ProcessTree};

pub const HEADER: &str = "stochastic process tree";

pub const FORMAT_SPECIFICATION: &str = "A stochastic process tree is a line-based structure. Lines starting with a \\# are ignored.
    This first line is exactly `stochastic process tree'.
    The subsequent lines contain the nodes:
    Each node is either:
    \\begin{itemize}
        \\item A line with the word `activity' followed on the same line by a space and the label of the activity leaf. The next line contains the weight of the activity;
        \\item The word `tau', followed on the next line by the weight of the leaf;
        \\item The name of an operator (`sequence', `xor', `concurrent', `loop', `interleaved', or `or') on its own line.
        The line thereafter contains the number of children of the node, after which the nodes are given.
        An operator node must have at least one child.
    \\end{itemize}
    Indentation of nodes is allowed, but not mandatory.\\
    The last line of the file contains the weight of termination.
    
    For instance:
    \\lstinputlisting[language=ebilines, style=boxed]{../testfiles/all_operators.sptree}";

pub const EBI_STOCHASTIC_PROCESS_TREE: EbiFileHandler = EbiFileHandler {
    name: "stochastic process tree",
    article: "a",
    file_extension: "sptree",
    format_specification: &FORMAT_SPECIFICATION,
    validator: ebi_input::validate::<StochasticProcessTree>,
    trait_importers: &[
        EbiTraitImporter::QueriableStochasticLanguage(
            ebi_trait_queriable_stochastic_language::import::<StochasticProcessTree>,
        ),
        EbiTraitImporter::StochasticDeterministicSemantics(
            StochasticProcessTree::import_as_stochastic_deterministic_semantics,
        ),
        EbiTraitImporter::Semantics(StochasticProcessTree::import_as_semantics),
        EbiTraitImporter::StochasticSemantics(
            StochasticProcessTree::import_as_stochastic_semantics,
        ),
        EbiTraitImporter::Graphable(ebi_trait_graphable::import::<StochasticProcessTree>),
    ],
    object_importers: &[
        EbiObjectImporter::StochasticProcessTree(StochasticProcessTree::import_as_object),
        EbiObjectImporter::ProcessTree(StochasticProcessTree::import_as_process_tree),
        EbiObjectImporter::LabelledPetriNet(StochasticProcessTree::import_as_labelled_petri_net),
    ],
    object_exporters: &[EbiObjectExporter::StochasticProcessTree(
        StochasticProcessTree::export_from_object,
    )],
    java_object_handlers: &[],
};

#[derive(Debug, ActivityKey, Clone)]
pub struct StochasticProcessTree {
    pub(crate) activity_key: ActivityKey,
    pub(crate) tree: Vec<Node>,
    pub(crate) transition2node: Vec<usize>,
    pub(crate) weights: Vec<Fraction>, //weights must be strictly positive; no deadlocks or livelocks in trees. Index are transitions, not nodes.
    pub(crate) termination_weight: Fraction,
}

impl StochasticProcessTree {
    pub(crate) fn node_to_string(
        &self,
        indent: usize,
        node: usize,
        f: &mut std::fmt::Formatter<'_>,
    ) -> Result<usize> {
        let id = "\t".repeat(indent);
        match &self.tree[node] {
            Node::Tau => {
                writeln!(
                    f,
                    "{}tau\n{}# weight node {}\n{}{}",
                    id,
                    id,
                    node,
                    id,
                    self.weights[self.node_to_transition(node).unwrap()]
                )?;
                Ok(node + 1)
            }
            Node::Activity(activity) => {
                writeln!(
                    f,
                    "{}activity {}\n{}# weight node {}\n{}{}",
                    id,
                    self.activity_key.get_activity_label(&activity),
                    id,
                    node,
                    id,
                    self.weights[self.node_to_transition(node).unwrap()]
                )?;
                Ok(node + 1)
            }
            Node::Operator(operator, number_of_children) => {
                writeln!(f, "{}{}", id, operator.to_string())?;
                writeln!(
                    f,
                    "{}# number of children\n{}{}",
                    id, id, number_of_children
                )?;
                let mut child = node + 1;
                for _ in 0..*number_of_children {
                    child = self.node_to_string(indent + 1, child, f)?;
                }
                Ok(child)
            }
        }
    }

    ///read one node, recursively
    fn string_to_tree(
        lreader: &mut LineReader<'_>,
        tree: &mut Vec<Node>,
        weights: &mut Vec<Fraction>,
        activity_key: &mut ActivityKey,
        root: bool,
    ) -> Result<()> {
        let node_type_line = match lreader.next_line_string().with_context(|| {
            format!(
                "Failed to read node {} at line {}",
                tree.len(),
                lreader.get_last_line_number()
            )
        }) {
            Ok(x) => x,
            Err(e) => {
                if root {
                    //The root may be missing: then, we have an empty tree.
                    return Ok(());
                } else {
                    return Err(e);
                }
            }
        };

        if node_type_line.trim_start().starts_with("tau") {
            let weight = lreader.next_line_weight().with_context(|| {
                format!(
                    "failed to read weight of node {} at line {}",
                    tree.len(),
                    lreader.get_last_line_number()
                )
            })?;
            if !weight.is_positive() {
                return Err(anyhow!(
                    "weight of node {} at line {} is not positive",
                    tree.len(),
                    lreader.get_last_line_number()
                ));
            }
            weights.push(weight);
            tree.push(Node::Tau);
        } else if node_type_line.trim_start().starts_with("activity ") {
            let label = node_type_line.trim_start()[9..].to_string();
            let activity = activity_key.process_activity(&label);

            let weight = lreader.next_line_weight().with_context(|| {
                format!(
                    "failed to read weight of node {} at line {}",
                    tree.len(),
                    lreader.get_last_line_number()
                )
            })?;
            if !weight.is_positive() {
                return Err(anyhow!(
                    "weight of node {} at line {} is not positive",
                    tree.len(),
                    lreader.get_last_line_number()
                ));
            }
            weights.push(weight);

            tree.push(Node::Activity(activity));
        } else if let Ok(operator) = node_type_line.trim_start().trim_end().parse::<Operator>() {
            let number_of_children = lreader.next_line_index().with_context(|| {
                format!(
                    "failed to read number of children for node {} at line {}",
                    tree.len(),
                    lreader.get_last_line_number()
                )
            })?;
            if number_of_children < 1 {
                return Err(anyhow!(
                    "loop node ending at node {} at line {} has no children",
                    tree.len(),
                    lreader.get_last_line_number()
                ));
            }
            tree.push(Node::Operator(operator, number_of_children));
            for _ in 0..number_of_children {
                Self::string_to_tree(lreader, tree, weights, activity_key, false)?;
            }
        } else if root && node_type_line.trim_start().is_empty() {
            //empty tree
            return Ok(());
        } else {
            return Err(anyhow!(
                "could not parse type of node {} at line {}; Expected `tau`, `activity`, `concurrent`, `interleaved`, `or`, `sequence` or `xor`",
                tree.len(),
                lreader.get_last_line_number()
            ));
        }

        Ok(())
    }

    pub(crate) fn node_to_dot(
        &self,
        graph: &mut VisualGraph,
        node: usize,
        entry: &NodeHandle,
        exit: &NodeHandle,
    ) -> usize {
        match self.tree[node] {
            Node::Tau => {
                <dyn EbiTraitGraphable>::create_edge(graph, entry, exit, "");
                node + 1
            }
            Node::Activity(activity) => {
                let transition = <dyn EbiTraitGraphable>::create_transition(
                    graph,
                    self.activity_key.get_activity_label(&activity),
                    "",
                );
                <dyn EbiTraitGraphable>::create_edge(graph, entry, &transition, "");
                <dyn EbiTraitGraphable>::create_edge(graph, &transition, exit, "");
                node + 1
            }
            Node::Operator(Operator::Xor, number_of_children) => {
                let mut child = node + 1;
                for _ in 0..number_of_children {
                    child = StochasticProcessTree::node_to_dot(&self, graph, child, entry, exit);
                }
                child
            }
            Node::Operator(Operator::Sequence, number_of_children) => {
                let intermediate_nodes = (0..(number_of_children - 1))
                    .map(|_| <dyn EbiTraitGraphable>::create_dot(graph))
                    .collect::<Vec<_>>();

                let mut child = node + 1;
                for i in 0..number_of_children {
                    let child_entry = if i == 0 {
                        entry
                    } else {
                        &intermediate_nodes[i - 1]
                    };
                    let child_exit = if i == number_of_children - 1 {
                        exit
                    } else {
                        &intermediate_nodes[i]
                    };

                    child = StochasticProcessTree::node_to_dot(
                        &self,
                        graph,
                        child,
                        child_entry,
                        child_exit,
                    );
                }
                child
            }
            Node::Operator(Operator::Concurrent, number_of_children) => {
                let split = <dyn EbiTraitGraphable>::create_gateway(graph, "+");
                <dyn EbiTraitGraphable>::create_edge(graph, entry, &split, "");
                let join = <dyn EbiTraitGraphable>::create_gateway(graph, "+");
                <dyn EbiTraitGraphable>::create_edge(graph, &join, exit, "");

                let mut child = node + 1;
                for _ in 0..number_of_children {
                    child = StochasticProcessTree::node_to_dot(&self, graph, child, &split, &join);
                }
                child
            }
            Node::Operator(Operator::Or, number_of_children) => {
                let split = <dyn EbiTraitGraphable>::create_gateway(graph, "o");
                <dyn EbiTraitGraphable>::create_edge(graph, entry, &split, "");
                let join = <dyn EbiTraitGraphable>::create_gateway(graph, "o");
                <dyn EbiTraitGraphable>::create_edge(graph, &join, exit, "");

                let mut child = node + 1;
                for _ in 0..number_of_children {
                    child = StochasticProcessTree::node_to_dot(&self, graph, child, &split, &join);
                }
                child
            }
            Node::Operator(Operator::Interleaved, number_of_children) => {
                let split = <dyn EbiTraitGraphable>::create_gateway(graph, "↔");
                <dyn EbiTraitGraphable>::create_edge(graph, entry, &split, "");
                let join = <dyn EbiTraitGraphable>::create_gateway(graph, "↔");
                <dyn EbiTraitGraphable>::create_edge(graph, &join, exit, "");

                let mut child = node + 1;
                for _ in 0..number_of_children {
                    child = StochasticProcessTree::node_to_dot(&self, graph, child, &split, &join);
                }
                child
            }
            Node::Operator(Operator::Loop, number_of_children) => {
                let split = <dyn EbiTraitGraphable>::create_dot(graph);
                <dyn EbiTraitGraphable>::create_edge(graph, entry, &split, "");
                let join = <dyn EbiTraitGraphable>::create_dot(graph);
                <dyn EbiTraitGraphable>::create_edge(graph, &join, exit, "");

                let mut child = node + 1;

                child = StochasticProcessTree::node_to_dot(&self, graph, child, &split, &join);

                if number_of_children == 1 {
                    <dyn EbiTraitGraphable>::create_edge(graph, &join, &split, "");
                } else {
                    for _ in 1..number_of_children {
                        child =
                            StochasticProcessTree::node_to_dot(&self, graph, child, &join, &split);
                    }
                }
                child
            }
        }
    }

    pub fn import_as_process_tree(reader: &mut dyn BufRead) -> Result<EbiObject> {
        let tree = Self::import(reader)?;
        Ok(EbiObject::ProcessTree(tree.into()))
    }
}

impl From<(ActivityKey, Vec<Node>, Vec<Fraction>, Fraction)> for StochasticProcessTree {
    fn from(value: (ActivityKey, Vec<Node>, Vec<Fraction>, Fraction)) -> Self {
        let (activity_key, tree, weights, termination_weight) = value;

        let mut transition2node = vec![];
        for (node_index, node) in tree.iter().enumerate() {
            match node {
                Node::Tau | Node::Activity(_) => {
                    transition2node.push(node_index);
                }
                Node::Operator(_, _) => {}
            }
        }

        Self {
            activity_key: activity_key,
            tree: tree,
            transition2node: transition2node,
            weights: weights,
            termination_weight: termination_weight,
        }
    }
}

impl Display for StochasticProcessTree {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        writeln!(f, "{}", HEADER)?;
        if !self.tree.is_empty() {
            let _ = self.node_to_string(0, 0, f);
        };
        writeln!(f, "# termination weight\n{}", self.termination_weight)
    }
}

impl FromEbiTraitObject for StochasticProcessTree {
    fn from_trait_object(object: EbiInput) -> Result<Box<Self>> {
        match object {
            EbiInput::Object(EbiObject::StochasticProcessTree(e), _) => Ok(Box::new(e)),
            _ => Err(anyhow!(
                "cannot read {} {} as a stochastic process tree",
                object.get_type().get_article(),
                object.get_type()
            )),
        }
    }
}

impl Importable for StochasticProcessTree {
    fn import_as_object(reader: &mut dyn std::io::BufRead) -> Result<EbiObject> {
        Ok(EbiObject::StochasticProcessTree(Self::import(reader)?))
    }

    fn import(reader: &mut dyn std::io::BufRead) -> Result<Self>
    where
        Self: Sized,
    {
        let mut lreader = LineReader::new(reader);

        let head = lreader
            .next_line_string()
            .with_context(|| format!("failed to read header, which should be {}", HEADER))?;
        if head != HEADER {
            return Err(anyhow!(
                "first line should be exactly `{}`, but found `{}` on line `{}`",
                HEADER,
                lreader.get_last_line(),
                lreader.get_last_line_number()
            ));
        }

        let mut activity_key = ActivityKey::new();
        let mut tree = vec![];
        let mut weights = vec![];
        Self::string_to_tree(
            &mut lreader,
            &mut tree,
            &mut weights,
            &mut activity_key,
            true,
        )?;

        let termination_weight = lreader
            .next_line_weight()
            .with_context(|| format!("could not read termination weight at end of file"))?;
        if !termination_weight.is_positive() {
            return Err(anyhow!(
                "termination weight ({}) is not positive",
                termination_weight
            ));
        }

        Ok(StochasticProcessTree::from((
            activity_key,
            tree,
            weights,
            termination_weight,
        )))
    }
}

impl Exportable for StochasticProcessTree {
    fn export_from_object(object: EbiOutput, f: &mut dyn std::io::Write) -> Result<()> {
        match object {
            EbiOutput::Object(EbiObject::StochasticProcessTree(lpn)) => lpn.export(f),
            _ => unreachable!(),
        }
    }

    fn export(&self, f: &mut dyn std::io::Write) -> Result<()> {
        Ok(write!(f, "{}", self)?)
    }
}

impl ToStochasticSemantics for StochasticProcessTree {
    fn to_stochastic_semantics(self) -> EbiTraitStochasticSemantics {
        EbiTraitStochasticSemantics::NodeStates(Box::new(self))
    }
}

impl ToStochasticDeterministicSemantics for StochasticProcessTree {
    fn to_stochastic_deterministic_semantics(self) -> EbiTraitStochasticDeterministicSemantics {
        EbiTraitStochasticDeterministicSemantics::NodeStatesDistribution(Box::new(self))
    }
}

impl From<(ProcessTree, Vec<Fraction>, Fraction)> for StochasticProcessTree {
    fn from(value: (ProcessTree, Vec<Fraction>, Fraction)) -> Self {
        assert_eq!(value.0.get_number_of_transitions() - 1, value.1.len());
        Self {
            activity_key: value.0.activity_key,
            tree: value.0.tree,
            transition2node: value.0.transition2node,
            termination_weight: value.2,
            weights: value.1,
        }
    }
}
