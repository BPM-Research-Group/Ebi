use std::{
    fmt::Display,
    io::{self, BufRead},
    str::FromStr,
};

use anyhow::{Context, Error, Result, anyhow};
use layout::{adt::dag::NodeHandle, topo::layout::VisualGraph};
use strum::IntoEnumIterator;
use strum_macros::EnumIter;

use crate::{
    ebi_framework::{
        activity_key::{
            Activity, ActivityKey, ActivityKeyTranslator, HasActivityKey, TranslateActivityKey,
        },
        ebi_file_handler::EbiFileHandler,
        ebi_input::{self, EbiInput, EbiObjectImporter, EbiTraitImporter},
        ebi_object::EbiObject,
        ebi_output::{EbiObjectExporter, EbiOutput},
        ebi_trait::FromEbiTraitObject,
        exportable::Exportable,
        importable::Importable,
        infoable::Infoable,
        prom_link::JavaObjectHandler,
    },
    ebi_traits::{
        ebi_trait_graphable::{self, EbiTraitGraphable},
        ebi_trait_semantics::{EbiTraitSemantics, ToSemantics},
        ebi_trait_stochastic_semantics::TransitionIndex,
    },
    line_reader::LineReader,
};

use super::stochastic_process_tree::StochasticProcessTree;

pub const HEADER: &str = "process tree";

pub const FORMAT_SPECIFICATION: &str = "A process tree is a line-based structure. Lines starting with a \\# are ignored.
    This first line is exactly `process tree'.
    The subsequent lines contain the nodes:
    Each node is either:
    \\begin{itemize}
        \\item A line with the word `activity' followed on the same line by a space and the label of the activity leaf;
        \\item The word `tau';
        \\item The name of an operator (`sequence', `xor', `concurrent', `loop', `interleaved', or `or') on its own line.
        The line thereafter contains the number of children of the node, after which the nodes are given.
        An operator node must have at least one child.
    \\end{itemize}
    Indentation of nodes is allowed, but not mandatory.
    
    For instance:
    \\lstinputlisting[language=ebilines, style=boxed]{../testfiles/all_operators.ptree}";

pub const EBI_PROCESS_TREE: EbiFileHandler = EbiFileHandler {
    name: "process tree",
    article: "a",
    file_extension: "ptree",
    format_specification: &FORMAT_SPECIFICATION,
    validator: ebi_input::validate::<ProcessTree>,
    trait_importers: &[
        EbiTraitImporter::Semantics(ProcessTree::import_as_semantics),
        EbiTraitImporter::Graphable(ebi_trait_graphable::import::<ProcessTree>),
    ],
    object_importers: &[
        EbiObjectImporter::ProcessTree(ProcessTree::import_as_object),
        EbiObjectImporter::LabelledPetriNet(ProcessTree::import_as_labelled_petri_net),
    ],
    object_exporters: &[EbiObjectExporter::ProcessTree(
        ProcessTree::export_from_object,
    )],
    java_object_handlers: &[JavaObjectHandler {
        name: "process tree",
        java_class: "org.processmining.plugins.InductiveMiner.efficienttree.EfficientTree",
        translator_ebi_to_java: Some(
            "org.processmining.ebi.objects.EbiProcessTree.EbiString2EfficientTree",
        ),
        translator_java_to_ebi: Some(
            "org.processmining.ebi.objects.EbiProcessTree.EfficientTree2EbiString",
        ),
        input_gui: None,
    }],
};

#[derive(Debug, ActivityKey, Clone)]
pub struct ProcessTree {
    pub(crate) activity_key: ActivityKey,
    pub(crate) tree: Vec<Node>,
    pub(crate) transition2node: Vec<usize>,
}

impl ProcessTree {
    fn node_to_string(
        &self,
        indent: usize,
        node: usize,
        f: &mut std::fmt::Formatter<'_>,
    ) -> Result<usize> {
        let id = "\t".repeat(indent);
        match &self.tree[node] {
            Node::Tau => {
                writeln!(f, "{}tau", id)?;
                Ok(node + 1)
            }
            Node::Activity(activity) => {
                writeln!(
                    f,
                    "{}activity {}",
                    id,
                    self.activity_key.get_activity_label(&activity)
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

    fn node_to_dot(
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
                    child = ProcessTree::node_to_dot(&self, graph, child, entry, exit);
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

                    child = ProcessTree::node_to_dot(&self, graph, child, child_entry, child_exit);
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
                    child = ProcessTree::node_to_dot(&self, graph, child, &split, &join);
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
                    child = ProcessTree::node_to_dot(&self, graph, child, &split, &join);
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
                    child = ProcessTree::node_to_dot(&self, graph, child, &split, &join);
                }
                child
            }
            Node::Operator(Operator::Loop, number_of_children) => {
                let split = <dyn EbiTraitGraphable>::create_dot(graph);
                <dyn EbiTraitGraphable>::create_edge(graph, entry, &split, "");
                let join = <dyn EbiTraitGraphable>::create_dot(graph);
                <dyn EbiTraitGraphable>::create_edge(graph, &join, exit, "");

                let mut child = node + 1;

                child = ProcessTree::node_to_dot(&self, graph, child, &split, &join);

                if number_of_children == 1 {
                    <dyn EbiTraitGraphable>::create_edge(graph, &join, &split, "");
                } else {
                    for _ in 1..number_of_children {
                        child = ProcessTree::node_to_dot(&self, graph, child, &join, &split);
                    }
                }
                child
            }
        }
    }

    ///read one node, recursively
    fn string_to_tree(
        lreader: &mut LineReader<'_>,
        tree: &mut Vec<Node>,
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
            tree.push(Node::Tau);
        } else if node_type_line.trim_start().starts_with("activity ") {
            let label = node_type_line.trim_start()[9..].to_string();
            let activity = activity_key.process_activity(&label);
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
                Self::string_to_tree(lreader, tree, activity_key, false)?;
            }
        } else if root && node_type_line.trim_start().is_empty() {
            //empty tree
            return Ok(());
        } else {
            return Err(anyhow!(
                "could not parse type of node {} at line {}. Expected `tau`, `activity`, `concurrent`, `interleaved`, `or`, `sequence` or `xor`",
                tree.len(),
                lreader.get_last_line_number()
            ));
        }

        Ok(())
    }
}

macro_rules! tree {
    ($t:ident, $u:ident, $v:ident) => {
        impl $t {
            pub fn get_number_of_nodes(&self) -> usize {
                return self.tree.len();
            }

            pub fn get_node(&self, node: usize) -> Option<&Node> {
                self.tree.get(node)
            }

            pub fn get_root(&self) -> usize {
                0
            }

            pub fn get_node_of_transition(&self, transition: TransitionIndex) -> Result<&Node> {
                self.tree
                    .get(
                        *self
                            .transition2node
                            .get(transition)
                            .ok_or_else(|| anyhow!("Transition does not exist."))?,
                    )
                    .ok_or_else(|| anyhow!("Node does not exist."))
            }

            /**
             * Returns the parent of node. Notice that
             * this is an expensive operation; avoid if possible.
             *
             * @param node
             * @return The parent of node, and the rank of the child
             */
            pub fn get_parent(&self, node: usize) -> Option<(usize, usize)> {
                if node == 0 {
                    return None;
                }

                let mut potential_parent = node - 1;
                while self.traverse(potential_parent) <= node {
                    potential_parent -= 1;
                }

                let child_rank = self.get_child_rank_with(potential_parent, node)?;

                Some((potential_parent, child_rank))
            }

            /**
             *
             * @param parent
             * @param grandChild
             * @return The number of the child within parent that contains grandChild.
             *         If grandChild is not a child of parent, will return -1.
             */
            pub fn get_child_rank_with(&self, parent: usize, grand_child: usize) -> Option<usize> {
                let mut child_rank = 0;
                for child in self.get_children(parent) {
                    if self.is_parent_of(child, grand_child) {
                        return Some(child_rank);
                    }
                    child_rank += 1;
                }
                None
            }

            pub fn get_children(&self, node: usize) -> $u {
                $u::new(self, node)
            }

            pub fn get_parents(&self, node: usize) -> $v {
                $v::new(self, node)
            }

            pub fn get_descendants(&self, node: usize) -> &[Node] {
                let next = self.traverse(node);
                &self.tree[node..next]
            }

            /**
             *
             * @param parent
             * @param child
             * @return Whether the child is a direct or indirect child of parent.
             */
            pub fn is_parent_of(&self, parent: usize, child: usize) -> bool {
                if parent > child {
                    return false;
                }
                return self.traverse(parent) > child;
            }

            /**
             * Find the next node of this node: the next sibling, and if that is not available, the next sibling up the tree.
             * May return a non-existing node if there is no sibling.
             */
            pub fn traverse(&self, node: usize) -> usize {
                match self.tree[node] {
                    Node::Tau => node + 1,
                    Node::Activity(_) => node + 1,
                    Node::Operator(_, number_of_children) => {
                        let mut n = node + 1;
                        for _ in 0..number_of_children {
                            n = self.traverse(n);
                        }
                        n
                    }
                }
            }

            pub fn get_child(&self, parent: usize, child_rank: usize) -> usize {
                let mut i = parent + 1;
                for _ in 0..child_rank {
                    i = self.traverse(i);
                }
                return i;
            }

            pub fn get_number_of_children(&self, parent: usize) -> Option<usize> {
                match self.tree.get(parent)? {
                    Node::Tau => Some(0),
                    Node::Activity(_) => Some(0),
                    Node::Operator(_, number_of_children) => Some(*number_of_children),
                }
            }

            pub fn node_to_transition(&self, node: usize) -> Option<usize> {
                let mut transitions = 0;
                let mut last = false;
                for node in self.tree.iter().take(node + 1) {
                    match node {
                        Node::Activity(_) | Node::Tau => {
                            transitions += 1;
                            last = true
                        }
                        _ => last = false,
                    }
                }

                if last { Some(transitions - 1) } else { None }
            }

            pub fn import_as_labelled_petri_net(reader: &mut dyn BufRead) -> Result<EbiObject> {
                let tree = Self::import(reader)?;
                Ok(EbiObject::LabelledPetriNet(tree.into()))
            }
        }

        impl TranslateActivityKey for $t {
            fn translate_using_activity_key(&mut self, to_activity_key: &mut ActivityKey) {
                let translator = ActivityKeyTranslator::new(&self.activity_key, to_activity_key);
                self.tree.iter_mut().for_each(|node| {
                    if let Node::Activity(a) = node {
                        *a = translator.translate_activity(&a)
                    }
                });
                self.activity_key = to_activity_key.clone();
            }
        }

        impl ToSemantics for $t {
            fn to_semantics(self) -> EbiTraitSemantics {
                EbiTraitSemantics::NodeStates(Box::new(self))
            }
        }

        impl Infoable for $t {
            fn info(&self, f: &mut impl std::io::Write) -> Result<()> {
                writeln!(f, "Number of nodes\t\t{}", self.get_number_of_nodes())?;
                writeln!(
                    f,
                    "Number of activities\t\t{}",
                    $t::get_activity_key(self).get_number_of_activities()
                )?;

                Ok(write!(f, "")?)
            }
        }

        impl EbiTraitGraphable for $t {
            fn to_dot(&self) -> Result<layout::topo::layout::VisualGraph> {
                let mut graph = VisualGraph::new(layout::core::base::Orientation::LeftToRight);
                let source = <dyn EbiTraitGraphable>::create_place(&mut graph, "");
                let sink = <dyn EbiTraitGraphable>::create_place(&mut graph, "");
                if !self.tree.is_empty() {
                    $t::node_to_dot(&self, &mut graph, 0, &source, &sink);
                }
                Ok(graph)
            }
        }

        impl FromStr for $t {
            type Err = Error;

            fn from_str(s: &str) -> std::prelude::v1::Result<Self, Self::Err> {
                let mut reader = io::Cursor::new(s);
                Self::import(&mut reader)
            }
        }

        pub struct $u<'a> {
            //children iterator
            tree: &'a $t,
            node: usize,
            now: Option<usize>,
            next: usize,
            count: usize,
        }

        impl<'a> $u<'a> {
            fn new(tree: &'a $t, node: usize) -> Self {
                Self {
                    tree: tree,
                    node: node,
                    now: None,
                    next: node + 1,
                    count: 0,
                }
            }
        }

        impl<'a> Iterator for $u<'a> {
            type Item = usize;

            fn next(&mut self) -> Option<Self::Item> {
                if self.count >= self.tree.get_number_of_children(self.node)? {
                    return None;
                }
                self.count += 1;
                self.now = Some(self.next);
                self.next = self.tree.traverse(self.now.unwrap());
                Some(self.now.unwrap())
            }
        }

        pub struct $v<'a> {
            //parents iterator
            tree: &'a $t,
            node: Option<(usize, usize)>,
        }

        impl<'a> $v<'a> {
            fn new(tree: &'a $t, node: usize) -> Self {
                Self {
                    tree: tree,
                    node: tree.get_parent(node),
                }
            }
        }

        impl<'a> Iterator for $v<'a> {
            type Item = (usize, usize);

            fn next(&mut self) -> Option<Self::Item> {
                if let Some((node, child_rank)) = self.node {
                    self.node = self.tree.get_parent(node);

                    Some((node, child_rank))
                } else {
                    None
                }
            }
        }
    };
}

tree!(ProcessTree, ChildrenIterator, ParentsIterator);
tree!(
    StochasticProcessTree,
    StochasticChildrenIterator,
    StochasticParentsIterator
);

impl From<(ActivityKey, Vec<Node>)> for ProcessTree {
    fn from(value: (ActivityKey, Vec<Node>)) -> Self {
        let mut transition2node = vec![];
        for (node_index, node) in value.1.iter().enumerate() {
            match node {
                Node::Tau | Node::Activity(_) => {
                    transition2node.push(node_index);
                }
                Node::Operator(_, _) => {}
            }
        }

        Self {
            activity_key: value.0,
            tree: value.1,
            transition2node: transition2node,
        }
    }
}

impl Display for ProcessTree {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        writeln!(f, "{}", HEADER)?;
        if !self.tree.is_empty() {
            let _ = self.node_to_string(0, 0, f);
        };
        write!(f, "")
    }
}

impl FromEbiTraitObject for ProcessTree {
    fn from_trait_object(object: EbiInput) -> Result<Box<Self>> {
        match object {
            EbiInput::Object(EbiObject::ProcessTree(e), _) => Ok(Box::new(e)),
            _ => Err(anyhow!(
                "cannot read {} {} as a process tree",
                object.get_type().get_article(),
                object.get_type()
            )),
        }
    }
}

impl Importable for ProcessTree {
    fn import_as_object(reader: &mut dyn std::io::BufRead) -> Result<EbiObject> {
        Ok(EbiObject::ProcessTree(Self::import(reader)?))
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
        Self::string_to_tree(&mut lreader, &mut tree, &mut activity_key, true)?;

        Ok((activity_key, tree).into())
    }
}

impl Exportable for ProcessTree {
    fn export_from_object(object: EbiOutput, f: &mut dyn std::io::Write) -> Result<()> {
        match object {
            EbiOutput::Object(EbiObject::ProcessTree(lpn)) => lpn.export(f),
            _ => unreachable!(),
        }
    }

    fn export(&self, f: &mut dyn std::io::Write) -> Result<()> {
        Ok(write!(f, "{}", self)?)
    }
}

#[derive(Debug, Clone)]
pub enum Node {
    Tau,
    Activity(Activity),
    Operator(Operator, usize), //type, number of children
}

impl Node {
    pub fn is_leaf(&self) -> bool {
        match self {
            Self::Tau | Self::Activity(_) => true,
            Self::Operator(_, _) => false,
        }
    }
}

#[derive(EnumIter, Debug, Clone, Copy)]
pub enum Operator {
    Xor,
    Sequence,
    Interleaved,
    Concurrent,
    Or,
    Loop,
}

impl Operator {
    pub fn to_string(&self) -> &str {
        match self {
            Operator::Xor => "xor",
            Operator::Sequence => "sequence",
            Operator::Interleaved => "interleaved",
            Operator::Concurrent => "concurrent",
            Operator::Or => "or",
            Operator::Loop => "loop",
        }
    }
}

impl FromStr for Operator {
    type Err = Error;

    fn from_str(s: &str) -> std::result::Result<Self, Self::Err> {
        for op in Operator::iter() {
            if s == op.to_string() {
                return Ok(op);
            }
        }
        return Err(anyhow!("operator not recognised"));
    }
}
