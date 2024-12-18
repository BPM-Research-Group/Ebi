use std::{fmt::Display, io::{self, BufRead}, str::FromStr};

use anyhow::{anyhow, Context, Error, Result};
use layout::{adt::dag::NodeHandle, topo::layout::VisualGraph};
use strum::IntoEnumIterator;
use strum_macros::EnumIter;

use crate::{ebi_framework::{activity_key::{Activity, ActivityKey, ActivityKeyTranslator, HasActivityKey}, ebi_file_handler::EbiFileHandler, ebi_input::{self, EbiInput, EbiObjectImporter, EbiTraitImporter}, ebi_object::EbiObject, ebi_output::{EbiObjectExporter, EbiOutput}, ebi_trait::FromEbiTraitObject, exportable::Exportable, importable::Importable, infoable::Infoable}, ebi_traits::{ebi_trait_graphable::{self, EbiTraitGraphable}, ebi_trait_semantics::{EbiTraitSemantics, ToSemantics}, ebi_trait_stochastic_semantics::TransitionIndex}, line_reader::LineReader};

use super::labelled_petri_net::LabelledPetriNet;

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
    \\lstinputlisting[language=ebilines, style=boxed]{../testfiles/all_operators.tree}";

pub const EBI_PROCESS_TREE: EbiFileHandler = EbiFileHandler {
    name: "process tree",
    article: "a",
    file_extension: "ptree",
    format_specification: &FORMAT_SPECIFICATION,
    validator: ebi_input::validate::<ProcessTree>,
    trait_importers: &[
        EbiTraitImporter::Semantics(ProcessTree::import_as_semantics),
        EbiTraitImporter::Graphable(ebi_trait_graphable::import::<ProcessTree>)
    ],
    object_importers: &[
        EbiObjectImporter::ProcessTree(ProcessTree::import_as_object),
        EbiObjectImporter::LabelledPetriNet(ProcessTree::import_as_labelled_petri_net)
    ],
    object_exporters: &[
        EbiObjectExporter::ProcessTree(ProcessTree::export_from_object),
    ],
    java_object_handlers: &[
        
    ],
};

#[derive(Debug,ActivityKey)]
pub struct ProcessTree {
    activity_key: ActivityKey,
    pub(crate) tree: Vec<Node>,
    pub(crate) transition2node: Vec<usize>,
}

impl ProcessTree {

    pub fn new(activity_key: ActivityKey, tree: Vec<Node>) -> Self {
        let mut transition2node = vec![];
        for (node_index, node) in tree.iter().enumerate() {
            match node {
                Node::Tau |  Node::Activity(_) => {
                    transition2node.push(node_index);
                },
                Node::Operator(_, _) => {},
            }
        }

        Self {
            activity_key: activity_key,
            tree: tree,
            transition2node: transition2node,
        }
    }

    pub fn import_as_labelled_petri_net(reader: &mut dyn BufRead) -> Result<EbiObject> {
        let dfm = Self::import(reader)?;
        Ok(EbiObject::LabelledPetriNet(dfm.get_labelled_petri_net()))
    }
    
    pub fn get_number_of_nodes(&self) -> usize {
        return self.tree.len()
    }

    pub fn get_node(&self, node: usize) -> Option<&Node> {
        self.tree.get(node)
    }

    pub fn get_root(&self) -> usize {
        0
    }

    pub fn get_node_of_transition(&self, transition: TransitionIndex) -> Result<&Node> {
        self.tree.get(*self.transition2node.get(transition).ok_or_else(|| anyhow!("Transition does not exist."))?).ok_or_else(|| anyhow!("Node does not exist."))
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

    pub fn get_children(&self, node: usize) -> ChildrenIterator {
        ChildrenIterator::new(self, node)
	}

    pub fn get_parents(&self, node: usize) -> ParentsIterator {
        ParentsIterator::new(self, node)
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
    pub fn traverse(&self, node: usize)  -> usize {
        match self.tree[node] {
            Node::Tau => node + 1,
            Node::Activity(_) => node + 1,
            Node::Operator(_, number_of_children) => {
                let mut n = node + 1;
                for _ in 0..number_of_children {
                    n = self.traverse(n);
                }
                n
            },
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

    fn node_to_string(&self, indent: usize, node: usize, f: &mut std::fmt::Formatter<'_>) -> Result<usize> {
        let id = "\t".repeat(indent);
        match &self.tree[node] {
            Node::Tau => {
                writeln!(f, "{}tau", id)?; 
                Ok(node + 1)
            },
            Node::Activity(activity) => {
                writeln!(f, "{}activity {}", id, self.activity_key.get_activity_label(&activity))?;
                Ok(node + 1)
            },
            Node::Operator(operator, number_of_children) => {
                writeln!(f, "{}{}", id, operator.to_string())?;
                writeln!(f, "# number of children\n{}", number_of_children)?;
                let mut child = node + 1;
                for _ in 0..*number_of_children {
                    child = self.node_to_string(indent + 1, child, f)?;
                }
                Ok(child)
            },
        }
    }

    fn node_to_dot(&self, graph: &mut VisualGraph, node: usize, entry: &NodeHandle, exit: &NodeHandle) -> usize {
        match self.tree[node] {
            Node::Tau => {
                <dyn EbiTraitGraphable>::create_edge(graph, entry, exit, "");
                node + 1
            },
            Node::Activity(activity) => {
                let transition = <dyn EbiTraitGraphable>::create_transition(graph, self.activity_key.get_activity_label(&activity), "");
                <dyn EbiTraitGraphable>::create_edge(graph, entry, &transition, "");
                <dyn EbiTraitGraphable>::create_edge(graph, &transition, exit, "");
                node + 1
            },
            Node::Operator(Operator::Xor, number_of_children) => {
                let mut child = node + 1;
                for _ in 0..number_of_children {
                    child = ProcessTree::node_to_dot(&self, graph, child, entry, exit);
                }
                child
            },
            Node::Operator(Operator::Sequence, number_of_children) => {
                let intermediate_nodes = (0..(number_of_children-1)).map(|_| <dyn EbiTraitGraphable>::create_dot(graph)).collect::<Vec<_>>();

                let mut child = node + 1;
                for i in 0..number_of_children {
                    let child_entry = if i == 0 { entry } else { &intermediate_nodes[i-1] };
                    let child_exit = if i == number_of_children - 1 { exit } else { &intermediate_nodes[i] };

                    child = ProcessTree::node_to_dot(&self, graph, child, child_entry, child_exit);
                }
                child
            },
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
            },
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
            },
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
            },
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
    fn string_to_tree(lreader: &mut LineReader<'_>, tree: &mut Vec<Node>, activity_key: &mut ActivityKey) -> Result<()> {
        let node_type_line = lreader.next_line_string().with_context(|| format!("failed to read node {} at line {}", tree.len(), lreader.get_last_line_number()))?;

        if node_type_line.trim_start().starts_with("tau") {
            tree.push(Node::Tau);
        } else if node_type_line.trim_start().starts_with("activity ") {
            let label = node_type_line.trim_start()[9..].to_string();
            let activity = activity_key.process_activity(&label);
            tree.push(Node::Activity(activity));
        } else if let Ok(operator) = node_type_line.trim_start().trim_end().parse::<Operator>() {
            let number_of_children = lreader.next_line_index().with_context(|| format!("failed to read number of children for node {} at line {}", tree.len(), lreader.get_last_line_number()))?;
            if number_of_children < 1 {
                return Err(anyhow!("loop node ending at node {} at line {} has no children", tree.len(), lreader.get_last_line_number()));
            }
            tree.push(Node::Operator(operator, number_of_children));
            for _ in 0..number_of_children {
                Self::string_to_tree(lreader, tree, activity_key)?;
            }
        } else {
            return Err(anyhow!("Could not parse type of node {} at line {}. Expected `tau`, `activity`, `concurrent`, `interleaved`, `or`, `sequence` or `xor`.", tree.len(), lreader.get_last_line_number()));
        }

        Ok(())
    }

    fn node_to_lpn(&self, node: usize, net: &mut LabelledPetriNet, translator: &ActivityKeyTranslator, source: usize, sink: usize) -> Result<usize> {
        match self.tree[node] {
            Node::Tau => {
                let transition = net.add_transition(None);
                net.add_place_transition_arc(source, transition, 1)?;
                net.add_transition_place_arc(transition, sink, 1)?;
                Ok(node + 1)
            },
            Node::Activity(activity) => {
                let transition = net.add_transition(Some(translator.translate_activity(&activity)));
                net.add_place_transition_arc(source, transition, 1)?;
                net.add_transition_place_arc(transition, sink, 1)?;
                Ok(node + 1)
            },
            Node::Operator(Operator::Concurrent, number_of_children) => {
                let split = net.add_transition(None);
                net.add_place_transition_arc(source, split, 1)?;
                let join = net.add_transition(None);
                net.add_transition_place_arc(join, sink, 1)?;

                let mut child = node + 1;
                for _ in 0..number_of_children {
                    let child_source = net.add_place();
                    net.add_transition_place_arc(split, child_source, 1)?;
                    let child_sink = net.add_place();
                    net.add_place_transition_arc(child_sink, join, 1)?;
                    child = self.node_to_lpn(child, net, translator, child_source, child_sink)?;
                }
                Ok(child)
            },
            Node::Operator(Operator::Interleaved, number_of_children) => {
                let split = net.add_transition(None);
                net.add_place_transition_arc(source, split, 1)?;
                let join = net.add_transition(None);
                net.add_transition_place_arc(join, sink, 1)?;
                let milestone = net.add_place();
                net.add_transition_place_arc(split, milestone, 1)?;
                net.add_place_transition_arc(milestone, join, 1)?;

                let mut child = node + 1;
                for _ in 0..number_of_children {
                    let child_source = net.add_place();
                    net.add_transition_place_arc(split, child_source, 1)?;

                    let child_start = net.add_transition(None);
                    net.add_place_transition_arc(child_source, child_start, 1)?;
                    net.add_place_transition_arc(milestone, child_start, 1)?;

                    let child_source_2 = net.add_place();
                    net.add_transition_place_arc(child_start, child_source_2, 1)?;

                    let child_sink = net.add_place();
                    net.add_place_transition_arc(child_sink, join, 1)?;

                    let child_stop = net.add_transition(None);
                    net.add_transition_place_arc(child_stop, child_sink, 1)?;
                    net.add_transition_place_arc(child_stop, milestone, 1)?;

                    let child_sink_2 = net.add_place();
                    net.add_place_transition_arc(child_sink_2, child_stop, 1)?;

                    child = self.node_to_lpn(child, net, translator, child_source_2, child_sink_2)?;
                }
                Ok(child)
            },
            Node::Operator(Operator::Loop, number_of_children) => {
                let start = net.add_transition(None);
                net.add_place_transition_arc(source, start, 1)?;

                let join = net.add_place();
                net.add_transition_place_arc(start, join, 1)?;

                let split = net.add_place();
                let stop = net.add_transition(None);
                net.add_place_transition_arc(split, stop, 1)?;
                net.add_transition_place_arc(stop, sink, 1)?;

                let mut child = node + 1;
                child = self.node_to_lpn(child, net, translator, join, split)?;

                if number_of_children > 1 {
                    for _ in 1..number_of_children {
                        child = self.node_to_lpn(child, net, translator, split, join)?;
                    }
                } else {
                    let redo = net.add_transition(None);
                    net.add_place_transition_arc(split, redo, 1)?;
                    net.add_transition_place_arc(redo, join, 1)?;
                }

                Ok(child)
            },
            Node::Operator(Operator::Or, number_of_children) => {
                let start = net.add_transition(None);
                net.add_place_transition_arc(source, start, 1)?;
        
                let not_done_first = net.add_place();
                net.add_transition_place_arc(start, not_done_first, 1)?;
        
                let done_first = net.add_place();
                let end = net.add_transition(None);
                net.add_place_transition_arc(done_first, end, 1)?;
                net.add_transition_place_arc(end, sink, 1)?;
        
                let mut child = node + 1;
                for _ in 0..number_of_children {
                    let child_source = net.add_place();
                    net.add_transition_place_arc(start, child_source, 1)?;
                    let child_sink = net.add_place();
                    net.add_place_transition_arc(child_sink, end, 1)?;
                    let do_child = net.add_place();
        
                    //skip
                    let skip_child = net.add_transition(None);
                    net.add_place_transition_arc(child_source, skip_child, 1)?;
                    net.add_transition_place_arc(skip_child, child_sink, 1)?;
                    net.add_transition_place_arc(skip_child, done_first, 1)?;
                    net.add_place_transition_arc(done_first, skip_child, 1)?;
        
                    //first do
                    let first_do_child = net.add_transition(None);
                    net.add_place_transition_arc(child_source, first_do_child, 1)?;
                    net.add_place_transition_arc(not_done_first, first_do_child, 1)?;
                    net.add_transition_place_arc(first_do_child, done_first, 1)?;
                    net.add_transition_place_arc(first_do_child, do_child, 1)?;
        
                    //later do
                    let later_do_child = net.add_transition(None);
                    net.add_place_transition_arc(child_source, later_do_child, 1)?;
                    net.add_transition_place_arc(later_do_child, do_child, 1)?;
                    net.add_transition_place_arc(later_do_child, done_first, 1)?;
                    net.add_place_transition_arc(done_first, later_do_child, 1)?;

                    child = self.node_to_lpn(child, net, translator, do_child, child_sink)?;
                }

                Ok(child)
            },
            Node::Operator(Operator::Sequence, number_of_children) => {
                let intermediate_nodes = (0..(number_of_children-1)).map(|_| net.add_place()).collect::<Vec<_>>();

                let mut child = node + 1;
                for i in 0..number_of_children {
                    let child_entry = if i == 0 { source } else { intermediate_nodes[i-1] };
                    let child_exit = if i == number_of_children - 1 { sink } else { intermediate_nodes[i] };

                    child = ProcessTree::node_to_lpn(&self, child, net, translator, child_entry, child_exit)?;
                }
                Ok(child)
            },
            Node::Operator(Operator::Xor, number_of_children) => {
                let mut child = node + 1;
                for _ in 0..number_of_children {
                    child = ProcessTree::node_to_lpn(&self, child, net, translator, source, sink)?;
                }
                Ok(child)
            }
        }
    }

    pub fn get_labelled_petri_net(&self) -> LabelledPetriNet {
        let mut result = LabelledPetriNet::new();
        let translator = ActivityKeyTranslator::new(&self.activity_key, result.get_activity_key_mut());
        let source = result.add_place();
        let sink = result.add_place();
        result.get_initial_marking_mut().increase(source, 1).unwrap();

        self.node_to_lpn(0, &mut result, &translator, source, sink).unwrap();

        result
    }
}

impl FromEbiTraitObject for ProcessTree {
    fn from_trait_object(object: EbiInput) -> Result<Box<Self>> {
        match object {
            EbiInput::Object(EbiObject::ProcessTree(e), _) => Ok(Box::new(e)),
            _ => Err(anyhow!("cannot read {} {} as a process tree", object.get_type().get_article(), object.get_type()))
        }
    }
}

impl Importable for ProcessTree {
    fn import_as_object(reader: &mut dyn std::io::BufRead) -> Result<EbiObject> {
        Ok(EbiObject::ProcessTree(Self::import(reader)?))
    }

    fn import(reader: &mut dyn std::io::BufRead) -> Result<Self> where Self: Sized {
        let mut lreader = LineReader::new(reader);

        let head = lreader.next_line_string().with_context(|| format!("failed to read header, which should be {}", HEADER))?;
        if head != HEADER {
            return Err(anyhow!("first line should be exactly `{}`, but found `{}` on line `{}`", HEADER, lreader.get_last_line(), lreader.get_last_line_number()));
        }

        let mut activity_key = ActivityKey::new();
        let mut tree = vec![];
        Self::string_to_tree(&mut lreader, &mut tree, &mut activity_key)?;
        
        Ok(ProcessTree::new(activity_key, tree))
    }
}

impl FromStr for ProcessTree {
    type Err = Error;

    fn from_str(s: &str) -> std::prelude::v1::Result<Self, Self::Err> {
        let mut reader = io::Cursor::new(s);
        Self::import(&mut reader)
    }
}

impl Exportable for ProcessTree {
    fn export_from_object(object: EbiOutput, f: &mut dyn std::io::Write) -> Result<()> {
        match object {
            EbiOutput::Object(EbiObject::ProcessTree(lpn)) => lpn.export(f),
            _ => unreachable!()
        }
    }

    fn export(&self, f: &mut dyn std::io::Write) -> Result<()> {
        Ok(write!(f, "{}", self)?)
    }
}

impl Display for ProcessTree {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        writeln!(f, "{}", HEADER)?;
        match self.node_to_string(0, 0, f) {
            Ok(_) => Ok(()),
            Err(_) => write!(f, ""),
        }
    }
}

impl Infoable for ProcessTree {
    fn info(&self, f: &mut impl std::io::Write) -> Result<()> {
        writeln!(f, "Number of nodes\t\t{}", self.get_number_of_nodes())?;
        writeln!(f, "Number of activities\t\t{}", ProcessTree::get_activity_key(self).get_number_of_activities())?;

        Ok(write!(f, "")?)
    }
}

impl EbiTraitGraphable for ProcessTree {
    fn to_dot(&self) -> layout::topo::layout::VisualGraph {
        let mut graph = VisualGraph::new(layout::core::base::Orientation::LeftToRight);
        let source = <dyn EbiTraitGraphable>::create_place(&mut graph, "");
        let sink = <dyn EbiTraitGraphable>::create_place(&mut graph, "");
        ProcessTree::node_to_dot(&self, &mut graph,0, &source, &sink);
        graph
    }
}

impl ToSemantics for ProcessTree {
    fn to_semantics(self) -> EbiTraitSemantics {
        EbiTraitSemantics::NodeStates(Box::new(self))
    }
}



#[derive(Debug,Clone)]
pub enum Node {
    Tau,
    Activity(Activity),
    Operator(Operator, usize) //type, number of children
}

impl Node {
    pub fn is_leaf(&self) -> bool {
        match self {
            Self::Tau | Self::Activity(_) => true,
            Self::Operator(_, _) => false
        }
    }
}

#[derive(EnumIter,Debug,Clone,Copy)]
pub enum Operator {
    Xor,
    Sequence,
    Interleaved,
    Concurrent,
    Or,
    Loop
}

impl Operator {
    fn to_string(&self) -> &str {
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

pub struct ChildrenIterator<'a> {
    tree: &'a ProcessTree,
    node: usize,
    now: Option<usize>,
    next: usize,
    count: usize
}

impl <'a> ChildrenIterator<'a> {
    fn new(tree: &'a ProcessTree, node: usize) -> Self {
        Self{
            tree: tree,
            node: node,
            now: None,
            next: node + 1,
            count: 0
        }
    }
}

impl <'a> Iterator for ChildrenIterator<'a> {
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

pub struct ParentsIterator<'a> {
    tree: &'a ProcessTree,
    node: Option<(usize, usize)>
}

impl <'a> ParentsIterator<'a> {
    fn new(tree: &'a ProcessTree, node: usize) -> Self {
        Self{
            tree: tree,
            node: tree.get_parent(node),
        }
    }
}

impl <'a> Iterator for ParentsIterator<'a> {
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