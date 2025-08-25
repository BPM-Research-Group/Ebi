use std::{
    collections::{HashMap, hash_map::Entry},
    fmt::Display,
    io::{self, BufRead, Write},
    str::FromStr,
};

use anyhow::{Context, Error, Result, anyhow};
use quick_xml::{
    Reader,
    events::{BytesEnd, BytesStart, Event},
};

use crate::{
    ebi_framework::{
        activity_key::{Activity, ActivityKey, HasActivityKey},
        ebi_file_handler::EbiFileHandler,
        ebi_input::{EbiObjectImporter, EbiTraitImporter},
        ebi_object::EbiObject,
        ebi_output::{EbiObjectExporter, EbiOutput},
        exportable::Exportable,
        importable::Importable,
    },
    ebi_objects::process_tree::{Node, Operator},
    ebi_traits::{
        ebi_trait_activities::EbiTraitActivities, ebi_trait_graphable::{self, EbiTraitGraphable}, ebi_trait_semantics::{EbiTraitSemantics, ToSemantics}
    },
};

use super::{labelled_petri_net::LabelledPetriNet, process_tree::ProcessTree};

pub const FORMAT_SPECIFICATION: &str = "A process tree markup language file.
For instance:
    \\lstinputlisting[language=xml, style=boxed]{../testfiles/aa-ab-ba.ptml}";

pub const EBI_PROCESS_TREE_MARKUP_LANGUAGE: EbiFileHandler = EbiFileHandler {
    name: "process tree markup language",
    article: "a",
    file_extension: "ptml",
    is_binary: false,
    format_specification: &FORMAT_SPECIFICATION,
    validator: Some(ProcessTreeMarkupLanguage::validate),
    trait_importers: &[
        EbiTraitImporter::Semantics(ProcessTreeMarkupLanguage::import_as_semantics),
        EbiTraitImporter::Graphable(ebi_trait_graphable::import::<ProcessTreeMarkupLanguage>),
    ],
    object_importers: &[
        EbiObjectImporter::ProcessTree(ProcessTreeMarkupLanguage::import_as_object),
        EbiObjectImporter::LabelledPetriNet(
            ProcessTreeMarkupLanguage::import_as_labelled_petri_net,
        ),
    ],
    object_exporters: &[
        EbiObjectExporter::ProcessTree(ProcessTreeMarkupLanguage::export_from_object),
        EbiObjectExporter::StochasticProcessTree(ProcessTreeMarkupLanguage::export_from_object),
    ],
    java_object_handlers: &[], //java object handlers are through processtree
};

#[derive(Clone)]
pub struct ProcessTreeMarkupLanguage {
    pub(crate) tree: ProcessTree,
}

impl ProcessTreeMarkupLanguage {
    /**
     * PTML is always translated to a process tree in Ebi. Therefore, validate that step as well.
     */
    pub fn validate(reader: &mut dyn BufRead) -> Result<()> {
        let ptml = Self::import(reader)?;
        ProcessTree::try_from(ptml)?;
        Ok(())
    }

    pub fn import_as_labelled_petri_net(reader: &mut dyn BufRead) -> Result<EbiObject> {
        let ptml = Self::import(reader)?;
        Ok(EbiObject::LabelledPetriNet(ptml.into()))
    }

    pub fn import_as_activities(reader: &mut dyn BufRead) -> Result<Box<dyn EbiTraitActivities>> {
        let ptml = Self::import(reader)?;
        let ptree: ProcessTree = ptml.into();
        Ok(Box::new(ptree))
    }
}

struct State {
    in_tags: Vec<Vec<u8>>,
    open_ptml_tags: usize,
    open_process_tree_tags: usize,
    first_open_tag_is_ptml: bool,
    second_open_tag_is_process_tree: bool,
    seen_ptml_tag: bool,
    seen_process_tree_tag: bool,
    activity_key: ActivityKey,
    root: Option<Vec<u8>>,
    nodes: HashMap<Vec<u8>, Node>,
    edges: HashMap<Vec<u8>, Vec<Vec<u8>>>,
}

impl State {
    fn new() -> Self {
        Self {
            in_tags: vec![],
            open_ptml_tags: 0,
            open_process_tree_tags: 0,
            first_open_tag_is_ptml: false,
            second_open_tag_is_process_tree: false,
            seen_ptml_tag: false,
            seen_process_tree_tag: false,
            activity_key: ActivityKey::new(),
            root: None,
            nodes: HashMap::new(),
            edges: HashMap::new(),
        }
    }

    fn empty_tag(&mut self, e: BytesStart) -> Result<()> {
        self.open_tag(e.clone())?;
        self.close_tag(e.to_end())
    }

    fn open_tag(&mut self, e: BytesStart) -> Result<()> {
        match e.name().as_ref() {
            b"ptml" => {
                if self.in_tags.is_empty() {
                    self.first_open_tag_is_ptml = true;

                    //check for second ptml
                    if self.seen_ptml_tag {
                        return Err(anyhow!("found a second `ptml` tag, which is not allowed"));
                    } else {
                        self.seen_ptml_tag = true;
                    }
                }
                self.open_ptml_tags += 1
            }
            b"processTree" => {
                if self.in_tags.len() == 1 {
                    self.second_open_tag_is_process_tree = true;

                    //check for second tree
                    if self.seen_process_tree_tag {
                        return Err(anyhow!(
                            "found a second `processTree` tag, which is not allowed"
                        ));
                    } else {
                        self.seen_process_tree_tag = true;
                    }

                    //extract the root node
                    if let Ok(Some(attribute)) = e.try_get_attribute("root") {
                        self.root = Some(attribute.value.to_vec())
                    } else {
                        //the process tree may be empty, thus it is not necessary to declare a root node
                    }
                }
                self.open_process_tree_tags += 1
            }
            _ if self.can_node() => self.start_node(&e)?,
            _ => {}
        };

        self.in_tags.push(e.name().as_ref().to_owned());
        Ok(())
    }

    fn close_tag(&mut self, e: BytesEnd) -> Result<()> {
        if let Some(last_tag) = self.in_tags.pop() {
            if last_tag == e.name().as_ref() {
                match e.name().as_ref() {
                    b"ptml" => {
                        if self.in_tags.is_empty() {
                            self.first_open_tag_is_ptml = false;
                        }
                        self.open_ptml_tags -= 1
                    }
                    b"processTree" => {
                        if self.in_tags.len() == 1 {
                            self.second_open_tag_is_process_tree = false;
                        }
                        self.open_process_tree_tags -= 1
                    }
                    _ => {}
                }
                Ok(())
            } else {
                Err(anyhow!(
                    "attempted to close tag `{}` but `{}` was open",
                    String::from_utf8_lossy(e.name().as_ref()),
                    String::from_utf8_lossy(&last_tag)
                ))
            }
        } else {
            Err(anyhow!(
                "attempted to close tag `{}` that was not open",
                String::from_utf8_lossy(e.name().as_ref())
            ))
        }
    }

    fn can_node(&self) -> bool {
        self.first_open_tag_is_ptml
            && self.open_ptml_tags == 1
            && self.second_open_tag_is_process_tree
            && self.open_process_tree_tags == 1
    }

    /**
     * We are at a position to start a node, and we have found a tag
     */
    fn start_node(&mut self, e: &BytesStart) -> Result<()> {
        if let Some(tag) = PTMLTag::to_tag(e, &mut self.activity_key)? {
            //node
            if let Ok(Some(attribute)) = e.try_get_attribute("id") {
                let id = attribute.value;
                match self.nodes.entry(id.to_vec()) {
                    Entry::Occupied(_) => {
                        return Err(anyhow!(
                            "two nodes have the id `{}`",
                            String::from_utf8_lossy(&id)
                        ));
                    }
                    Entry::Vacant(vacant_entry) => {
                        vacant_entry.insert(tag.to_node());
                    }
                }
            } else {
                return Err(anyhow!(
                    "tree node `{}` does not have an `id` attribute",
                    tag
                ));
            }
        } else if e.name().as_ref() == b"parentsNode" {
            //edge
            let source = if let Ok(Some(attribute)) = e.try_get_attribute("sourceId") {
                attribute.value.to_vec()
            } else {
                return Err(anyhow!("a `parentsNode` tag has no `sourceId` attribute"));
            };
            let target = if let Ok(Some(attribute)) = e.try_get_attribute("targetId") {
                attribute.value.to_vec()
            } else {
                return Err(anyhow!("a `parentsNode` tag has no `targetId` attribute"));
            };

            match self.edges.entry(source) {
                Entry::Occupied(mut occupied_entry) => occupied_entry.get_mut().push(target),
                Entry::Vacant(vacant_entry) => {
                    vacant_entry.insert(vec![target]);
                }
            };
        }
        Ok(())
    }

    fn can_eof(&self) -> Result<()> {
        if let Some(tag) = self.in_tags.iter().next() {
            Err(anyhow!(
                "file ended while tag `{}` was still open",
                String::from_utf8_lossy(&tag)
            ))
        } else if !self.seen_ptml_tag {
            return Err(anyhow!("no `ptml` tag found at the top level"));
        } else if !self.seen_process_tree_tag {
            return Err(anyhow!(
                "no `processTree` tag found within the top-level `ptml` tag"
            ));
        } else {
            Ok(())
        }
    }

    fn to_tree(self) -> Result<ProcessTree> {
        if let Some(root) = self.root {
            let mut tree = vec![];
            let mut nodes: HashMap<Vec<u8>, Node> = self.nodes;
            let mut edges: HashMap<Vec<u8>, Vec<Vec<u8>>> = self.edges;

            Self::to_tree_node(&mut nodes, &mut edges, &mut tree, &root)?;

            //check that every node and edge has been used
            if let Some((node_id, _)) = nodes.into_iter().next() {
                return Err(anyhow!(
                    "node `{}` is not reachable from the root",
                    String::from_utf8_lossy(&node_id)
                ));
            }
            if let Some((source, targets)) = edges.into_iter().next() {
                return Err(anyhow!(
                    "the edge from `{}` to `{}` is not reachable from the root",
                    String::from_utf8_lossy(&source),
                    String::from_utf8_lossy(&targets.into_iter().next().unwrap())
                ));
            }

            Ok((self.activity_key, tree).into())
        } else {
            //safety: an absent root is allowed, but then there should not be any nodes
            if !self.nodes.is_empty() {
                return Err(anyhow!(
                    "no root was declared on the `ptml` tag, but there are nodes in the `ptml` tag"
                ));
            } else {
                return Ok(ProcessTree::from_str("process tree")?);
            }
        }
    }

    fn to_tree_node(
        nodes: &mut HashMap<Vec<u8>, Node>,
        edges: &mut HashMap<Vec<u8>, Vec<Vec<u8>>>,
        tree: &mut Vec<Node>,
        node_id: &[u8],
    ) -> Result<()> {
        log::debug!("tree {:?}", tree);
        log::debug!("nodes {:?}", nodes);
        log::debug!(
            "process node {:?}, {}",
            node_id,
            String::from_utf8_lossy(node_id)
        );

        match (nodes.remove(node_id), edges.remove(node_id)) {
            (Some(mut node), Some(children)) => {
                //node has edges -> operator
                if node.is_leaf() {
                    return Err(anyhow!("a leaf node cannot have children"));
                }

                if let Node::Operator(Operator::Loop, _) = node {
                    //loop nodes in PTML are 3-ary, whereas process trees have n-ary ones
                    //translate it to seq(loop(., .), .)
                    if children.len() != 3 {
                        return Err(anyhow!("a loop in PTML should have 3 children"));
                    }

                    tree.push(Node::Operator(Operator::Sequence, 2));
                    tree.push(Node::Operator(Operator::Loop, 2));
                    for child_id in children {
                        Self::to_tree_node(nodes, edges, tree, &child_id)?;
                    }
                } else {
                    node.set_number_of_children(children.len())
                        .with_context(|| {
                            format!("preparing node `{}`", String::from_utf8_lossy(node_id))
                        })?;
                    tree.push(node);

                    for child_id in children {
                        Self::to_tree_node(nodes, edges, tree, &child_id)?;
                    }
                }
            }
            (Some(node), None) => {
                //only a node, no edges -> leaf
                if !node.is_leaf() {
                    return Err(anyhow!(
                        "operator node `{}` has no children",
                        String::from_utf8_lossy(node_id)
                    ));
                }
                tree.push(node);
            }
            _ => {
                return Err(anyhow!(
                    "node `{}` is not declared, or it is a child of two parents",
                    String::from_utf8_lossy(node_id)
                ));
            }
        }

        Ok(())
    }
}

impl Importable for ProcessTreeMarkupLanguage {
    fn import_as_object(reader: &mut dyn BufRead) -> Result<EbiObject> {
        Ok(EbiObject::ProcessTree(Self::import(reader)?.into()))
    }

    fn import(reader: &mut dyn BufRead) -> Result<Self>
    where
        Self: Sized,
    {
        let mut xml_reader = Reader::from_reader(reader);
        xml_reader.config_mut().trim_text(true);

        let mut state = State::new();
        let mut buf = vec![];
        loop {
            buf.clear();
            let e = xml_reader.read_event_into(&mut buf);
            // log::debug!("xml reads {:?}", e);
            match e {
                //start tag
                Ok(Event::Start(e)) => {
                    state
                        .open_tag(e)
                        .with_context(|| format!("at position {}", xml_reader.buffer_position()))?;
                }

                //end of tag
                Ok(Event::End(e)) => state
                    .close_tag(e)
                    .with_context(|| format!("at position {}", xml_reader.buffer_position()))?,

                //empty tag
                Ok(Event::Empty(e)) => state
                    .empty_tag(e)
                    .with_context(|| format!("at position {}", xml_reader.buffer_position()))?,

                //end of file: return the tree if we can finish
                Ok(Event::Eof) => {
                    state.can_eof()?;
                    return Ok(Self {
                        tree: state.to_tree()?,
                    });
                }

                _ => (),
            }
        }
    }
}

impl FromStr for ProcessTreeMarkupLanguage {
    type Err = Error;

    fn from_str(s: &str) -> std::prelude::v1::Result<Self, Self::Err> {
        let mut reader = io::Cursor::new(s);
        Self::import(&mut reader)
    }
}

impl Exportable for ProcessTreeMarkupLanguage {
    fn export_from_object(object: EbiOutput, f: &mut dyn Write) -> Result<()> {
        match object {
            EbiOutput::Object(EbiObject::ProcessTree(tree)) => Self { tree }.export(f),
            EbiOutput::Object(EbiObject::StochasticProcessTree(stree)) => {
                Self { tree: stree.into() }.export(f)
            }
            EbiOutput::Bool(_) => Err(anyhow!("cannot export boolean as PTML")),
            EbiOutput::ContainsRoot(_) => Err(anyhow!("cannot export ContainsRoot as PTML")),
            EbiOutput::Fraction(_) => Err(anyhow!("cannot export fraction as PTML")),
            EbiOutput::LogDiv(_) => Err(anyhow!("cannot export LogDiv as PTML")),
            EbiOutput::RootLogDiv(_) => Err(anyhow!("cannot export RootLogDiv as PTML")),
            EbiOutput::String(_) => Err(anyhow!("cannot export string as PTML")),
            EbiOutput::Usize(_) => Err(anyhow!("cannot export integer as PTML")),
            EbiOutput::Object(_) => Err(anyhow!("cannot export object as PTML")),
        }
    }

    fn export(&self, f: &mut dyn std::io::Write) -> Result<()> {
        writeln!(f, "<?xml version=\"1.0\" encoding=\"ISO-8859-1\"?>")?;
        writeln!(f, "<ptml>")?;
        writeln!(f, "<processTree root=\"{}\">", self.tree.root())?;

        //nodes
        for (node_id, node) in self.tree.tree.iter().enumerate() {
            match node {
                Node::Tau => writeln!(f, "<automaticTask id=\"{}\" name=\"tau\"/>", node_id)?,
                Node::Activity(activity) => writeln!(
                    f,
                    "<manualTask id=\"{}\" name=\"{}\"/>",
                    node_id,
                    quick_xml::escape::escape(
                        self.tree.get_activity_key().deprocess_activity(activity)
                    )
                )?,
                Node::Operator(Operator::Loop, 1) => {
                    //special case: loop with 1 child gets a second "redo tau child and a third "exit" tau child
                    writeln!(
                        f,
                        "<{} id=\"{}\"/>",
                        PTMLTag::from(&Operator::Loop),
                        node_id
                    )?;
                    //edges
                    for child_id in self.tree.get_children(node_id) {
                        writeln!(
                            f,
                            "<parentsNode sourceId=\"{}\", targetId=\"{}\"/>",
                            node_id, child_id
                        )?;
                    }

                    //redo-tau
                    writeln!(f, "<automaticTask id=\"{}redo\" name=\"tau\"/>", node_id)?;
                    writeln!(
                        f,
                        "<parentsNode sourceId=\"{}\", targetId=\"{}redo\"/>",
                        node_id, node_id
                    )?;

                    //exit-tau
                    writeln!(f, "<automaticTask id=\"{}exit\" name=\"tau\"/>", node_id)?;
                    writeln!(
                        f,
                        "<parentsNode sourceId=\"{}\", targetId=\"{}exit\"/>",
                        node_id, node_id
                    )?
                }
                Node::Operator(Operator::Loop, 2) => {
                    //special case: loop with 2 children gets a third "exit" tau child
                    writeln!(
                        f,
                        "<{} id=\"{}\"/>",
                        PTMLTag::from(&Operator::Loop),
                        node_id
                    )?;
                    //edges
                    for child_id in self.tree.get_children(node_id) {
                        writeln!(
                            f,
                            "<parentsNode sourceId=\"{}\", targetId=\"{}\"/>",
                            node_id, child_id
                        )?;
                    }

                    //exit-tau
                    writeln!(f, "<automaticTask id=\"{}exit\" name=\"tau\"/>", node_id)?;
                    writeln!(
                        f,
                        "<parentsNode sourceId=\"{}\", targetId=\"{}exit\"/>",
                        node_id, node_id
                    )?
                }
                Node::Operator(Operator::Loop, _) => {
                    //special case: loop with more than 2 children gets an xor-child
                    writeln!(
                        f,
                        "<{} id=\"{}\"/>",
                        PTMLTag::from(&Operator::Loop),
                        node_id
                    )?;
                    writeln!(
                        f,
                        "<{} id=\"{}redo\"/>",
                        PTMLTag::from(&Operator::Xor),
                        node_id
                    )?;

                    //edges
                    for (i, child_id) in self.tree.get_children(node_id).enumerate() {
                        if i == 0 {
                            //body gets attached to the loop node
                            writeln!(
                                f,
                                "<parentsNode sourceId=\"{}\", targetId=\"{}\"/>",
                                node_id, child_id
                            )?;
                        } else {
                            writeln!(
                                f,
                                "<parentsNode sourceId=\"{}\", targetId=\"{}redo\"/>",
                                node_id, child_id
                            )?;
                        }
                    }

                    //exit-tau
                    writeln!(f, "<automaticTask id=\"{}exit\" name=\"tau\"/>", node_id)?;
                    writeln!(
                        f,
                        "<parentsNode sourceId=\"{}\", targetId=\"{}exit\"/>",
                        node_id, node_id
                    )?
                }
                Node::Operator(operator, _) => {
                    //other operators
                    writeln!(f, "<{} id=\"{}\"/>", PTMLTag::from(operator), node_id)?;

                    //edges
                    for child_id in self.tree.get_children(node_id) {
                        writeln!(
                            f,
                            "<parentsNode sourceId=\"{}\", targetId=\"{}\"/>",
                            node_id, child_id
                        )?;
                    }
                }
            }
        }
        writeln!(f, "</processTree>")?;
        Ok(writeln!(f, "</ptml>")?)
    }
}

impl EbiTraitGraphable for ProcessTreeMarkupLanguage {
    fn to_dot(&self) -> Result<layout::topo::layout::VisualGraph> {
        Into::<LabelledPetriNet>::into(self.clone()).to_dot()
    }
}

impl ToSemantics for ProcessTreeMarkupLanguage {
    fn to_semantics(self) -> EbiTraitSemantics {
        let lpn: LabelledPetriNet = self.into();
        EbiTraitSemantics::Marking(Box::new(lpn))
    }
}

#[derive(Copy, Clone, PartialEq)]
enum PTMLTag {
    And,
    Sequence,
    ManualTask(Activity),
    AutomaticTask,
    Xor,
    XorLoop,
    Or,
    Interleaved,
    Def,
    DefLoop,
}

impl PTMLTag {
    fn to_tag(e: &BytesStart, activity_key: &mut ActivityKey) -> Result<Option<Self>> {
        match e.name().as_ref() {
            b"and" => Ok(Some(Self::And)),
            b"sequence" => Ok(Some(Self::Sequence)),
            b"automaticTask" => Ok(Some(Self::AutomaticTask)),
            b"xor" => Ok(Some(Self::Xor)),
            b"def" => Ok(Some(Self::Def)),
            b"xorLoop" => Ok(Some(Self::XorLoop)),
            b"defLoop" => Ok(Some(Self::DefLoop)),
            b"or" => Ok(Some(Self::Or)),
            b"interleaved" => Ok(Some(Self::Interleaved)),
            b"manualTask" => {
                if let Ok(Some(attribute)) = e.try_get_attribute("name") {
                    let activity_label = String::from_utf8_lossy(&attribute.value);
                    let activity = activity_key.process_activity(&activity_label);
                    Ok(Some(Self::ManualTask(activity)))
                } else {
                    Err(anyhow!("a manualTask has no name"))
                }
            }
            _ => Ok(None),
        }
    }

    fn to_node(&self) -> Node {
        match self {
            PTMLTag::And => Node::Operator(Operator::Concurrent, 0),
            PTMLTag::Sequence => Node::Operator(Operator::Sequence, 0),
            PTMLTag::ManualTask(activity) => Node::Activity(*activity),
            PTMLTag::AutomaticTask => Node::Tau,
            PTMLTag::Xor => Node::Operator(Operator::Xor, 0),
            PTMLTag::XorLoop => Node::Operator(Operator::Loop, 0),
            PTMLTag::Or => Node::Operator(Operator::Loop, 0),
            PTMLTag::Interleaved => Node::Operator(Operator::Interleaved, 0),
            PTMLTag::Def => Node::Operator(Operator::Xor, 0),
            PTMLTag::DefLoop => Node::Operator(Operator::Loop, 0),
        }
    }
}

impl From<&Operator> for PTMLTag {
    fn from(value: &Operator) -> Self {
        match value {
            Operator::Xor => PTMLTag::Xor,
            Operator::Sequence => PTMLTag::Sequence,
            Operator::Interleaved => PTMLTag::Interleaved,
            Operator::Concurrent => PTMLTag::And,
            Operator::Or => PTMLTag::Or,
            Operator::Loop => PTMLTag::XorLoop,
        }
    }
}

impl Display for PTMLTag {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}",
            match self {
                PTMLTag::And => "and",
                PTMLTag::Sequence => "sequence",
                PTMLTag::ManualTask(_) => "manualTask",
                PTMLTag::AutomaticTask => "automaticTask",
                PTMLTag::Xor => "xor",
                PTMLTag::XorLoop => "xorLoop",
                PTMLTag::Or => "or",
                PTMLTag::Interleaved => "interleaved",
                PTMLTag::Def => "def",
                PTMLTag::DefLoop => "defLoop",
            }
        )
    }
}

#[cfg(test)]
mod tests {
    use std::fs::{self};

    use crate::{
        ebi_objects::process_tree_markup_language::ProcessTreeMarkupLanguage,
        ebi_traits::ebi_trait_semantics::Semantics,
    };

    #[test]
    fn nested_ptml() {
        let fin = fs::read_to_string("testfiles/valid nested.ptml").unwrap();
        let ptml = fin.parse::<ProcessTreeMarkupLanguage>().unwrap();

        let sem = ptml.tree;
        let state = sem.get_initial_state().unwrap();
        assert_eq!(sem.get_enabled_transitions(&state).len(), 1);
    }
}
