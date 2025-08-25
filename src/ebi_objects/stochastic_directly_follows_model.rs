use std::{
    cmp::Ordering,
    fmt::Display,
    io::{self, BufRead, Write},
    str::FromStr,
};

use anyhow::{Context, Error, Result, anyhow};
use ebi_arithmetic::{
    ebi_number::{Signed, Zero},
    fraction::Fraction,
};
use ebi_derive::ActivityKey;
use itertools::Itertools;
use layout::topo::layout::VisualGraph;

use crate::{
    ebi_framework::{
        activity_key::{
            Activity, ActivityKey, ActivityKeyTranslator, HasActivityKey, TranslateActivityKey,
        },
        ebi_file_handler::EbiFileHandler,
        ebi_input::{self, EbiObjectImporter, EbiTraitImporter},
        ebi_object::EbiObject,
        ebi_output::{EbiObjectExporter, EbiOutput},
        exportable::Exportable,
        importable::Importable,
        infoable::Infoable,
    },
    ebi_traits::{
        ebi_trait_activities,
        ebi_trait_graphable::{self, EbiTraitGraphable},
        ebi_trait_semantics::{EbiTraitSemantics, ToSemantics},
        ebi_trait_stochastic_deterministic_semantics::{
            EbiTraitStochasticDeterministicSemantics, ToStochasticDeterministicSemantics,
        },
        ebi_trait_stochastic_semantics::{EbiTraitStochasticSemantics, ToStochasticSemantics},
    },
    format_comparison,
    line_reader::LineReader,
};

pub const HEADER: &str = "stochastic directly follows model";

pub const FORMAT_SPECIFICATION: &str = concat!("A stochstic directly follows model is a line-based structure. Lines starting with a \\# are ignored.
    This first line is exactly `directly follows model'.\\
    The second line is a boolean indicating whether the model supports empty traces.\\
    The third line is the number of activities in the model.\\
    The following lines each contain an activity. Duplicated labels are accepted.\\
    The next line contains the number of start activities, followed by, for each start activity, a line with the index of the start activity, followed by a `w` and the weight of the start activity.\\
    The next line contains the number of end activities, followed by, for each end activity, a line with the index of the end activity, followed by a `w` and the weight of the end activity.\\
    The next line contains the number of edges, followed by, for each edge, a line with first the index of the source activity, then the `>` symbol, then the index of the target activity, then a `w`, and then the weight of the transition.
    
    For instance:
    \\lstinputlisting[language=ebilines, style=boxed]{../testfiles/aa-ab-ba.sdfm}", format_comparison!());

pub const EBI_STOCHASTIC_DIRECTLY_FOLLOWS_MODEL: EbiFileHandler = EbiFileHandler {
    name: "stochastic directly follows model",
    article: "a",
    file_extension: "sdfm",
    is_binary: false,
    format_specification: &FORMAT_SPECIFICATION,
    validator: Some(ebi_input::validate::<StochasticDirectlyFollowsModel>),
    trait_importers: &[
        EbiTraitImporter::Activities(
            ebi_trait_activities::import::<StochasticDirectlyFollowsModel>,
        ),
        EbiTraitImporter::Semantics(StochasticDirectlyFollowsModel::import_as_semantics),
        EbiTraitImporter::StochasticSemantics(
            StochasticDirectlyFollowsModel::import_as_stochastic_semantics,
        ),
        EbiTraitImporter::StochasticDeterministicSemantics(
            StochasticDirectlyFollowsModel::import_as_stochastic_deterministic_semantics,
        ),
        EbiTraitImporter::Graphable(ebi_trait_graphable::import::<StochasticDirectlyFollowsModel>),
    ],
    object_importers: &[
        EbiObjectImporter::StochasticDirectlyFollowsModel(
            StochasticDirectlyFollowsModel::import_as_object,
        ),
        EbiObjectImporter::DirectlyFollowsModel(
            StochasticDirectlyFollowsModel::import_as_directly_follows_model,
        ),
        EbiObjectImporter::LabelledPetriNet(
            StochasticDirectlyFollowsModel::import_as_labelled_petri_net,
        ),
        EbiObjectImporter::StochasticLabelledPetriNet(
            StochasticDirectlyFollowsModel::import_as_stochastic_labelled_petri_net,
        ),
    ],
    object_exporters: &[
        EbiObjectExporter::StochasticDirectlyFollowsModel(
            StochasticDirectlyFollowsModel::export_from_object,
        ),
        EbiObjectExporter::DirectlyFollowsGraph(StochasticDirectlyFollowsModel::export_from_object),
    ],
    java_object_handlers: &[],
};

#[derive(ActivityKey, Debug, Clone)]
pub struct StochasticDirectlyFollowsModel {
    pub(crate) activity_key: ActivityKey,
    pub(crate) node_2_activity: Vec<Activity>,
    pub(crate) empty_traces_weight: Fraction,
    pub(crate) sources: Vec<NodeIndex>, //edge -> source of edge
    pub(crate) targets: Vec<NodeIndex>, //edge -> target of edge
    pub(crate) weights: Vec<Fraction>,  //edge -> how often observed
    pub(crate) start_node_weights: Vec<Fraction>, //node -> how often observed
    pub(crate) end_node_weights: Vec<Fraction>, //node -> how often observed
}

pub type NodeIndex = usize;

impl StochasticDirectlyFollowsModel {
    /**
     * Creates a new stochastic directly follows model. This has the empty stochastic language, until a state or an empty trace is added.
     */
    pub fn new(activity_key: ActivityKey) -> Self {
        Self {
            activity_key: activity_key,
            node_2_activity: vec![],
            empty_traces_weight: Fraction::zero(),
            sources: vec![],
            targets: vec![],
            weights: vec![],
            start_node_weights: vec![],
            end_node_weights: vec![],
        }
    }

    pub fn import_as_directly_follows_model(reader: &mut dyn BufRead) -> Result<EbiObject> {
        let dfg = Self::import(reader)?;
        Ok(EbiObject::DirectlyFollowsModel(dfg.into()))
    }

    pub fn import_as_labelled_petri_net(reader: &mut dyn BufRead) -> Result<EbiObject> {
        let dfg = Self::import(reader)?;
        Ok(EbiObject::LabelledPetriNet(dfg.into()))
    }

    pub fn import_as_stochastic_labelled_petri_net(reader: &mut dyn BufRead) -> Result<EbiObject> {
        let dfg = Self::import(reader)?;
        Ok(EbiObject::StochasticLabelledPetriNet(dfg.into()))
    }

    pub fn has_empty_traces(&self) -> bool {
        self.empty_traces_weight.is_positive()
    }

    pub(crate) fn can_terminate_in_node(&self, node: NodeIndex) -> bool {
        self.end_node_weights[node].is_positive()
    }

    pub(crate) fn number_of_start_nodes(&self) -> usize {
        self.start_node_weights
            .iter()
            .fold(0, |a, b| if b.is_positive() { a + 1 } else { a })
    }

    pub(crate) fn number_of_end_nodes(&self) -> usize {
        self.end_node_weights
            .iter()
            .fold(0, |a, b| if b.is_positive() { a + 1 } else { a })
    }

    pub(crate) fn is_start_node(&self, node: NodeIndex) -> bool {
        self.start_node_weights[node].is_positive()
    }

    pub(crate) fn is_end_node(&self, node: NodeIndex) -> bool {
        self.end_node_weights[node].is_positive()
    }

    pub(crate) fn can_execute_edge(&self, edge: usize) -> bool {
        self.weights[edge].is_positive()
    }

    pub fn get_max_state(&self) -> usize {
        self.node_2_activity.len() + 2
    }

    pub fn add_node(&mut self, activity: Activity) -> NodeIndex {
        let index = self.node_2_activity.len();
        self.node_2_activity.push(activity);
        self.start_node_weights.push(Fraction::zero());
        self.end_node_weights.push(Fraction::zero());
        index
    }

    pub fn add_empty_trace(&mut self, weight: &Fraction) {
        self.empty_traces_weight += weight;
    }

    pub fn add_edge(&mut self, source: NodeIndex, target: NodeIndex, weight: Fraction) {
        let (found, from) = self.binary_search(source, target);
        if found {
            //edge already present
            self.weights[from] += weight;
        } else {
            //new edge
            self.sources.insert(from, source);
            self.targets.insert(from, target);
            self.weights.insert(from, weight);
        }
    }

    pub(crate) fn binary_search(&self, source: NodeIndex, target: NodeIndex) -> (bool, usize) {
        if self.sources.is_empty() {
            return (false, 0);
        }

        let mut size = self.sources.len();
        let mut left = 0;
        let mut right = size;
        while left < right {
            let mid = left + size / 2;

            let cmp = Self::compare(source, target, self.sources[mid], self.targets[mid]);

            left = if cmp == Ordering::Less { mid + 1 } else { left };
            right = if cmp == Ordering::Greater { mid } else { right };
            if cmp == Ordering::Equal {
                assert!(mid < self.sources.len());
                return (true, mid);
            }

            size = right - left;
        }

        assert!(left <= self.sources.len());
        (false, left)
    }

    fn compare(
        source1: NodeIndex,
        target1: NodeIndex,
        source2: NodeIndex,
        target2: NodeIndex,
    ) -> Ordering {
        if source1 < source2 {
            return Ordering::Greater;
        } else if source1 > source2 {
            return Ordering::Less;
        } else if target2 > target1 {
            return Ordering::Greater;
        } else if target2 < target1 {
            return Ordering::Less;
        } else {
            return Ordering::Equal;
        }
    }
}

impl Importable for StochasticDirectlyFollowsModel {
    fn import_as_object(reader: &mut dyn BufRead) -> Result<EbiObject> {
        Ok(EbiObject::StochasticDirectlyFollowsModel(Self::import(
            reader,
        )?))
    }

    fn import(reader: &mut dyn BufRead) -> Result<Self>
    where
        Self: Sized,
    {
        let mut lreader = LineReader::new(reader);

        let head = lreader
            .next_line_string()
            .with_context(|| format!("failed to read header, which should be {}", HEADER))?;
        if head != HEADER {
            return Err(anyhow!(
                "first line should be exactly `{}`, but found `{}`",
                HEADER,
                lreader.get_last_line()
            ));
        }

        //read empty traces
        let empty_traces_weight = lreader
            .next_line_weight()
            .context("could not read whether the model supports empty traces")?;

        //read nodes
        let number_of_nodes = lreader
            .next_line_index()
            .context("could not read the number of nodes")?;
        let mut activity_key = ActivityKey::new();
        let mut node_2_activity = vec![];
        for activity in 0..number_of_nodes {
            let label = lreader
                .next_line_string()
                .with_context(|| format!("could not read node {}", activity))?;
            let activity = activity_key.process_activity(&label);
            node_2_activity.push(activity);
        }

        //read start nodes
        let mut start_node_weights = vec![Fraction::zero(); node_2_activity.len()];
        let number_of_start_nodes = lreader
            .next_line_index()
            .context("could not read the number of start nodes")?;
        for i in 0..number_of_start_nodes {
            let line = lreader
                .next_line_string()
                .with_context(|| format!("could not read start node {}", i))?;
            let (node, weight) = line.split('w').next_tuple().with_context(|| {
                format!("start node {} should be a node, then w, then a weight", i)
            })?;

            let node = node.parse::<usize>().with_context(|| {
                format!("start node {} should be a node, then w, then a weight", i)
            })?;
            let weight = weight.parse::<Fraction>().with_context(|| {
                format!("start node {} should be a node, then w, then a weight", i)
            })?;

            start_node_weights[node] = weight;
        }

        //read end activities
        let mut end_node_weights = vec![Fraction::zero(); node_2_activity.len()];
        let number_of_end_nodes = lreader
            .next_line_index()
            .context("could not read the number of end nodes")?;
        for i in 0..number_of_end_nodes {
            let line = lreader
                .next_line_string()
                .with_context(|| format!("could not read end node {}", i))?;
            let (node, weight) = line.split('w').next_tuple().with_context(|| {
                format!("end node {} should be a node, then w, then a weight", i)
            })?;

            let node = node.parse::<usize>().with_context(|| {
                format!("end node {} should be a node, then w, then a weight", i)
            })?;
            let weight = weight.parse::<Fraction>().with_context(|| {
                format!("end node {} should be a node, then w, then a weight", i)
            })?;

            end_node_weights[node] = weight;
        }

        let mut result = Self {
            activity_key,
            node_2_activity,
            empty_traces_weight,
            sources: vec![],
            targets: vec![],
            weights: vec![],
            start_node_weights,
            end_node_weights,
        };

        //read edges
        let number_of_edges = lreader
            .next_line_index()
            .context("could not read number of edges")?;
        for e in 0..number_of_edges {
            let edge_line = lreader
                .next_line_string()
                .with_context(|| format!("could not read edge {}", e))?;

            let mut arr = edge_line.split('>');
            if let Some((source, remainder)) = arr.next_tuple() {
                let source = source
                    .parse::<usize>()
                    .with_context(|| format!("could not read source of edge {}", e))?;

                let mut arr = remainder.split('w');
                if let Some((target, weight)) = arr.next_tuple() {
                    let target = target
                        .parse::<usize>()
                        .with_context(|| format!("could not read target of edge {}", e))?;

                    let weight = weight
                        .parse::<Fraction>()
                        .with_context(|| format!("could not read weight of edge {}", e))?;

                    result.add_edge(source, target, weight);
                } else {
                    return Err(anyhow!(
                        "could not read edge, which must be two numbers separated by >, followed by w and a weight"
                    ));
                }
            } else {
                return Err(anyhow!(
                    "could not read edge, which must be two numbers separated by >, followed by w and a weight"
                ));
            }
        }

        Ok(result)
    }
}

impl FromStr for StochasticDirectlyFollowsModel {
    type Err = Error;

    fn from_str(s: &str) -> std::result::Result<Self, Self::Err> {
        let mut reader = io::Cursor::new(s);
        Self::import(&mut reader)
    }
}

impl Exportable for StochasticDirectlyFollowsModel {
    fn export_from_object(object: EbiOutput, f: &mut dyn Write) -> Result<()> {
        match object {
            EbiOutput::Object(EbiObject::StochasticDirectlyFollowsModel(dfm)) => {
                Self::export(&dfm, f)
            }
            EbiOutput::Object(EbiObject::DirectlyFollowsGraph(dfm)) => {
                Self::export(&Into::<StochasticDirectlyFollowsModel>::into(dfm), f)
            }
            _ => Err(anyhow!("Cannot export as SDFM.")),
        }
    }

    fn export(&self, f: &mut dyn std::io::Write) -> Result<()> {
        Ok(write!(f, "{}", self.to_string())?)
    }
}

impl TranslateActivityKey for StochasticDirectlyFollowsModel {
    fn translate_using_activity_key(&mut self, to_activity_key: &mut ActivityKey) {
        let translator = ActivityKeyTranslator::new(&self.activity_key, to_activity_key);
        self.node_2_activity
            .iter_mut()
            .for_each(|activity| *activity = translator.translate_activity(&activity));
        self.activity_key = to_activity_key.clone();
    }
}

impl Display for StochasticDirectlyFollowsModel {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        writeln!(f, "{}", HEADER)?;

        writeln!(f, "# empty traces weight\n{}", self.empty_traces_weight)?;

        //activities
        writeln!(f, "# number of nodes\n{}", self.node_2_activity.len())?;
        for (a, activity) in self.node_2_activity.iter().enumerate() {
            writeln!(
                f,
                "#node {}\n{}",
                a,
                self.activity_key.get_activity_label(activity)
            )?;
        }

        //start activities
        writeln!(
            f,
            "# number of start nodes\n{}",
            self.number_of_start_nodes()
        )?;
        for (i, weight) in self.start_node_weights.iter().enumerate() {
            if weight.is_positive() {
                writeln!(f, "{}w{}", i, weight)?;
            }
        }

        //end activities
        writeln!(f, "# number of end nodes\n{}", self.number_of_end_nodes())?;
        for (i, weight) in self.end_node_weights.iter().enumerate() {
            if weight.is_positive() {
                writeln!(f, "{}w{}", i, weight)?;
            }
        }

        //edges
        writeln!(f, "# number of edges\n{}\n# edges", self.sources.len())?;
        for (source, (target, weight)) in self
            .sources
            .iter()
            .zip(self.targets.iter().zip(self.weights.iter()))
        {
            writeln!(f, "{}>{}w{}", source, target, weight)?;
        }

        Ok(write!(f, "")?)
    }
}

impl EbiTraitGraphable for StochasticDirectlyFollowsModel {
    fn to_dot(&self) -> Result<layout::topo::layout::VisualGraph> {
        let mut graph = VisualGraph::new(layout::core::base::Orientation::LeftToRight);

        //source + sink
        let source = <dyn EbiTraitGraphable>::create_place(&mut graph, "");
        let sink = <dyn EbiTraitGraphable>::create_place(&mut graph, "");

        //empty traces
        if self.empty_traces_weight.is_positive() {
            <dyn EbiTraitGraphable>::create_edge(
                &mut graph,
                &source,
                &sink,
                &format!("{}", self.empty_traces_weight),
            );
        }

        //nodes
        let mut nodes = vec![];
        for node in &self.node_2_activity {
            nodes.push(<dyn EbiTraitGraphable>::create_transition(
                &mut graph,
                self.activity_key.get_activity_label(node),
                "",
            ));
        }

        //start activities
        for (activity, weight) in self.start_node_weights.iter().enumerate() {
            if weight.is_positive() {
                <dyn EbiTraitGraphable>::create_edge(
                    &mut graph,
                    &source,
                    &nodes[activity],
                    &format!("{}", weight),
                );
            }
        }

        //end activities
        for (activity, weight) in self.end_node_weights.iter().enumerate() {
            if weight.is_positive() {
                <dyn EbiTraitGraphable>::create_edge(
                    &mut graph,
                    &nodes[activity],
                    &sink,
                    &format!("{}", weight),
                );
            }
        }

        //edges
        for (source, (target, weight)) in self
            .sources
            .iter()
            .zip(self.targets.iter().zip(self.weights.iter()))
        {
            <dyn EbiTraitGraphable>::create_edge(
                &mut graph,
                &nodes[*source],
                &nodes[*target],
                &format!("{}", weight),
            );
        }

        Ok(graph)
    }
}

impl Infoable for StochasticDirectlyFollowsModel {
    fn info(&self, f: &mut impl std::io::Write) -> Result<()> {
        writeln!(f, "Number of transitions\t{}", self.node_2_activity.len())?;
        writeln!(
            f,
            "Number of activities\t{}",
            self.activity_key.activity2name.len()
        )?;
        writeln!(f, "Number of nodes\t\t{}", self.node_2_activity.len())?;
        writeln!(f, "Number of edges\t\t{}", self.sources.len())?;
        writeln!(f, "Number of start nodes\t{}", self.number_of_start_nodes())?;
        writeln!(f, "Number of end nodes\t{}", self.number_of_end_nodes())?;

        let mut sum: Fraction = self.weights.iter().sum();
        sum += &self.start_node_weights.iter().sum::<Fraction>();
        sum += &self.end_node_weights.iter().sum::<Fraction>();
        writeln!(f, "Sum weight of edges\t{}", sum)?;

        writeln!(f, "")?;
        self.get_activity_key().info(f)?;

        Ok(write!(f, "")?)
    }
}

impl ToSemantics for StochasticDirectlyFollowsModel {
    fn to_semantics(self) -> EbiTraitSemantics {
        EbiTraitSemantics::Usize(Box::new(self))
    }
}

impl ToStochasticSemantics for StochasticDirectlyFollowsModel {
    fn to_stochastic_semantics(self) -> EbiTraitStochasticSemantics {
        EbiTraitStochasticSemantics::Usize(Box::new(self))
    }
}

impl ToStochasticDeterministicSemantics for StochasticDirectlyFollowsModel {
    fn to_stochastic_deterministic_semantics(self) -> EbiTraitStochasticDeterministicSemantics {
        EbiTraitStochasticDeterministicSemantics::UsizeDistribution(Box::new(self))
    }
}
