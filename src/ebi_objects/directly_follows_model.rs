use std::{collections::HashSet, fmt::Display, io::{self, BufRead, Write}, str::FromStr};
use anyhow::{anyhow, Context, Result, Error};
use itertools::Itertools;
use layout::topo::layout::VisualGraph;

use crate::{ebi_framework::{activity_key::{Activity, ActivityKey, ActivityKeyTranslator, HasActivityKey, TranslateActivityKey}, ebi_file_handler::EbiFileHandler, ebi_input::{self, EbiObjectImporter, EbiTraitImporter}, ebi_object::EbiObject, ebi_output::{EbiObjectExporter, EbiOutput}, exportable::Exportable, importable::Importable, infoable::Infoable}, ebi_traits::{ebi_trait_graphable::{self, EbiTraitGraphable}, ebi_trait_semantics::{EbiTraitSemantics, ToSemantics}}, line_reader::LineReader};

use super::labelled_petri_net::LabelledPetriNet;

pub const HEADER: &str = "directly follows model";

pub const FORMAT_SPECIFICATION: &str = "A directly follows model is a line-based structure. Lines starting with a \\# are ignored.
    This first line is exactly `directly follows model'.\\
    The second line is a boolean indicating whether the model supports empty traces.\\
    The third line is the number of activities in the model.\\
    The following lines each contain an activity. Duplicated labels are accepted.\\
    The next line contains the number of start activities, followed by, for each start activity, a line with the index of the start activity.\\
    The next line contains the number of end activities, followed by, for each end activity, a line with the index of the end activity.\\
    The next line contains the number of edges, followed by, for each edge, a line with first the index of the source activity, then the `>` symbol, then the index of the target activity.
    
    For instance:
    \\lstinputlisting[language=ebilines, style=boxed]{../testfiles/a-b_star.dfm}";

pub const EBI_DIRCTLY_FOLLOWS_MODEL: EbiFileHandler = EbiFileHandler {
    name: "directly follows model",
    article: "a",
    file_extension: "dfm",
    format_specification: &FORMAT_SPECIFICATION,
    validator: ebi_input::validate::<DirectlyFollowsModel>,
    trait_importers: &[
        EbiTraitImporter::Semantics(DirectlyFollowsModel::import_as_semantics),
        EbiTraitImporter::Graphable(ebi_trait_graphable::import::<DirectlyFollowsModel>),
    ],
    object_importers: &[
        EbiObjectImporter::DirectlyFollowsModel(DirectlyFollowsModel::import_as_object),
        EbiObjectImporter::LabelledPetriNet(DirectlyFollowsModel::import_as_labelled_petri_net)
    ],
    object_exporters: &[
        EbiObjectExporter::DirectlyFollowsModel(DirectlyFollowsModel::export_from_object)
    ],
    java_object_handlers: &[],
};

#[derive(ActivityKey,Debug,Clone)]
pub struct DirectlyFollowsModel {
    activity_key: ActivityKey,
    pub(crate) empty_traces: bool,
	pub(crate) edges: Vec<Vec<bool>>, //matrix of edges
    pub(crate) node_2_activity: Vec<Activity>,
    pub(crate) start_nodes: HashSet<usize>,
    pub(crate) end_nodes: HashSet<usize>
}

impl DirectlyFollowsModel {

    pub fn import_as_labelled_petri_net(reader: &mut dyn BufRead) -> Result<EbiObject> {
        let dfm = Self::import(reader)?;
        Ok(EbiObject::LabelledPetriNet(dfm.get_labelled_petri_net()))
    }

    pub fn get_number_of_edges(&self) -> usize {
        self.edges.iter().fold(0usize, |a, b| {
            a + b.into_iter().filter(|c| **c).count()
        })
    }

    pub fn get_number_of_nodes(&self) -> usize {
        self.node_2_activity.len()
    }
    
    pub fn get_labelled_petri_net(&self) -> LabelledPetriNet {
        let mut result = LabelledPetriNet::new();
        let translator = ActivityKeyTranslator::new(&self.activity_key, result.get_activity_key_mut());
        let source = result.add_place();
        let sink = result.add_place();
        result.get_initial_marking_mut().increase(source, 1).unwrap();
        
		/*
		 * empty traces
		 */
		if self.empty_traces {
            let transition = result.add_transition(None);
            
            result.add_place_transition_arc(source, transition, 1).unwrap();
            result.add_transition_place_arc(transition, sink, 1).unwrap();
		}

		/*
		 * Nodes (states): after doing a node you end up in the corresponding place.
		 */
        let mut node2place = vec![];
        for _ in 0..self.get_number_of_nodes() {
            let place = result.add_place();
            node2place.push(place);
        }

		/*
		 * Transitions
		 */
        for source_node in 0..self.get_number_of_nodes() {
            for target_node in 0..self.get_number_of_nodes() {
                if self.edges[source_node][target_node] {

                    let from_place = node2place[source_node];
                    let to_place = node2place[target_node];
                    let activity = translator.translate_activity(&self.node_2_activity[target_node]);
                    let transition = result.add_transition(Some(activity));

                    result.add_place_transition_arc(from_place, transition, 1).unwrap();
                    result.add_transition_place_arc(transition, to_place, 1).unwrap();
                }
            }
        }

		/*
		 * Starts
		 */
        for start_node in self.start_nodes.iter() {
            let activity = translator.translate_activity(&self.node_2_activity[*start_node]);
            let transition = result.add_transition(Some(activity));
            result.add_place_transition_arc(source, transition, 1).unwrap();
            let target_place = node2place[*start_node];
            result.add_transition_place_arc(transition, target_place, 1).unwrap();
            
        }

		/*
		 * Ends
		*/
        for end_node in self.end_nodes.iter() {
            let transition = result.add_transition(None);
            let source_place = node2place[*end_node];
            result.add_place_transition_arc(source_place, transition, 1).unwrap();
            result.add_transition_place_arc(transition, sink, 1).unwrap();
        }

        result
    }
}

impl TranslateActivityKey for DirectlyFollowsModel {
    fn translate_using_activity_key(&mut self, to_activity_key: &mut ActivityKey) {
        let translator = ActivityKeyTranslator::new(&self.activity_key, to_activity_key);
        self.node_2_activity.iter_mut().for_each(|activity| *activity = translator.translate_activity(&activity));
        self.activity_key = to_activity_key.clone();
    }
}

impl Importable for DirectlyFollowsModel {
    fn import_as_object(reader: &mut dyn std::io::prelude::BufRead) -> Result<EbiObject> {
        Ok(EbiObject::DirectlyFollowsModel(Self::import(reader)?))
    }

    fn import(reader: &mut dyn std::io::prelude::BufRead) -> anyhow::Result<Self> where Self: Sized {
        let mut lreader = LineReader::new(reader);

        let head = lreader.next_line_string().with_context(|| format!("failed to read header, which should be {}", HEADER))?;
        if head != HEADER {
            return Err(anyhow!("first line should be exactly `{}`, but found `{}`", HEADER, lreader.get_last_line()));
        }

        //read empty traces
		let empty_traces = lreader.next_line_bool().context("could not read whether the model supports empty traces")?;

        //read activities
        let number_of_activities = lreader.next_line_index().context("could not read the number of activities")?;
        let mut activity_key = ActivityKey::new();
        let mut node_2_activity = vec![];
        for activity in 0 .. number_of_activities {
            let label = lreader.next_line_string().with_context(|| format!("could not read activity {}", activity))?;
            let activity = activity_key.process_activity(&label);
            node_2_activity.push(activity);
        }

        //read start activities
        let mut start_nodes = HashSet::new();
        let number_of_start_activities = lreader.next_line_index().context("could not read the number of start activities")?;
        for i in 0..number_of_start_activities {
            let start_activity = lreader.next_line_index().with_context(|| format!("could not read start activity {}", i))?;
            start_nodes.insert(start_activity);
        }

        //read end activities
        let mut end_nodes = HashSet::new();
        let number_of_end_activities = lreader.next_line_index().context("could not read the number of end activities")?;
        for i in 0..number_of_end_activities {
            let end_activity = lreader.next_line_index().with_context(|| format!("could not read end activity {}", i))?;
            end_nodes.insert(end_activity);
        }

        //read edges
        let mut edges = vec![vec![false; number_of_activities]; number_of_activities];
        let number_of_edges = lreader.next_line_index().context("could not read number of edges")?;
        for e in 0..number_of_edges {
            let edge_line = lreader.next_line_string().with_context(|| format!("could not read edge {}", e))?;

            let mut arr = edge_line.split('>');
            if let Some((source, target)) = arr.next_tuple() {
                let source = source.parse::<usize>().with_context(|| format!("could not read source of edge {}", e))?;
                let target = target.parse::<usize>().with_context(|| format!("could not read target of edge {}", e))?;
                edges[source][target] = true;
            } else {
                return Err(anyhow!("could not read edge, which must be two numbers separated by >"));
            }
        }

        Ok(Self {
            empty_traces,
            edges,
            activity_key,
            node_2_activity,
            start_nodes,
            end_nodes
        })
    }
}

impl FromStr for DirectlyFollowsModel {
    type Err = Error;

    fn from_str(s: &str) -> std::prelude::v1::Result<Self, Self::Err> {
        let mut reader = io::Cursor::new(s);
        Self::import(&mut reader)
    }
}

impl ToSemantics for DirectlyFollowsModel {
    fn to_semantics(self) -> EbiTraitSemantics {
        EbiTraitSemantics::Usize(Box::new(self))
    }
}

impl Display for DirectlyFollowsModel {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        writeln!(f, "{}", HEADER)?;
        
        writeln!(f, "# empty trace\n{}", self.empty_traces)?;

        //activities
        writeln!(f, "# number of activites\n{}", self.node_2_activity.len())?;
        for (a, activity) in self.node_2_activity.iter().enumerate() {
            writeln!(f, "#activity {}\n{}", a, self.activity_key.get_activity_label(activity))?;
        }

        //start activities
        writeln!(f, "# number of start activites\n{}", self.start_nodes.len())?;
        for (i, start_activity) in self.start_nodes.iter().enumerate() {
            writeln!(f, "# start activity {}\n{}", i, start_activity)?;
        }

        //end activities
        writeln!(f, "# number of end activites\n{}", self.end_nodes.len())?;
        for (i, end_activity) in self.end_nodes.iter().enumerate() {
            writeln!(f, "# end activity {}\n{}", i, end_activity)?;
        }

        //edges
        writeln!(f, "# number of edges\n{}\n# edges", self.get_number_of_edges())?;
        for source in 0..self.edges.len() {
            for target in 0..self.edges.len() {
                if self.edges[source][target] {
                    writeln!(f, "{}>{}", source, target)?;
                }
            }
        }

        Ok(write!(f, "")?)
    }
}

impl EbiTraitGraphable for DirectlyFollowsModel {
    fn to_dot(&self) -> layout::topo::layout::VisualGraph {
        let mut graph = VisualGraph::new(layout::core::base::Orientation::LeftToRight);

        //source + sink
        let source = <dyn EbiTraitGraphable>::create_place(&mut graph, "");
        let sink = <dyn EbiTraitGraphable>::create_place(&mut graph, "");

        //empty traces
        if self.empty_traces {
            <dyn EbiTraitGraphable>::create_edge(&mut graph, &source, &sink, "");
        }

        //nodes
        let mut nodes = vec![];
        for n in &self.node_2_activity {
            nodes.push(<dyn EbiTraitGraphable>::create_transition(&mut graph, self.activity_key.get_activity_label(n), ""));
        }

        //start activities
        for start_activity in self.start_nodes.iter() {
            <dyn EbiTraitGraphable>::create_edge(&mut graph, &source, &nodes[*start_activity], "");
        }

        //end activities
        for end_activity in self.end_nodes.iter() {
            <dyn EbiTraitGraphable>::create_edge(&mut graph, &nodes[*end_activity], &sink, "");
        }

        //edges
        for source in 0..self.edges.len() {
            for target in 0..self.edges.len() {
                if self.edges[source][target] {
                    <dyn EbiTraitGraphable>::create_edge(&mut graph, &nodes[source], &nodes[target], "");
                }
            }
        }

        return graph;
    }
}

impl Exportable for DirectlyFollowsModel {
    fn export_from_object(object: EbiOutput, f: &mut dyn Write) -> Result<()> {
        match object {
            EbiOutput::Object(EbiObject::DirectlyFollowsModel(dfm)) => Self::export(&dfm, f),
            _ => unreachable!()
        }
    }

    fn export(&self, f: &mut dyn std::io::Write) -> Result<()> {
        Ok(write!(f, "{}", self.to_string())?)
    }
}

impl Infoable for DirectlyFollowsModel {
    fn info(&self, f: &mut impl std::io::Write) -> Result<()> {
        writeln!(f, "Number of transitions\t{}", self.node_2_activity.len())?;
        writeln!(f, "Number of activities\t{}", self.activity_key.activity2name.len())?;
        writeln!(f, "Number of edges\t\t{}", self.get_number_of_edges())?;

        Ok(write!(f, "")?)
    }
}

#[cfg(test)]
mod tests{
    use crate::ebi_framework::{ebi_output::EbiOutput, exportable::Exportable};

    use super::DirectlyFollowsModel;


    #[test]
    #[should_panic]
    fn unreacahble() {
        let mut f = vec![];
        DirectlyFollowsModel::export_from_object(EbiOutput::Usize(0), &mut f).unwrap_err();
    }
}