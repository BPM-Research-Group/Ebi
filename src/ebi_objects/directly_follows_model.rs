use std::{collections::{HashMap, HashSet}, fmt::Display, io::{self, BufRead}, str::FromStr};
use anyhow::{anyhow, Context, Result, Error};
use layout::topo::layout::VisualGraph;
use crate::{activity_key::{self, Activity, ActivityKey}, dottable::Dottable, ebi_commands::ebi_command_info::Infoable, ebi_traits::{ebi_trait::EbiTrait, ebi_trait_labelled_petri_net::EbiTraitLabelledPetriNet}, export::{EbiObjectExporter, EbiOutput, Exportable}, file_handler::EbiFileHandler, import::{self, EbiObjectImporter, EbiTraitImporter, Importable}, line_reader::LineReader, marking::Marking, net::Transition};

use super::{ebi_object::EbiObject, labelled_petri_net::LabelledPetriNet, stochastic_deterministic_finite_automaton::StochasticDeterministicFiniteAutomaton};

pub const HEADER: &str = "directly follows model";

pub const EBI_DIRCTLY_FOLLOWS_MODEL: EbiFileHandler = EbiFileHandler {
    name: "directly follows model",
    article: "a",
    file_extension: "dfm",
    validator: import::validate::<DirectlyFollowsModel>,
    trait_importers: &[
        EbiTraitImporter::LabelledPetriNet(DirectlyFollowsModel::import_as_labelled_petri_net)
    ],
    object_importers: &[
        EbiObjectImporter::DirectlyFollowsModel(DirectlyFollowsModel::import_as_object),
        EbiObjectImporter::LabelledPetriNet(DirectlyFollowsModel::import_as_labelled_petri_net_object)
    ],
    object_exporters: &[
        EbiObjectExporter::DirectlyFollowsModel(DirectlyFollowsModel::export_from_object)
    ]
    
};

pub struct DirectlyFollowsModel {
    empty_traces: bool,
	edges: Vec<Vec<bool>>, //matrix of edges
	activity_key: ActivityKey,
    node_2_activity: Vec<Activity>,
    start_activities: HashSet<usize>,
    end_activities: HashSet<usize>
}

impl DirectlyFollowsModel {

    pub fn get_number_of_edges(&self) -> usize {
        self.edges.iter().fold(0usize, |a, b| {
            a + b.into_iter().filter(|c| **c).count()
        })
    }

    pub fn get_number_of_nodes(&self) -> usize {
        self.node_2_activity.len()
    }

    pub fn import_as_labelled_petri_net(reader: &mut dyn BufRead) -> Result<Box<dyn EbiTraitLabelledPetriNet>> {
        let lang = Self::import(reader)?;
        Ok(Box::new(lang.to_labelled_petri_net()))
    }

    pub fn import_as_labelled_petri_net_object(reader: &mut dyn BufRead) -> Result<EbiObject> {
        let dfm = Self::import(reader)?;
        Ok(EbiObject::LabelledPetriNet(dfm.to_labelled_petri_net()))
    }
    
    pub fn to_labelled_petri_net(&self) -> LabelledPetriNet {
        let mut places = 2; //0 = source, 1 = sink
        let mut transitions = vec![];
        
		/**
		 * empty traces
		 */
		if (self.empty_traces) {
            let mut transition = Transition::new_silent(0);
			transition.incoming.push(0);
            transition.outgoing.push(1);
            transitions.push(transition);
		}

		/*
		 * Activities (states)
		 */
        places += self.node_2_activity.len();

        /**
         * Initial marking
         */
        let mut initial_marking = vec![0u64; places];
        initial_marking[0] = 1;

		/**
		 * Transitions
		 */
        for source in 0..self.edges.len() {
            for target in 0..self.edges.len() {
                if self.edges[source][target] {
                    let source_place = source + 2;
                    let target_place = target + 2;

                    let target_activity = self.node_2_activity[target];
                    let mut transition = Transition::new_labelled(transitions.len(), target_activity);

                    transition.incoming.push(source_place);
                    transition.outgoing.push(target_place);

                    transitions.push(transition);
                }
            }
        }

		/**
		 * Starts
		 */
        for start_activity in self.start_activities.iter() {
            let source_activity = self.node_2_activity[*start_activity];
            let mut transition = Transition::new_labelled(transitions.len(), source_activity);
            let target_place = start_activity + 2;
            transition.incoming.push(0);
            transition.outgoing.push(target_place);
            transitions.push(transition);
        }

		/**
		 * Ends
		*/
        for end_activity in self.end_activities.iter() {
            let mut transition = Transition::new_silent(transitions.len());
            let source_place = end_activity + 2;
            transition.incoming.push(source_place);
            transition.outgoing.push(1);
            transitions.push(transition);
        }

        LabelledPetriNet::from_fields(self.activity_key.clone(), places, transitions, initial_marking.into())
    }
}

impl Importable for DirectlyFollowsModel {
    fn import_as_object(reader: &mut dyn std::io::prelude::BufRead) -> anyhow::Result<super::ebi_object::EbiObject> {
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
        let mut start_activities = HashSet::new();
        let number_of_start_activities = lreader.next_line_index().context("could not read the number of start activities")?;
        for i in 0..number_of_start_activities {
            let start_activity = lreader.next_line_index().with_context(|| format!("could not read start activity {}", i))?;
            start_activities.insert(start_activity);
        }

        //read end activities
        let mut end_activities = HashSet::new();
        let number_of_end_activities = lreader.next_line_index().context("could not read the number of end activities")?;
        for i in 0..number_of_end_activities {
            let end_activity = lreader.next_line_index().with_context(|| format!("could not read end activity {}", i))?;
            end_activities.insert(end_activity);
        }

        //read edges
        let mut edges = vec![vec![false; number_of_activities]; number_of_activities];
        let number_of_edges = lreader.next_line_index().context("could not read number of edges")?;
        for e in 0..number_of_edges {
            let edge_line = lreader.next_line_string().with_context(|| format!("could not read edge {}", e))?;

            let mut arr = edge_line.split('>');
            let source = match arr.next() {
                Some(s) => s.parse::<usize>()?,
                None => return Err(anyhow!("could not read source of edge {}", e)),
            };
            let target = match arr.next() {
                Some(t) => t.parse::<usize>()?,
                None => return Err(anyhow!("could not read target of edge {}", e)),
            };
            
            edges[source][target] = true;
        }

        Ok(Self {
            empty_traces: empty_traces,
            edges: edges,
            activity_key: activity_key,
            node_2_activity: node_2_activity,
            start_activities: start_activities,
            end_activities: end_activities
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

impl Display for DirectlyFollowsModel {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        writeln!(f, "{}", HEADER)?;
        
        writeln!(f, "# empty trace\n{}", self.empty_traces)?;

        //activities
        writeln!(f, "# number of activites\n{}", self.node_2_activity.len())?;
        for (a, activity) in self.node_2_activity.iter().enumerate() {
            writeln!(f, "#activity {}\n{}", a, self.activity_key.get_activity_label(activity));
        }

        //start activities
        writeln!(f, "# number of start activites\n{}", self.start_activities.len())?;
        for (i, start_activity) in self.start_activities.iter().enumerate() {
            writeln!(f, "# start activity {}\n{}", i, start_activity)?;
        }

        //end activities
        writeln!(f, "# number of end activites\n{}", self.end_activities.len())?;
        for (i, end_activity) in self.end_activities.iter().enumerate() {
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

impl Dottable for DirectlyFollowsModel {
    fn to_dot(&self) -> layout::topo::layout::VisualGraph {
        let mut graph = VisualGraph::new(layout::core::base::Orientation::LeftToRight);

        //source + sink
        let source = <dyn Dottable>::create_place(&mut graph, "");
        let sink = <dyn Dottable>::create_place(&mut graph, "");

        //empty traces
        if self.empty_traces {
            <dyn Dottable>::create_edge(&mut graph, &source, &sink, "");
        }

        //nodes
        let mut nodes = vec![];
        for n in &self.node_2_activity {
            nodes.push(<dyn Dottable>::create_transition(&mut graph, self.activity_key.get_activity_label(n), ""));
        }

        //start activities
        for start_activity in self.start_activities.iter() {
            <dyn Dottable>::create_edge(&mut graph, &source, &nodes[*start_activity], "");
        }

        //end activities
        for end_activity in self.end_activities.iter() {
            <dyn Dottable>::create_edge(&mut graph, &nodes[*end_activity], &sink, "");
        }

        //edges
        for source in 0..self.edges.len() {
            for target in 0..self.edges.len() {
                if self.edges[source][target] {
                    <dyn Dottable>::create_edge(&mut graph, &nodes[source], &nodes[target], "");
                }
            }
        }

        return graph;
    }
}

impl Exportable for DirectlyFollowsModel {
    fn export_from_object(object: EbiOutput, f: &mut dyn std::io::Write) -> Result<()> {
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