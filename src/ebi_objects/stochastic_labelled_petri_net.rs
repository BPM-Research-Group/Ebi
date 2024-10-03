use std::io;
use std::str::FromStr;
use std::{fmt, io::BufRead, rc::Rc};
use anyhow::{anyhow, Result, Context, Error};
use layout::topo::layout::VisualGraph;

use crate::deterministic_semantics_for_stochastic_semantics::DeterministicStochasticSemantics;
use crate::ebi_framework::activity_key::{Activity, ActivityKey};
use crate::ebi_framework::dottable::Dottable;
use crate::ebi_framework::ebi_file_handler::EbiFileHandler;
use crate::ebi_framework::ebi_input::{self, EbiObjectImporter, EbiTraitImporter};
use crate::ebi_framework::ebi_object::EbiObject;
use crate::ebi_framework::ebi_output::{EbiObjectExporter, EbiOutput};
use crate::ebi_framework::exportable::Exportable;
use crate::ebi_framework::importable::Importable;
use crate::ebi_framework::infoable::Infoable;
use crate::ebi_framework::prom_link::JavaObjectHandler;
use crate::ebi_traits::ebi_trait_queriable_stochastic_language;
use crate::ebi_traits::ebi_trait_semantics::{EbiTraitSemantics, Semantics};
use crate::ebi_traits::ebi_trait_stochastic_deterministic_semantics::EbiTraitStochasticDeterministicSemantics;
use crate::ebi_traits::ebi_trait_stochastic_semantics::{EbiTraitStochasticSemantics, TransitionIndex};
use crate::line_reader::LineReader;
use crate::marking::Marking;
use crate::math::fraction::Fraction;

use super::labelled_petri_net::LabelledPetriNet;

pub const HEADER: &str = "stochastic labelled Petri net";

pub const EBI_STOCHASTIC_LABELLED_PETRI_NET: EbiFileHandler = EbiFileHandler {
    name: "stochastic labelled Petri net",
    article: "a",
    file_extension: "slpn",
    validator: ebi_input::validate::<StochasticLabelledPetriNet>,
    trait_importers: &[
        EbiTraitImporter::QueriableStochasticLanguage(ebi_trait_queriable_stochastic_language::import::<StochasticLabelledPetriNet>),
        EbiTraitImporter::StochasticSemantics(StochasticLabelledPetriNet::import_as_stochastic_semantics),
        EbiTraitImporter::StochasticDeterministicSemantics(StochasticLabelledPetriNet::import_as_deterministic_stochastic_semantics),
        EbiTraitImporter::Semantics(StochasticLabelledPetriNet::import_as_semantics),
    ],
    object_importers: &[
        EbiObjectImporter::StochasticLabelledPetriNet(StochasticLabelledPetriNet::import_as_object),
        EbiObjectImporter::LabelledPetriNet(StochasticLabelledPetriNet::import_as_labelled_petri_net)
    ],
    object_exporters: &[
        EbiObjectExporter::StochasticLabelledPetriNet(StochasticLabelledPetriNet::export_from_object)
    ],
    java_object_handlers: &[
        JavaObjectHandler{ 
            name: "StochasticLabelledPetriNet", 
            translator_ebi_to_java: "org.processmining.ebi.objects.EbiStochasticLabelledPetriNet.EbiLabelledPetriNet2LabelledPetriNet", 
            translator_java_to_ebi: Some("org.processmining.ebi.objects.EbiStochasticLabelledPetriNet.LabelledPetriNet2EbiLabelledPetriNet"),
            java_class: "org.processmining.stochasticlabelledpetrinets.StochasticLabelledPetriNet",
            input_gui: None,
        },
    ],
};

#[derive(Clone,Debug)]
pub struct StochasticLabelledPetriNet {
    pub(crate) activity_key: ActivityKey,
    pub(crate) initial_marking: Marking,
    pub(crate) labels: Vec<Option<Activity>>,
    pub(crate) transition2input_places:  Vec<Vec<usize>>,
	pub(crate) transition2output_places: Vec<Vec<usize>>,
    pub(crate) transition2input_places_cardinality: Vec<Vec<u64>>,
    pub(crate) transition2output_places_cardinality: Vec<Vec<u64>>, //fields are the same as in LabelledPetriNet
    pub(crate) place2output_transitions: Vec<Vec<usize>>,
    pub(crate) weights: Vec<Fraction>,
}

impl StochasticLabelledPetriNet {

    pub fn import_as_semantics(reader: &mut dyn BufRead) -> Result<EbiTraitSemantics> {
        let slpn = Self::import(reader)?;
        Ok(EbiTraitSemantics::Marking(Box::new(slpn)))
    }

    pub fn import_as_stochastic_semantics(reader: &mut dyn BufRead) -> Result<EbiTraitStochasticSemantics> {
        let slpn = Self::import(reader)?;
        Ok(EbiTraitStochasticSemantics::Marking(Box::new(slpn)))
    }

    pub fn get_deterministic_stochastic_semantics(self) -> EbiTraitStochasticDeterministicSemantics {
        EbiTraitStochasticDeterministicSemantics::PMarking(Box::new(DeterministicStochasticSemantics::new(Rc::new(self))))
    }

    pub fn import_as_deterministic_stochastic_semantics(reader: &mut dyn BufRead) -> Result<EbiTraitStochasticDeterministicSemantics> {
        let net = Self::import(reader)?;
        Ok(net.get_deterministic_stochastic_semantics())
    }

    pub fn import_as_labelled_petri_net(reader: &mut dyn BufRead) -> Result<EbiObject> {
        let net = Self::import(reader)?;
        Ok(EbiObject::LabelledPetriNet(net.into()))
    }

    pub fn get_number_of_places(&self) -> usize {
        self.place2output_transitions.len()
    }

    pub fn get_number_of_transitions(&self) -> usize {
        self.transition2input_places.len()
    }

    pub fn get_initial_marking(&self) -> &Marking {
        &self.initial_marking
    }

    pub fn is_transition_silent(&self, transition: TransitionIndex) -> bool {
        self.labels[transition].is_some()
    }
    
    pub fn get_transition_label(&self, transition: TransitionIndex) -> Option<Activity> {
        self.labels[transition]
    }

    pub fn get_transition_weight(&self, transition: TransitionIndex) -> &Fraction {
        &self.weights[transition]
    }
}

impl Exportable for StochasticLabelledPetriNet {
    
    fn export_from_object(object: EbiOutput, f: &mut dyn std::io::Write) -> Result<()> {
        match object {
            EbiOutput::Object(EbiObject::StochasticLabelledPetriNet(log)) => log.export(f),
            _ => unreachable!()
        }
    }

    fn export(&self, f: &mut dyn std::io::Write) -> Result<()> {
        Ok(write!(f, "{}", self)?)
    }
}

impl Infoable for StochasticLabelledPetriNet {
    fn info(&self, f: &mut impl std::io::Write) -> Result<()> {
        writeln!(f, "Number of places\t\t{}", self.get_number_of_places())?;
        writeln!(f, "Number of transitions\t\t{}", self.get_number_of_transitions())?;
        writeln!(f, "Number of activities\t\t{}", Semantics::get_activity_key(self).get_number_of_activities())?;
        writeln!(f, "Number of silent transitions\t{}", (0..self.get_number_of_transitions()).into_iter().filter(|transition| self.is_transition_silent(*transition)).count())?;

        Ok(write!(f, "")?)
    }
}

impl fmt::Display for StochasticLabelledPetriNet {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        writeln!(f, "{}", HEADER)?;
        writeln!(f, "# number of places\n{}", self.get_number_of_places())?;

        writeln!(f, "# initial marking")?;
        for place in self.initial_marking.get_place2token() {
            writeln!(f, "{}", place)?;
        }

        writeln!(f, "# number of transitions\n{}", self.get_number_of_transitions())?;

        for transition in 0..self.get_number_of_transitions() {
            writeln!(f, "# transition {}", transition)?;

            if let Some(activity) = self.get_transition_label(transition) {
                writeln!(f, "label {}", self.activity_key.get_activity_label(&activity))?;
            } else {
                writeln!(f, "silent")?;
            }

            writeln!(f, "# weight\n{}", &self.weights[transition])?;

            writeln!(f, "# number of input places\n{}", self.transition2input_places[transition].len())?;
            for (pos, place) in self.transition2input_places[transition].iter().enumerate() {
                for _ in 0..self.transition2input_places_cardinality[transition][pos] {
                    writeln!(f, "{}", place)?;
                }
            }

            writeln!(f, "# number of output places\n{}", self.transition2output_places[transition].len())?;
            for (pos, place) in self.transition2output_places[transition].iter().enumerate() {
                for _ in 0..self.transition2output_places_cardinality[transition][pos] {
                    writeln!(f, "{}", place)?;
                }
            }
        }

        write!(f, "")
    }
}

impl FromStr for StochasticLabelledPetriNet {
    type Err = Error;

    fn from_str(s: &str) -> std::prelude::v1::Result<Self, Self::Err> {
        let mut reader = io::Cursor::new(s);
        Self::import(&mut reader)
    }
}

impl Importable for StochasticLabelledPetriNet {

    fn import_as_object(reader: &mut dyn BufRead) -> Result<EbiObject> {
        Ok(EbiObject::StochasticLabelledPetriNet(Self::import(reader)?))
    }

    fn import(reader: &mut dyn BufRead) -> Result<Self> {
        let mut lreader = LineReader::new(reader);

        let head = lreader.next_line_string().with_context(|| format!("failed to read header, which should be {}", HEADER))?;
        if head != HEADER {
            return Err(anyhow!("first line should be exactly `{}`, but found `{}` on line `{}`", HEADER, lreader.get_last_line(), lreader.get_last_line_number()));
        }

        let mut activity_key = ActivityKey::new();
        
        let number_of_places = lreader.next_line_index().context("failed to read number of places")?;
        let mut place2output_transitions = vec![vec![]; number_of_places];

        //read initial marking
        let mut initial_marking = vec![0u64; number_of_places];
        for place in 0 .. number_of_places {
            initial_marking[place] = lreader.next_line_natural().with_context(|| format!("failed to read initial marking of place {}", place))?;
        }

        //read transitions
        let number_of_transitions = lreader.next_line_index().context("failed to read number of transitions")?;

        let mut weights = Vec::with_capacity(number_of_transitions);
        let mut labels = Vec::with_capacity(number_of_transitions);
        let mut transition2input_places: Vec<Vec<usize>> = vec![vec![]; number_of_transitions];
        let mut transition2output_places: Vec<Vec<usize>> = vec![vec![]; number_of_transitions];
        let mut transition2input_places_cardinality: Vec<Vec<u64>> = vec![vec![]; number_of_transitions];
        let mut transition2output_places_cardinality: Vec<Vec<u64>> = vec![vec![]; number_of_transitions];
        
        for transition in 0 .. number_of_transitions {
            let label_line = lreader.next_line_string().with_context(|| format!("failed to read label of transition {}", transition))?;

            //read label
            if label_line.trim_start().starts_with("label ") {
                let label = label_line.trim_start()[6..].to_string();
                let activity = activity_key.process_activity(&label);
                labels.push(Some(activity));
            } else {
                labels.push(None);
            }

            //read weight
            {
                let weight = lreader.next_line_weight().with_context(|| format!("failed to read weight of transition {}", transition))?;
                weights.push(weight);
            }

            //read input places
            {
                let number_of_input_places = lreader.next_line_index().with_context(|| format!("Failed to read number of input places of transition {}.", transition))?;                
                for p in 0 .. number_of_input_places {
                    let place = lreader.next_line_index().with_context(|| format!("Failed to read input place number {} of transition {}.", p, transition))?;

                    if place >= number_of_places {
                        return Err(anyhow!("Non-existing place referenced for transition {}, input place number {}, at line {}; found `{}`.", transition, p, lreader.get_last_line_number(), lreader.get_last_line()));
                    }

                    place2output_transitions[place].push(transition);

                    if let Some(pos) = transition2input_places[transition].iter().position(|p| *p == place) {
                        transition2input_places_cardinality[transition][pos] += 1;
                    } else {
                        transition2input_places[transition].push(place);
                        transition2input_places_cardinality[transition].push(1);
                    }
                }
            }

            //read output places
            {
                let number_of_output_places= lreader.next_line_index().with_context(|| format!("failed to read number of output places of transition {}", transition))?;
                for p in 0 .. number_of_output_places {
                    let place = lreader.next_line_index().with_context(|| format!("failed to read output place number {} of transition {}", p, transition))?;
                    
                    if place >= number_of_places {
                        return Err(anyhow!("non-existing place referenced for transition {}, output place number {}, at line {}; found `{}`", transition, p, lreader.get_last_line_number(), lreader.get_last_line()));
                    }

                    if let Some(pos) = transition2output_places[transition].iter().position(|p| *p == place) {
                        transition2output_places_cardinality[transition][pos] += 1;
                    } else {
                        transition2output_places[transition].push(place);
                        transition2output_places_cardinality[transition].push(1);
                    }
                }
            }
        }


        Ok(StochasticLabelledPetriNet {
            activity_key,
            initial_marking: Marking {place2token: initial_marking},
            labels,
            place2output_transitions,
            transition2input_places,
            transition2output_places,
            transition2input_places_cardinality,
            transition2output_places_cardinality,
            weights,
        })
    }
}

impl From<(LabelledPetriNet, Vec<Fraction>)> for StochasticLabelledPetriNet {
    fn from(value: (LabelledPetriNet, Vec<Fraction>)) -> Self {
        Self {
            activity_key: value.0.activity_key,
            initial_marking: value.0.initial_marking,
            labels: value.0.labels,
            place2output_transitions: value.0.place2output_transitions,
            transition2input_places: value.0.transition2input_places,
            transition2output_places: value.0.transition2output_places,
            transition2input_places_cardinality: value.0.transition2input_places_cardinality,
            transition2output_places_cardinality: value.0.transition2output_places_cardinality,
            weights: value.1,
        }
    }
}

impl Dottable for StochasticLabelledPetriNet {
    fn to_dot(&self) -> VisualGraph {
        let mut graph = VisualGraph::new(layout::core::base::Orientation::LeftToRight);

        let mut places = vec![];
        for place in 0 .. self.get_number_of_places() {

            let label = if let Some(marked) = self.initial_marking.place2token.get(place) {
                if marked > &0 {
                    marked.to_string()
                } else {
                    "".to_string()
                }
            } else {
                "".to_string()
            };

            places.push(<dyn Dottable>::create_place(&mut graph, &label));
        }

        for transition in 0..self.get_number_of_transitions() {

            let node = if let Some(activity) = self.get_transition_label(transition) {
                <dyn Dottable>::create_transition(&mut graph, self.activity_key.get_activity_label(&activity), &self.get_transition_weight(transition).to_string())
            } else {
                <dyn Dottable>::create_silent_transition(&mut graph, &self.get_transition_weight(transition).to_string())
            };

            for (pos, inplace) in self.transition2input_places[transition].iter().enumerate() {
                let place_node = places.get(*inplace).unwrap();
                if self.transition2input_places_cardinality[transition][pos] > 1 {
                    <dyn Dottable>::create_edge(&mut graph, place_node, &node, &format!("{}", self.transition2input_places_cardinality[transition][pos]));
                } else {
                    <dyn Dottable>::create_edge(&mut graph, place_node, &node, "");
                }
            }

            for (pos, outplace) in self.transition2output_places[transition].iter().enumerate() {
                let place_node = places.get(*outplace).unwrap();
                if self.transition2output_places_cardinality[transition][pos] > 1 {
                    <dyn Dottable>::create_edge(&mut graph, &node, place_node, &format!("{}", self.transition2output_places_cardinality[transition][pos]));
                } else {
                    <dyn Dottable>::create_edge(&mut graph, &node, place_node, "");
                }
            }
        }

        return graph;
    }
}