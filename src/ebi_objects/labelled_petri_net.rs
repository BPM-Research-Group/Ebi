use std::{fmt::{self, Debug}, hash::{Hash, Hasher}, io::{self, BufRead}, str::FromStr};
use anyhow::{anyhow, Context, Error, Ok, Result};
use bitvec::vec::BitVec;
use layout::topo::layout::VisualGraph;

use crate::{ebi_framework::{activity_key::{Activity, ActivityKey}, displayable::Displayable, dottable::Dottable, ebi_file_handler::EbiFileHandler, ebi_input::{self, EbiInput, EbiObjectImporter, EbiTraitImporter}, ebi_object::EbiObject, ebi_output::{EbiObjectExporter, EbiOutput}, ebi_trait::FromEbiTraitObject, exportable::Exportable, importable::Importable, infoable::Infoable, prom_link::JavaObjectHandler}, ebi_traits::{ebi_trait_semantics::{EbiTraitSemantics, Semantics}, ebi_trait_stochastic_semantics::TransitionIndex}, line_reader::LineReader, marking::Marking};

use super::stochastic_labelled_petri_net::StochasticLabelledPetriNet;


pub const HEADER: &str = "labelled Petri net";

pub const EBI_LABELLED_PETRI_NET: EbiFileHandler = EbiFileHandler {
    name: "labelled Petri net",
    article: "a",
    file_extension: "lpn",
    validator: ebi_input::validate::<LabelledPetriNet>,
    trait_importers: &[
        EbiTraitImporter::Semantics(LabelledPetriNet::import_as_semantics),
    ],
    object_importers: &[
        EbiObjectImporter::LabelledPetriNet(LabelledPetriNet::import_as_object),
    ],
    object_exporters: &[
        EbiObjectExporter::LabelledPetriNet(LabelledPetriNet::export_from_object),
        EbiObjectExporter::StochasticLabelledPetriNet(LabelledPetriNet::export_from_stochastic_labelled_petri_net),
        EbiObjectExporter::StochasticDeterministicFiniteAutomaton(LabelledPetriNet::export_from_stochastic_deterministic_finite_automaton),
        EbiObjectExporter::DirectlyFollowsModel(LabelledPetriNet::export_from_directly_follows_model),
    ],
    java_object_handlers: &[
        JavaObjectHandler{ 
            name: "AcceptingPetriNet", 
            translator_ebi_to_java: "org.processmining.ebi.objects.EbiLabelledPetriNet.EbiLabelledPetriNet2AcceptingPetrinet", 
            translator_java_to_ebi: Some("org.processmining.ebi.objects.EbiLabelledPetriNet.AcceptingPetrinet2EbiLabelledPetriNet"),
            java_class: "org.processmining.acceptingpetrinet.models.AcceptingPetriNet"
        }
    ],
};

#[derive(Clone,Debug)]
pub struct LabelledPetriNet {
    pub(crate) activity_key: ActivityKey,
    pub(crate) initial_marking: Marking,
    pub(crate) labels: Vec<Option<Activity>>,
    pub(crate) place2output_transitions: Vec<Vec<usize>>,
    pub(crate) transition2input_places:  Vec<Vec<usize>>,
	pub(crate) transition2output_places: Vec<Vec<usize>>,
    pub(crate) transition2input_places_cardinality: Vec<Vec<u64>>,
    pub(crate) transition2output_places_cardinality: Vec<Vec<u64>>, //fields are the same as in StochasticLabelledPetriNet
}

impl LabelledPetriNet {

    pub fn import_as_semantics(reader: &mut dyn BufRead) -> Result<EbiTraitSemantics> {
        let net = Self::import(reader)?;
        Ok(EbiTraitSemantics::Marking(Box::new(net)))
    }

    pub fn get_number_of_transitions(&self) -> usize {
        self.transition2input_places.len()
    }

    pub fn get_number_of_places(&self) -> usize {
        self.place2output_transitions.len()
    }

    pub fn get_initial_marking(&self) -> &Marking {
        &self.initial_marking
    }

    pub fn get_activity_key(&self) -> &ActivityKey {
        &self.activity_key
    }
    
    pub fn get_activity_key_mut(&mut self) -> &mut ActivityKey {
        &mut self.activity_key
    }
    
    pub fn is_transition_silent(&self, transition: TransitionIndex) -> bool {
        self.labels[transition].is_some()
    }
    
    pub fn get_transition_label(&self, transition: TransitionIndex) -> Option<Activity> {
        self.labels[transition]
    }

    pub fn new() -> Self {
        Self {
            activity_key: ActivityKey::new(),
            initial_marking: Marking::new(0),
            labels: vec![],
            place2output_transitions: vec![],
            transition2input_places: vec![],
            transition2output_places: vec![],
            transition2input_places_cardinality: vec![],
            transition2output_places_cardinality: vec![],
        }
    }

    pub fn add_place(&mut self) -> usize {
        let place = self.get_number_of_places();
        self.place2output_transitions.push(vec![]);
        self.initial_marking.add_place();
        place
    }

    pub fn get_initial_marking_mut(&mut self) -> &mut Marking {
        &mut self.initial_marking
    }

    pub fn add_transition(&mut self, label: Option<Activity>) -> TransitionIndex {
        self.labels.push(label);
        self.transition2input_places.push(vec![]);
        self.transition2input_places_cardinality.push(vec![]);
        self.transition2output_places.push(vec![]);
        self.transition2output_places_cardinality.push(vec![]);
        self.get_number_of_transitions() - 1
    }

    pub fn add_transition_place_arc(&mut self, from_transition: TransitionIndex, to_place: usize, cardinality: u64) -> Result<()> {
        if from_transition >= self.get_number_of_transitions() {
            return Err(anyhow!("non-existing transition {} referenced, while there are {}", from_transition, self.get_number_of_transitions()));
        } else if to_place >= self.get_number_of_places() {
            return Err(anyhow!("non-existing place {} referenced, while there are {}", to_place, self.get_number_of_places()));
        }

        if let Some(pos) = self.transition2output_places[from_transition].iter().position(|p| *p == to_place) {
            self.transition2output_places_cardinality[from_transition][pos] += cardinality;
        } else {
            self.transition2output_places[from_transition].push(to_place);
            self.transition2output_places_cardinality[from_transition].push(1);
        }
        Ok(())
    }

    pub fn add_place_transition_arc(&mut self, from_place: usize, to_transition: TransitionIndex, cardinality: u64) -> Result<()> {
        if to_transition >= self.get_number_of_transitions() {
            return Err(anyhow!("non-existing transition {} referenced, while there are {}", to_transition, self.get_number_of_transitions()));
        } else if from_place >= self.get_number_of_places() {
            return Err(anyhow!("non-existing place {} referenced, while there are {}", from_place, self.get_number_of_places()));
        }

        self.place2output_transitions[from_place].push(to_transition);

        if let Some(pos) = self.transition2input_places[to_transition].iter().position(|p| *p == from_place) {
            self.transition2input_places_cardinality[to_transition][pos] += cardinality;
        } else {
            self.transition2input_places[to_transition].push(from_place);
            self.transition2input_places_cardinality[to_transition].push(1);
        }
        Ok(())
    }

    fn export_from_stochastic_labelled_petri_net(object: EbiOutput, f: &mut dyn std::io::Write) -> Result<()> {
        match object {
            EbiOutput::Object(EbiObject::StochasticLabelledPetriNet(slpn)) => <StochasticLabelledPetriNet as Into<LabelledPetriNet>>::into(slpn).export(f),
            _ => unreachable!()
        }
    }

    fn export_from_stochastic_deterministic_finite_automaton(object: EbiOutput, f: &mut dyn std::io::Write) -> Result<()> {
        match object {
            EbiOutput::Object(EbiObject::StochasticDeterministicFiniteAutomaton(sdfa)) => <StochasticLabelledPetriNet as Into<LabelledPetriNet>>::into(sdfa.get_stochastic_labelled_petri_net()).export(f),
            _ => unreachable!()
        }
    }

    fn export_from_directly_follows_model(object: EbiOutput, f: &mut dyn std::io::Write) -> Result<()> {
        match object {
            EbiOutput::Object(EbiObject::DirectlyFollowsModel(dfm)) => dfm.get_labelled_petri_net().export(f),
            _ => unreachable!()
        }
    }

    
}

impl FromEbiTraitObject for LabelledPetriNet {
    fn from_trait_object(object: EbiInput) -> Result<Box<Self>> {
        match object {
            EbiInput::Object(EbiObject::LabelledPetriNet(e), _) => Ok(Box::new(e)),
            _ => Err(anyhow!("cannot read {} {} as a finite language", object.get_type().get_article(), object.get_type()))
        }
    }
}

impl Exportable for LabelledPetriNet {

    fn export_from_object(object: EbiOutput, f: &mut dyn std::io::Write) -> Result<()> {
        match object {
            EbiOutput::Object(EbiObject::LabelledPetriNet(lpn)) => lpn.export(f),
            _ => unreachable!()
        }
    }

    fn export(&self, f: &mut dyn std::io::Write) -> Result<()> {
        Ok(write!(f, "{}", self)?)
    }
}

impl Infoable for LabelledPetriNet {
    fn info(&self, f: &mut impl std::io::Write) -> Result<()> {
        writeln!(f, "Number of places\t\t{}", self.get_number_of_places())?;
        writeln!(f, "Number of transitions\t\t{}", self.get_number_of_transitions())?;
        writeln!(f, "Number of activities\t\t{}", Semantics::get_activity_key(self).get_number_of_activities())?;
        writeln!(f, "Number of silent transitions\t{}", (0..self.get_number_of_transitions()).into_iter().filter(|transition| self.is_transition_silent(*transition)).count())?;

        Ok(write!(f, "")?)
    }
}

impl fmt::Display for LabelledPetriNet {
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

impl FromStr for LabelledPetriNet {
    type Err = Error;

    fn from_str(s: &str) -> std::prelude::v1::Result<Self, Self::Err> {
        let mut reader = io::Cursor::new(s);
        Self::import(&mut reader)
    }
}

impl Importable for LabelledPetriNet {

    fn import_as_object(reader: &mut dyn BufRead) -> Result<EbiObject> {
        Ok(EbiObject::LabelledPetriNet(Self::import(reader)?))
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


        Ok(LabelledPetriNet {
            activity_key,
            initial_marking: Marking {place2token: initial_marking},
            labels,
            place2output_transitions,
            transition2input_places,
            transition2output_places,
            transition2input_places_cardinality,
            transition2output_places_cardinality
        })
    }
}

impl Dottable for LabelledPetriNet {
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
                <dyn Dottable>::create_transition(&mut graph, self.activity_key.get_activity_label(&activity), "")
            } else {
                <dyn Dottable>::create_silent_transition(&mut graph, "")
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

impl From<StochasticLabelledPetriNet> for LabelledPetriNet {
    fn from(value: StochasticLabelledPetriNet) -> Self {
        Self {
            activity_key: value.activity_key,
            initial_marking: value.initial_marking,
            labels: value.labels,
            place2output_transitions: value.place2output_transitions,
            transition2input_places: value.transition2input_places,
            transition2output_places: value.transition2output_places,
            transition2input_places_cardinality: value.transition2input_places_cardinality,
            transition2output_places_cardinality: value.transition2output_places_cardinality,
        }
    }
}

#[derive(Clone)]
pub struct LPNMarking {
    pub(crate) marking: Marking,
    pub(crate) enabled_transitions: BitVec,
    pub(crate) number_of_enabled_transitions: usize,
}

impl Displayable for LPNMarking {
    fn debug(&self) -> String {
        return "SLPN marking".to_string();
    }
}

impl Eq for LPNMarking {}

impl Hash for LPNMarking {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.marking.hash(state);
    }
}

impl PartialEq for LPNMarking {
    fn eq(&self, other: &Self) -> bool {
        self.marking == other.marking
    }
}

impl fmt::Display for LPNMarking {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.marking)
    }
}

impl Debug for LPNMarking {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{:?}", self.marking)
    }
}