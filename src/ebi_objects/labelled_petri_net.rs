use std::{fmt::{self, Debug, Display}, hash::{Hash, Hasher}, io::{self, BufRead}, rc::Rc, str::FromStr};
use anyhow::{anyhow, Result, Context, Error};
use bitvec::vec::BitVec;
use layout::topo::{layout::VisualGraph, placer::place};

use crate::{activity_key::ActivityKey, dottable::Dottable, ebi_commands::ebi_command_info::Infoable, ebi_traits::{ebi_trait_semantics::{EbiTraitSemantics, Semantics}, ebi_trait_labelled_petri_net::EbiTraitLabelledPetriNet}, export::{Displayable, EbiObjectExporter, EbiOutput, Exportable}, file_handler::EbiFileHandler, import::{self, EbiObjectImporter, EbiTraitImporter, Importable}, line_reader::LineReader, marking::Marking, net::Transition};

use super::{ebi_object::EbiObject, labelled_petri_net_semantics::LabelledPetriNetSemantics};

pub const HEADER: &str = "labelled Petri net";

pub const EBI_LABELLED_PETRI_NET: EbiFileHandler = EbiFileHandler {
    name: "labelled Petri net",
    article: "a",
    file_extension: "lpn",
    validator: import::validate::<LabelledPetriNet>,
    trait_importers: &[
        EbiTraitImporter::LabelledPetriNet(import::read_as_labelled_petri_net::<LabelledPetriNet>),
        EbiTraitImporter::Semantics(LabelledPetriNet::import_as_semantics),
    ],
    object_importers: &[
        EbiObjectImporter::LabelledPetriNet(LabelledPetriNet::import_as_object),
    ],
    object_exporters: &[
        EbiObjectExporter::LabelledPetriNet(LabelledPetriNet::export_from_object),
    ],
};

#[derive(Clone)]
pub struct LabelledPetriNet {
    pub(crate) activity_key: ActivityKey,
    pub(crate) places: usize, //number of places in the net
    pub(crate) transitions: Vec<Transition>,
    pub(crate) initial_marking: Marking,
}

impl LabelledPetriNet {
    pub fn from_fields(activity_key: ActivityKey, places: usize, transitions: Vec<Transition>, initial_marking: Marking) -> Self {
        Self { activity_key: activity_key, places: places, transitions: transitions, initial_marking: initial_marking }
    }

    pub fn get_transitions(&self) -> &Vec<Transition> {
        &self.transitions
    }

    pub fn to_semantics(self) -> Box<LabelledPetriNetSemantics> {
        let stochastic_semantics = LabelledPetriNetSemantics::from_lpn(Box::new(self));
        Box::new(stochastic_semantics)
    }

    pub fn import_as_semantics(reader: &mut dyn BufRead) -> Result<EbiTraitSemantics> {
        let net = Self::import(reader)?;
        Ok(EbiTraitSemantics::Marking(net.to_semantics()))
    }
}

impl EbiTraitLabelledPetriNet for LabelledPetriNet {

    fn get_number_of_transitions(&self) -> usize {
        self.transitions.len()
    }
    
    fn get_transitions(&self) -> &Vec<Transition> {
        &self.transitions
    }

    fn get_number_of_places(&self) -> usize {
        self.places
    }

    fn get_initial_marking(&self) -> &Marking {
        &self.initial_marking
    }

    fn get_activity_key(&self) -> &ActivityKey {
        &self.activity_key
    }
    
    fn get_activity_key_mut(&mut self) -> &mut ActivityKey {
        &mut self.activity_key
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
        writeln!(f, "Number of places\t\t{}", self.places)?;
        writeln!(f, "Number of transitions\t\t{}", self.transitions.len())?;
        writeln!(f, "Number of activities\t\t{}", self.activity_key.get_number_of_activities())?;
        writeln!(f, "Number of silent transitions\t{}", self.transitions.iter().filter(|t| t.is_silent()).count())?;

        Ok(write!(f, "")?)
    }
}

impl fmt::Display for LabelledPetriNet {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        writeln!(f, "{}", HEADER)?;
        
        writeln!(f, "# number of places\n{}", self.places)?;

        writeln!(f, "# initial marking")?;
        for place in self.initial_marking.get_place2token() {
            writeln!(f, "{}", place)?;
        }

        writeln!(f, "# number of transitions\n{}", self.transitions.len())?;
        for (_pos, transition) in self.transitions.iter().enumerate() {
            writeln!(f, "# transition {}", transition.index)?;

            if transition.is_silent() {
                writeln!(f, "silent")?;
            } else {
                writeln!(f, "label {}", self.activity_key.get_activity_label(&transition.get_label().unwrap()))?;
            }

            writeln!(f, "# number of input places\n{}", transition.incoming.len())?;
            for place in &transition.incoming {
                writeln!(f, "{}", place)?;
            }

            writeln!(f, "# number of output places\n{}", transition.outgoing.len())?;
            for place in &transition.outgoing {
                writeln!(f, "{}", place)?;
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
        let mut activity_key = ActivityKey::new();

        let head = lreader.next_line_string().with_context(|| format!("failed to read header, which should be {}", HEADER))?;
        if head != HEADER {
            return Err(anyhow!("first line should be exactly `{}`, but found `{}`", HEADER, lreader.get_last_line()));
        }

        let number_of_places = lreader.next_line_index().context("failed to read number of places")?;

        //read initial marking
        let mut place2token = vec![0u64; number_of_places];
        for place in 0 .. number_of_places {
            place2token[place] = lreader.next_line_natural().with_context(|| format!("failed to read initial marking of place {}", place))?;
        }

        //read transitions
        let number_of_transitions = lreader.next_line_index().context("failed to read number of transitions")?;

        let mut transitions: Vec<Transition> = Vec::new();
        for transition in 0 .. number_of_transitions {
            let label_line = lreader.next_line_string()?;

            let mut transitiono: Transition;
            
            //read label
            if label_line.trim_start().starts_with("label ") {
                let label = label_line.trim_start()[6..].to_string();
                transitiono  = Transition::new_labelled(transition, activity_key.process_activity(&label));
            } else {
                transitiono = Transition::new_silent(transition);
            }

            //read input places
            let mut input_places;
            {
                let number_of_input_places = lreader.next_line_index().with_context(|| format!("failed to read number of input places of transition {}", transition))?;                
                input_places = vec![0; number_of_input_places];
                for p in 0 .. number_of_input_places {
                    input_places[p] = lreader.next_line_index().with_context(|| format!("failed to read input place number {} of transition {}", p, transition))?;

                    if input_places[p] >= number_of_places {
                        return Err(anyhow!("non-existing place referenced for transition {}, input place number {}, at line {}; found `{}`", transition, p, lreader.get_last_line_number(), lreader.get_last_line()));
                    }
                }
            }

            //read output places
            let mut output_places;
            {
                let number_of_output_places= lreader.next_line_index().with_context(|| format!("failed to read number of output places of transition {}", transition))?;
                output_places = vec![0; number_of_output_places];
                for p in 0 .. number_of_output_places {
                    output_places[p] = lreader.next_line_index().with_context(|| format!("failed to read output place number {} of transition {}", p, transition))?;
                    
                    if output_places[p] >= number_of_places {
                        return Err(anyhow!("non-existing place referenced for transition {}, output place number {}, at line {}; found `{}`", transition, p, lreader.get_last_line_number(), lreader.get_last_line()));
                    }
                }
            }

            transitiono.incoming = input_places;
            transitiono.outgoing = output_places;
            transitions.push(transitiono);
        }


        Ok(LabelledPetriNet {
            activity_key: activity_key,
            places: number_of_places,
            transitions: transitions,
            initial_marking: Marking::from_vec(place2token)
        })
    }
}

impl Dottable for LabelledPetriNet {
    fn to_dot(&self) -> VisualGraph {
        let mut graph = VisualGraph::new(layout::core::base::Orientation::LeftToRight);

        let mut places = vec![];
        for place in 0 .. self.places {

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

        for transition in &self.transitions {

            let node;
            if transition.is_silent() {
                node = <dyn Dottable>::create_silent_transition(&mut graph, transition.index, "");
            } else {
                node = <dyn Dottable>::create_transition(&mut graph, self.activity_key.get_activity_label(&transition.get_label().unwrap()), "");
            }

            for inplace in &transition.incoming {
                let place_node = places.get(*inplace).unwrap();
                <dyn Dottable>::create_edge(&mut graph, place_node, &node, "");
            }

            for outplace in &transition.outgoing {
                let place_node = places.get(*outplace).unwrap();
                <dyn Dottable>::create_edge(&mut graph, &node, place_node, "");
            }
        }

        return graph;
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