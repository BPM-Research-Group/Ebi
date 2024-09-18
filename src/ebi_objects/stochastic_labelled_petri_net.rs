use std::io;
use std::str::FromStr;
use std::{fmt, io::BufRead, rc::Rc};
use std::collections::HashMap;
use anyhow::{anyhow, Result, Context, Error};
use fraction::{One, Zero};
use layout::topo::layout::VisualGraph;
use rand::{thread_rng,Rng};

use crate::ebi_commands::ebi_command_info::Infoable;
use crate::ebi_traits::ebi_trait_semantics::{EbiTraitSemantics, Semantics};
use crate::ebi_traits::ebi_trait_labelled_petri_net::EbiTraitLabelledPetriNet;
use crate::ebi_traits::ebi_trait_queriable_stochastic_language::EbiTraitQueriableStochasticLanguage;
use crate::ebi_traits::ebi_trait_stochastic_deterministic_semantics::EbiTraitStochasticDeterministicSemantics;
use crate::ebi_traits::ebi_trait_stochastic_semantics::{EbiTraitStochasticSemantics, StochasticSemantics, ToStochasticSemantics};
use crate::export::{EbiObjectExporter, EbiOutput, Exportable};
use crate::file_handler::EbiFileHandler;
use crate::labelled_petri_net_semantics::LabelledPetriNetSemantics;
use crate::math::fraction::Fraction;
use crate::{activity_key::ActivityKey, dottable::Dottable, follower_semantics::FollowerSemantics, import, line_reader::LineReader, marking::Marking, net::{Net, StochasticNet, Transition}, deterministic_semantics_for_stochastic_semantics::{PMarking, DeterministicStochasticSemantics}, stochastic_labelled_petri_net_semantics::StochasticLabelledPetriNetSemantics, trace_probability};
use crate::import::{EbiObjectImporter, EbiTraitImporter, Importable};

use super::ebi_object::EbiObject;
use super::finite_stochastic_language::FiniteStochasticLanguage;
use super::labelled_petri_net::{LPNMarking, LabelledPetriNet};

pub const HEADER: &str = "stochastic labelled Petri net";

pub const EBI_STOCHASTIC_LABELLED_PETRI_NET: EbiFileHandler = EbiFileHandler {
    name: "stochastic labelled Petri net",
    article: "a",
    file_extension: "slpn",
    validator: import::validate::<StochasticLabelledPetriNet>,
    trait_importers: &[
        EbiTraitImporter::QueriableStochasticLanguage(import::read_as_queriable_stochastic_language::<StochasticLabelledPetriNet>),
        EbiTraitImporter::StochasticSemantics(StochasticLabelledPetriNet::import_as_stochastic_semantics),
        EbiTraitImporter::StochasticDeterministicSemantics(StochasticLabelledPetriNet::import_as_deterministic_stochastic_semantics),
        EbiTraitImporter::LabelledPetriNet(StochasticLabelledPetriNet::import_as_labelled_petri_net),
        EbiTraitImporter::Semantics(StochasticLabelledPetriNet::import_as_semantics),
    ],
    object_importers: &[
        EbiObjectImporter::StochasticLabelledPetriNet(StochasticLabelledPetriNet::import_as_object),
        EbiObjectImporter::LabelledPetriNet(StochasticLabelledPetriNet::import_as_labelled_petri_net_object)
    ],
    object_exporters: &[
        EbiObjectExporter::StochasticLabelledPetriNet(StochasticLabelledPetriNet::export_from_object)
    ],
};

#[derive(Clone,Debug)]
pub struct StochasticLabelledPetriNet {
    activity_key: ActivityKey,
    places: usize, //number of places in the net
    transitions: Vec<Transition>,
    weights: Vec<Fraction>,
    initial_marking: Marking,
    semantics: Option<Box<StochasticLabelledPetriNetSemantics>>
}

impl StochasticLabelledPetriNet {
    pub fn from_fields(activity_key: ActivityKey, places: usize, transitions: Vec<Transition>, initial_marking: Marking, weights: Vec<Fraction>) -> Self{
        Self {
            activity_key: activity_key,
            places: places,
            transitions: transitions,
            initial_marking: initial_marking,
            weights: weights,
            semantics: None
        }
    }

    pub fn to_labelled_petri_net(self) -> LabelledPetriNet {
        LabelledPetriNet::from_fields(self.activity_key, self.places, self.transitions, self.initial_marking)
    }

    pub fn get_activity_index(&self) -> &ActivityKey {
        &self.activity_key
    }

    pub fn get_semantics(net: Box<Self>) -> EbiTraitSemantics {
        let semantics = LabelledPetriNetSemantics::from_lpn(net);
        EbiTraitSemantics::Marking(Box::new(semantics))
    }

    fn import_as_stochastic_semantics(reader: &mut dyn BufRead) -> Result<EbiTraitStochasticSemantics> {
        let slpn = Self::import(reader)?;
        Ok(slpn.get_stochastic_semantics())
    }

    pub fn get_stochastic_semantics(&self) -> EbiTraitStochasticSemantics {
        let stochastic_semantics = StochasticLabelledPetriNetSemantics::from_slpn(self);
        EbiTraitStochasticSemantics::Marking(Box::new(stochastic_semantics))
    }

    pub fn get_deterministic_stochastic_semantics(&self) -> EbiTraitStochasticDeterministicSemantics {
        let stochastic_semantics = StochasticLabelledPetriNetSemantics::from_slpn(self);
        EbiTraitStochasticDeterministicSemantics::PMarking(Box::new(DeterministicStochasticSemantics::new(Rc::new(stochastic_semantics))))
    }

    pub fn import_as_deterministic_stochastic_semantics(reader: &mut dyn BufRead) -> Result<EbiTraitStochasticDeterministicSemantics> {
        let net = Rc::new(Self::import(reader)?);
        Ok(net.get_deterministic_stochastic_semantics())
    }

    pub fn import_as_labelled_petri_net(reader: &mut dyn BufRead) -> Result<Box<dyn EbiTraitLabelledPetriNet>> {
        let net = Self::import(reader)?;
        Ok(Box::new(net))
    }

    pub fn import_as_labelled_petri_net_object(reader: &mut dyn BufRead) -> Result<EbiObject> {
        let net = Self::import(reader)?;
        Ok(EbiObject::LabelledPetriNet(net.to_labelled_petri_net()))
    }

    pub fn import_as_semantics(reader: &mut dyn BufRead) -> Result<EbiTraitSemantics> {
        let net = Box::new(Self::import(reader)?);
        Ok(Self::get_semantics(net))
    }

}

impl EbiTraitLabelledPetriNet for StochasticLabelledPetriNet {
    fn get_activity_key(&self) -> &ActivityKey {
        &self.activity_key
    }

    fn get_activity_key_mut(&mut self) -> &mut ActivityKey {
        &mut self.activity_key
    }

    fn get_number_of_places(&self) -> usize {
        self.places
    }

    fn get_number_of_transitions(&self) -> usize {
        self.transitions.len()
    }

    fn get_transitions(&self) -> &Vec<Transition> {
        &self.transitions
    }

    fn get_initial_marking(&self) -> &Marking {
        &self.initial_marking
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
        writeln!(f, "Number of places\t{}", self.places)?;
        writeln!(f, "Number of transitions\t{}", self.transitions.len())?;
        writeln!(f, "Number of activities\t{}", self.activity_key.get_number_of_activities())?;

        Ok(write!(f, "")?)
    }
}

impl fmt::Display for StochasticLabelledPetriNet {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        writeln!(f, "{}", HEADER)?;
        writeln!(f, "# number of places\n{}", self.places)?;

        writeln!(f, "# initial marking")?;
        for place in self.initial_marking.get_place2token() {
            writeln!(f, "{}", place)?;
        }

        writeln!(f, "# number of transitions\n{}", self.transitions.len())?;

        for (pos, transition) in self.transitions.iter().enumerate() {
            writeln!(f, "# transition {}", transition.index)?;

            if transition.is_silent() {
                writeln!(f, "silent")?;
            } else {
                writeln!(f, "label {}", self.activity_key.get_activity_label(&transition.get_label().unwrap()))?;
            }

            writeln!(f, "# weight\n{}", &self.weights[pos])?;

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
        let mut activity_key = ActivityKey::new();

        let head = lreader.next_line_string().with_context(|| format!("failed to read header, which should be {}", HEADER))?;
        if head != HEADER {
            return Err(anyhow!("first line should be exactly `{}`, but found `{}` on line `{}`", HEADER, lreader.get_last_line(), lreader.get_last_line_number()));
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
        let mut weights: Vec<Fraction> = Vec::new();
        for transition in 0 .. number_of_transitions {
            let label_line = lreader.next_line_string().with_context(|| format!("failed to read label of transition {}", transition))?;

            let mut transitiono: Transition;
            
            //read label
            if label_line.trim_start().starts_with("label ") {
                let label = label_line.trim_start()[6..].to_string();
                transitiono  = Transition::new_labelled(transition, activity_key.process_activity(&label));
            } else {
                transitiono = Transition::new_silent(transition);
            }

            //read weight
            {
                let weight = lreader.next_line_weight().with_context(|| format!("failed to read weight of transition {}", transition))?;
                // if !weight.is_positive() {
                //     return Err(anyhow!("Transition {}, at line {}, has a non-positive weight. Found `{}`.", transition, lreader.get_last_line_number(), lreader.get_last_line()));
                // }
                weights.push(weight);
            }

            //read input places
            let mut input_places;
            {
                let number_of_input_places = lreader.next_line_index().with_context(|| format!("Failed to read number of input places of transition {}.", transition))?;                
                input_places = vec![0; number_of_input_places];
                for p in 0 .. number_of_input_places {
                    input_places[p] = lreader.next_line_index().with_context(|| format!("Failed to read input place number {} of transition {}.", p, transition))?;

                    if input_places[p] >= number_of_places {
                        return Err(anyhow!("Non-existing place referenced for transition {}, input place number {}, at line {}; found `{}`.", transition, p, lreader.get_last_line_number(), lreader.get_last_line()));
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


        Ok(StochasticLabelledPetriNet {
            activity_key: activity_key,
            places: number_of_places,
            transitions: transitions,
            weights: weights,
            initial_marking: Marking::from_vec(place2token),
            semantics: None
        })
    }
}

impl From<(Box<dyn EbiTraitLabelledPetriNet>, Vec<Fraction>)> for StochasticLabelledPetriNet {
    fn from(mut value: (Box<dyn EbiTraitLabelledPetriNet>, Vec<Fraction>)) -> Self {
        Self {
            activity_key: value.0.get_activity_key().clone(),
            places: value.0.get_number_of_places(),
            transitions: value.0.get_transitions().clone(),
            weights: value.1,
            initial_marking: value.0.get_initial_marking().clone(),
            semantics: None,
        }
    }
}

impl StochasticNet for StochasticLabelledPetriNet {
    fn get_weight(&self, transition: &Transition) -> &Fraction {
        &self.weights[transition.index]
    }
}

impl Dottable for StochasticLabelledPetriNet {
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
                node = <dyn Dottable>::create_silent_transition(&mut graph, transition.index, &self.get_weight(transition).to_string());
            } else {
                node = <dyn Dottable>::create_transition(&mut graph, self.activity_key.get_activity_label(&transition.get_label().unwrap()), &self.get_weight(transition).to_string());
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


#[derive(Debug)]
struct Prob {
    outgoing_states: Vec<usize>,
    outgoing_state_probabilities: Vec<Fraction>
}


impl EbiTraitQueriableStochasticLanguage for StochasticLabelledPetriNet {

    fn get_probability(&mut self, follower_semantics: &FollowerSemantics) -> Result<Fraction> {
        if self.semantics.is_none() {
            self.semantics = Some(Box::new(StochasticLabelledPetriNetSemantics::from_slpn(self)));
        }


        let binding = self.semantics.as_ref().unwrap();
        let semantics1: &StochasticLabelledPetriNetSemantics = binding.as_ref();

        trace_probability::trace_probability_semantics(semantics1, follower_semantics)
    }
    
    fn get_activity_key(&self) -> &ActivityKey {
        &self.activity_key
    }
    
    fn get_activity_key_mut(&mut self) -> &mut ActivityKey {
        &mut self.activity_key
    }

}