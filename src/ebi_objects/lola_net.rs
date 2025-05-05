use crate::{
    ebi_framework::{
        activity_key::HasActivityKey,
        ebi_file_handler::EbiFileHandler,
        ebi_input::{self, EbiTraitImporter},
        ebi_object::EbiObject,
        ebi_output::{EbiObjectExporter, EbiOutput},
        exportable::Exportable,
        importable::Importable,
    },
    ebi_traits::{
        ebi_trait_graphable::{self, EbiTraitGraphable},
        ebi_trait_semantics::{EbiTraitSemantics, Semantics},
    },
};
use anyhow::{Context, Result, anyhow};
use itertools::Itertools;
use std::{
    collections::HashMap,
    io::{BufRead, Write},
};

use super::{
    deterministic_finite_automaton::DeterministicFiniteAutomaton,
    directly_follows_model::DirectlyFollowsModel, labelled_petri_net::LabelledPetriNet,
    process_tree::ProcessTree,
    stochastic_deterministic_finite_automaton::StochasticDeterministicFiniteAutomaton,
    stochastic_labelled_petri_net::StochasticLabelledPetriNet,
    stochastic_process_tree::StochasticProcessTree,
};

pub const EBI_LOLA_NET: EbiFileHandler = EbiFileHandler {
    name: "LoLa Petri net",
    article: "a",
    file_extension: "lola",
    format_specification: &FORMAT_SPECIFICATION,
    validator: ebi_input::validate::<LolaNet>,
    trait_importers: &[
        EbiTraitImporter::Semantics(LolaNet::import_as_semantics),
        EbiTraitImporter::Graphable(ebi_trait_graphable::import::<LolaNet>),
    ],
    object_importers: &[],
    object_exporters: &[
        EbiObjectExporter::LabelledPetriNet(LolaNet::export_from_object),
        EbiObjectExporter::DeterministicFiniteAutomaton(LolaNet::export_from_object),
        EbiObjectExporter::StochasticLabelledPetriNet(LolaNet::export_from_object),
        EbiObjectExporter::StochasticDeterministicFiniteAutomaton(LolaNet::export_from_object),
        EbiObjectExporter::DirectlyFollowsModel(LolaNet::export_from_object),
        EbiObjectExporter::ProcessTree(LolaNet::export_from_object),
    ],
    java_object_handlers: &[], //java translations covered by LabelledPetrinet
};

pub const FORMAT_SPECIFICATION: &str =
    "A LoLA Petri net language file adheres to the grammar described in~\\cite{DBLP:conf/apn/Wolf18a}.
    Note that Ebi does not support place bounds or fairness, and that LoLA nets do not support silent transitions, and has some restrictions on labeling.
For instance:
    \\lstinputlisting[language=xml, style=boxed]{../testfiles/mutex.lola}";

pub struct LolaNet(pub(crate) LabelledPetriNet);

impl LolaNet {
    pub fn escape_transition_label(label: &str) -> String {
        let mut result = label.replace(".", ".p");
        result = result.replace(",", ".c");
        result = result.replace(";", ".x");
        result = result.replace("(", ".l");
        result = result.replace(")", ".q");
        result = result.replace("{", ".u");
        result = result.replace("}", ".d");
        result = result.replace("\t", ".t");
        result = result.replace("\n", ".n");
        result = result.replace("\r", ".r");
        result = result.replace(" ", ".s");
        result
    }

    pub fn import_as_semantics(reader: &mut dyn BufRead) -> Result<EbiTraitSemantics> {
        let lola_net = Self::import(reader)?;
        let lpn = LabelledPetriNet::from(lola_net);
        Ok(EbiTraitSemantics::Marking(Box::new(lpn)))
    }
}

impl Importable for LolaNet {
    fn import_as_object(reader: &mut dyn BufRead) -> Result<EbiObject> {
        Ok(EbiObject::LabelledPetriNet(Self::import(reader)?.into()))
    }

    fn import(reader: &mut dyn BufRead) -> Result<Self>
    where
        Self: Sized,
    {
        let mut string = String::new();
        reader.read_to_string(&mut string)?;
        let mut tokeniser = Tokeniser::new(&string);

        let mut lpn = LabelledPetriNet::new();

        let place_id_2_place = tokeniser
            .read_places(&mut lpn)
            .with_context(|| "Parsing places.")?;

        // println!("places {:?}", place_id_2_place);

        tokeniser
            .read_initial_marking(&place_id_2_place, &mut lpn)
            .with_context(|| "Parsing initial marking.")?;

        // println!("marking {:?}", lpn.get_initial_marking());

        tokeniser
            .read_transitions(place_id_2_place, &mut lpn)
            .with_context(|| "Parsing transitions.")?;

        Ok(Self(lpn))
    }
}

impl Exportable for LolaNet {
    fn export_from_object(object: EbiOutput, f: &mut dyn Write) -> Result<()> {
        match object {
            EbiOutput::Object(EbiObject::LabelledPetriNet(lpn)) => LolaNet::from(lpn).export(f),
            EbiOutput::Object(EbiObject::StochasticLabelledPetriNet(slpn)) => {
                <StochasticLabelledPetriNet as Into<LolaNet>>::into(slpn).export(f)
            }
            EbiOutput::Object(EbiObject::StochasticDeterministicFiniteAutomaton(sdfa)) => {
                <StochasticDeterministicFiniteAutomaton as Into<LolaNet>>::into(sdfa).export(f)
            }
            EbiOutput::Object(EbiObject::DirectlyFollowsModel(dfm)) => {
                <DirectlyFollowsModel as Into<LolaNet>>::into(dfm).export(f)
            }
            EbiOutput::Object(EbiObject::DeterministicFiniteAutomaton(dfa)) => {
                <DeterministicFiniteAutomaton as Into<LolaNet>>::into(dfa).export(f)
            }
            EbiOutput::Object(EbiObject::ProcessTree(tree)) => {
                <ProcessTree as Into<LolaNet>>::into(tree).export(f)
            }
            EbiOutput::Object(EbiObject::StochasticProcessTree(tree)) => {
                <StochasticProcessTree as Into<LolaNet>>::into(tree).export(f)
            }
            EbiOutput::Bool(_) => Err(anyhow!("Cannot export boolean as Lolanet.")),
            EbiOutput::ContainsRoot(_) => Err(anyhow!("Cannot export ContainsRoot as Lolanet.")),
            EbiOutput::Fraction(_) => Err(anyhow!("Cannot export fraction as Lolanet.")),
            EbiOutput::LogDiv(_) => Err(anyhow!("Cannot export LogDiv as Lolanet.")),
            EbiOutput::PDF(_) => Err(anyhow!("Cannot export PDF as Lolanet.")),
            EbiOutput::RootLogDiv(_) => Err(anyhow!("Cannot export RootLogDiv as Lolanet.")),
            EbiOutput::SVG(_) => Err(anyhow!("Cannot export SVG as Lolanet.")),
            EbiOutput::String(_) => Err(anyhow!("Cannot export string as Lolanet.")),
            EbiOutput::Usize(_) => Err(anyhow!("Cannot export integer as Lolanet.")),
            EbiOutput::Object(EbiObject::EventLog(_)) => {
                Err(anyhow!("Cannot export event log as Lolanet."))
            }
            EbiOutput::Object(EbiObject::Executions(_)) => {
                Err(anyhow!("Cannot export executions as Lolanet."))
            }
            EbiOutput::Object(EbiObject::FiniteLanguage(_)) => {
                Err(anyhow!("Cannot export finite language as Lolanet."))
            }
            EbiOutput::Object(EbiObject::FiniteStochasticLanguage(_)) => Err(anyhow!(
                "Cannot export finite stochastic language as Lolanet."
            )),
            EbiOutput::Object(EbiObject::LanguageOfAlignments(_)) => {
                Err(anyhow!("Cannot export language of alignments as Lolanet."))
            }
            EbiOutput::Object(EbiObject::StochasticLanguageOfAlignments(_)) => Err(anyhow!(
                "Cannot export stochastic language of alignments as Lolanet."
            )),
        }
    }

    fn export(&self, f: &mut dyn std::io::Write) -> Result<()> {
        writeln!(f, "PLACE")?;
        let mut it = (0..self.0.get_number_of_places()).into_iter().peekable();
        while let Some(place) = it.next() {
            write!(f, "place.{}", place)?;
            if it.peek().is_some() {
                writeln!(f, ",")?;
            } else {
                writeln!(f, ";")?;
            }
        }

        writeln!(f, "MARKING")?;
        writeln!(
            f,
            "{};",
            (0..self.0.get_number_of_places())
                .into_iter()
                .filter_map(|place| {
                    if self.0.get_initial_marking().place2token[place] > 0 {
                        Some(self.0.get_initial_marking().place2token[place])
                    } else {
                        None
                    }
                })
                .join(",\n")
        )?;

        for transition in 0..self.0.get_number_of_transitions() {
            write!(f, "TRANSITION")?;
            if let Some(activity) = self.0.get_transition_activity(transition) {
                //labelled transition
                writeln!(
                    f,
                    "{}.{}",
                    Self::escape_transition_label(
                        self.0.get_activity_key().get_activity_label(&activity)
                    ),
                    transition
                )?;
            } else {
                //silent transition
                writeln!(f, "silent.{}", transition)?;
            }

            writeln!(
                f,
                "CONSUME {};",
                self.0.transition2input_places[transition]
                    .iter()
                    .enumerate()
                    .map(|(place_pos, place)| {
                        let arc_weight =
                            self.0.transition2input_places_cardinality[transition][place_pos];
                        format!("place.{}: {}", place, arc_weight)
                    })
                    .join(", ")
            )?;

            writeln!(
                f,
                "PRODUCE {};",
                self.0.transition2output_places[transition]
                    .iter()
                    .enumerate()
                    .map(|(place_pos, place)| {
                        let arc_weight =
                            self.0.transition2output_places_cardinality[transition][place_pos];
                        format!("place.{}: {}", place, arc_weight)
                    })
                    .join(", ")
            )?;
        }

        Ok(())
    }
}

impl EbiTraitGraphable for LolaNet {
    fn to_dot(&self) -> Result<layout::topo::layout::VisualGraph> {
        self.0.to_dot()
    }
}

struct Tokeniser<'a> {
    remainder: &'a str,
}

impl<'a> Tokeniser<'a> {
    fn new(string: &'a str) -> Self {
        Self {
            remainder: &string.trim(),
        }
    }

    fn next(&mut self) -> Option<&str> {
        if self.remainder.is_empty() {
            //no more tokens
            return None;
        } else if let Some(mut index) = self.remainder.find(&[' ', '\t', '\n', '\r', ',', ';', ':'])
        {
            let mut token;
            let remainder;
            if index == 0 {
                //, and ; are tokens, so preserve them (note that trim has been applied as an invariant)
                if let Some((pos_next, _)) = self.remainder.char_indices().nth(2) {
                    // println!("\tfound , or ' at {}, pos {}", index, pos_next);
                    index = pos_next;
                    (token, remainder) = self.remainder.split_at(index);
                    token = token.trim_end();
                } else {
                    //last token
                    token = self.remainder;
                    remainder = "";
                }
            } else {
                (token, remainder) = self.remainder.split_at(index);
            }
            self.remainder = remainder.trim_start();
            // println!("token {}", token);
            Some(token)
        } else {
            //last token
            let token = self.remainder;
            self.remainder = "";
            // println!("token {}", token);
            Some(token)
        }
    }

    fn is_identifier(string: &str) -> bool {
        !string.contains([',', ';', '(', ')', '{', '}', '\t', '\n', '\r', ' '])
    }

    fn read_places(&mut self, lpn: &mut LabelledPetriNet) -> Result<HashMap<String, usize>> {
        //read place
        match self.next() {
            Some("PLACE") => {}
            Some(x) => return Err(anyhow!("expected 'PLACE', got '{}'", x)),
            None => return Err(anyhow!("expected PLACE")),
        };

        let mut map = HashMap::new();

        //read places
        while let Some(token) = self.next() {
            match token {
                ";" => {
                    if map.is_empty() {
                        return Err(anyhow!("Found no places."));
                    } else {
                        return Ok(map);
                    }
                } //end of place list
                "," => {} //next place hereafter
                "SAFE" => {
                    return Err(anyhow!(
                        "Ebi does not support place bounds. As this potentially changes the semantics of the net, Ebi will not import this file."
                    ));
                }
                _ if Self::is_identifier(token) => match map.entry(token.to_string()) {
                    std::collections::hash_map::Entry::Occupied(_) => {
                        return Err(anyhow!("The place '{}' was encountered twice.", token));
                    }
                    std::collections::hash_map::Entry::Vacant(vacant_entry) => {
                        vacant_entry.insert(lpn.add_place());
                    }
                },
                _ => {
                    return Err(anyhow!(
                        "Place identifier '{}' contains a forbidden character.",
                        token
                    ));
                }
            }
        }
        return Err(anyhow!("Expected ';' to close places."));
    }

    fn read_initial_marking(
        &mut self,
        place_id_2_place: &HashMap<String, usize>,
        lpn: &mut LabelledPetriNet,
    ) -> Result<()> {
        //read marking
        match self.next() {
            Some("MARKING") => {}
            Some(x) => return Err(anyhow!("Expected 'MARKING', got '{}'.", x)),
            None => return Err(anyhow!("Expected MARKING.")),
        };

        //read places
        let mut place: Option<&usize> = None;
        let mut cardinality = None;
        let mut cardinality_must_be_next = false;
        let marking = lpn.get_initial_marking_mut();
        while let Some(token) = self.next() {
            if cardinality_must_be_next {
                if let Ok(number) = token.parse::<u64>() {
                    cardinality = Some(number);
                    cardinality_must_be_next = false;
                } else {
                    return Err(anyhow!(
                        "Expected a cardinality after ':' in the initial marking."
                    ));
                }
            } else {
                match token {
                    ";" => {
                        //end of marking list

                        //process last place assignment
                        if let Some(place_s) = place {
                            if let Some(c) = cardinality {
                                marking.increase(*place_s, c)?;
                            } else {
                                marking.increase(*place_s, 1)?;
                            }
                            return Ok(());
                        } else {
                            return Err(anyhow!("Expected place, found ',' in initial marking."));
                        }
                    }
                    "," => {
                        //place assignment finished
                        if let Some(place_s) = place {
                            if let Some(c) = cardinality {
                                marking.increase(*place_s, c)?;
                            } else {
                                marking.increase(*place_s, 1)?;
                            }
                            cardinality = None;
                            place = None;
                        } else {
                            return Err(anyhow!("Expected place, found ',' in initial marking."));
                        }
                    }
                    ":" => {
                        if place.is_some() && cardinality.is_none() {
                            //after a colon, the cardinality must be next
                            cardinality_must_be_next = true;
                        } else {
                            return Err(anyhow!("Unexpected ':' found in initial marking."));
                        }
                    }
                    _ => {
                        if place.is_none() && cardinality.is_none() {
                            if let Some(place_s) = place_id_2_place.get(token) {
                                place = Some(place_s);
                            } else {
                                return Err(anyhow!(
                                    "Undeclared place '{}' mentioned in initial marking.",
                                    token
                                ));
                            }
                        } else {
                            return Err(anyhow!(
                                "Unexpected '{}' found in initial marking.",
                                token
                            ));
                        }
                    }
                }
            }
        }
        return Err(anyhow!("Expected ';' to close marking."));
    }

    fn read_transitions(
        &mut self,
        place_id_2_place: HashMap<String, usize>,
        lpn: &mut LabelledPetriNet,
    ) -> Result<()> {
        #[derive(PartialEq)]
        enum State {
            Start,
            SeenTransition,
            SeenLabel(usize), //transition index
            SeenStrongWeak(usize),
            SeenConsume(usize),
            SeenConsumeSemicolon(usize),
            SeenConsumePlace(usize, usize), //transition index, place index
            SeenConsumePlaceColon(usize, usize),
            SeenConsumePlaceColonCardinality(usize), //transition index
            SeenProduce(usize),                      //transition index
            SeenProducePlace(usize, usize),          //transition index, place index
            SeenProducePlaceColon(usize, usize),
            SeenProducePlaceColonCardinality(usize), //transition index
        }
        let mut state = State::Start;

        while let Some(token) = self.next() {
            match state {
                State::Start => {
                    if token == "TRANSITION" {
                        state = State::SeenTransition;
                    } else {
                        return Err(anyhow!("Expected 'TRANSITION' but found '{}'.", token));
                    }
                }
                State::SeenTransition => {
                    if Self::is_identifier(token) {
                        let label = lpn.get_activity_key_mut().process_activity(token);
                        let transition = lpn.add_transition(Some(label));
                        state = State::SeenLabel(transition);
                    } else {
                        return Err(anyhow!(
                            "Transition identifier '{}' contains a forbidden character.",
                            token
                        ));
                    }
                }
                State::SeenLabel(transition) => match token {
                    "STRONG" | "WEAK" => state = State::SeenStrongWeak(transition),
                    "CONSUME" => state = State::SeenConsume(transition),
                    _ => {
                        return Err(anyhow!(
                            "Expected 'CONSUME', 'STRONG' or 'WEAK', but found {}.",
                            token
                        ));
                    }
                },
                State::SeenStrongWeak(transition) => {
                    if token == "FAIR" {
                        state = State::SeenLabel(transition); //cutting a corner: two fairness may be given
                    } else {
                        return Err(anyhow!("Expected 'FAIR' but found '{}'.", token));
                    }
                }
                State::SeenConsume(transition) => match token {
                    ";" => state = State::SeenConsumeSemicolon(transition), //end of list
                    _ if Self::is_identifier(token) => {
                        if let Some(place) = place_id_2_place.get(token) {
                            lpn.add_place_transition_arc(*place, transition, 1)?;
                            state = State::SeenConsumePlace(transition, *place);
                        } else {
                            return Err(anyhow!("Undeclared place '{}' mentioned.", token));
                        }
                    }
                    _ => {
                        return Err(anyhow!(
                            "Place identifier '{}' contains a forbidden character.",
                            token
                        ));
                    }
                },
                State::SeenConsumePlace(transition, place) => match token {
                    ":" => state = State::SeenConsumePlaceColon(transition, place),
                    ";" => state = State::SeenConsumeSemicolon(transition),
                    "," => state = State::SeenConsume(transition), //cutting corner: ,; is allowed.
                    _ => {
                        return Err(anyhow!("Expected ':', ';', ',', but found {}.", token));
                    }
                },
                State::SeenConsumePlaceColon(transition, place) => {
                    if let Ok(cardinality) = token.parse::<u64>() {
                        //the arc was already added with cardinality 1
                        if cardinality == 0 {
                            return Err(anyhow!(
                                "Attempted to add an arc with arc weight 0 in consumers of transition {}.",
                                transition
                            ));
                        } else if cardinality > 1 {
                            lpn.add_place_transition_arc(place, transition, cardinality - 1)?;
                        }
                        state = State::SeenConsumePlaceColonCardinality(transition);
                    } else {
                        return Err(anyhow!(
                            "Expected a cardinality, found '{}' for consumers of transition {}.",
                            token,
                            transition
                        ));
                    }
                }
                State::SeenConsumePlaceColonCardinality(transition) => match token {
                    ";" => state = State::SeenConsumeSemicolon(transition),
                    "," => state = State::SeenConsume(transition), //cutting corner: ,; is allowed.
                    _ => {
                        return Err(anyhow!(
                            "Expected ';' or ',', but found {} for consumers of transition {}.",
                            token,
                            transition
                        ));
                    }
                },
                State::SeenConsumeSemicolon(transition) => {
                    if token == "PRODUCE" {
                        state = State::SeenProduce(transition)
                    } else {
                        return Err(anyhow!("Expected 'PRODUCE' but found '{}'.", token));
                    }
                }
                State::SeenProduce(transition) => match token {
                    ";" => state = State::Start, //end of list
                    _ if Self::is_identifier(token) => {
                        if let Some(place) = place_id_2_place.get(token) {
                            lpn.add_transition_place_arc(transition, *place, 1)?;
                            state = State::SeenProducePlace(transition, *place);
                        } else {
                            return Err(anyhow!("Undeclared place '{}' mentioned.", token));
                        }
                    }
                    _ => {
                        return Err(anyhow!(
                            "Place identifier '{}' contains a forbidden character.",
                            token
                        ));
                    }
                },
                State::SeenProducePlace(transition, place) => match token {
                    ":" => state = State::SeenProducePlaceColon(transition, place),
                    ";" => state = State::Start,
                    "," => state = State::SeenProduce(transition), //cutting corner: ,; is allowed.
                    _ => {
                        return Err(anyhow!(
                            "Expected ':', ';' or ',', but found {} for producers of transition {}.",
                            token,
                            transition
                        ));
                    }
                },
                State::SeenProducePlaceColon(transition, place) => {
                    if let Ok(cardinality) = token.parse::<u64>() {
                        //the arc was already added with cardinality 1
                        if cardinality == 0 {
                            return Err(anyhow!(
                                "Attempted to add an arc with arc weight 0 for producers of transition {}.",
                                transition
                            ));
                        } else if cardinality > 1 {
                            lpn.add_transition_place_arc(transition, place, cardinality - 1)?;
                        }
                        state = State::SeenProducePlaceColonCardinality(transition);
                    } else {
                        return Err(anyhow!(
                            "Expected a cardinality, found '{}' for producers of transition {}.",
                            token,
                            transition
                        ));
                    }
                }
                State::SeenProducePlaceColonCardinality(transition) => match token {
                    ";" => state = State::Start,
                    "," => state = State::SeenProduce(transition), //cutting corner: ,; is allowed.
                    _ => {
                        return Err(anyhow!(
                            "Expected ';' or ',', but found {} for producers of transition {}.",
                            token,
                            transition
                        ));
                    }
                },
            }
        }

        if state == State::Start {
            Ok(())
        } else {
            Err(anyhow!("Transition not properly terminated with ';'."))
        }
    }
}
