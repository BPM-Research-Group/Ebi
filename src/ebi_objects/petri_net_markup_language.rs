use std::{collections::HashMap, io::{BufRead, Write}};

use anyhow::{anyhow, Error, Result};
use bitvec::bitvec;
use fraction::ToPrimitive;
use process_mining::{petri_net::petri_net_struct::{self, ArcType}, PetriNet};

use crate::{ebi_framework::{ebi_file_handler::EbiFileHandler, ebi_input::{EbiObjectImporter, EbiTraitImporter}, ebi_object::EbiObject, ebi_output::{EbiObjectExporter, EbiOutput}, exportable::Exportable, importable::Importable}, ebi_objects::labelled_petri_net::LPNMarking, ebi_traits::ebi_trait_semantics::{EbiTraitSemantics, Semantics}, marking::Marking};

use super::labelled_petri_net::LabelledPetriNet;

pub const EBI_PETRI_NET_MARKUP_LANGUAGE: EbiFileHandler = EbiFileHandler {
    name: "Petri net markup language",
    article: "a",
    file_extension: "pnml",
    validator: PetriNetMarkupLanguage::validate,
    trait_importers: &[
        EbiTraitImporter::Semantics(PetriNetMarkupLanguage::import_as_semantics),
    ],
    object_importers: &[
        EbiObjectImporter::LabelledPetriNet(PetriNetMarkupLanguage::import_as_object),
    ],
    object_exporters: &[
        EbiObjectExporter::LabelledPetriNet(PetriNetMarkupLanguage::export_from_object),
    ],
    java_object_handlers: &[], //java translations covered by LabelledPetrinet
};

pub struct PetriNetMarkupLanguage {
    net: process_mining::PetriNet
}

impl PetriNetMarkupLanguage {
    /**
     * PNML is always translated to LPN in Ebi. Therefore, validate that step as well.
     */
    pub fn validate(reader: &mut dyn BufRead) -> Result<()> {
        let pnml = Self::import(reader)?;
        LabelledPetriNet::try_from(pnml)?;
        Ok(())
    }

    pub fn import_as_labelled_petri_net(reader: &mut dyn BufRead) -> Result<LabelledPetriNet> {
        match Self::import(reader) {
            Ok(pnml) => Ok(LabelledPetriNet::try_from(pnml)?),
            Err(x) => Err(x),
        }
    }

    pub fn import_as_semantics(reader: &mut dyn BufRead) -> Result<EbiTraitSemantics> {
        let pnml = Self::import(reader)?;
        let lpn = LabelledPetriNet::try_from(pnml)?;
        Ok(EbiTraitSemantics::Marking(Box::new(lpn)))
    }
}

impl Importable for PetriNetMarkupLanguage {
    fn import_as_object(reader: &mut dyn BufRead) -> Result<EbiObject> {
        Ok(EbiObject::LabelledPetriNet(Self::import(reader)?.try_into()?))
    }

    fn import(reader: &mut dyn BufRead) -> Result<Self> where Self: Sized {
        match process_mining::petri_net::import_pnml::import_pnml_reader(&mut Box::new(reader)) {
            Ok(pnml) => {
                //pnml.export_svg("/home/sander/Documents/work/research/Ebi/text.svg");
                Ok(Self{net: pnml})
            },
            Err(e) => Err(anyhow!("{}", e)),
        }
    }
}

impl Exportable for PetriNetMarkupLanguage {
    fn export_from_object(object: EbiOutput, f: &mut dyn Write) -> Result<()> {
        match object {
            EbiOutput::Object(EbiObject::LabelledPetriNet(lpn)) => PetriNetMarkupLanguage::try_from(lpn)?.export(f),
            _ => unreachable!()
        }
    }

    fn export(&self, f: &mut dyn std::io::Write) -> Result<()> {
        process_mining::petri_net::export_pnml::export_petri_net_to_pnml(&self.net, f)?;
        Ok(())
    }
}

impl TryFrom<PetriNetMarkupLanguage> for LabelledPetriNet {
    type Error = Error;

    fn try_from(pnml: PetriNetMarkupLanguage) -> std::result::Result<Self, Self::Error> {
        log::info!("Convert PNML into LPN.");

        let mut result = LabelledPetriNet::new();

        //create map of places
        let mut place2index = HashMap::new();
        for (place_id, _) in pnml.net.places {
            let place = result.add_place();
            place2index.insert(place_id, place);
        }

        //transitions
        let mut transition2index = HashMap::new();
        for (transition_id, transition) in &pnml.net.transitions {
            let label = match &transition.label {
                Some(activity) => Some(result.get_activity_key_mut().process_activity(activity)),
                None => None
            };
            let transition = result.add_transition(label);

            transition2index.insert(transition_id, transition);
        }

        //arcs
        for arc in pnml.net.arcs.iter() {
            match arc.from_to {
                process_mining::petri_net::petri_net_struct::ArcType::PlaceTransition(place_id, transition_id) => {
                    let new_place = place2index.get(&place_id).ok_or(anyhow!("Undeclared place referenced."))?;
                    let new_transition = transition2index.get(&transition_id).ok_or(anyhow!("undeclared transition referenced"))?;
                    result.add_place_transition_arc(*new_place, *new_transition, arc.weight.into())?;
                },
                process_mining::petri_net::petri_net_struct::ArcType::TransitionPlace(transition_id, place_id) => {
                    let new_place = place2index.get(&place_id).ok_or(anyhow!("Undeclared place referenced."))?;
                    let new_transition = transition2index.get(&transition_id).ok_or(anyhow!("undeclared transition referenced"))?;
                    result.add_transition_place_arc(*new_transition, *new_place, arc.weight.into())?;
                },
            };
        }
        
        //initial marking
        for (place_id, cardinality) in pnml.net.initial_marking.as_ref().ok_or(anyhow!("The given net has no initial marking. Ebi requires an innitial marking for its Petri nets."))?.iter() {
            let new_place = place2index.get(&place_id.get_uuid()).ok_or(anyhow!("Undeclared place found."))?;
            result.get_initial_marking_mut().increase(*new_place, *cardinality)?;
        }

        //final markings
        if let Some(final_markings) = &pnml.net.final_markings {
            //The nets used by Ebi do not have final markings, as each of their deadlocks is taken as a final marking.
            //The best we can do here is to verify that no non-deadlocks have been declared as final markings.
            for final_marking in final_markings.iter() {
                //transform to an Ebi-final marking
                let mut new_final_marking = Marking::new(result.get_number_of_places());
                for (place_id, cardinality) in final_marking.iter() {
                    let new_place = place2index.get(&place_id.get_uuid()).ok_or(anyhow!("Undeclared place found."))?;
                    new_final_marking.increase(*new_place, *cardinality)?;
                }

                //verify that this is a deadlock marking
                let mut state = LPNMarking { marking: new_final_marking, enabled_transitions: bitvec![0; result.get_number_of_transitions()], number_of_enabled_transitions: 0 };
                result.compute_enabled_transitions(&mut state);
                if !result.is_final_state(&state) {
                    return Err(anyhow!("This PNML file has a final marking that is not a deadlock. In Ebi, each final marking must be a deadlock. This final marking is {:?}", final_marking))
                }
            }
        }

        Ok(result)
    }
}

impl TryFrom<LabelledPetriNet> for PetriNetMarkupLanguage {
    type Error = anyhow::Error;

    fn try_from(lpn: LabelledPetriNet) -> std::result::Result<Self, Self::Error> {
        log::info!("Convert LPN into PNML.");

        let mut result = PetriNet::new();

        //create places
        let mut place2new_place = HashMap::new();
        {
            for place in 0..lpn.get_number_of_places() {
                let new_place = result.add_place(None);
                place2new_place.insert(place, new_place);
            }
        }

        //create transitions
        for transition in 0..lpn.get_number_of_transitions() {
            let new_transition = result.add_transition(match lpn.get_transition_label(transition) {
                Some(activity) => Some(lpn.activity_key.get_activity_label(&activity).to_string()),
                None => None,
            }, None);

            //incoming
            {
                //transform to a map of places and arc weights
                let mut map = HashMap::new();
                for (pos, place) in lpn.transition2input_places[transition].iter().enumerate() {
                    *map.entry(*place).or_insert(0) += u32::try_from(lpn.transition2input_places_cardinality[transition][pos])?;
                }
                
                //add
                for (place, weight) in map {
                    let new_place = place2new_place.get(&place).ok_or(anyhow!("Non-existing place referenced."))?;
                    result.add_arc(ArcType::place_to_transition(*new_place, new_transition), Some(weight));
                }
            }

            //outgoing
            {
                //transform to a map of places and arc weights
                let mut map = HashMap::new();
                for (pos, place) in lpn.transition2output_places[transition].iter().enumerate() {
                    *map.entry(*place).or_insert(0) += u32::try_from(lpn.transition2output_places_cardinality[transition][pos])?;
                }

                //add
                for (place, weight) in map {
                    let new_place = place2new_place.get(&place).ok_or(anyhow!("Non-existing place referenced."))?;
                    result.add_arc(ArcType::transition_to_place(new_transition, *new_place), Some(weight.to_u32().ok_or(anyhow!("value out of bounds"))?));
                }
            }
        }

        //initial marking
        let mut new_initial_marking = petri_net_struct::Marking::new();
        for (place, cardinality) in lpn.initial_marking.get_place2token().into_iter().enumerate() {
            if cardinality > &0u64 {
                let new_place = place2new_place.get(&place).ok_or(anyhow!("non-existing place referenced"))?;
                new_initial_marking.insert(*new_place, *cardinality);
            }
        }
        result.initial_marking = Some(new_initial_marking);

        Ok(Self{net: result})
    }
}