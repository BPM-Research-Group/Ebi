use anyhow::{anyhow, Result};
use std::fmt::Debug;
use crate::{activity_key::ActivityKey, ebi_input_output::EbiInput, ebi_objects::ebi_object::{EbiObject, EbiTraitObject}, marking::Marking, net::Transition};

use super::ebi_trait::FromEbiTraitObject;


pub trait EbiTraitLabelledPetriNet {
    fn get_activity_key(&self) -> &ActivityKey;

    fn get_activity_key_mut(&mut self) -> &mut ActivityKey;

    fn get_number_of_places(&self) -> usize;

    fn get_number_of_transitions(&self) -> usize;

    fn get_transitions(&self) -> &Vec<Transition>;

    fn get_initial_marking(&self) -> &Marking;
}

impl FromEbiTraitObject for dyn EbiTraitLabelledPetriNet {
    fn from_trait_object(object: EbiInput) -> Result<Box<Self>> {
        match object {
            EbiInput::Trait(EbiTraitObject::LabelledPetriNet(e), _) => Ok(e),
            _ => Err(anyhow!("Cannot read {} {} as a labelled Petri net.", object.get_type().get_article(), object.get_type()))
        }
    }
}

impl Debug for dyn EbiTraitLabelledPetriNet {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "Number of places: {}, number of transitions: {}", self.get_number_of_places(), self.get_number_of_transitions())
    }
}