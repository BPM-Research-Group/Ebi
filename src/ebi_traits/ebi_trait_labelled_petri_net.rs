use anyhow::{anyhow, Result};
use std::{fmt::Debug, rc::Rc};
use crate::{activity_key::ActivityKey, ebi_input_output::EbiInput, ebi_objects::ebi_object::{EbiObject, EbiTraitObject}, labelled_petri_net_semantics::LabelledPetriNetSemantics, marking::Marking, net::Transition};

use super::{ebi_trait::FromEbiTraitObject, ebi_trait_semantics::EbiTraitSemantics};

pub trait EbiTraitLabelledPetriNet: EbiTraitLabelledPetriNetClone {
    fn get_activity_key(&self) -> &ActivityKey;

    fn get_activity_key_mut(&mut self) -> &mut ActivityKey;

    fn get_number_of_places(&self) -> usize;

    fn get_number_of_transitions(&self) -> usize;

    fn get_transitions(&self) -> &Vec<Transition>;

    fn get_initial_marking(&self) -> &Marking;
}

impl dyn EbiTraitLabelledPetriNet {
    pub fn get_semantics(self: Box<Self>) -> EbiTraitSemantics {
        let stochastic_semantics = LabelledPetriNetSemantics::from_lpn(self);
        EbiTraitSemantics::Marking(Box::new(stochastic_semantics))
    }
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


pub trait EbiTraitLabelledPetriNetClone {
    fn clone_box(&self) -> Box<dyn EbiTraitLabelledPetriNet>;
}

impl<T> EbiTraitLabelledPetriNetClone for T where T: 'static + EbiTraitLabelledPetriNet + Clone {
    fn clone_box(&self) -> Box<dyn EbiTraitLabelledPetriNet> {
        Box::new(self.clone())
    }
}

impl Clone for Box<dyn EbiTraitLabelledPetriNet> {
    fn clone(&self) -> Box<dyn EbiTraitLabelledPetriNet> {
        self.clone_box()
    }
}