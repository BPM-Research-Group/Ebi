use core::fmt;
use std::hash::{Hash, Hasher};
use crate::{activity_key::Activity, ebi_traits::ebi_trait_stochastic_semantics::TransitionIndex, marking::Marking, math::fraction::Fraction};

pub trait Net: Clone {
    fn get_number_of_places(&self) -> usize;
    fn get_number_of_transitions(&self) -> usize;
    fn get_transitions(&self) -> &[Transition];
    fn get_initial_marking(&self) -> &Marking;
}

pub trait StochasticNet {
    fn get_weight(&self, transition: &Transition) -> &Fraction;
}


#[derive(Clone,Debug)]
pub struct Transition {
    pub index: usize,
    pub incoming: Vec<usize>, //list of places; may contains doubles for arc weights > 1
    pub outgoing: Vec<usize>, //list of places; may contains doubles for arc weights > 1
    label: Option<Activity>, //if None then the transition is silent
}

impl fmt::Display for Transition {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if self.is_silent() {
            write!(f, "silent transition")
        } else {
            write!(f, "{} {:?}", self.index, self.get_label())
        }
    }
}

impl Transition {
    pub fn is_silent(&self) -> bool {
        self.label.is_none()
    }

    pub fn get_label(&self) -> Option<Activity> {
        self.label
    }

    pub fn new_silent(index: TransitionIndex) -> Self {
        Transition {
            index: index,
            incoming: vec![],
            outgoing: vec![],
            label: None,
        }
    }

    pub fn new_labelled(index: TransitionIndex, label: Activity) -> Self {
        Transition {
            index: index,
            incoming: vec![],
            outgoing: vec![],
            label: Some(label),
        }
    }


}

impl Eq for Transition {}

impl Hash for Transition {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.index.hash(state);
    }
}

impl PartialEq for Transition {
    fn eq(&self, other: &Self) -> bool {
        self.index == other.index
    }
}