use core::fmt;
use std::hash::{Hash, Hasher};

use crate::stochastic_semantics::{ActivityIndex, TransitionIndex};

#[derive(Clone,Debug)]
pub struct Transition {
    pub index: usize,
    pub incoming: Vec<usize>, //for each place: arc weight from that place
    pub outgoing: Vec<usize>, //for each place: arc weight to that place
    label: Option<ActivityIndex>, //if None then the transition is silent
}

impl fmt::Display for Transition {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{} {:?}", self.index, self.get_label())
    }
}


impl Transition {
    pub fn is_silent(&self) -> bool {
        self.label.is_none()
    }

    // /**
    //  * Gets the index of the activity that this transition is labelled with. If the transition is not labelled, this will panic.
    //  */
    pub fn get_label(&self) -> ActivityIndex {
        if let Some(x) = self.label {
            x
        } else {
            panic!("cannot get label of silent transition")
        }
    }

    /**
     * Gets the index of the activity that this transition is labelled with. If the transition is not labelled, this will panic.
     */
    pub fn get_non_silent_label(&self) -> ActivityIndex {
        if let Some(x) = self.label {
            x
        } else {
            usize::MAX
        }
    }

    pub fn new_silent(index: TransitionIndex) -> Self {
        Transition {
            index: index,
            incoming: vec![],
            outgoing: vec![],
            label: None,
        }
    }

    pub fn new_labelled(index: TransitionIndex, label: ActivityIndex) -> Self {
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