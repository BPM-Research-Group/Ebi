use std::{fmt::{Debug, Display}, hash::Hash};

use crate::ebi_objects::process_tree::NodeStates;

pub trait Displayable: Hash + Clone + Eq + Display + Debug + Send + Sync {}

impl Displayable for usize {}

impl Displayable for NodeStates {}