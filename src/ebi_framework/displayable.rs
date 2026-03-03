use ebi_objects::{ebi_bpmn::semantics::BPMNMarking, ebi_objects::process_tree::TreeMarking, marking::Marking};
use std::{
    fmt::{Debug, Display},
    hash::Hash,
};

pub trait Displayable: Hash + Clone + Eq + Display + Debug + Send + Sync {}

impl Displayable for usize {}

impl Displayable for TreeMarking {}

impl Displayable for Marking {}

impl Displayable for BPMNMarking {}
