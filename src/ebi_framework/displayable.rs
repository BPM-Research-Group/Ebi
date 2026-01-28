use ebi_objects::ebi_objects::process_tree::TreeMarking;
use std::{
    fmt::{Debug, Display},
    hash::Hash,
};

pub trait Displayable: Hash + Clone + Eq + Display + Debug + Send + Sync {}

impl Displayable for usize {}

impl Displayable for TreeMarking {}
