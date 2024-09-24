use std::{fmt::{Debug, Display}, hash::Hash};
pub trait Displayable: Hash + Clone + Eq + Display + Debug + Send + Sync {
    fn debug(&self) -> String;
}