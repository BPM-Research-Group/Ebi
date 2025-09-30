use crate::ebi_framework::{ebi_input::EbiInput, ebi_trait::FromEbiTraitObject};
use anyhow::{Result, anyhow};
use ebi_derive::EbiInputEnum;
use ebi_objects::Activity;

#[derive(EbiInputEnum)]
pub enum EventSelector {
    Any,
    All,
    None,
    Start,
    End,
}

impl EventSelector {
    pub fn apply<F>(&self, trace: &Vec<Activity>, f: F) -> bool
    where
        F: Fn(&Activity) -> bool,
    {
        match self {
            EventSelector::Any => trace.iter().any(f),
            EventSelector::All => trace.iter().all(f),
            EventSelector::None => !trace.iter().any(f),
            EventSelector::Start => match trace.iter().next() {
                Some(act) => (f)(act),
                None => false,
            },
            EventSelector::End => match trace.iter().last() {
                Some(act) => (f)(act),
                None => false,
            },
        }
    }
}

#[derive(EbiInputEnum)]
pub enum Operator {
    #[strum(serialize = "<")]
    Smaller,
    #[strum(serialize = "<=")]
    SmallerEqual,
    #[strum(serialize = ">")]
    Larger,
    #[strum(serialize = ">=")]
    LargerEqual,
    #[strum(serialize = "=")]
    Equal,
    #[strum(serialize = "<>")]
    UnEqual,
}

impl Operator {
    pub fn apply<T>(&self, a: T, b: T) -> bool
    where
        T: PartialOrd,
    {
        match self {
            Operator::Smaller => a < b,
            Operator::SmallerEqual => a <= b,
            Operator::Larger => a > b,
            Operator::LargerEqual => a >= b,
            Operator::Equal => a == b,
            Operator::UnEqual => a != b,
        }
    }
}
