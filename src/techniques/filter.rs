use crate::{
    ebi_framework::{ebi_input::EbiInput, ebi_trait::FromEbiTraitObject},
    ebi_traits::ebi_trait_event_log::EbiTraitEventLog,
};
use anyhow::{Result, anyhow};
use ebi_objects::{Activity, EventLog, EventLogTraceAttributes, EventLogXes, ebi_derive::EbiInputEnum};

pub trait Filter {
    /// keep all traces that have a given length
    fn remove_traces_length(&mut self, operator: Operator, value: usize);

    /// keep all non-empty traces
    fn remove_traces_empty(&mut self);

    /// remove all traces that have [event_selector] [activity]
    fn remove_traces_event_activity(&mut self, event_selector: EventSelector, activity: Activity);
}

macro_rules! filter {
    ($t:ident) => {
        impl Filter for $t {
            fn remove_traces_length(&mut self, operator: Operator, value: usize) {
                self.retain_traces(Box::new(move |trace| !operator.apply(trace.len(), value)));
            }

            fn remove_traces_empty(&mut self) {
                self.retain_traces(Box::new(|trace| trace.len() != 0));
            }

            fn remove_traces_event_activity(
                &mut self,
                event_selector: EventSelector,
                activity: Activity,
            ) {
                self.retain_traces(Box::new(move |trace| {
                    !event_selector.apply(trace, |act| act == &activity)
                }));
            }
        }
    };
}

filter!(EventLog);
filter!(EventLogTraceAttributes);
filter!(EventLogXes);

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

#[cfg(test)]
mod tests {
    use std::fs;

    use ebi_objects::{EventLog, EventLogTraceAttributes, HasActivityKey, NumberOfTraces};

    use crate::techniques::filter::{EventSelector, Filter, Operator};

    #[test]
    fn filter_traces_length() {
        let fin = fs::read_to_string("testfiles/a-b.xes").unwrap();
        let mut log = Box::new(fin.parse::<EventLogTraceAttributes>().unwrap());

        log.remove_traces_length(Operator::Larger, 10);

        assert_eq!(log.number_of_traces(), 2);

        log.remove_traces_length(Operator::SmallerEqual, 2);

        assert_eq!(log.number_of_traces(), 0);
    }

    #[test]
    fn filter_traces_empty() {
        let fin = fs::read_to_string("testfiles/a-b.xes").unwrap();
        let mut log = Box::new(fin.parse::<EventLog>().unwrap());

        log.remove_traces_empty();

        assert_eq!(log.number_of_traces(), 2);
    }

    #[test]
    fn filter_traces_event_activity() {
        let fin = fs::read_to_string("testfiles/a-b.xes").unwrap();

        let mut log = Box::new(fin.parse::<EventLog>().unwrap());
        let a = log.activity_key_mut().process_activity("a");
        log.remove_traces_event_activity(EventSelector::All, a);
        assert_eq!(log.number_of_traces(), 1);

        let mut log = Box::new(fin.parse::<EventLog>().unwrap());
        log.remove_traces_event_activity(EventSelector::Any, a);
        assert_eq!(log.number_of_traces(), 1);

        let mut log = Box::new(fin.parse::<EventLog>().unwrap());
        log.remove_traces_event_activity(EventSelector::End, a);
        assert_eq!(log.number_of_traces(), 1);

        let mut log = Box::new(fin.parse::<EventLog>().unwrap());
        log.remove_traces_event_activity(EventSelector::Start, a);
        assert_eq!(log.number_of_traces(), 1);

        let mut log = Box::new(fin.parse::<EventLog>().unwrap());
        log.remove_traces_event_activity(EventSelector::None, a);
        assert_eq!(log.number_of_traces(), 1);
    }
}
