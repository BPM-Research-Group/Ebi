use ebi_arithmetic::{ebi_number::One, fraction::Fraction};

use crate::{
    ebi_objects::directly_follows_graph::DirectlyFollowsGraph,
    ebi_traits::{
        ebi_trait_event_log::EbiTraitEventLog,
        ebi_trait_finite_stochastic_language::EbiTraitFiniteStochasticLanguage,
    },
};

pub trait DirectlyFollowsAbstractor {
    fn abstract_to_directly_follows_graph(&self) -> DirectlyFollowsGraph;
}

impl DirectlyFollowsAbstractor for dyn EbiTraitEventLog {
    fn abstract_to_directly_follows_graph(&self) -> DirectlyFollowsGraph {
        let mut result = DirectlyFollowsGraph::new(self.get_activity_key().clone());

        //If the language has no traces, then the directly follows graph has no start activities and no empty traces.

        for trace_index in 0..self.len() {
            if let Some(trace) = self.get_trace(trace_index) {
                let mut last_activity = None;
                for activity in trace {
                    match last_activity {
                        Some(previous) => {
                            //edge
                            result.add_edge(previous, *activity, &Fraction::one());
                        }
                        None => {
                            //start activity
                            result.add_start_activity(*activity, &Fraction::one());
                        }
                    }
                    last_activity = Some(*activity);
                }

                match last_activity {
                    Some(activity) => result.add_end_activity(activity, &Fraction::one()),
                    None => result.add_empty_trace(&Fraction::one()),
                };
            }
        }
        result
    }
}

impl DirectlyFollowsAbstractor for dyn EbiTraitFiniteStochasticLanguage {
    fn abstract_to_directly_follows_graph(&self) -> DirectlyFollowsGraph {
        let mut result = DirectlyFollowsGraph::new(self.get_activity_key().clone());

        //If the language has no traces, then the directly follows graph has no start activities and no empty traces.

        for (trace, probability) in self.iter_trace_probability() {
            let mut last_activity = None;
            for activity in trace {
                match last_activity {
                    Some(previous) => {
                        //edge
                        result.add_edge(previous, *activity, probability);
                    }
                    None => {
                        //start activity
                        result.add_start_activity(*activity, probability);
                    }
                }
                last_activity = Some(*activity);
            }

            match last_activity {
                Some(activity) => result.add_end_activity(activity, probability),
                None => result.add_empty_trace(probability),
            };
        }

        result
    }
}
