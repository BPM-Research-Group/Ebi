use bitvec::{bitvec, vec::BitVec};
use ebi_objects::{
    Activity,
    ebi_arithmetic::{Fraction, Zero},
    ebi_objects::finite_stochastic_partially_ordered_language::PartiallyOrderedTrace,
};
use itertools::Itertools;
use pathfinding::prelude::astar;
use std::fmt::{Debug, Display};

pub trait PartiallyOrderedTraceDistance {
    /// Applies a generalisation of the Levenshtein distance: returns the minimum number of insertions, deletions and substitutions that is necessary to transform one trace into the other.
    fn partially_ordered_trace_distance(&self, other: &PartiallyOrderedTrace) -> usize;

    /// Applies a generalisation of the Levenshtein distance: returns the minimum number of insertions, deletions and substitutions that is necessary to transform one trace into the other.
    /// Normalised to the interval [0, 1].
    fn normalised_partially_ordered_trace_distance(
        &self,
        other: &PartiallyOrderedTrace,
    ) -> Fraction;
}

impl PartiallyOrderedTraceDistance for Vec<Activity> {
    fn partially_ordered_trace_distance(&self, other: &PartiallyOrderedTrace) -> usize {
        //initial synchronous product state
        let initial_po_marking = PartiallyOrderedTraceMarking::from_trace(other);
        let start = (0, initial_po_marking);

        //function that returns the successor states of the given state
        let successors = |(trace_index, state): &(usize, PartiallyOrderedTraceMarking)| {
            let mut result = vec![];

            //log move
            if trace_index < &self.len() {
                //we can do a log move
                result.push(((trace_index + 1, state.clone()), 1));
            }

            //model moves
            for event in state.enabled_events(other) {
                let new_state = state.execute_event(event);
                //model move
                result.push(((*trace_index, new_state.clone()), 1));

                //substitution
                if trace_index < &self.len() {
                    result.push(((*trace_index + 1, new_state.clone()), 1));
                }

                //synchronous move
                if let Some(po_activity) = other.event_2_activity.get(event) {
                    if let Some(activity) = self.get(*trace_index)
                        && po_activity == activity
                    {
                        result.push(((trace_index + 1, new_state), 0));
                    }
                }
            }
            result
        };

        //function that returns a heuristic on how far we are still at least from a final state
        let heuristic = |(_, _): &(usize, PartiallyOrderedTraceMarking)| 0;

        //function that returns whether we are in a final synchronous product state
        let success = |(trace_index, state): &(usize, PartiallyOrderedTraceMarking)| {
            trace_index == &self.len() && state.event_2_executed.all()
        };

        astar(&start, successors, heuristic, success).unwrap().1
    }

    fn normalised_partially_ordered_trace_distance(
        &self,
        other: &PartiallyOrderedTrace,
    ) -> Fraction {
        if !self.is_empty() || other.number_of_events() != 0 {
            let distance = self.partially_ordered_trace_distance(other);
            Fraction::from((distance, self.len().max(other.number_of_events())))
        } else {
            Fraction::zero()
        }
    }
}

impl PartiallyOrderedTraceDistance for PartiallyOrderedTrace {
    fn partially_ordered_trace_distance(&self, other: &PartiallyOrderedTrace) -> usize {
        //initial synchronous product state
        let marking_self = PartiallyOrderedTraceMarking::from_trace(self);
        let marking_other = PartiallyOrderedTraceMarking::from_trace(other);
        let start = (marking_self, marking_other);

        //function that returns the successor states of the given state
        let successors = |(marking_self, marking_other): &(
            PartiallyOrderedTraceMarking,
            PartiallyOrderedTraceMarking,
        )| {
            let mut result = vec![];

            //self move
            let mut successors_self = vec![];
            for event in marking_self.enabled_events(self) {
                //execute transition
                let new_marking_self = marking_self.execute_event(event);
                //labelled
                successors_self.push((new_marking_self.clone(), self.event_2_activity[event]));

                //self move
                result.push(((new_marking_self, marking_other.clone()), 1));
            }

            //other move
            let mut successors_other = vec![];
            for event in marking_other.enabled_events(other) {
                let new_marking_other = marking_other.execute_event(event);
                //other move
                successors_other.push((new_marking_other.clone(), other.event_2_activity[event]));
                result.push(((marking_self.clone(), new_marking_other), 1));
            }

            //synchronous moves
            for ((new_marking_self, activity_self), (new_marking_other, activity_other)) in
                successors_self
                    .into_iter()
                    .cartesian_product(successors_other.into_iter())
            {
                if activity_self == activity_other {
                    //synchronous move
                    result.push(((new_marking_self, new_marking_other), 0));
                } else {
                    //substitution move
                    result.push(((new_marking_self, new_marking_other), 1));
                }
            }

            result
        };

        //function that returns a heuristic on how far we are still at least from a final state
        let heuristic = |(_, _): &(PartiallyOrderedTraceMarking, PartiallyOrderedTraceMarking)| 0;

        //function that returns whether we are in a final synchronous product state
        let success = |(marking_self, marking_other): &(
            PartiallyOrderedTraceMarking,
            PartiallyOrderedTraceMarking,
        )| {
            marking_self.event_2_executed.all() && marking_other.event_2_executed.all()
        };

        astar(&start, successors, heuristic, success).unwrap().1
    }

    fn normalised_partially_ordered_trace_distance(
        &self,
        other: &PartiallyOrderedTrace,
    ) -> Fraction {
        if self.number_of_events() > 0 || other.number_of_events() > 0 {
            let distance = self.partially_ordered_trace_distance(other);

            Fraction::from((
                distance,
                self.number_of_events().max(other.number_of_events()),
            ))
        } else {
            Fraction::zero()
        }
    }
}

#[derive(Clone, Eq, PartialEq, Hash)]
struct PartiallyOrderedTraceMarking {
    event_2_executed: BitVec,
}

impl PartiallyOrderedTraceMarking {
    pub fn from_trace(po_trace: &PartiallyOrderedTrace) -> Self {
        let result = Self {
            event_2_executed: bitvec!(0; po_trace.number_of_events()),
        };
        result
    }

    pub fn enabled_events(&self, po_trace: &PartiallyOrderedTrace) -> impl Iterator<Item = usize> {
        self.event_2_executed.iter_zeros().filter(|event| {
            po_trace.event_2_predecessors[*event]
                .iter()
                .all(|input| self.event_2_executed[*input])
        })
    }

    pub fn execute_event(&self, event: usize) -> Self {
        let mut result = self.clone();
        result.event_2_executed.set(event, true);
        result
    }
}

impl Debug for PartiallyOrderedTraceMarking {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self)
    }
}

impl Display for PartiallyOrderedTraceMarking {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "partially ordered trace marking; events {:?}",
            self.event_2_executed.iter_ones().collect::<Vec<_>>()
        )
    }
}

#[cfg(test)]
mod tests {
    use crate::techniques::partially_ordered_trace_distance::PartiallyOrderedTraceDistance;
    use ebi_objects::{
        FiniteStochasticLanguage, FiniteStochasticPartiallyOrderedLanguage, HasActivityKey,
        ebi_arithmetic::{Fraction, One, Zero},
    };
    use std::fs;

    #[test]
    fn po_trace_distance() {
        let fin1 = fs::read_to_string("testfiles/model.sbpmn.spolang").unwrap();
        let spolang = fin1
            .parse::<FiniteStochasticPartiallyOrderedLanguage>()
            .unwrap();

        let po_trace = &spolang.traces[4];
        println!("{:?}", po_trace);

        let ac0 = spolang
            .activity_key()
            .process_activity_attempt("Register claim\n(2min)")
            .unwrap();
        let ac1 = spolang
            .activity_key()
            .process_activity_attempt("Check easy claim\n(5 min)")
            .unwrap();

        let trace = vec![ac0, ac1];
        assert_eq!(trace.partially_ordered_trace_distance(po_trace), 0);
        assert_eq!(
            trace.normalised_partially_ordered_trace_distance(po_trace),
            Fraction::zero()
        );

        let trace = vec![ac1, ac0];
        assert_eq!(trace.partially_ordered_trace_distance(po_trace), 2);
        assert_eq!(
            trace.normalised_partially_ordered_trace_distance(po_trace),
            Fraction::one()
        );
    }

    #[test]
    fn po_trace_distance_flower() {
        let fin1 = fs::read_to_string("testfiles/flower.sbpmn.spolang").unwrap();
        let spolang = fin1
            .parse::<FiniteStochasticPartiallyOrderedLanguage>()
            .unwrap();

        let fin2 = fs::read_to_string("testfiles/empty_trace.slang").unwrap();
        let slang = fin2.parse::<FiniteStochasticLanguage>().unwrap();
        let (slangtrace, _) = slang.traces.iter().next().unwrap();

        for spotrace in spolang.traces {
            println!(
                "trace length {:?}, distance {}",
                spotrace.number_of_events(),
                slangtrace.partially_ordered_trace_distance(&spotrace)
            );

            assert_eq!(
                spotrace.number_of_events(),
                slangtrace.partially_ordered_trace_distance(&spotrace)
            );
            if spotrace.number_of_events() == 0 {
                assert_eq!(
                    slangtrace.normalised_partially_ordered_trace_distance(&spotrace),
                    Fraction::zero()
                );
            } else {
                assert_eq!(
                    slangtrace.normalised_partially_ordered_trace_distance(&spotrace),
                    Fraction::one()
                );
            }
        }
    }
}
