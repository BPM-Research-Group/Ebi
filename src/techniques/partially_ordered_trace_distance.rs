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

            //model moves and silent moves
            for node in state.enabled_nodes(other) {
                let new_state = state.execute_node(node);
                //model move
                result.push(((*trace_index, new_state.clone()), 1));

                //substitution
                if trace_index < &self.len() {
                    result.push(((*trace_index + 1, new_state.clone()), 1));
                }

                //synchronous move
                if other.node_2_activity[node] == self[*trace_index] {
                    result.push(((trace_index + 1, new_state), 0));
                }
            }

            result
        };

        //function that returns a heuristic on how far we are still at least from a final state
        let heuristic = |(_, _): &(usize, PartiallyOrderedTraceMarking)| 0;

        //function that returns whether we are in a final synchronous product state
        let success = |(trace_index, state): &(usize, PartiallyOrderedTraceMarking)| {
            trace_index == &self.len() && state.node_2_executed.all()
        };

        astar(&start, successors, heuristic, success).unwrap().1
    }

    fn normalised_partially_ordered_trace_distance(
        &self,
        other: &PartiallyOrderedTrace,
    ) -> Fraction {
        if !self.is_empty() || other.number_of_nodes() != 0 {
            let distance = self.partially_ordered_trace_distance(other);
            Fraction::from((distance, self.len().max(other.number_of_nodes())))
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
            for node in marking_self.enabled_nodes(self) {
                //execute transition
                let new_marking_self = marking_self.execute_node(node);
                //labelled
                successors_self.push((new_marking_self.clone(), self.node_2_activity[node]));

                //self move
                result.push(((new_marking_self, marking_other.clone()), 1));
            }

            //other move
            let mut successors_other = vec![];
            for node in marking_other.enabled_nodes(other) {
                let new_marking_other = marking_other.execute_node(node);
                //other move
                successors_other.push((new_marking_other.clone(), other.node_2_activity[node]));
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
            marking_self.node_2_executed.all() && marking_other.node_2_executed.all()
        };

        astar(&start, successors, heuristic, success).unwrap().1
    }

    fn normalised_partially_ordered_trace_distance(
        &self,
        other: &PartiallyOrderedTrace,
    ) -> Fraction {
        if self.number_of_nodes() > 0 || other.number_of_nodes() > 0 {
            let distance = self.partially_ordered_trace_distance(other);

            Fraction::from((
                distance,
                self.number_of_nodes().max(other.number_of_nodes()),
            ))
        } else {
            Fraction::zero()
        }
    }
}

#[derive(Clone, Eq, PartialEq, Hash)]
struct PartiallyOrderedTraceMarking {
    node_2_executed: BitVec,
}

impl PartiallyOrderedTraceMarking {
    pub fn from_trace(po_trace: &PartiallyOrderedTrace) -> Self {
        let mut result = Self {
            node_2_executed: bitvec!(0; po_trace.number_of_nodes()),
        };

        po_trace.start_nodes().for_each(|node| {
            result.node_2_executed.set(node, true);
        });

        result
    }

    pub fn enabled_nodes(&self, po_trace: &PartiallyOrderedTrace) -> impl Iterator<Item = usize> {
        self.node_2_executed.iter_zeros().filter(|node| {
            po_trace.node_2_predecessors[*node]
                .iter()
                .all(|input| self.node_2_executed[*input])
        })
    }

    pub fn execute_node(&self, node: usize) -> Self {
        let mut result = self.clone();
        result.node_2_executed.set(node, true);
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
            "partially ordered trace marking; nodes {:?}",
            self.node_2_executed.iter_ones().collect::<Vec<_>>()
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

        let po_trace = &spolang.traces[0];

        let ac0 = spolang.activity_key().get_activity_by_id(0);
        let ac1 = spolang.activity_key().get_activity_by_id(1);

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
                spotrace.number_of_nodes(),
                slangtrace.partially_ordered_trace_distance(&spotrace)
            );

            assert_eq!(
                spotrace.number_of_nodes(),
                slangtrace.partially_ordered_trace_distance(&spotrace)
            );
            if spotrace.number_of_nodes() == 0 {
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
