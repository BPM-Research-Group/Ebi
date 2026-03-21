use bitvec::{bitvec, vec::BitVec};
use ebi_objects::{
    Activity,
    ebi_arithmetic::Fraction,
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
            for edge in state.enabled_edges(other) {
                let mut new_state = state.execute_edge(edge, other);
                new_state.edges.set(edge, true);
                if other.edge_2_activity[edge].is_some() {
                    //model move
                    result.push(((*trace_index, new_state.clone()), 1));

                    //substitution
                    if trace_index < &self.len() {
                        result.push(((*trace_index + 1, new_state.clone()), 1));
                    }

                    //synchronous move
                    if let Some(po_activity) = other.edge_2_activity[edge] {
                        if let Some(activity) = self.get(*trace_index)
                            && po_activity == *activity
                        {
                            result.push(((trace_index + 1, new_state), 0));
                        }
                    }
                } else {
                    //silent move
                    result.push(((*trace_index, new_state), 0));
                }
            }

            result
        };

        //function that returns a heuristic on how far we are still at least from a final state
        let heuristic = |(_, _): &(usize, PartiallyOrderedTraceMarking)| 0;

        //function that returns whether we are in a final synchronous product state
        let success = |(trace_index, state): &(usize, PartiallyOrderedTraceMarking)| {
            trace_index == &self.len() && state.edges.all()
        };

        astar(&start, successors, heuristic, success).unwrap().1
    }

    fn normalised_partially_ordered_trace_distance(
        &self,
        other: &PartiallyOrderedTrace,
    ) -> Fraction {
        let distance = self.partially_ordered_trace_distance(other);
        Fraction::from((distance, self.len().max(other.number_of_labelled_edges())))
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
            for edge in marking_self.enabled_edges(self) {
                //execute transition
                let mut new_marking_self = marking_self.execute_edge(edge, self);
                new_marking_self.edges.set(edge, true);
                if let Some(activity) = self.edge_2_activity[edge] {
                    //labelled
                    successors_self.push((new_marking_self.clone(), activity));

                    //self move
                    result.push(((new_marking_self, marking_other.clone()), 1));
                } else {
                    //silent
                    result.push(((new_marking_self, marking_other.clone()), 0));
                }
            }

            //other move
            let mut successors_other = vec![];
            for edge in marking_other.enabled_edges(other) {
                let mut new_marking_other = marking_other.execute_edge(edge, other);

                new_marking_other.edges.set(edge, true);
                if let Some(activity) = other.edge_2_activity[edge] {
                    //labelled

                    //other move
                    successors_other.push((new_marking_other.clone(), activity));
                    result.push(((marking_self.clone(), new_marking_other), 1));
                } else {
                    //silent
                    result.push(((marking_self.clone(), new_marking_other), 0));
                }
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
        )| { marking_self.edges.all() && marking_other.edges.all() };

        astar(&start, successors, heuristic, success).unwrap().1
    }

    fn normalised_partially_ordered_trace_distance(
        &self,
        other: &PartiallyOrderedTrace,
    ) -> Fraction {
        let distance = self.partially_ordered_trace_distance(other);

        Fraction::from((
            distance,
            self.number_of_labelled_edges()
                .max(other.number_of_labelled_edges()),
        ))
    }
}

#[derive(Clone, Eq, PartialEq, Hash)]
struct PartiallyOrderedTraceMarking {
    states: BitVec,
    edges: BitVec,
}

impl PartiallyOrderedTraceMarking {
    pub fn from_trace(po_trace: &PartiallyOrderedTrace) -> Self {
        let mut result = Self {
            states: bitvec!(0; po_trace.number_of_states()),
            edges: bitvec!(0; po_trace.number_of_edges()),
        };

        po_trace.start_states().for_each(|state| {
            result.states.set(state, true);
        });

        result
    }

    pub fn enabled_edges(&self, po_trace: &PartiallyOrderedTrace) -> impl Iterator<Item = usize> {
        self.edges.iter_zeros().filter(|edge| {
            po_trace.edge_2_inputs[*edge]
                .iter()
                .all(|input| self.states[*input])
        })
    }

    pub fn execute_edge(&self, edge: usize, po_trace: &PartiallyOrderedTrace) -> Self {
        let mut result = self.clone();
        for output in &po_trace.edge_2_outputs[edge] {
            result.states.set(*output, true);
        }
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
            "partially ordered trace marking; states {:?}, edges {:?}",
            self.states.iter_ones().collect::<Vec<_>>(),
            self.edges.iter_ones().collect::<Vec<_>>()
        )
    }
}

#[cfg(test)]
mod tests {
    use crate::techniques::partially_ordered_trace_distance::PartiallyOrderedTraceDistance;
    use ebi_objects::{
        FiniteStochasticPartiallyOrderedLanguage, HasActivityKey,
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
}
