use crate::{
    ebi_framework::displayable::Displayable,
    ebi_traits::ebi_trait_stochastic_semantics::EbiTraitStochasticSemantics,
    stochastic_semantics::stochastic_semantics::StochasticSemantics,
    techniques::align::transform_alignment,
};
use anyhow::{Result, anyhow};
use ebi_objects::{
    Activity, LanguageOfAlignments,
    ebi_arithmetic::{Fraction, One, OneMinus, Zero},
};
use ebi_optimisation::astar;
use std::ops::{Add, AddAssign};

#[derive(Clone, Debug)]
pub struct StochasticWeightedCost {
    cost: u32,
    probability: Fraction,
    stochastic_weighted_cost: Fraction,
}

impl Zero for StochasticWeightedCost {
    fn zero() -> Self {
        StochasticWeightedCost {
            cost: 0,
            probability: Fraction::one(),
            stochastic_weighted_cost: Fraction::zero(),
        }
    }

    fn is_zero(&self) -> bool {
        self.stochastic_weighted_cost.is_zero()
    }
}

impl Add for StochasticWeightedCost {
    type Output = Self;

    fn add(self, other: Self) -> Self {
        let probability = &self.probability * &other.probability;
        let cost = self.cost + other.cost;
        StochasticWeightedCost {
            cost: cost,
            probability: probability.clone(),
            stochastic_weighted_cost: &probability.clone().one_minus() * cost,
        }
    }
}

impl AddAssign for StochasticWeightedCost {
    fn add_assign(&mut self, other: Self) {
        self.cost += other.cost;
        self.probability *= other.probability;
    }
}

impl Ord for StochasticWeightedCost {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        let self_stochastic_cost = &self.probability.clone().one_minus() * self.cost;
        let other_stochastic_cost = &other.probability.clone().one_minus() * other.cost;
        self_stochastic_cost.cmp(&other_stochastic_cost)
    }
}

impl PartialOrd for StochasticWeightedCost {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        Some(self.cmp(other))
    }
}

impl PartialEq for StochasticWeightedCost {
    fn eq(&self, other: &Self) -> bool {
        self.cmp(other) == std::cmp::Ordering::Equal
    }
}

impl Eq for StochasticWeightedCost {}

pub trait ExplainTrace {
    fn explain_trace(
        &self,
        trace: &Vec<Activity>,
        balance: &Fraction,
    ) -> Result<LanguageOfAlignments>;
}

impl ExplainTrace for EbiTraitStochasticSemantics {
    fn explain_trace(
        &self,
        trace: &Vec<Activity>,
        balance: &Fraction,
    ) -> Result<LanguageOfAlignments> {
        match self {
            EbiTraitStochasticSemantics::Usize(sem) => sem.explain_trace(trace, balance),
            EbiTraitStochasticSemantics::Marking(sem) => sem.explain_trace(trace, balance),
            EbiTraitStochasticSemantics::TreeMarking(sem) => sem.explain_trace(trace, balance),
        }
    }
}

impl<State: Displayable> dyn StochasticSemantics<StoSemState = State, SemState = State, AliState = State> {
    pub fn explain_trace(
        &self,
        trace: &Vec<Activity>,
        _balance: &Fraction,
    ) -> Result<LanguageOfAlignments> {
        // get the start state
        if let Some(initial_state) = self.get_initial_state() {
            let start = (0, initial_state);

            // successor relation in the model
            let successors = |(trace_index, state): &(usize, State)| {
                let mut result: Vec<((usize, State), StochasticWeightedCost)> = vec![];

                // log::debug!("successors of log {} model {}", trace_index, state);
                if trace_index < &trace.len() {
                    //we can do a log move
                    // log::debug!("\tlog move {}", trace[*trace_index]);

                    result.push((
                        (trace_index + 1, state.clone()),
                        StochasticWeightedCost {
                            cost: 10000,
                            probability: Fraction::one(),
                            stochastic_weighted_cost: Fraction::zero(),
                        },
                    ));
                }

                //walk through the enabled transitions in the model
                for transition in self.get_enabled_transitions(&state) {
                    let total_weight = self
                        .get_total_weight_of_enabled_transitions(&state)
                        .unwrap();

                    let mut new_state = state.clone();
                    // log::debug!("\t\tnew state before {}", new_state);
                    let _ = self.execute_transition(&mut new_state, transition);
                    // log::debug!("\t\tnew state after {}", new_state);

                    let transition_weight = self.get_transition_weight(&state, transition);
                    let transition_probability = transition_weight / &total_weight;

                    if let Some(activity) = self.get_transition_activity(transition) {
                        //non-silent model move
                        result.push((
                            (*trace_index, new_state.clone()),
                            StochasticWeightedCost {
                                cost: 10000,
                                probability: transition_probability.clone(),
                                stochastic_weighted_cost: &transition_probability
                                    .clone()
                                    .one_minus()
                                    * 10000,
                            },
                        ));
                        // log::debug!("\tmodel move t{} {} to {}", transition, activity, new_state);

                        //which may also be a synchronous move
                        if trace_index < &trace.len() && activity == trace[*trace_index] {
                            //synchronous move
                            // log::debug!("\tsynchronous move t{} {} to {}", transition, activity, new_state);
                            result.push((
                                (trace_index + 1, new_state),
                                StochasticWeightedCost {
                                    cost: 0,
                                    probability: transition_probability,
                                    stochastic_weighted_cost: Fraction::zero(),
                                },
                            ));
                        }
                    } else {
                        //silent move
                        result.push((
                            (*trace_index, new_state),
                            StochasticWeightedCost {
                                cost: 1,
                                probability: transition_probability.clone(),
                                stochastic_weighted_cost: transition_probability
                                    .clone()
                                    .one_minus(),
                            },
                        ));
                    }
                }

                // log::debug!("successors of {} {}: {:?}", trace_index, state, result);
                result
            };

            //function that returns a heuristic on how far we are still minimally from a final state
            let heuristic = |_astate: &(usize, State)| StochasticWeightedCost::zero();

            //function that returns whether we are in a final synchronous product state
            let success = |(trace_index, state): &(usize, State)| {
                trace_index == &trace.len() && self.is_final_state(&state)
            };

            match astar::astar(&start, successors, heuristic, success) {
                Some((path, _cost)) => {
                    let moves = transform_alignment(self, &trace, path)?;
                    let mut alignments = LanguageOfAlignments::new(self.activity_key().clone());
                    alignments.push(moves);
                    // log::debug!("cost:{:.4},", cost, prefix_cost, prefix_probability);
                    Ok(alignments)
                }
                None => Err(anyhow!("no alignment found")),
            }
        } else {
            Err(anyhow!("Cannot align from an empty language."))
        }
    }
}
