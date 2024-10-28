use std::{cmp::Ordering, fmt::{Debug, Display}, hash::Hash, ops::{Add, AddAssign}};
use anyhow::{anyhow, Result};
use fraction::Zero;
use crate::{ebi_framework::activity_key::Activity, ebi_objects::alignments::Alignments, ebi_traits::ebi_trait_stochastic_semantics::{EbiTraitStochasticSemantics, StochasticSemantics}, math::{astar,fraction::Fraction}, techniques::align::transform_alignment};

#[derive(Debug, Clone, Copy)]
struct StochasticWeightedCost {
    pub cost: f64,
    pub probability: f64,
    pub stochastic_weighted_cost: f64,
}

impl Zero for StochasticWeightedCost{
    fn zero() -> Self{
        StochasticWeightedCost{
            cost: 0.0,
            probability: 1.0,
            stochastic_weighted_cost: 0.0
        }
    }
    
    fn is_zero(&self) -> bool {
        self.stochastic_weighted_cost.is_zero()
    }
}


impl Add for StochasticWeightedCost{
    type Output = Self;

    fn add(self, other: Self) -> Self{
        let probability = &self.probability * &other.probability;
        let cost = self.cost + other.cost;
        StochasticWeightedCost{
            cost: cost,
            probability: probability.clone(),
            stochastic_weighted_cost: cost*(1.0-probability)
        }
    }
}

// Implement AddAssign operation
impl AddAssign for StochasticWeightedCost {
    fn add_assign(&mut self, other: Self) {
        self.cost += other.cost;
        self.probability *= other.probability;
        self.stochastic_weighted_cost = self.cost * (1.0 - self.probability);
    }
}

// Implement ordering
impl PartialEq for StochasticWeightedCost {
    fn eq(&self, other: &Self) -> bool {
        self.stochastic_weighted_cost == other.stochastic_weighted_cost && self.cost == other.cost
    }
}

impl Eq for StochasticWeightedCost {}

impl PartialOrd for StochasticWeightedCost {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

impl Ord for StochasticWeightedCost {
    fn cmp(&self, other: &Self) -> Ordering {
        // First compare by stochastic_weighted_cost
        match self.stochastic_weighted_cost.total_cmp(&other.stochastic_weighted_cost) {
            Ordering::Equal => {
                // If stochastic_weighted_costs are equal, compare by cost
                self.cost.total_cmp(&other.cost)
            },
            ordering => ordering
        }
    }
}

pub trait ExplainTrace {
    fn explain_trace(&self, trace: &Vec<Activity>, balance: &Fraction) -> Result<Alignments>;
}

impl ExplainTrace for EbiTraitStochasticSemantics {
    fn explain_trace(&self, trace: &Vec<Activity>, balance: &Fraction) -> Result<Alignments> {
        match self {
            EbiTraitStochasticSemantics::Usize(sem) => sem.explain_trace(trace, balance),
            EbiTraitStochasticSemantics::Marking(sem) => sem.explain_trace(trace, balance),
        }
    }
}

impl <FS: Hash + Display + Debug + Clone + Eq + Send + Sync> dyn StochasticSemantics<State = FS, AState = FS> {

    pub fn explain_trace(&self, trace: &Vec<Activity>, balance: &Fraction) -> Result<Alignments> {

        let balance_to_f64 = balance.fraction_to_f64().unwrap();

        // get the start state
        let start = (0, self.get_initial_state());

        // successor relation in the model
        let successors = |(trace_index, state) : &(usize, FS)| {

            let mut result: Vec<((usize, FS), StochasticWeightedCost)> = vec![];

            // log::debug!("successors of log {} model {}", trace_index, state);
            if trace_index < &trace.len() {
                //we can do a log move
                // log::debug!("\tlog move {}", trace[*trace_index]);

                result.push(((trace_index + 1, state.clone()), StochasticWeightedCost{cost:1.0, probability:1.0, stochastic_weighted_cost:0.0}));
            }
        
            //walk through the enabled transitions in the model
            for transition in self.get_enabled_transitions(&state) {

                let mut new_state = state.clone();
                // log::debug!("\t\tnew state before {}", new_state);
                let _ = self.execute_transition(&mut new_state, transition);
                // log::debug!("\t\tnew state after {}", new_state);

                let transition_weight = self.get_transition_weight(&state, transition);
                let total_weight = self.get_total_weight_of_enabled_transitions(&state).unwrap();
                let transition_probability = (transition_weight / &total_weight).fraction_to_f64().unwrap();

                if let Some(activity) = self.get_transition_activity(transition) {

                    //non-silent model move
                    result.push(((*trace_index, new_state.clone()),
                    StochasticWeightedCost{cost:1.0,
                        probability:transition_probability,
                        stochastic_weighted_cost: 1.0-transition_probability}));

                    //which may also be a synchronous move
                    if trace_index < &trace.len() && activity == trace[*trace_index] {
                        //synchronous move
                        // log::debug!("\tsynchronous move t{} {} to {}", transition, activity, new_state);
                        result.push(((trace_index + 1, new_state), StochasticWeightedCost{
                            cost:0.0, 
                            probability:transition_probability, 
                            stochastic_weighted_cost:0.0}));
                    }
                } else {
                    //silent move
                    result.push(((*trace_index, new_state), StochasticWeightedCost{
                        cost:0.0, 
                        probability:transition_probability,
                        stochastic_weighted_cost:0.0}));
                }
            }
            // log::debug!("successors of {} {}: {:?}", trace_index, state, result);
            result
        };

        let cost_function = self.get_move_cost_function(trace);
        let probability_function = self.get_probability_function(trace);
        let result1 = self.get_incidence_matrix(trace);
        let incidence_matrix = result1.0;
        let transition_prest = result1.1;

        //function that returns a heuristic on how far we are still minimally from a final state
        let heuristic = |_astate : &(usize, FS)| {
            // StochasticWeightedCost::zero()
            let marking_vec = self.get_marking_vector(trace, &_astate.0, &_astate.1);
            let move_cost = self.underestimate_move_cost(&marking_vec, &cost_function, &incidence_matrix, &transition_prest).unwrap();
            let probability_gain = self.overestimate_probability_gain(&marking_vec, &probability_function, &incidence_matrix, &transition_prest).unwrap();

            StochasticWeightedCost{
                cost:move_cost.powf(balance_to_f64), 
                probability:2.0_f64.powf(probability_gain).powf(1.0-balance_to_f64),
                stochastic_weighted_cost:(1.0-probability_gain)*move_cost}
        };

        //function that returns whether we are in a final synchronous product state
        let success = |(trace_index, state): &(usize, FS)| {
            trace_index == &trace.len() && self.is_final_state(&state)
        };

        match astar::astar(&start, successors, heuristic, success){
            Some((path, _cost)) => {
                let moves = transform_alignment(self, &trace,path)?;
                let mut alignments = Alignments::new(self.get_activity_key().clone());
                alignments.push(moves);
                // println!("cost:{:.4},", cost, prefix_cost, prefix_probability);
                Ok(alignments)
            },
            None => Err(anyhow!("no alignment found"))
        }

    }
}