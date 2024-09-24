use std::{fmt::{Alignment, Debug, Display}, hash::Hash, io::BufRead, ops::{Add, AddAssign}, rc::Rc};
use anyhow::{anyhow, Context, Result};
use fraction::{One, Zero};
use crate::{ebi_framework::{activity_key::{self, Activity, ActivityKey}}, ebi_objects::alignments::{Alignments,Move}, ebi_traits::{ebi_trait_stochastic_semantics::{EbiTraitStochasticSemantics, StochasticSemantics}}, math::{astar,fraction::Fraction}};

#[derive (Clone, Debug)]
pub struct StochasticWeightedCost{
    cost: u32,
    probability: Fraction,
    stochastic_weighted_cost: Fraction
}

impl Zero for StochasticWeightedCost{
    fn zero() -> Self{
        StochasticWeightedCost{
            cost: 0,
            probability: Fraction::one(),
            stochastic_weighted_cost: Fraction::zero()
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
            stochastic_weighted_cost: &probability.clone().one_minus() * cost
        }
    }
}

impl AddAssign for StochasticWeightedCost{
    fn add_assign(&mut self, other: Self){
        self.cost += other.cost;
        self.probability *= other.probability;
    }
}

impl Ord for StochasticWeightedCost{
    fn cmp(&self, other: &Self) -> std::cmp::Ordering{
        let self_stochastic_cost =  &self.probability.clone().one_minus() * self.cost;
        let other_stochastic_cost = &other.probability.clone().one_minus() * other.cost;
        self_stochastic_cost.cmp(&other_stochastic_cost)
    }
}

impl PartialOrd for StochasticWeightedCost{
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering>{
        Some(self.cmp(other))
    }
}

impl PartialEq for StochasticWeightedCost{
    fn eq(&self, other: &Self) -> bool{
        self.cmp(other) == std::cmp::Ordering::Equal
    }
}

impl Eq for StochasticWeightedCost{}


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

impl <FS: Hash + Display + Debug + Clone + Eq> dyn StochasticSemantics<State = FS> {

    pub fn explain_trace(&self, trace: &Vec<Activity>, balance: &Fraction) -> Result<Alignments> {

        // get the start state
        let start = (0, self.get_initial_state());

        // successor relation in the model
        let successors = |(trace_index, state) : &(usize, FS)| {

            let mut result: Vec<((usize, FS), StochasticWeightedCost)> = vec![];

            // log::debug!("successors of log {} model {}", trace_index, state);
            if trace_index < &trace.len() {
                //we can do a log move
                // log::debug!("\tlog move {}", trace[*trace_index]);

                result.push(((trace_index + 1, state.clone()), StochasticWeightedCost{cost:10000, probability:Fraction::one(), stochastic_weighted_cost:Fraction::zero()}));
            }
        
            //walk through the enabled transitions in the model
            for transition in self.get_enabled_transitions(&state) {
                let total_weight = self.get_total_weight_of_enabled_transitions(&state).unwrap();

                let mut new_state = state.clone();
                // log::debug!("\t\tnew state before {}", new_state);
                let _ = self.execute_transition(&mut new_state, transition);
                // log::debug!("\t\tnew state after {}", new_state);

                let transition_weight = self.get_transition_weight(&state, transition);
                let transition_probability = transition_weight / &total_weight;

                if let Some(activity) = self.get_transition_activity(transition) {

                    //non-silent model move
                    result.push(((*trace_index, new_state.clone()),StochasticWeightedCost{cost:10000, probability:transition_probability.clone(),stochastic_weighted_cost:&transition_probability.clone().one_minus()*10000}));
                    // log::debug!("\tmodel move t{} {} to {}", transition, activity, new_state);

                    //which may also be a synchronous move
                    if trace_index < &trace.len() && activity == trace[*trace_index] {
                        //synchronous move
                        // log::debug!("\tsynchronous move t{} {} to {}", transition, activity, new_state);
                        result.push(((trace_index + 1, new_state), StochasticWeightedCost{cost:0, probability:transition_probability, stochastic_weighted_cost:Fraction::zero()}));
                    }
                } else {
                    //silent move
                    result.push(((*trace_index, new_state), StochasticWeightedCost{cost:1, probability:transition_probability.clone(),stochastic_weighted_cost:transition_probability.clone().one_minus()}));
                }
            }

            // log::debug!("successors of {} {}: {:?}", trace_index, state, result);
            result
        };

        //function that returns a heuristic on how far we are still minimally from a final state
        let heuristic = |astate : &(usize, FS)| {
            StochasticWeightedCost::zero()
        };

        //function that returns whether we are in a final synchronous product state
        let success = |(trace_index, state): &(usize, FS)| {
            trace_index == &trace.len() && self.is_final_state(&state)
        };

        match astar::astar(&start, successors, heuristic, success){
            Some((path, cost)) => {
                let mut moves = self.transform_alignment(&trace,path)?;
                let mut alignments = Alignments::new(self.get_activity_key().clone());
                alignments.push(moves);
                // println!("cost:{:.4},", cost, prefix_cost, prefix_probability);
                Ok(alignments)
            },
            None => Err(anyhow!("no alignment found"))
        }

    }

    /**
     * The a* function we use returns a sequence of states, while we need a sequence of moves.
     * This function transforms the sequence of states into a sequence of moves.
     * 
     * This function assumes equal costs (of 10000) for the log and model mvoes, and 1 for the silent moves.
     */
    pub fn transform_alignment(&self, trace: &Vec<Activity>, states: Vec<(usize, FS)>) -> Result<Vec<Move>> {
        let mut alignment = vec![];

        let mut it = states.into_iter();
        
        let (mut previous_trace_index, mut previous_state) = it.next().unwrap();
        
        for (trace_index, state) in it {

            if trace_index != previous_trace_index {
                //we did a move on the log
                let activity = trace[previous_trace_index];

                if previous_state == state {
                    //we did not move on the model => log move
                    alignment.push(Move::LogMove(activity));
                } else {
                    //we moved on the model => synchronous move
                    let transition = self.find_transition_with_label(&previous_state, &state, activity).with_context(|| format!("Map synchronous move from {} {} to {} {} with label {}", previous_trace_index, previous_state, trace_index, state, activity))?;
                    alignment.push(Move::SynchronousMove(activity, transition));
                }

            } else {
                //we did not do a move on the log

                if let Some(transition) = self.is_there_a_silent_transition_enabled(&previous_state, &state) {
                    //there is a silent transition enabled, which is the cheapest
                    alignment.push(Move::SilentMove(transition));
                } else {
                    //otherwise, we take an arbitrary labelled model move
                    let transition = self.find_labelled_transition(&previous_state, &state)?;
                    alignment.push(Move::ModelMove(self.get_transition_activity(transition).unwrap(), transition));
                }
            }

            previous_trace_index = trace_index;
            previous_state = state;

            // log::debug!("prefix: {:?}", alignment);
        }

        Ok(alignment)
    }

    fn find_transition_with_label(&self, from: &FS, to: &FS, label: Activity) -> Result<TransitionIndex> {
        // log::debug!("find transition with label {}", label);
        for transition in self.get_enabled_transitions(from) {
            // log::debug!("transition {} is enabled", transition);
            if self.get_transition_activity(transition) == Some(label) {
                let mut from = from.clone();
                self.execute_transition(&mut from, transition)?;
                if &from == to {
                    return Ok(transition);
                }
            }
        }
        Err(anyhow!("There is no transition with activity {} that brings the model from {} to {}", label, from, to))
    }

    fn find_labelled_transition(&self, from: &FS, to: &FS) -> Result<TransitionIndex> {
        for transition in self.get_enabled_transitions(from) {
            if !self.is_transition_silent(transition) {
                let mut from = from.clone();
                self.execute_transition(&mut from, transition)?;
                if &from == to {
                    return Ok(transition);
                }
            }
        }
        Err(anyhow!("There is no transition with any activity enabled that brings the model from {} to {}", from, to))
    }

    fn is_there_a_silent_transition_enabled(&self, from: &FS, to: &FS) -> Option<TransitionIndex> {
        for transition in self.get_enabled_transitions(from) {
            if self.is_transition_silent(transition) {
                let mut from = from.clone();
                self.execute_transition(&mut from, transition);
                if &from == to {
                    return Some(transition);
                }
            }
        }
        None
    }
}


pub type TransitionIndex = usize;

pub trait ToStochasticSemantics {
	type State: Eq + Hash + Clone + Display;
	fn get_stochastic_semantics(net: Rc<Self>) -> Box<dyn StochasticSemantics<State = Self::State>>;

	fn import_as_stochastic_semantics(reader: &mut dyn BufRead) -> Result<EbiTraitStochasticSemantics>;
}