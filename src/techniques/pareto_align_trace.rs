use core::f64;
use std::{cmp::Ordering, fmt::{Debug, Display}, hash::Hash, ops::{Add, AddAssign}};
use anyhow::{anyhow, Result};
use fraction::Zero;
use crate::{ebi_framework::activity_key::Activity, ebi_objects::alignments::Alignments, ebi_traits::ebi_trait_stochastic_semantics::{EbiTraitStochasticSemantics, StochasticSemantics, TransitionIndex}, math::{astar, fraction::Fraction}, techniques::align::transform_alignment};



#[derive(Debug, Clone, Copy)]
struct StochasticWeightedCost {
    cost: f64,
    probability: f64,
    balance_factor: f64,
    stochastic_loss: f64,
}

impl Zero for StochasticWeightedCost{
    fn zero() -> Self{
        StochasticWeightedCost{
            cost: 0.0,
            probability: 1.0,
            balance_factor: 0.5,
            stochastic_loss: 0.0
        }
    }
    
    fn is_zero(&self) -> bool {
        self.stochastic_loss.is_zero()
    }
}


impl Add for StochasticWeightedCost{
    type Output = Self;

    fn add(self, other: Self) -> Self{
        let probability = &self.probability * &other.probability;
        let cost = self.cost + other.cost;
        StochasticWeightedCost{
            cost: cost,
            probability: probability,
            balance_factor: self.balance_factor,
            stochastic_loss: ((cost+1.0).ln()).powf(self.balance_factor)
            *((1.0-(probability).ln()).powf(1.0-self.balance_factor))
        }
    }
}

// Implement AddAssign operation
impl AddAssign for StochasticWeightedCost {
    fn add_assign(&mut self, other: Self) {
        self.cost += other.cost;
        self.probability *= other.probability;
        self.stochastic_loss = ((self.cost+1.0).ln()).powf(self.balance_factor)*((1.0-self.probability.ln()).powf(1.0-self.balance_factor));
    }
}

// Implement ordering
impl PartialEq for StochasticWeightedCost {
    fn eq(&self, other: &Self) -> bool {
        self.stochastic_loss == other.stochastic_loss
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
        // First compare by stochastic_loss
        match self.stochastic_loss.total_cmp(&other.stochastic_loss) {
            Ordering::Equal => {
                // If stochastic_losss are equal, compare by cost
                other.cost.total_cmp(&self.cost)
            },
            ordering => ordering
        }
    }
}

// pub trait ExplainTrace {
//     fn explain_trace(&self, trace: &Vec<Activity>, balance: &Fraction) -> Result<Alignments>;
// }

// impl ExplainTrace for EbiTraitStochasticSemantics {
//     fn explain_trace(&self, trace: &Vec<Activity>, balance: &Fraction) -> Result<Alignments> {
//         match self {
//             EbiTraitStochasticSemantics::Usize(sem) => sem.explain_trace(trace, balance),
//             EbiTraitStochasticSemantics::Marking(sem) => sem.explain_trace(trace, balance),
//         }
//     }
// }
pub trait ExplainTrace {
    fn explain_trace(&self, trace: &Vec<Activity>, balance: &Fraction) -> Result<Alignments>;

    fn explain_trace_without_reuse(&self, trace: &Vec<Activity>, balance: &Fraction) -> Result<Alignments>;

    fn explain_trace_with_reuse(&self, trace: &Vec<Activity>, balance: &Fraction) -> Result<Alignments>;

    fn explain_trace_without_heuristic(&self, trace: &Vec<Activity>, balance: &Fraction) -> Result<Alignments>;

    fn explain_log(&self, trace: &Vec<Activity>, balance: &Fraction) -> Result<(usize,f64,f64)>;

    fn explain_log_without_h(&self, trace: &Vec<Activity>, balance: &Fraction) -> Result<(usize,f64,f64)>;
}

impl ExplainTrace for EbiTraitStochasticSemantics {
    fn explain_trace(&self, trace: &Vec<Activity>, balance: &Fraction) -> Result<Alignments> {
        match self {
            EbiTraitStochasticSemantics::Usize(sem) => sem.explain_trace(trace, balance),
            EbiTraitStochasticSemantics::Marking(sem) => sem.explain_trace(trace, balance),
        }
    }

    fn explain_log(&self, trace: &Vec<Activity>, balance: &Fraction) -> Result<(usize,f64,f64)> {
        match self {
            EbiTraitStochasticSemantics::Usize(sem) => sem.explain_log(trace, balance),
            EbiTraitStochasticSemantics::Marking(sem) => sem.explain_log(trace, balance),
        }
    }

    fn explain_log_without_h(&self, trace: &Vec<Activity>, balance: &Fraction) -> Result<(usize,f64,f64)> {
        match self {
            EbiTraitStochasticSemantics::Usize(sem) => sem.explain_log_without_h(trace, balance),
            EbiTraitStochasticSemantics::Marking(sem) => sem.explain_log_without_h(trace, balance),
        }
    }
    
    fn explain_trace_without_reuse(&self, trace: &Vec<Activity>, balance: &Fraction) -> Result<Alignments> {
        match self {
            EbiTraitStochasticSemantics::Usize(sem) => sem.explain_trace_without_reuse(trace, balance),
            EbiTraitStochasticSemantics::Marking(sem) => sem.explain_trace_without_reuse(trace, balance),
        }
    }
    
    fn explain_trace_with_reuse(&self, trace: &Vec<Activity>, balance: &Fraction) -> Result<Alignments> {
        match self {
            EbiTraitStochasticSemantics::Usize(sem) => sem.explain_trace_with_reuse(trace, balance),
            EbiTraitStochasticSemantics::Marking(sem) => sem.explain_trace_with_reuse(trace, balance),
        }
    }
    
    fn explain_trace_without_heuristic(&self, trace: &Vec<Activity>, balance: &Fraction) -> Result<Alignments> {
        match self {
            EbiTraitStochasticSemantics::Usize(sem) => sem.explain_trace_without_heuristic(trace, balance),
            EbiTraitStochasticSemantics::Marking(sem) => sem.explain_trace_without_heuristic(trace, balance),
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

            let mut result: Vec<((usize, FS), (usize, TransitionIndex), StochasticWeightedCost)> = vec![];

            // log::debug!("successors of log {} model {}", trace_index, state);
            if trace_index < &trace.len() {
                //we can do a log move
                // log::debug!("\tlog move {}", trace[*trace_index]);
                result.push(((trace_index + 1, state.clone()),(*trace_index+1, 0), 
                StochasticWeightedCost{
                    cost:1.0, 
                    probability:1.0, 
                    balance_factor: balance_to_f64, 
                    stochastic_loss:((2.0_f64).log10()).powf(balance_to_f64)}));
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
                    (0, transition+1),
                    StochasticWeightedCost{
                        cost:1.0,
                        probability:transition_probability,
                        balance_factor: balance_to_f64,
                        stochastic_loss: 1.0-transition_probability}));

                    //which may also be a synchronous move
                    if trace_index < &trace.len() && activity == trace[*trace_index] {
                        //synchronous move
                        // log::debug!("\tsynchronous move t{} {} to {}", transition, activity, new_state);
                        result.push(((trace_index + 1, new_state), 
                        (*trace_index+1, transition+1),
                        StochasticWeightedCost{
                            cost:0.0, 
                            probability:transition_probability, 
                            balance_factor: balance_to_f64,
                            stochastic_loss:0.0}));
                    }
                } else {
                    //silent move
                    result.push(((*trace_index, new_state), 
                    (0, transition+1),
                    StochasticWeightedCost{
                        cost:0.0, 
                        probability:transition_probability,
                        balance_factor: balance_to_f64,
                        stochastic_loss:0.0}));
                }
            }
            // log::debug!("successors of {} {}: {:?}", trace_index, state, result);
            result
        };

        //function that returns a heuristic on how far we are still minimally from a final state
        let heuristic = 
        |_astate : &(usize, FS)| {
            StochasticWeightedCost::zero()
        };

        //function that returns whether we are in a final synchronous product state
        let success = |(trace_index, state): &(usize, FS)| {
            trace_index == &trace.len() && self.is_final_state(&state)
        };

        match astar::astar(&start, successors, heuristic, success){
            Some((path, _cost, _states)) => {
                let moves = transform_alignment(self, &trace,path)?;
                let mut alignments = Alignments::new(self.get_activity_key().clone());
                alignments.push(moves);
                // println!("cost:{:.4},", cost, prefix_cost, prefix_probability);
                Ok(alignments)
            },
            None => Err(anyhow!("no alignment found"))
        }

    }

    pub fn explain_trace_with_reuse(&self, trace: &Vec<Activity>, balance: &Fraction) -> Result<Alignments> {
        let balance_to_f64 = balance.fraction_to_f64().unwrap();
  
        // get the start state
        let start = (0, self.get_initial_state());

        // successor relation in the model
        let successors = |(trace_index, state) : &(usize, FS)| {

            let mut result: Vec<((usize, FS), (usize, TransitionIndex), StochasticWeightedCost)> = vec![];

            // log::debug!("successors of log {} model {}", trace_index, state);
            if trace_index < &trace.len() {
                //we can do a log move
                // log::debug!("\tlog move {}", trace[*trace_index]);
                result.push(((trace_index + 1, state.clone()),(*trace_index+1, 0), 
                StochasticWeightedCost{
                    cost:1.0, 
                    probability:1.0, 
                    balance_factor: balance_to_f64, 
                    stochastic_loss:((2.0_f64).log10()).powf(balance_to_f64)}));
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
                    (0, transition+1),
                    StochasticWeightedCost{
                        cost:1.0,
                        probability:transition_probability,
                        balance_factor: balance_to_f64,
                        stochastic_loss: 1.0-transition_probability}));

                    //which may also be a synchronous move
                    if trace_index < &trace.len() && activity == trace[*trace_index] {
                        //synchronous move
                        // log::debug!("\tsynchronous move t{} {} to {}", transition, activity, new_state);
                        result.push(((trace_index + 1, new_state), 
                        (*trace_index+1, transition+1),
                        StochasticWeightedCost{
                            cost:0.0, 
                            probability:transition_probability, 
                            balance_factor: balance_to_f64,
                            stochastic_loss:0.0}));
                    }
                } else {
                    //silent move
                    result.push(((*trace_index, new_state), 
                    (0, transition+1),
                    StochasticWeightedCost{
                        cost:0.0, 
                        probability:transition_probability,
                        balance_factor: balance_to_f64,
                        stochastic_loss:0.0}));
                }
            }
            // log::debug!("successors of {} {}: {:?}", trace_index, state, result);
            result
        };

        let result_cost_function = self.get_move_cost_function(trace);

        let cost_function = result_cost_function.0;
        let cost_function_map = result_cost_function.1;
        let result_probability_function = self.get_probability_function(trace);
        let probability_function = result_probability_function.0;

        println!("cost function:{:?}", cost_function);
        println!("probability function:{:?}", probability_function);
        let result1 = self.get_incidence_matrix(trace);
        let incidence_matrix = result1.0;
        let transition_prest = result1.1;
        //function that returns a heuristic on how far we are still minimally from a final state
        let heuristic_estimated = 
        |_astate : &(usize, FS), 
        previous_transition:(usize, TransitionIndex),
        previous_estimated_move_cost: f64, 
        previous_estimated_probability_gain: f64, 
        previous_estimated_move_cost_solution:Vec<f64>, 
        previous_estimated_probability_gain_solution:Vec<f64>| {
   
            // get the index of the transition
            let transition_idx = *cost_function_map.get(&previous_transition).unwrap();
            let mut move_cost=0.0;
            let mut move_cost_solution_vector=Vec::new();
            let mut probability_gain=0.0;
            let mut probability_gain_solution_vector=Vec::new();
            let mut heuristic4cost_is_valid = false;
            let mut heuristic4probability_is_valid = false;
            
            // if the solution vector for move cost can be reused
            if previous_estimated_move_cost_solution[transition_idx] >= 1.0{
                println!("reuse mc");
                move_cost = previous_estimated_move_cost - cost_function[transition_idx];
                move_cost_solution_vector = previous_estimated_move_cost_solution.clone();
                move_cost_solution_vector[transition_idx] -= 1.0;
                heuristic4cost_is_valid = true;
                // println!("\nestimated move cost:{}", move_cost);
            }

            // if the solution vector for the probability can be reused
            if previous_estimated_probability_gain_solution[transition_idx] >= 1.0{
                println!("reuse pg");
                probability_gain = previous_estimated_probability_gain - probability_function[transition_idx];
                probability_gain_solution_vector = previous_estimated_probability_gain_solution.clone();
                probability_gain_solution_vector[transition_idx] -= 1.0;
                heuristic4probability_is_valid = true;
                // println!("estimated probability gain:{}", probability_gain);
            }
            (StochasticWeightedCost{
                cost:move_cost, 
                probability:std::f64::consts::E.powf(probability_gain),
                balance_factor: balance_to_f64,
                stochastic_loss:0.0},
                move_cost.clone(),
                probability_gain.clone(),
                move_cost_solution_vector.clone(),
                probability_gain_solution_vector.clone(),
                heuristic4cost_is_valid,
                heuristic4probability_is_valid
                )
        };

        let cost_heuristic = 
        |_astate : &(usize, FS)| {
            let marking_vec = self.get_marking_vector(trace, &_astate.0, &_astate.1);
            // get the index of the transition
            let mut heuristic4cost_is_valid = false;
            let result = match self.underestimate_move_cost(&marking_vec, &cost_function, &incidence_matrix, &transition_prest){
                Ok((move_cost,move_cost_solution_vector)) => {
                    heuristic4cost_is_valid=true;
                    (move_cost, move_cost_solution_vector)},
                Err(_) => {println!("cost failed lp");
                (f64::MAX, vec![0.0; probability_function.len()])}
            };
            let move_cost = result.0;
            let move_cost_solution_vector = result.1;
            
            // println!("probability gain solution vector:{:?}", probability_gain_solution_vector);
            (move_cost,
            move_cost_solution_vector,
            heuristic4cost_is_valid
            )    
        };

        let probability_heuristic = 
        |_astate : &(usize, FS)| {
                let marking_vec = self.get_marking_vector(trace, &_astate.0, &_astate.1);
                let mut heuristic4probability_is_valid = false;

                let result = match self.overestimate_probability_gain(&marking_vec, &probability_function, &incidence_matrix, &transition_prest){
                    Ok((probability_gain, probability_gain_solution_vector)) => {
                        heuristic4probability_is_valid = true;
                        (probability_gain, probability_gain_solution_vector)},
                    Err(_) => {println!("prob failed lp");
                    (0.0, vec![0.0; probability_function.len()])}
                };
                let probability_gain = result.0;
                let probability_gain_solution_vector = result.1;
            
                (probability_gain,
                probability_gain_solution_vector,
                heuristic4probability_is_valid)    
            };

        //function that returns whether we are in a final synchronous product state
        let success = |(trace_index, state): &(usize, FS)| {
            trace_index == &trace.len() && self.is_final_state(&state)
        };

        match astar::astar4slpn_with_reuse(&start, 
            successors, 
            heuristic_estimated, 
            cost_heuristic,
            probability_heuristic,
            success, 
            cost_function.len()){
            Some((path, _cost, _states)) => {
                let moves = transform_alignment(self, &trace,path)?;
                let mut alignments = Alignments::new(self.get_activity_key().clone());
                alignments.push(moves);
                // println!("cost:{:.4},", cost, prefix_cost, prefix_probability);
                Ok(alignments)
            },
            None => Err(anyhow!("no alignment found"))
        }

    }


    pub fn explain_trace_without_reuse(&self, trace: &Vec<Activity>, balance: &Fraction) -> Result<Alignments> {

        // get the length of the shortest path in the model
        // let shortest_path = self.get_shortest_path_len()?;
        // let maximum_alignment_len = (shortest_path + trace.len()) as f64;

        let balance_to_f64 = balance.fraction_to_f64().unwrap();

       
        // get the start state
        let start = (0, self.get_initial_state());

        // successor relation in the model
        let successors = |(trace_index, state) : &(usize, FS)| {

            let mut result: Vec<((usize, FS), (usize, TransitionIndex), StochasticWeightedCost)> = vec![];

            // log::debug!("successors of log {} model {}", trace_index, state);
            if trace_index < &trace.len() {
                //we can do a log move
                // log::debug!("\tlog move {}", trace[*trace_index]);
                result.push(((trace_index + 1, state.clone()),(*trace_index+1, 0), 
                StochasticWeightedCost{
                    cost:1.0, 
                    probability:1.0, 
                    balance_factor: balance_to_f64, 
                    stochastic_loss:((2.0_f64).log10()).powf(balance_to_f64)}));
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
                    (0, transition+1),
                    StochasticWeightedCost{
                        cost:1.0,
                        probability:transition_probability,
                        balance_factor: balance_to_f64,
                        stochastic_loss: 1.0-transition_probability}));

                    //which may also be a synchronous move
                    if trace_index < &trace.len() && activity == trace[*trace_index] {
                        //synchronous move
                        // log::debug!("\tsynchronous move t{} {} to {}", transition, activity, new_state);
                        result.push(((trace_index + 1, new_state), 
                        (*trace_index+1, transition+1),
                        StochasticWeightedCost{
                            cost:0.0, 
                            probability:transition_probability, 
                            balance_factor: balance_to_f64,
                            stochastic_loss:0.0}));
                    }
                } else {
                    //silent move
                    result.push(((*trace_index, new_state), 
                    (0, transition+1),
                    StochasticWeightedCost{
                        cost:0.0, 
                        probability:transition_probability,
                        balance_factor: balance_to_f64,
                        stochastic_loss:0.0}));
                }
            }
            // log::debug!("successors of {} {}: {:?}", trace_index, state, result);
            result
        };

        let result_cost_function = self.get_move_cost_function(trace);
        let cost_function = result_cost_function.0;
        let result_probability_function = self.get_probability_function(trace);
        let probability_function = result_probability_function.0;
        let result1 = self.get_incidence_matrix(trace);
        let incidence_matrix = result1.0;
        let transition_prest = result1.1;
        //function that returns a heuristic on how far we are still minimally from a final state
        let heuristic = 
        |_astate : &(usize, FS),
        mut number_lp: usize| {
            let marking_vec = self.get_marking_vector(trace, &_astate.0, &_astate.1);
            // get the index of the transition
            let move_cost;
            let probability_gain;
            number_lp += 1;

            let result = match self.underestimate_move_cost(&marking_vec, &cost_function, &incidence_matrix, &transition_prest){
                Ok((move_cost,move_cost_solution_vector)) => {(move_cost, move_cost_solution_vector)},
                Err(_) => {println!("cost failed lp");
                (f64::MAX, vec![0.0; probability_function.len()])}
            };
            move_cost = result.0;
            // println!("\nprecise move cost:{}", move_cost);
            // println!("move cost solution vector:{:?}", move_cost_solution_vector);
        
            number_lp += 1;

            let result = match self.overestimate_probability_gain(&marking_vec, &probability_function, &incidence_matrix, &transition_prest){
                Ok((probability_gain, probability_gain_solution_vector)) => {(probability_gain, probability_gain_solution_vector)},
                Err(_) => {println!("prob failed lp");
                (0.0, vec![0.0; probability_function.len()])}
            };
            probability_gain = result.0;
            // println!("prcecise probability gain:{}", probability_gain);
            // println!("probability gain solution vector:{:?}", probability_gain_solution_vector);
            (StochasticWeightedCost{
                cost:move_cost, 
                probability:std::f64::consts::E.powf(probability_gain),
                balance_factor: balance_to_f64,
                stochastic_loss:0.0},
                number_lp)      
            };
           
        //function that returns whether we are in a final synchronous product state
        let success = |(trace_index, state): &(usize, FS)| {
            trace_index == &trace.len() && self.is_final_state(&state)
        };

        match astar::astar4slpn_without_reuse(&start, successors, heuristic, success){
            Some((path, _cost, _states)) => {
                let moves = transform_alignment(self, &trace,path)?;
                let mut alignments = Alignments::new(self.get_activity_key().clone());
                alignments.push(moves);
                // println!("cost:{:.4},", cost, prefix_cost, prefix_probability);
                Ok(alignments)
            },
            None => Err(anyhow!("no alignment found"))
        }

    }


    pub fn explain_trace_without_heuristic(&self, trace: &Vec<Activity>, balance: &Fraction) -> Result<Alignments> {

        // get the length of the shortest path in the model
        // let shortest_path = self.get_shortest_path_len()?;
        // let maximum_alignment_len = (shortest_path + trace.len()) as f64;

        let balance_to_f64 = balance.fraction_to_f64().unwrap();

       
        // get the start state
        let start = (0, self.get_initial_state());

        // successor relation in the model
        let successors = |(trace_index, state) : &(usize, FS)| {

            let mut result: Vec<((usize, FS), (usize, TransitionIndex), StochasticWeightedCost)> = vec![];

            // log::debug!("successors of log {} model {}", trace_index, state);
            if trace_index < &trace.len() {
                //we can do a log move
                // log::debug!("\tlog move {}", trace[*trace_index]);
                result.push(((trace_index + 1, state.clone()),(*trace_index+1, 0), 
                StochasticWeightedCost{
                    cost:1.0, 
                    probability:1.0, 
                    balance_factor: balance_to_f64, 
                    stochastic_loss:((2.0_f64).log10()).powf(balance_to_f64)}));
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
                    (0, transition+1),
                    StochasticWeightedCost{
                        cost:1.0,
                        probability:transition_probability,
                        balance_factor: balance_to_f64,
                        stochastic_loss: 1.0-transition_probability}));

                    //which may also be a synchronous move
                    if trace_index < &trace.len() && activity == trace[*trace_index] {
                        //synchronous move
                        // log::debug!("\tsynchronous move t{} {} to {}", transition, activity, new_state);
                        result.push(((trace_index + 1, new_state), 
                        (*trace_index+1, transition+1),
                        StochasticWeightedCost{
                            cost:0.0, 
                            probability:transition_probability, 
                            balance_factor: balance_to_f64,
                            stochastic_loss:0.0}));
                    }
                } else {
                    //silent move
                    result.push(((*trace_index, new_state), 
                    (0, transition+1),
                    StochasticWeightedCost{
                        cost:0.0, 
                        probability:transition_probability,
                        balance_factor: balance_to_f64,
                        stochastic_loss:0.0}));
                }
            }
            // log::debug!("successors of {} {}: {:?}", trace_index, state, result);
            result
        };

        //function that returns a heuristic on how far we are still minimally from a final state
        let heuristic = 
        |_astate : &(usize, FS), 
        _previous_transition:(usize, TransitionIndex)| {
            StochasticWeightedCost::zero()
        };

        //function that returns whether we are in a final synchronous product state
        let success = |(trace_index, state): &(usize, FS)| {
            trace_index == &trace.len() && self.is_final_state(&state)
        };

        match astar::astar4slpn_without_heuristic(&start, successors, heuristic, success){
            Some((path, _cost, _states)) => {
                let moves = transform_alignment(self, &trace,path)?;
                let mut alignments = Alignments::new(self.get_activity_key().clone());
                alignments.push(moves);
                // println!("cost:{:.4},", cost, prefix_cost, prefix_probability);
                Ok(alignments)
            },
            None => Err(anyhow!("no alignment found"))
        }

    }

    pub fn explain_log(&self, trace: &Vec<Activity>, balance: &Fraction) -> Result<(usize,f64,f64)> {
        let balance_to_f64 = balance.fraction_to_f64().unwrap();
  
        // get the start state
        let start = (0, self.get_initial_state());

        // successor relation in the model
        let successors = |(trace_index, state) : &(usize, FS)| {

            let mut result: Vec<((usize, FS), (usize, TransitionIndex), StochasticWeightedCost)> = vec![];

            // log::debug!("successors of log {} model {}", trace_index, state);
            if trace_index < &trace.len() {
                //we can do a log move
                // log::debug!("\tlog move {}", trace[*trace_index]);
                result.push(((trace_index + 1, state.clone()),(*trace_index+1, 0), 
                StochasticWeightedCost{
                    cost:1.0, 
                    probability:1.0, 
                    balance_factor: balance_to_f64, 
                    stochastic_loss:((2.0_f64).log10()).powf(balance_to_f64)}));
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
                    (0, transition+1),
                    StochasticWeightedCost{
                        cost:1.0,
                        probability:transition_probability,
                        balance_factor: balance_to_f64,
                        stochastic_loss: 1.0-transition_probability}));

                    //which may also be a synchronous move
                    if trace_index < &trace.len() && activity == trace[*trace_index] {
                        //synchronous move
                        // log::debug!("\tsynchronous move t{} {} to {}", transition, activity, new_state);
                        result.push(((trace_index + 1, new_state), 
                        (*trace_index+1, transition+1),
                        StochasticWeightedCost{
                            cost:0.0, 
                            probability:transition_probability, 
                            balance_factor: balance_to_f64,
                            stochastic_loss:0.0}));
                    }
                } else {
                    //silent move
                    result.push(((*trace_index, new_state), 
                    (0, transition+1),
                    StochasticWeightedCost{
                        cost:0.0, 
                        probability:transition_probability,
                        balance_factor: balance_to_f64,
                        stochastic_loss:0.0}));
                }
            }
            // log::debug!("successors of {} {}: {:?}", trace_index, state, result);
            result
        };

        let result_cost_function = self.get_move_cost_function(trace);

        let cost_function = result_cost_function.0;
        let cost_function_map = result_cost_function.1;

        let result_probability_function = self.get_probability_function(trace);
        let probability_function = result_probability_function.0;
        let result1 = self.get_incidence_matrix(trace);
        let incidence_matrix = result1.0;
        let transition_prest = result1.1;
        //function that returns a heuristic on how far we are still minimally from a final state
        let heuristic_estimated = 
        |_astate : &(usize, FS), 
        previous_transition:(usize, TransitionIndex),
        previous_estimated_move_cost: f64, 
        previous_estimated_probability_gain: f64, 
        previous_estimated_move_cost_solution:Vec<f64>, 
        previous_estimated_probability_gain_solution:Vec<f64>| {
            // get the index of the transition
            let transition_idx = *cost_function_map.get(&previous_transition).unwrap();
            let mut move_cost=0.0;
            let mut move_cost_solution_vector=Vec::new();
            let mut probability_gain=0.0;
            let mut probability_gain_solution_vector=Vec::new();
            let mut heuristic4cost_is_valid = false;
            let mut heuristic4probability_is_valid = false;
            
            // if the solution vector for move cost can be reused
            if previous_estimated_move_cost_solution[transition_idx] >= 1.0{
                move_cost = previous_estimated_move_cost - cost_function[transition_idx];
                move_cost_solution_vector = previous_estimated_move_cost_solution.clone();
                move_cost_solution_vector[transition_idx] -= 1.0;
                heuristic4cost_is_valid = true;
                // println!("\nestimated move cost:{}", move_cost);
            }

            // if the solution vector for the probability can be reused
            if previous_estimated_probability_gain_solution[transition_idx] >= 1.0{
                probability_gain = previous_estimated_probability_gain - probability_function[transition_idx];
                probability_gain_solution_vector = previous_estimated_probability_gain_solution.clone();
                probability_gain_solution_vector[transition_idx] -= 1.0;
                heuristic4probability_is_valid = true;
                // println!("estimated probability gain:{}", probability_gain);
            }
            (StochasticWeightedCost{
                cost:move_cost, 
                probability:std::f64::consts::E.powf(probability_gain),
                balance_factor: balance_to_f64,
                stochastic_loss:0.0},
                move_cost.clone(),
                probability_gain.clone(),
                move_cost_solution_vector.clone(),
                probability_gain_solution_vector.clone(),
                heuristic4cost_is_valid,
                heuristic4probability_is_valid
                )
        };

        let cost_heuristic = 
        |_astate : &(usize, FS)| {
            let marking_vec = self.get_marking_vector(trace, &_astate.0, &_astate.1);
            // get the index of the transition
            let mut heuristic4cost_is_valid = false;
            let result = match self.underestimate_move_cost(&marking_vec, &cost_function, &incidence_matrix, &transition_prest){
                Ok((move_cost,move_cost_solution_vector)) => {
                    heuristic4cost_is_valid=true;
                    (move_cost, move_cost_solution_vector)},
                Err(_) => {println!("cost failed lp");
                (f64::MAX, vec![0.0; probability_function.len()])}
            };
            let move_cost = result.0;
            let move_cost_solution_vector = result.1;
            
            // println!("probability gain solution vector:{:?}", probability_gain_solution_vector);
            (move_cost,
            move_cost_solution_vector,
            heuristic4cost_is_valid
            )    
        };

        let probability_heuristic = 
        |_astate : &(usize, FS)| {
                let marking_vec = self.get_marking_vector(trace, &_astate.0, &_astate.1);
                // get the index of the transition
                let mut heuristic4probability_is_valid = false;

                let result = match self.overestimate_probability_gain(&marking_vec, &probability_function, &incidence_matrix, &transition_prest){
                    Ok((probability_gain, probability_gain_solution_vector)) => {
                        heuristic4probability_is_valid = true;
                        (probability_gain, probability_gain_solution_vector)},
                    Err(_) => {
                    (0.0, vec![0.0; probability_function.len()])}
                };
                let probability_gain = result.0;
                let probability_gain_solution_vector = result.1;
            
                (probability_gain,
                probability_gain_solution_vector,
                heuristic4probability_is_valid)    
            };

        //function that returns whether we are in a final synchronous product state
        let success = |(trace_index, state): &(usize, FS)| {
            trace_index == &trace.len() && self.is_final_state(&state)
        };

        match astar::astar4slpn_with_reuse(&start, 
            successors, 
            heuristic_estimated, 
            cost_heuristic,
            probability_heuristic,
            success, 
            cost_function.len()){
            Some((path, cost, states)) => {
                let moves = transform_alignment(self, &trace,path)?;
                let mut alignments = Alignments::new(self.get_activity_key().clone());
                alignments.push(moves);
                // println!("cost:{:.4},", cost, prefix_cost, prefix_probability);
                println!("cost and probability:{:.4},{}", cost.cost, cost.probability);

                Ok((states, cost.cost, cost.probability))
            },
            None => Err(anyhow!("no alignment found"))
        }

    }

    pub fn explain_log_without_h(&self, trace: &Vec<Activity>, balance: &Fraction) -> Result<(usize,f64,f64)> {
        let balance_to_f64 = balance.fraction_to_f64().unwrap();

       
        // get the start state
        let start = (0, self.get_initial_state());

        // successor relation in the model
        let successors = |(trace_index, state) : &(usize, FS)| {

            let mut result: Vec<((usize, FS), (usize, TransitionIndex), StochasticWeightedCost)> = vec![];

            // log::debug!("successors of log {} model {}", trace_index, state);
            if trace_index < &trace.len() {
                //we can do a log move
                // log::debug!("\tlog move {}", trace[*trace_index]);
                result.push(((trace_index + 1, state.clone()),(*trace_index+1, 0), 
                StochasticWeightedCost{
                    cost:1.0, 
                    probability:1.0, 
                    balance_factor: balance_to_f64, 
                    stochastic_loss:((2.0_f64).log10()).powf(balance_to_f64)}));
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
                    (0, transition+1),
                    StochasticWeightedCost{
                        cost:1.0,
                        probability:transition_probability,
                        balance_factor: balance_to_f64,
                        stochastic_loss: 1.0-transition_probability}));

                    //which may also be a synchronous move
                    if trace_index < &trace.len() && activity == trace[*trace_index] {
                        //synchronous move
                        // log::debug!("\tsynchronous move t{} {} to {}", transition, activity, new_state);
                        result.push(((trace_index + 1, new_state), 
                        (*trace_index+1, transition+1),
                        StochasticWeightedCost{
                            cost:0.0, 
                            probability:transition_probability, 
                            balance_factor: balance_to_f64,
                            stochastic_loss:0.0}));
                    }
                } else {
                    //silent move
                    result.push(((*trace_index, new_state), 
                    (0, transition+1),
                    StochasticWeightedCost{
                        cost:0.0, 
                        probability:transition_probability,
                        balance_factor: balance_to_f64,
                        stochastic_loss:0.0}));
                }
            }
            // log::debug!("successors of {} {}: {:?}", trace_index, state, result);
            result
        };

        //function that returns a heuristic on how far we are still minimally from a final state
        let heuristic = 
        |_astate : &(usize, FS), 
        _previous_transition:(usize, TransitionIndex)| {
            StochasticWeightedCost::zero()
        };

        //function that returns whether we are in a final synchronous product state
        let success = |(trace_index, state): &(usize, FS)| {
            trace_index == &trace.len() && self.is_final_state(&state)
        };

        match astar::astar4slpn_without_heuristic(&start, successors, heuristic, success){
            Some((path, cost, states)) => {
                let moves = transform_alignment(self, &trace,path)?;
                let mut alignments = Alignments::new(self.get_activity_key().clone());
                alignments.push(moves);
                Ok((states, cost.cost, cost.probability))
            },
            None => Err(anyhow!("no alignment found"))
        }

    }
}