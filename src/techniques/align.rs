use std::{fmt::{Debug, Display}, hash::Hash, sync::{Arc, Mutex}, u128};
use bitvec::vec;
use rayon::iter::{IntoParallelIterator, ParallelIterator};
use anyhow::{anyhow, Context, Error, Result};
use good_lp::*;
use crate::{ebi_framework::{activity_key::{Activity, ActivityKeyTranslator}, ebi_command::EbiCommand}, ebi_objects::{alignments::{Alignments, Move}, deterministic_finite_automaton_semantics::DeterministicFiniteAutomatonSemantics, finite_stochastic_language_semantics::FiniteStochasticLanguageSemantics, labelled_petri_net::{LPNMarking, LabelledPetriNet}, stochastic_deterministic_finite_automaton_semantics::StochasticDeterministicFiniteAutomatonSemantics, stochastic_labelled_petri_net::StochasticLabelledPetriNet, stochastic_language_of_alignments::StochasticLanguageOfAlignments}, ebi_traits::{ebi_trait_finite_language::EbiTraitFiniteLanguage, ebi_trait_finite_stochastic_language::EbiTraitFiniteStochasticLanguage, ebi_trait_semantics::{EbiTraitSemantics, Semantics}, ebi_trait_stochastic_semantics::TransitionIndex}, math::{fraction::Fraction, log_div::LogDiv}};

pub trait Align {
    fn align_language(&mut self, log: Box<dyn EbiTraitFiniteLanguage>) -> Result<Alignments>;

    fn align_stochastic_language(&mut self, log: Box<dyn EbiTraitFiniteStochasticLanguage>) -> Result<StochasticLanguageOfAlignments>;

    /**
	 * Please note to ensure the trace and the semantics use the same ActivityKey, or they have been translated
	 */
    fn align_trace(&self, trace: &Vec<Activity>) -> Result<(Vec<Move>, usize)>;
}

impl Align for EbiTraitSemantics {
    fn align_language(&mut self, log: Box<dyn EbiTraitFiniteLanguage>) -> Result<Alignments> {
		match self {
			EbiTraitSemantics::Usize(sem) => sem.align_language(log),
			EbiTraitSemantics::Marking(sem) => sem.align_language(log),
		}
	}

    fn align_stochastic_language(&mut self, log: Box<dyn EbiTraitFiniteStochasticLanguage>) -> Result<StochasticLanguageOfAlignments> {
		match self {
			EbiTraitSemantics::Usize(sem) => sem.align_stochastic_language(log),
			EbiTraitSemantics::Marking(sem) => sem.align_stochastic_language(log),
		}
	}

	fn align_trace(&self, trace: &Vec<Activity>) -> Result<(Vec<Move>, usize)> {
		match self {
			EbiTraitSemantics::Usize(sem) => sem.align_trace(trace),
			EbiTraitSemantics::Marking(sem) => sem.align_trace(trace),
		}
	}
}

impl <T, FS> Align for T where T: Semantics<State = FS> + Send + Sync + ?Sized, FS: Display + Debug + Clone + Hash + Eq {
    
    fn align_language(&mut self, log: Box<dyn EbiTraitFiniteLanguage>) -> Result<Alignments> {
        let mut activity_key = self.get_activity_key().clone();
        let translator = Arc::new(ActivityKeyTranslator::new(log.get_activity_key(), &mut activity_key));
        let log = Arc::new(log);
        let error: Arc<Mutex<Option<Error>>> = Arc::new(Mutex::new(None));

        log::info!("Compute alignments");
        let progress_bar = EbiCommand::get_progress_bar(log.len());

        //compute alignments multi-threadedly
        let mut aligned_traces = (0..log.len()).into_par_iter().filter_map(|trace_index| {
            let log = Arc::clone(&log);
            let translator = Arc::clone(&translator);
            let self2 = Arc::from(&self);

            let trace = log.get_trace(trace_index).unwrap();
            let trace_translated = translator.translate_trace(trace);


            let result = self2.align_trace(&trace_translated).with_context(|| format!("Aligning trace {:?}", trace));
            progress_bar.inc(1);

            match result {
                Ok((aligned_trace, _)) => Some(aligned_trace),
                Err(err) => {
                    let error = Arc::clone(&error);
                *error.lock().unwrap() = Some(err);
                None
                },
            }
        }).collect::<Vec<_>>();// end parallel execution

        if aligned_traces.len() != log.len() {
            //something went wrong
            if let Ok(mutex) = Arc::try_unwrap(error) {
                if let Ok(err) = mutex.into_inner() {
                    if let Some(err) = err {
                        return Err(err);
                    }
                }
            }
            return Err(anyhow!("Something went wrong when computing alignments."));
        }

        let mut result = Alignments::new(activity_key);
        result.append(&mut aligned_traces);
        Ok(result)
    }

    fn align_stochastic_language(&mut self, log: Box<dyn EbiTraitFiniteStochasticLanguage>) -> Result<StochasticLanguageOfAlignments> {
        let translator = ActivityKeyTranslator::new(log.get_activity_key(), self.get_activity_key_mut());

        let mut result = StochasticLanguageOfAlignments::new(self.get_activity_key().clone());
        for (trace, probability) in log.iter_trace_probability() {
            let trace_translated = translator.translate_trace(trace);
            result.push(self.align_trace(&trace_translated)?.0,probability.clone());
        }

        Ok(result)
    }

    /**
	 * Please note to ensure the trace and the semantics use the same ActivityKey, or they have been translated
	 */
    fn align_trace(&self, trace: &Vec<Activity>) -> Result<(Vec<Move>, usize)> {
        if let Some((states, cost)) = align_astar(self, trace) {
            Ok((transform_alignment(self, trace, states)?, cost))
        } else {
            Err(anyhow!("The synchronous product has no way to terminate: it is impossible to reach a deadlock."))
        }
    }

}

pub fn align_astar<T, FS>(semantics: &T, trace: &Vec<Activity>) -> Option<(Vec<(usize, FS)>, usize)> where T: Semantics<State = FS> + ?Sized, FS: Display + Debug + Clone + Hash + Eq {
    // log::debug!("activity key {:?}", semantics.get_activity_key());
    // log::debug!("align trace {:?}", trace);

    let cache = semantics.initialise_alignment_heuristic_cache();

    let start = (0, semantics.get_initial_state());
    let successors = |(trace_index, state) : &(usize, FS)| {

        let mut result = vec![];

        // log::debug!("successors of log {} model {}", trace_index, state);
        
        if trace_index < &trace.len() {
            //we can do a log move
            let new_trace_index = trace_index + 1;
            let new_state = state.clone();
            // log::debug!("\tlog move {} to {} {}", trace[*trace_index], new_trace_index, new_state);
            result.push(((new_trace_index, new_state), 10000));
        }

        //walk through the enabled transitions in the model
        for transition in semantics.get_enabled_transitions(&state) {

            let mut new_state = state.clone();
            let _ = semantics.execute_transition(&mut new_state, transition);

            if let Some(activity) = semantics.get_transition_activity(transition) {
                //non-silent model move
                result.push(((*trace_index, new_state.clone()), 10000));
                // log::debug!("\tmodel move t{} {} to {} {}", transition, activity, trace_index, new_state);

                //which may also be a synchronous move
                if trace_index < &trace.len() && activity == trace[*trace_index] {
                    //synchronous move
                    let new_trace_index = trace_index + 1;
                    // log::debug!("\tsynchronous move t{} {} to {} {}", transition, activity, new_trace_index, new_state);
                    result.push(((new_trace_index, new_state), 0));
                }
            } else {
                //silent move
                // log::debug!("\tsilent move t{} to {} {}", transition, trace_index, new_state);
                result.push(((*trace_index, new_state), 1));
            }
        }

        // log::debug!("successors of {} {}: {:?}", trace_index, state, result);
        result
    };

    //function that returns a heuristic on how far we are still at least from a final state
    let heuristic = |(trace_index, state) : &(usize, FS)| {
        semantics.underestimate_cost_to_final_synchronous_state(trace, trace_index, state, &cache)
    };

    //function that returns whether we are in a final synchronous product state
    let success = |(trace_index, state): &(usize, FS)| {
        let result = trace_index == &trace.len() && semantics.is_final_state(&state);
        // log::debug!("state {} {} is final: {}", trace_index, state, result);
        result
    };

    pathfinding::prelude::astar(&start, successors, heuristic, success)
}

/**
 * The a* function we use returns a sequence of states, while we need a sequence of moves.
 * This function transforms the sequence of states into a sequence of moves.
 * 
 * This function assumes equal costs (of 10000) for the log and model moves, and 1 for the silent moves.
 */
pub fn transform_alignment<T, FS>(semantics: &T, trace: &Vec<Activity>, states: Vec<(usize, FS)>) -> Result<Vec<Move>> where T: Semantics<State = FS> + ?Sized, FS: Display + Debug + Clone + Hash + Eq {
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
                let transition = find_transition_with_label(semantics, &previous_state, &state, activity).with_context(|| format!("Map synchronous move from {} {} to {} {} with label {}", previous_trace_index, previous_state, trace_index, state, activity))?;
                alignment.push(Move::SynchronousMove(activity, transition));
            }

        } else {
            //we did not do a move on the log

            if let Some(transition) = is_there_a_silent_transition_enabled(semantics, &previous_state, &state) {
                //there is a silent transition enabled, which is the cheapest
                alignment.push(Move::SilentMove(transition));
            } else {
                //otherwise, we take an arbitrary labelled model move
                let transition = find_labelled_transition(semantics, &previous_state, &state)?;
                alignment.push(Move::ModelMove(semantics.get_transition_activity(transition).unwrap(), transition));
            }
        }

        previous_trace_index = trace_index;
        previous_state = state;

        // log::debug!("prefix: {:?}", alignment);
    }

    Ok(alignment)
}

pub fn is_there_a_silent_transition_enabled<T, FS>(semantics: &T, from: &FS, to: &FS) -> Option<TransitionIndex> where T: Semantics<State = FS> + ?Sized, FS: Display + Debug + Clone + Hash + Eq {
    for transition in semantics.get_enabled_transitions(from) {
        if semantics.is_transition_silent(transition) {
            let mut from = from.clone();
            let _ = semantics.execute_transition(&mut from, transition);
            if &from == to {
                return Some(transition);
            }
        }
    }
    None
}

pub fn find_transition_with_label<T, FS>(semantics: &T, from: &FS, to: &FS, label: Activity) -> Result<TransitionIndex> where T: Semantics<State = FS> + ?Sized, FS: Display + Debug + Clone + Hash + Eq {
    // log::debug!("find transition with label {}", label);
    for transition in semantics.get_enabled_transitions(from) {
        // log::debug!("transition {} is enabled", transition);
        if semantics.get_transition_activity(transition) == Some(label) {
            let mut from = from.clone();
            semantics.execute_transition(&mut from, transition)?;
            if &from == to {
                return Ok(transition);
            }
        }
    }
    Err(anyhow!("There is no transition with activity {} that brings the model from {} to {}", label, from, to))
}

pub fn find_labelled_transition<T, FS>(semantics: &T, from: &FS, to: &FS) -> Result<TransitionIndex> where T: Semantics<State = FS> + ?Sized, FS: Display + Debug + Clone + Hash + Eq {
    for transition in semantics.get_enabled_transitions(from) {
        if !semantics.is_transition_silent(transition) {
            let mut from = from.clone();
            semantics.execute_transition(&mut from, transition)?;
            if &from == to {
                return Ok(transition);
            }
        }
    }
    Err(anyhow!("There is no transition with any activity enabled that brings the model from {} to {}", from, to))
}

pub trait AlignmentHeuristics {
    type AState;

    fn initialise_alignment_heuristic_cache(&self) -> Vec<Vec<usize>>;

    fn get_incidence_matrix(&self, trace: &Vec<Activity>) -> (Vec<Vec<f64>>,Vec<Vec<usize>>);

    /*
    The function returns the length of the shortest path from the initial state to the final state
     */
    fn get_shortest_path(&self) -> usize;

    /*
    The function transforms a marking in the synchronous product net into a marking vector
     */
    fn get_marking_vector(&self, trace: &Vec<Activity>, trace_index: &usize, state: &Self::AState) -> Vec<f64>;

    /*
    The function gets the move cost function for each transition in the synchronous product net
     */
    fn get_move_cost_function(&self, trace:&Vec<Activity> ) -> Vec<f64>;

    /*
    The function assigns the maximum firing probability for each transition.
    Transition from trace has a probability of 1, for sync and model move, the probaility is different.
    */
    fn get_probability_function(&self, _: &Vec<Activity>, _: &usize, _: &Self::AState, _: &Vec<Vec<usize>>) -> Vec<usize>;

    /*
    The function gets the total number of transitions in the synchronous product net
     */
    fn get_transition_num(&self, trace:&Vec<Activity>) -> usize;

    /*
    The function underestimates the move cost based on the current marking, cost function, incidence matrix and transition preset
     */
    fn underestimate_move_cost(&self, 
        marking_vec: &Vec<f64>,
        move_cost_function: &Vec<f64>,
        incidence_matrix: &Vec<Vec<f64>>,
        transition_preset: &Vec<Vec<usize>>) -> Result<f64>;

        /*
    The function overestimates the probability based on the current marking, cost function, incidence matrix and transition preset
     */
    fn overestimate_probability_gain(&self, 
        marking_vec: &Vec<f64>,
        probability_function: &Vec<f64>,
        incidence_matrix: &Vec<Vec<f64>>,
        transition_preset: &Vec<Vec<usize>>) -> Result<f64>;
    
    fn underestimate_cost_to_final_synchronous_state(&self, trace: &Vec<Activity>, trace_index: &usize, state: &Self::AState, cache: &Vec<Vec<usize>>) -> usize;

}

impl AlignmentHeuristics for DeterministicFiniteAutomatonSemantics {
    type AState = usize;

    fn initialise_alignment_heuristic_cache(&self) -> Vec<Vec<usize>> {
        vec![]
    }
    
    fn get_incidence_matrix(&self, trace: &Vec<Activity>) -> (Vec<Vec<f64>>,Vec<Vec<usize>>) {
        todo!()
    }
    
    fn get_move_cost_function(&self, trace:&Vec<Activity> ) -> Vec<f64> {
        todo!()
    }
    
    fn get_probability_function(&self, _: &Vec<Activity>, _: &usize, _: &Self::AState, _: &Vec<Vec<usize>>) -> Vec<usize> {
        todo!()
    }
    
    fn get_marking_vector(&self, trace: &Vec<Activity>, trace_index: &usize, state: &Self::AState) -> Vec<f64> {
        todo!()
    }
    
    fn get_transition_num(&self, trace:&Vec<Activity>) -> usize {
        todo!()
    }
    
    fn underestimate_move_cost(&self, 
        marking_vec: &Vec<f64>,
        cost_function: &Vec<f64>,
        incidence_matrix: &Vec<Vec<f64>>,
        transition_preset: &Vec<Vec<usize>>
        ) -> Result<f64> {
        todo!()
    }
    
    fn underestimate_cost_to_final_synchronous_state(&self, trace: &Vec<Activity>, trace_index: &usize, state: &Self::AState, cache: &Vec<Vec<usize>>) -> usize {
        todo!()
    }

    
    fn overestimate_probability_gain(&self, 
    marking_vec: &Vec<f64>,
    cost_function: &Vec<f64>,
    incidence_matrix: &Vec<Vec<f64>>,
    transition_preset: &Vec<Vec<usize>>) -> Result<f64> {
        todo!()
    }
    
    fn get_shortest_path(&self) -> usize {
        todo!()
    }

    
}

impl AlignmentHeuristics for FiniteStochasticLanguageSemantics {
    type AState = usize;

    fn initialise_alignment_heuristic_cache(&self) -> Vec<Vec<usize>> {
        vec![]
    }
    
    fn get_incidence_matrix(&self, trace: &Vec<Activity>) -> (Vec<Vec<f64>>,Vec<Vec<usize>>) {
        todo!()
    }
    
    fn get_move_cost_function(&self, trace:&Vec<Activity> ) -> Vec<f64> {
        todo!()
    }
    
    fn get_probability_function(&self, _: &Vec<Activity>, _: &usize, _: &Self::AState, _: &Vec<Vec<usize>>) -> Vec<usize> {
        todo!()
    }
    
    fn get_marking_vector(&self, trace: &Vec<Activity>, trace_index: &usize, state: &Self::AState) -> Vec<f64> {
        todo!()
    }
    
    fn get_transition_num(&self, trace:&Vec<Activity>) -> usize {
        todo!()
    }
    
    fn underestimate_move_cost(&self, 
        marking_vec: &Vec<f64>,
        cost_function: &Vec<f64>,
        incidence_matrix: &Vec<Vec<f64>>,
        transition_preset: &Vec<Vec<usize>>
        ) -> Result<f64> {
        todo!()
    }
    
    fn underestimate_cost_to_final_synchronous_state(&self, trace: &Vec<Activity>, trace_index: &usize, state: &Self::AState, cache: &Vec<Vec<usize>>) -> usize {
        todo!()
    }

    
    fn overestimate_probability_gain(&self, 
    _marking_vec: &Vec<f64>,
    _cost_function: &Vec<f64>,
    _incidence_matrix: &Vec<Vec<f64>>,
    _transition_preset: &Vec<Vec<usize>>) -> Result<f64> {
        todo!()
    }
    
    fn get_shortest_path(&self) -> usize {
        todo!()
    }
}

impl AlignmentHeuristics for LabelledPetriNet {
    type AState = LPNMarking;

    fn initialise_alignment_heuristic_cache(&self) -> Vec<Vec<usize>> {
        vec![]
    }
    
    fn get_incidence_matrix(&self, _trace: &Vec<Activity>) -> (Vec<Vec<f64>>,Vec<Vec<usize>>) {
        todo!()
    }
    
    fn get_move_cost_function(&self, _trace:&Vec<Activity> ) -> Vec<f64> {
        todo!()
    }
    
    fn get_probability_function(&self, _: &Vec<Activity>, _: &usize, _: &Self::AState, _: &Vec<Vec<usize>>) -> Vec<usize> {
        todo!()
    }
    
    fn get_marking_vector(&self, _trace: &Vec<Activity>, _trace_index: &usize, _state: &Self::AState) -> Vec<f64> {
        todo!()
    }
    
    fn get_transition_num(&self, _trace:&Vec<Activity>) -> usize {
        todo!()
    }
    
    fn underestimate_move_cost(&self, 
        _marking_vec: &Vec<f64>,
        _cost_function: &Vec<f64>,
        _incidence_matrix: &Vec<Vec<f64>>,
        _transition_preset: &Vec<Vec<usize>>
        ) -> Result<f64> {
        todo!()
    }
    
    fn underestimate_cost_to_final_synchronous_state(&self, _trace: &Vec<Activity>, _trace_index: &usize, _state: &Self::AState, _cache: &Vec<Vec<usize>>) -> usize {
        todo!()
    }
    
    fn overestimate_probability_gain(&self, 
        _marking_vec: &Vec<f64>,
        _cost_function: &Vec<f64>,
        _incidence_matrix: &Vec<Vec<f64>>,
        _transition_preset: &Vec<Vec<usize>>) -> Result<f64> {
        todo!()
    }
    
    fn get_shortest_path(&self) -> usize {
        todo!()
    }
}

impl AlignmentHeuristics for StochasticLabelledPetriNet {
    type AState = LPNMarking;
    
    fn initialise_alignment_heuristic_cache(&self) -> Vec<Vec<usize>> {
        vec![]
    }

    fn underestimate_move_cost(
        &self, 
        current_marking: &Vec<f64>,
        move_cost_function: &Vec<f64>,
        incidence_matrix: &Vec<Vec<f64>>,
        transition_preset: &Vec<Vec<usize>>
        ) -> Result<f64> {
         // create the problem variables (transition firing vector)
         println!("current marking:{:?}", current_marking);

        let num_transitions = incidence_matrix[0].len();

        let num_places = incidence_matrix.len();

        variables!{problem: 0 <= x;}
        let solution_vec: Vec<Variable> = problem.add_vector(variable().min(0).integer(), num_transitions);

        // decision variables are used to check whether the deadlock marking enables any transitions
        let decision_var_vec: Vec<Variable> = problem.add_vector(variable().binary(), num_places);

        // create the objective function, use the dot product of cost function and solution vector
        let objective: Expression = solution_vec.iter().zip(move_cost_function.iter()).map(|(&x, &y)| x * y).sum();

        // define the linear programming model
        let mut model = problem.minimise(objective).using(default_solver);

        // do not let solver printout any information in terminal
        model.set_parameter("log", "0");

        // add constraints based on the marking equation
        let mut deadlock_marking_vec = vec![];

        for p in 0..num_places {
            let mut expr = Expression::from(current_marking[p]);
            for t in 0..num_transitions {
                expr += incidence_matrix[p][t] * solution_vec[t];
            }
            deadlock_marking_vec.push(expr);
        }

        println!("deadlock marking:{:?}", deadlock_marking_vec);
        println!("transition preset: {:?}", transition_preset);

        // for each transition, at lead one of the places in its preset does not have enough tokens
        for transition_idx in 0..num_transitions{
            // for each transition, it should not be enabled after reaching the deadlock marking
            let each_transition_preset: &Vec<usize> = &transition_preset[transition_idx];

            // accumulate the decision variables for places
            let mut expr: Expression = Expression::from(0);
            
            for (place_idx, each_place) in deadlock_marking_vec.iter().enumerate(){
                if each_transition_preset.contains(&place_idx){
                    // use big m method to formulate a conditional constraint
                    let first_right_side_expr: Expression = 1 + decision_var_vec[place_idx] * 1000000;
                    model.add_constraint(each_place.clone().leq(first_right_side_expr));
                    let second_right_side_expr: Expression = 1 - (1 - decision_var_vec[place_idx]) * 1000000;
                    model.add_constraint(each_place.clone().geq(second_right_side_expr));
                   
                    expr += decision_var_vec[place_idx];
                }
            }
            // println!("expr:{:?} and each transition preset len: {:?}",expr,each_transition_preset.len());
            // println!("sum up decision: {:?} and transition preset: {}", expr, each_transition_preset.len());
            model.add_constraint(expr.leq((each_transition_preset.len() as f64)-1.0));
        }
        // solve the problem
        let solution: solvers::coin_cbc::CoinCbcSolution = model.solve()?; 

        // get the solution vector
        let mut solution_vector = Vec::new();
        for i in 0..num_transitions {
            solution_vector.push(solution.value(solution_vec[i]) as i32);
        }
        // println!("solution is:{}",solution.model().obj_value().round());
        println!("solution is:{} and solution vector:{:?}\n",solution.model().obj_value().round(), solution.model().obj_coefficients());
        Ok(solution.model().obj_value().round())
    }
    
    fn overestimate_probability_gain(&self, 
        current_marking: &Vec<f64>,
        probability_function: &Vec<f64>,
        incidence_matrix: &Vec<Vec<f64>>,
        transition_preset: &Vec<Vec<usize>>
        ) -> Result<f64> {
            todo!()
         // create the problem variables (transition firing vector)
        // let num_transitions = incidence_matrix[0].len();
        // let num_places = incidence_matrix.len();

        // variables!{problem: 0 <= x;}
        // let solution_vec: Vec<Variable> = problem.add_vector(variable().min(0).integer(), num_transitions);

        // // create the objective function, use the dot product of cost function and solution vector
        // let probability_objective: Expression = probability_function.iter()
        // .zip(solution_vec.iter())  // pairs up elements from both vectors
        // .map(|(&a, &b)| a * b)  // multiplies paired elements
        // .sum();

        // // define the linear programming model
        // let mut model = problem.maximise(probability_objective).using(default_solver);

        // // do not let solver printout any information in terminal
        // model.set_parameter("log", "0");

        // // add constraints based on the marking equation
        // let mut deadlock_marking_vec = vec![];

        // for p in 0..num_places {
        //     let mut expr = Expression::from(current_marking[p]);
        //     for t in 0..num_transitions {
        //         expr += incidence_matrix[p][t] * solution_vec[t];
        //     }
        //     deadlock_marking_vec.push(expr);
        // }

        // for transition_idx in 0..num_transitions{
        //     // for each transition, it should not be enabled after reaching the deadlock marking
        //     let transition_column: &Vec<usize> = &transition_preset[transition_idx];
        //     let mut expr: Expression = Expression::from(0);
        //     for (place_idx, each_place) in deadlock_marking_vec.iter().enumerate(){
        //         if transition_column.contains(&place_idx){
        //             expr += each_place.clone();
        //         }
        //     }
        //     model.add_constraint(expr.leq((transition_column.len() as f64)-1.0));
            
        // }
        // // solve the problem
        // let solution: solvers::coin_cbc::CoinCbcSolution = model.solve()?; 

        // // get the solution vector
        // let mut solution_vector = Vec::new();
        // for i in 0..num_transitions {
        //     solution_vector.push(solution.value(solution_vec[i]) as i32);
        // }
        // println!("solution is:{}",solution.model().obj_value().round());
        // Ok(solution.model().obj_value().round())
    }
    

    fn get_move_cost_function(&self, trace:&Vec<Activity> ) -> Vec<f64>{
        let mut move_cost_function = vec![];

        // add cost for model move
        for model_transition in 0..self.get_number_of_transitions() {
            // if it is silent model move
            if self.is_transition_silent(model_transition){
                move_cost_function.push(0.0);
            } else {
                move_cost_function.push(1.0);
            }
        }

        // add cost for log move
        for _ in 0..trace.len() {
            move_cost_function.push(1.0);
        }

        // add cost for synchronous move
        for trace_transition_idx in 0..trace.len() {
            for model_transition in 0..self.get_number_of_transitions() {
                if self.get_transition_activity(model_transition) == Some(trace[trace_transition_idx]) {
                    move_cost_function.push(0.0);
                }
            }
        }

        move_cost_function
    }
    
    fn get_probability_function(&self, trace: &Vec<Activity>, _: &usize, _: &Self::AState, _: &Vec<Vec<usize>>) -> Vec<usize> {
        todo!()
        // let mut probability_function = vec![];
        
        // let mut result:Vec<Vec<usize>> = vec![];
        // // for each transition, get the maximum firing probability
        // // when a transition is enabled, get other enabled transitions
        // for i in 0..self.get_number_of_transitions(){     
        //     let mut weight_sum = self.get_transition_weight(i).clone();

        //     // check whether other transition has the same preset
        //     for j in 0..self.get_number_of_transitions(){
        //         if i != j {  
        //         // Check if vecs[i] is a subset of vecs[j]
        //         let is_subset = self.transition2input_places[i].iter().all(|item| self.transition2input_places[j].contains(item));
                
        //         if is_subset {
        //                 weight_sum += self.get_transition_weight(j).clone();
        //             }
        //         }
        //     }
            
        //     let mut firing_probability = self.get_transition_weight(i).clone();
        //     firing_probability/=weight_sum;
        //     probability_function[i] = LogDiv::log2(firing_probability).;
        // }

        // for trace_transition_idx in 0..trace.len() {
        //     for model_transition in 0..self.get_number_of_transitions() {
        //         if self.get_transition_activity(model_transition) == Some(trace[trace_transition_idx]) {
        //             // assign the probability to sync move 
        //             probability_function[self.get_number_of_transitions() + trace.len()+ trace_transition_idx] = probability_function[model_transition];
        //         }
        //     }
        // }
        // probability_function
    }

    fn get_transition_num(&self, trace:&Vec<Activity>) -> usize {
        let mut transition_num = trace.len() + self.get_number_of_transitions();
        
        // add synchronous move
        for trace_transition_idx in 0..trace.len() {
            for model_transition in 0..self.get_number_of_transitions() {
                if self.get_transition_activity(model_transition) == Some(trace[trace_transition_idx]) {
                    transition_num += 1;
                }
            }
        }

        transition_num
    }


    fn get_marking_vector(&self, trace: &Vec<Activity>, trace_index: &usize, state: &Self::AState) -> Vec<f64> {
        let mut marking_vec:Vec<u64> = vec![];

        // iterate all places in the petri net
        marking_vec.extend(state.marking.place2token.clone());

        // get the marking vector from trace
        let mut marking_vec_from_trace = vec![0; trace.len()+1];
        marking_vec_from_trace[*trace_index] = 1;

        // construct the marking vector
        marking_vec.extend(marking_vec_from_trace);

        marking_vec.iter().map(|&x| x as f64).collect()

    }

    fn get_incidence_matrix(&self, trace: &Vec<Activity>) -> (Vec<Vec<f64>>,Vec<Vec<usize>>) {
        let place_num = self.get_number_of_places() + trace.len() + 1;
        let transition_num = self.get_transition_num(trace);
        let mut incidence_matrix:Vec<Vec<f64>> = vec![vec![0.0; transition_num]; place_num];
        let mut transition_preset = vec![vec![]; transition_num];

        // add place2transition to incidence matrix from model
        for (transition_idx, places) in self.transition2input_places.iter().enumerate() {
            for place in places {
                incidence_matrix[*place][transition_idx] = -1.0;
                transition_preset[transition_idx].push(*place);
            }
        }

        // add place2transition to incidence matrix from model
        for (transition_idx, places) in self.transition2output_places.iter().enumerate() {
            for place in places {
                incidence_matrix[*place][transition_idx] = 1.0;
            }
        }

        // add place2transition to incidence matrix from trace
        for trace_idx in 0..trace.len() {
            incidence_matrix[self.get_number_of_places() + trace_idx][self.get_number_of_transitions() + trace_idx] = -1.0;
            transition_preset[self.get_number_of_transitions() + trace_idx].push(self.get_number_of_places() + trace_idx);
            incidence_matrix[self.get_number_of_places() + trace_idx + 1][self.get_number_of_transitions() + trace_idx] = 1.0;
        }

        // add place2transition to incidence matrix from sync move
        let mut sync_num = 0;
        for trace_transition_idx in 0..trace.len() {
            for model_transition in 0..self.get_number_of_transitions() {
                if self.get_transition_activity(model_transition) == Some(trace[trace_transition_idx]) {
                    sync_num+=1;

                    // get output places of model 
                    self.transition2output_places[model_transition].iter().for_each(|place| {
                        incidence_matrix[*place][self.get_number_of_transitions() + trace.len()+ trace_transition_idx] = 1.0;
                    });

                    // get input places of model 
                    self.transition2input_places[model_transition].iter().for_each(|place| {
                        incidence_matrix[*place][self.get_number_of_transitions() + trace.len()+ trace_transition_idx] = -1.0;
                        transition_preset[self.get_number_of_transitions() + trace.len()+ trace_transition_idx].push(*place);
                    });

                    // get output place of trace net
                    incidence_matrix[self.get_number_of_places()+trace_transition_idx+1][self.get_number_of_transitions() + trace.len()+ trace_transition_idx] = 1.0;

                    incidence_matrix[self.get_number_of_places()+trace_transition_idx][self.get_number_of_transitions() + trace.len()+ trace_transition_idx] = -1.0;
                    transition_preset[self.get_number_of_transitions() + trace.len()+ trace_transition_idx].push(self.get_number_of_places()+trace_transition_idx);

                }
            }
        }
        (incidence_matrix, transition_preset)
    }
    
    fn underestimate_cost_to_final_synchronous_state(&self, trace: &Vec<Activity>, trace_index: &usize, state: &Self::AState, cache: &Vec<Vec<usize>>) -> usize {
        todo!()
    }
    
    fn get_shortest_path(&self) -> usize {
        todo!()
    }
}

impl AlignmentHeuristics for StochasticDeterministicFiniteAutomatonSemantics {
    type AState = usize;
    
    fn initialise_alignment_heuristic_cache(&self) -> Vec<Vec<usize>> {
        vec![]
    }
    
    fn get_incidence_matrix(&self, trace: &Vec<Activity>) -> (Vec<Vec<f64>>,Vec<Vec<usize>>) {
        todo!()
    }
    
    fn get_move_cost_function(&self, trace:&Vec<Activity> ) -> Vec<f64> {
        todo!()
    }
    
    fn get_probability_function(&self, _: &Vec<Activity>, _: &usize, _: &Self::AState, _: &Vec<Vec<usize>>) -> Vec<usize> {
        todo!()
    }
    
    fn get_marking_vector(&self, trace: &Vec<Activity>, trace_index: &usize, state: &Self::AState) -> Vec<f64> {
        todo!()
    }
    
    fn get_transition_num(&self, trace:&Vec<Activity>) -> usize {
        todo!()
    }
    
    fn underestimate_move_cost(&self, 
        marking_vec: &Vec<f64>,
        cost_function: &Vec<f64>,
        incidence_matrix: &Vec<Vec<f64>>,
        transition_preset: &Vec<Vec<usize>>
        ) -> Result<f64> {
        todo!()
    }
    
    fn underestimate_cost_to_final_synchronous_state(&self, trace: &Vec<Activity>, trace_index: &usize, state: &Self::AState, cache: &Vec<Vec<usize>>) -> usize {
        todo!()
    }

    fn overestimate_probability_gain(&self, 
    marking_vec: &Vec<f64>,
    cost_function: &Vec<f64>,
    incidence_matrix: &Vec<Vec<f64>>,
    transition_preset: &Vec<Vec<usize>>) -> Result<f64> {
        todo!()
    }
    
    fn get_shortest_path(&self) -> usize {
        todo!()
    }
}