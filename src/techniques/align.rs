use std::{collections::HashMap, fmt::{Debug, Display}, hash::Hash, sync::{Arc, Mutex}};
use rayon::iter::{IntoParallelIterator, ParallelIterator};
use anyhow::{anyhow, Context, Error, Result};
use good_lp::*;


use crate::{ebi_framework::{activity_key::{Activity, ActivityKeyTranslator}, ebi_command::EbiCommand}, ebi_objects::{alignments::{Alignments, Move}, deterministic_finite_automaton_semantics::DeterministicFiniteAutomatonSemantics, finite_stochastic_language_semantics::FiniteStochasticLanguageSemantics, labelled_petri_net::{LPNMarking, LabelledPetriNet}, stochastic_deterministic_finite_automaton_semantics::StochasticDeterministicFiniteAutomatonSemantics, stochastic_labelled_petri_net::StochasticLabelledPetriNet, stochastic_language_of_alignments::StochasticLanguageOfAlignments}, ebi_traits::{ebi_trait_finite_language::EbiTraitFiniteLanguage, ebi_trait_finite_stochastic_language::EbiTraitFiniteStochasticLanguage, ebi_trait_semantics::{EbiTraitSemantics, Semantics}, ebi_trait_stochastic_semantics::TransitionIndex}};

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

    fn get_incidence_matrix(&self, _trace: &Vec<Activity>) -> (Vec<Vec<f64>>,Vec<Vec<usize>>);

    /*
    The function returns the length of the shortest path from the initial state to the final state
     */
    fn get_shortest_path(&self) -> usize;

    /*
    The function transforms a marking in the synchronous product net into a marking vector
     */
    fn get_marking_vector(&self, _trace: &Vec<Activity>, _trace_index: &usize, _state: &Self::AState) -> Vec<f64>;

    /*
    The function gets the move cost function for each transition in the synchronous product net
     */
    fn get_move_cost_function(&self, _trace:&Vec<Activity> ) -> (Vec<f64>,HashMap<(usize,TransitionIndex), usize>);

    /*
    The function assigns the maximum firing probability for each transition.
    Transition from trace has a probability of 1, for sync and model move, the probaility is different.
    */
    fn get_probability_function(&self, trace: &Vec<Activity>) -> (Vec<f64>,HashMap<(usize,TransitionIndex), usize>);

    /*
    The function gets the total number of transitions in the synchronous product net
     */
    fn get_transition_num(&self, _trace:&Vec<Activity>) -> usize;

    /*
    The function underestimates the move cost based on the current marking, cost function, incidence matrix and transition preset
     */
    fn underestimate_move_cost(&self, 
        marking_vec: &Vec<f64>,
        move_cost_function: &Vec<f64>,
        incidence_matrix: &Vec<Vec<f64>>,
        transition_preset: &Vec<Vec<usize>>) -> Result<(f64,Vec<f64>)>;

        /*
    The function overestimates the probability based on the current marking, cost function, incidence matrix and transition preset
     */
    fn overestimate_probability_gain(&self, 
        marking_vec: &Vec<f64>,
        probability_function: &Vec<f64>,
        incidence_matrix: &Vec<Vec<f64>>,
        transition_preset: &Vec<Vec<usize>>) -> Result<(f64,Vec<f64>)>;
    
    fn underestimate_cost_to_final_synchronous_state(&self, _trace: &Vec<Activity>, _trace_index: &usize, _state: &Self::AState, _cache: &Vec<Vec<usize>>) -> usize;

}

impl AlignmentHeuristics for DeterministicFiniteAutomatonSemantics {
    type AState = usize;

    fn initialise_alignment_heuristic_cache(&self) -> Vec<Vec<usize>> {
        vec![]
    }
    
    fn get_incidence_matrix(&self, _trace: &Vec<Activity>) -> (Vec<Vec<f64>>,Vec<Vec<usize>>) {
        todo!()
    }
    
    fn get_move_cost_function(&self, _trace:&Vec<Activity> ) -> (Vec<f64>,HashMap<(usize,TransitionIndex), usize>) {
        todo!()
    }
    
    fn get_probability_function(&self, _trace: &Vec<Activity>) -> (Vec<f64>,HashMap<(usize,TransitionIndex), usize>) {
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
        ) -> Result<(f64,Vec<f64>)> {
        todo!()
    }
    
    fn underestimate_cost_to_final_synchronous_state(&self, _trace: &Vec<Activity>, _trace_index: &usize, _state: &Self::AState, _cache: &Vec<Vec<usize>>) -> usize {
        todo!()
    }

    
    fn overestimate_probability_gain(&self, 
    _marking_vec: &Vec<f64>,
    _cost_function: &Vec<f64>,
    _incidence_matrix: &Vec<Vec<f64>>,
    _transition_preset: &Vec<Vec<usize>>) -> Result<(f64,Vec<f64>)> {
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
    
    fn get_incidence_matrix(&self, _trace: &Vec<Activity>) -> (Vec<Vec<f64>>,Vec<Vec<usize>>) {
        todo!()
    }
    
    fn get_move_cost_function(&self, _trace:&Vec<Activity> ) -> (Vec<f64>,HashMap<(usize,TransitionIndex), usize>) {
        todo!()
    }
    
    fn get_probability_function(&self, _trace: &Vec<Activity>) -> (Vec<f64>,HashMap<(usize,TransitionIndex), usize>) {
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
        ) -> Result<(f64,Vec<f64>)> {
        todo!()
    }
    
    fn underestimate_cost_to_final_synchronous_state(&self, _trace: &Vec<Activity>, _trace_index: &usize, _state: &Self::AState, _cache: &Vec<Vec<usize>>) -> usize {
        todo!()
    }

    
    fn overestimate_probability_gain(&self, 
    _marking_vec: &Vec<f64>,
    _cost_function: &Vec<f64>,
    _incidence_matrix: &Vec<Vec<f64>>,
    _transition_preset: &Vec<Vec<usize>>) -> Result<(f64,Vec<f64>)> {
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
    
    fn get_move_cost_function(&self, _trace:&Vec<Activity> ) -> (Vec<f64>,HashMap<(usize,TransitionIndex), usize>) {
        todo!()
    }
    
    fn get_probability_function(&self, _trace: &Vec<Activity>) -> (Vec<f64>,HashMap<(usize,TransitionIndex), usize>) {
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
        ) -> Result<(f64,Vec<f64>)> {
        todo!()
    }
    
    fn underestimate_cost_to_final_synchronous_state(&self, _trace: &Vec<Activity>, _trace_index: &usize, _state: &Self::AState, _cache: &Vec<Vec<usize>>) -> usize {
        todo!()
    }
    
    fn overestimate_probability_gain(&self, 
        _marking_vec: &Vec<f64>,
        _cost_function: &Vec<f64>,
        _incidence_matrix: &Vec<Vec<f64>>,
        _transition_preset: &Vec<Vec<usize>>) -> Result<(f64,Vec<f64>)> {
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

    // the function is suitable for safe (1-bounded) SLPN
    // fn underestimate_move_cost(
    //     &self, 
    //     current_marking: &Vec<f64>,
    //     move_cost_function: &Vec<f64>,
    //     incidence_matrix: &Vec<Vec<f64>>,
    //     transition_preset: &Vec<Vec<usize>>
    //     ) -> Result<f64> {
        
    //     println!("solution vec:{:?}", move_cost_function);
        
    //      // create the problem variables (transition firing vector)
    //     let mut vars = variables!();
    //     let num_transitions = incidence_matrix[1].len();
    //     let num_places = incidence_matrix.len();

    //     variables!{problem: 0 <= x;}
    //     let solution_vector: Vec<Variable> = problem.add_vector(variable().min(0).integer(), num_transitions);

    //     // create the objective function, use the dot product of cost function and solution vector
    //     let objective: Expression = solution_vector.iter().zip(move_cost_function.iter()).map(|(&x, &y)| x * y).sum();

    //     // define the linear programming model
    //     let mut model = problem.minimise(objective).using(default_solver);
    //     model.set_parameter("log", "0");

    //     let mut dead_lock_marking = vec![];
    //     // add constraints based on the marking equation
    //     for p in 0..num_places {
    //         let mut expr = Expression::from(current_marking[p]);
    //         for t in 0..num_transitions {
    //             expr += incidence_matrix[p][t] * solution_vector[t];
    //         }
    //         dead_lock_marking.push(expr);
    //     }

    //     // iterate each transition to ensure no one is enabled
    //     for transition_idx in 0..num_transitions{
    //         let each_preset = transition_preset[transition_idx].clone();
    //         let mut place_num = Expression::from(0);

    //         for (place_idx, place_expr) in dead_lock_marking.iter().enumerate(){
    //             if (each_preset.contains(&place_idx)){
    //                 place_num += place_expr;
    //             }
    //         }
    //         model.add_constraint(place_num.leq((each_preset.len() as f64)-1.0));
    //     }

    //     // solve the problem
    //     let solution: solvers::coin_cbc::CoinCbcSolution = model.solve()?; 

    //     // get the solution vector
    //     let mut solution_vec = Vec::new();
    //     for i in 0..num_transitions {
    //         solution_vec.push(solution.value(solution_vector[i]));
    //     }
    //     println!("solution is:{} and solution vector:{:?}\n",solution.model().obj_value().round(), solution_vec);
    //     Ok(solution.model().obj_value().round())
    // }

    fn underestimate_move_cost(
        &self, 
        current_marking: &Vec<f64>,
        move_cost_function: &Vec<f64>,
        incidence_matrix: &Vec<Vec<f64>>,
        transition_preset: &Vec<Vec<usize>>
        ) -> Result<(f64,Vec<f64>)> {
         // create the problem variables (transition firing vector)

        // println!("start estimate move cost");
        let num_transitions = incidence_matrix[0].len();
        let num_places = incidence_matrix.len();
        variables!{problem:}
        let solution_vec: Vec<Variable> = problem.add_vector(variable().min(0).integer(), num_transitions);

        // decision variables are used to check whether the deadlock marking enables any transitions
        let decision_var_vec: Vec<Variable> = problem.add_vector(variable().binary(), num_places * num_transitions);

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
            // each place should be non-negative
            model.add_constraint(expr.clone().geq(0.0));
            deadlock_marking_vec.push(expr);
        }

        // println!("deadlock marking is:{:?}",deadlock_marking_vec);
        // for each transition, at lead one of the places in its preset does not have enough tokens
        for transition_idx in 0..num_transitions{
            // for each transition, it should not be enabled after reaching the deadlock marking
            let each_transition_preset: &Vec<usize> = &transition_preset[transition_idx];
            // accumulate the decision variables for places
            let mut num_place_without_enough_token: Expression = Expression::from(0);
            
            for (place_idx, each_place) in deadlock_marking_vec.iter().enumerate(){
                if each_transition_preset.contains(&place_idx){
                    // use big-M method to formulate a conditional constraint
                    let mut first_right_side_expr: Expression = decision_var_vec[transition_idx*num_places + place_idx] * 1000;
                    first_right_side_expr += each_place;
                    model.add_constraint(Expression::from(1).leq(first_right_side_expr));
                    let mut second_right_side_expr: Expression = -1000 * (1 - decision_var_vec[transition_idx*num_places+place_idx]);
                    second_right_side_expr += each_place;
                    second_right_side_expr += 0.01; 
                    model.add_constraint(Expression::from(1).geq(second_right_side_expr));
                    num_place_without_enough_token += decision_var_vec[transition_idx*num_places+place_idx];
                }
            }
            // at least one place does not have enough tokens
            // println!("expr:{:?}",num_place_without_enough_token);
            model.add_constraint(num_place_without_enough_token.geq(0.0001));

            // model.add_constraint(expr.geq((each_transition_preset.len() as f64)-1.0));
        }

        // solve the problem
        let solution: solvers::coin_cbc::CoinCbcSolution = model.solve()?; 

        // get the solution vector
        let mut solution_vector = Vec::new();
        for i in 0..num_transitions {
            solution_vector.push(solution.value(solution_vec[i]) as f64);
        }

        // println!("move cost solution is:{} and solution vector:{:?}\n",solution.model().obj_value(), solution_vector);
        Ok((solution.model().obj_value(), solution_vector))
        // let mut decision_var_vector:Vec<f64> = Vec::new();
        // for i in 0..num_places*num_transitions {
        //     decision_var_vector.push(solution.value(decision_var_vec[i]) as f64);
        // }
        // let mut deadlock_marking_vector = vec![];
        // for p in 0..num_places {
        //     let mut cp = current_marking[p];
        //     for t in 0..num_transitions {
        //         cp += incidence_matrix[p][t] * solution_vector[t];
        //     }
        //     deadlock_marking_vector.push(cp);
        // }

        // // println!("decision variable is:{:?}",decision_var_vector);

        
        // for transition_idx in 0..num_transitions{
        //     // for each transition, it should not be enabled after reaching the deadlock marking
        //     let each_transition_preset: &Vec<usize> = &transition_preset[transition_idx];
        //     // println!("transition_idx:{}, an its transition preset is:{:?}", transition_idx, each_transition_preset);

        //     // accumulate the decision variables for places
        //     let mut num_place_without_enough_token= 0.0;
            
        //     for (place_idx, each_place) in deadlock_marking_vector.iter().enumerate(){
        //         if each_transition_preset.contains(&place_idx){
        //             // use big-M method to formulate a conditional constraint
        //             num_place_without_enough_token += decision_var_vector[place_idx + transition_idx*num_places];
        //         }
        //     }

        // }
    }
    
    fn overestimate_probability_gain(&self, 
        current_marking: &Vec<f64>,
        probability_gain_function: &Vec<f64>,
        incidence_matrix: &Vec<Vec<f64>>,
        transition_preset: &Vec<Vec<usize>>
        ) -> Result<(f64, Vec<f64>)> {

            // println!("start estimate probability gain");

            // create the problem variables (transition firing vector)
            let num_transitions = incidence_matrix[0].len();
            let num_places = incidence_matrix.len();

            variables!{problem:}
            let solution_vec: Vec<Variable> = problem.add_vector(variable().min(0).integer(), num_transitions);

            // decision variables are used to check whether the deadlock marking enables any transitions
            let decision_var_vec: Vec<Variable> = problem.add_vector(variable().binary(), num_places * num_transitions);

            // create the objective function, use the dot product of cost function and solution vector
            let objective: Expression = solution_vec.iter().zip(probability_gain_function.iter()).map(|(&x, &y)| x * y).sum();

            // define the linear programming model
            let mut model = problem.maximise(objective).using(default_solver);

            // do not let solver printout any information in terminal
            model.set_parameter("log", "0");

            // add constraints based on the marking equation
            let mut deadlock_marking_vec = vec![];

            for p in 0..num_places {
                let mut expr = Expression::from(current_marking[p]);
                for t in 0..num_transitions {
                    expr += incidence_matrix[p][t] * solution_vec[t];
                }
                // each place should be non-negative
                model.add_constraint(expr.clone().geq(0.0));
                deadlock_marking_vec.push(expr);
            }

            // println!("deadlock marking is:{:?}",deadlock_marking_vec);
            // for each transition, at lead one of the places in its preset does not have enough tokens
            for transition_idx in 0..num_transitions{
                // for each transition, it should not be enabled after reaching the deadlock marking
                let each_transition_preset: &Vec<usize> = &transition_preset[transition_idx];
                // accumulate the decision variables for places
                let mut num_place_without_enough_token: Expression = Expression::from(0);
                
                for (place_idx, each_place) in deadlock_marking_vec.iter().enumerate(){
                    if each_transition_preset.contains(&place_idx){
                        // use big-M method to formulate a conditional constraint
                        let mut first_right_side_expr: Expression = decision_var_vec[transition_idx*num_places + place_idx] * 100;
                        first_right_side_expr += each_place;
                        model.add_constraint(Expression::from(1).leq(first_right_side_expr));
                        let mut second_right_side_expr: Expression = -100 * (1 - decision_var_vec[transition_idx*num_places+place_idx]);
                        second_right_side_expr += each_place;
                        second_right_side_expr += 0.01; 
                        model.add_constraint(Expression::from(1).geq(second_right_side_expr));
                        num_place_without_enough_token += decision_var_vec[transition_idx*num_places+place_idx];
                    }
                }
                // at least one place does not have enough tokens
                // println!("expr:{:?}",num_place_without_enough_token);
                model.add_constraint(num_place_without_enough_token.geq(0.0001));

            }

            // solve the problem
            let solution: solvers::coin_cbc::CoinCbcSolution = model.solve()?; 

            // get the solution vector
            let mut solution_vector = Vec::new();
            for i in 0..num_transitions {
                solution_vector.push(solution.value(solution_vec[i]) as f64);
            }
            // println!("prob solution is:{} and solution vector:{:?}\n",solution.model().obj_value(), solution_vector);

            Ok((solution.model().obj_value(), solution_vector))

    }
    

    fn get_move_cost_function(&self, trace:&Vec<Activity> ) -> (Vec<f64>,HashMap<(usize,TransitionIndex), usize>){
        let mut move_cost_function = vec![];

        let mut transition_count:usize = 0;

        let mut transition2cost:HashMap<(usize,TransitionIndex), usize> = HashMap::new();
        // add cost for model move
        for model_transition in 0..self.get_number_of_transitions() {
            // if it is silent model move
            if self.is_transition_silent(model_transition){
                move_cost_function.push(0.0);
                transition2cost.insert((0,model_transition+1), transition_count);
                transition_count += 1;
            } else {
                move_cost_function.push(1.0);
                transition2cost.insert((0,model_transition+1), transition_count);
                transition_count += 1;
            }
        }

        // add cost for log move
        for trace_transition_idx in 0..trace.len() {
            move_cost_function.push(1.0);
            transition2cost.insert((trace_transition_idx+1,0), transition_count);
            transition_count += 1
        }

        // add cost for synchronous move
        for trace_transition_idx in 0..trace.len() {
            for model_transition in 0..self.get_number_of_transitions() {
                if self.get_transition_activity(model_transition) == Some(trace[trace_transition_idx]) {
                    move_cost_function.push(0.0);
                    transition2cost.insert((trace_transition_idx+1,model_transition+1), transition_count);
                    transition_count += 1;
                }
            }
        }
        (move_cost_function,transition2cost)
    }
    
    fn get_probability_function(&self, trace: &Vec<Activity>) -> (Vec<f64>,HashMap<(usize,TransitionIndex), usize>) {
        let mut probability_function = vec![];

        let mut transition_count:usize = 0;

        let mut transition2probability:HashMap<(usize,TransitionIndex), usize> = HashMap::new();
        
        // for each transition, get the maximum firing probability
        // when a transition is enabled, get other enabled transitions
        for i in 0..self.get_number_of_transitions(){     
            let mut weight_sum = self.get_transition_weight(i).clone();

            // check whether other transition has the same preset
            for j in 0..self.get_number_of_transitions(){
                if i != j {  
                // Check if vecs[i] is a subset of vecs[j]
                let is_subset = self.transition2input_places[i].iter().all(|item| self.transition2input_places[j].contains(item));
                
                if is_subset {
                        weight_sum += self.get_transition_weight(j).clone();
                    }
                }
            }
            let mut firing_probability = self.get_transition_weight(i).clone();

            firing_probability/=weight_sum;

            transition2probability.insert((0,i+1), transition_count);
            transition_count += 1;
            probability_function.push(firing_probability.fraction_to_f64().unwrap().ln().max(f64::MIN));
        }

        // add probability gain for log move
        for trace_transition_idx in 0..trace.len() {
            probability_function.push(0.0);
            transition2probability.insert((trace_transition_idx+1,0), transition_count);
            transition_count += 1;
        }

        for trace_transition_idx in 0..trace.len() {
            for model_transition in 0..self.get_number_of_transitions() {
                if self.get_transition_activity(model_transition) == Some(trace[trace_transition_idx]) {
                    // assign the probability to sync move 
                    probability_function.push(probability_function[model_transition]);
                    transition2probability.insert((trace_transition_idx+1,model_transition+1), transition_count);
                    transition_count += 1;
                }
            }
        }
        (probability_function,transition2probability)
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

        let mut sync_idx = 0;
        // add place2transition to incidence matrix from sync move
        for trace_transition_idx in 0..trace.len() {
            for model_transition in 0..self.get_number_of_transitions() {
                if self.get_transition_activity(model_transition) == Some(trace[trace_transition_idx]) {
                    // get output places of model 
                    for place in self.transition2output_places[model_transition].clone(){
                        incidence_matrix[place][self.get_number_of_transitions() + trace.len()+ sync_idx] = 1.0;
                    }

                    // get input places of model 
                    for place in self.transition2input_places[model_transition].clone(){
                        incidence_matrix[place][self.get_number_of_transitions() + trace.len()+ sync_idx] = -1.0;
                        transition_preset[self.get_number_of_transitions() + trace.len()+ sync_idx].push(place);
                    }

                    // get output place of trace net
                    incidence_matrix[self.get_number_of_places()+trace_transition_idx+1][self.get_number_of_transitions() + trace.len()+ sync_idx] = 1.0;

                    incidence_matrix[self.get_number_of_places()+trace_transition_idx][self.get_number_of_transitions() + trace.len()+ sync_idx] = -1.0;
                    transition_preset[self.get_number_of_transitions() + trace.len()+ sync_idx].push(self.get_number_of_places()+sync_idx);
                    sync_idx+=1;

                }
            }
        }
        (incidence_matrix, transition_preset)
    }
    
    fn underestimate_cost_to_final_synchronous_state(&self, _trace: &Vec<Activity>, _trace_index: &usize, _state: &Self::AState, _cache: &Vec<Vec<usize>>) -> usize {
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
    
    fn get_incidence_matrix(&self, _trace: &Vec<Activity>) -> (Vec<Vec<f64>>,Vec<Vec<usize>>) {
        todo!()
    }
    
    fn get_move_cost_function(&self, _trace:&Vec<Activity> ) -> (Vec<f64>,HashMap<(usize,TransitionIndex), usize>) {
        todo!()
    }
    
    fn get_probability_function(&self, _trace: &Vec<Activity>) -> (Vec<f64>,HashMap<(usize,TransitionIndex), usize>) {
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
        ) -> Result<(f64,Vec<f64>)> {
        todo!()
    }
    
    fn underestimate_cost_to_final_synchronous_state(&self, _trace: &Vec<Activity>, _trace_index: &usize, _state: &Self::AState, _cache: &Vec<Vec<usize>>) -> usize {
        todo!()
    }

    fn overestimate_probability_gain(&self, 
    _marking_vec: &Vec<f64>,
    _cost_function: &Vec<f64>,
    _incidence_matrix: &Vec<Vec<f64>>,
    _transition_preset: &Vec<Vec<usize>>) -> Result<(f64,Vec<f64>)> {
        todo!()
    }
    
    fn get_shortest_path(&self) -> usize {
        todo!()
    }
}