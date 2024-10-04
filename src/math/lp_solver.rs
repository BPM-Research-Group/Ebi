use good_lp::*;
use std::collections::{HashMap, HashSet};

/*
Given the incidence matrix of the synchronous product net, the current marking, the cost function and the preset of transitions,
return a tuple that represents the solution vector and an estimated cost till one deadlock marking.
*/
pub fn underestimate_move_cost_to_deadlock_marking(
    incidence_matrix: &[Vec<i32>],
    current_marking: &[i32],
    move_cost_function: &[usize],
    transition_preset:&HashSet<usize>
) -> Result<(Vec<i32>,i32,bool), Box<dyn std::error::Error>> {

    // create the problem variables (transition firing vector)
    let mut vars = variables!();
    let num_transitions = incidence_matrix.len();
    let num_places = incidence_matrix[1].len();

    variables!{problem: 0 <= x;}
    let solution_vec: Vec<Variable> = problem.add_vector(variable().min(0).integer(), num_transitions);

    // create the objective function, use the dot product of cost function and solution vector
    let objective: Expression = solution_vec.iter().zip(move_cost_function.iter()).map(|(&x, &y)| x * y).sum();

    // define the linear programming model
    let mut model = problem.minimise(objective).using(default_solver);

    // add constraints based on the marking equation
    for p in 0..num_places {
        let mut expr = Expression::from(current_marking[p]);
        for t in 0..num_transitions {
            expr += incidence_matrix[p][t] * solution_vec[t];
        }

        // guarantee that this transition is not enabled afterwards
        if transition_preset.contains(&p) {
            model.add_constraint(expr.eq(0));
        }
    }

    // solve the problem
    let solution: solvers::coin_cbc::CoinCbcSolution = model.solve()?; 

    // get the solution vector
    let mut solution_vector = Vec::new();
    for i in 0..num_transitions {
        solution_vector.push(solution.value(solution_vec[i]) as i32);
    }
    let result = (solution_vector,solution.model().obj_value().round() as i32, solution.model().is_proven_optimal());

    Ok(result)
}

/*
Given the incidence matrix of the synchronous product net, the current marking, the probability gain function and the preset of transitions,
return a tuple that represents the solution vector and an overestimated probability till one deadlock marking.
*/
pub fn overestimate_likelihood_gain_to_deadlock_marking(
    incidence_matrix: &[Vec<i32>],
    current_marking: &[i32],
    probability_gain_function: &[f32],
    transition_preset:&HashSet<usize>
) -> Result<(Vec<i32>,f64,bool), Box<dyn std::error::Error>> {

    // create the problem variables (transition firing vector)
    let mut vars = variables!();
    let num_transitions = incidence_matrix.len();
    let num_places = incidence_matrix[1].len();

    variables!{problem: 0 <= x;}
    let solution_vec: Vec<Variable> = problem.add_vector(variable().min(0).integer(), num_transitions);

    // create the objective function, use the dot product of likelihood gain function and solution vector
    let objective: Expression = solution_vec.iter().zip(probability_gain_function.iter()).map(|(&x, &y)| x * y).sum();

    // define the linear programming model
    let mut model = problem.maximise(objective).using(default_solver);

    // add constraints based on the marking equation
    for p in 0..num_places {
        let mut expr = Expression::from(current_marking[p]);
        for t in 0..num_transitions {
            expr += incidence_matrix[p][t] * solution_vec[t];
        }

        // guarantee that this transition is not enabled afterwards
        if transition_preset.contains(&p) {
            model.add_constraint(expr.eq(0));
        }
    }

    // solve the problem
    let solution: solvers::coin_cbc::CoinCbcSolution = model.solve()?; 

    // get the solution vector
    let mut solution_vector = Vec::new();
    for i in 0..num_transitions {
        solution_vector.push(solution.value(solution_vec[i]) as i32);
    }
    let result = (solution_vector,solution.model().obj_value().round() as f64, solution.model().is_proven_optimal());

    Ok(result)
}