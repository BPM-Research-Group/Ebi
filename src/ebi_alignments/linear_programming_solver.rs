// use good_lp::*;
// use serde::{Deserialize, Serialize};
// use process_mining::petri_net::{import_pnml::import_pnml_from_path, petri_net_struct::{Marking, ArcType, PlaceID}, PetriNet};
// use uuid::Uuid;
// use std::collections::HashMap;
// use crate::ebi_alignments::{cross_product_transition::{CrossProductNetTransition, CrossProductNetTransitionCost, CrossProductNetTransitionType}, cross_product_arc::{CrossProductNetArc, CrossProductNetArcType}, cross_product_place::CrossProductNetPlace};


// fn solve_marking_equation(
//     initial_marking: &[i32],
//     final_marking: &[i32],
//     cost_function: &[i32],
//     incidence_matrix: &[Vec<i32>]
// ) -> Result<(Vec<i32>,i32,bool), Box<dyn std::error::Error>> {

//     // Create the problem variables (transition firing vector x)
//     let mut vars = variables!();
//     let num_transitions = incidence_matrix.len();
//     let num_places = incidence_matrix[1].len();

//     variables!{problem: 0 <= x;}
//     let solution_vec: Vec<Variable> = problem.add_vector(variable().min(0).integer(), num_transitions);

//     // Create the objective function, use the dot product of cost function and solution vector
//     // Define the objective function (dot product)
//     let objective: Expression = solution_vec.iter().zip(cost_function.iter()).map(|(&x, &y)| x * y).sum();

//     // define the linear programming model
//     let mut model = problem.minimise(objective).using(default_solver);

//     // add constraints based on the marking equation
//     for p in 0..num_places {
//         let mut expr = Expression::from(initial_marking[p]);
//         for t in 0..num_transitions {
//             expr += incidence_matrix[p][t] * solution_vec[t];
//         }
//         model.add_constraint(expr.eq(final_marking[p]));
//     }

//     // solve the problem
//     let solution: solvers::coin_cbc::CoinCbcSolution = model.solve()?; 

//     println!("Solution value: {:?}", solution.model().obj_value());

//     let mut solution_vector = Vec::new();

//     for i in 0..num_transitions {
//         solution_vector.push(solution.value(solution_vec[i]) as i32);
//     }
//     let result = (solution_vector,solution.model().obj_value().round() as i32, solution.model().is_proven_optimal());

//     Ok(result)
// }

// fn main(){
//     // let initial_marking = vec![1, 0, 0, 1, 0, 0];
//     // let final_marking = vec![0, 0, 1, 0, 0, 1];
//     // let incidence_matrix = vec![
//     //     vec![-1, 0, -1, 0, 0, 0],
//     //     vec![1, -1, 1, -1, 0, 0],
//     //     vec![0, 1, 0, 1, 0, 0],
//     //     vec![0, 0, 0, -1, -1, 0],
//     //     vec![0, 0, -1, 1, 1, -1],
//     //     vec![0, 0, 1, 0, 0, 1],
//     // ];
//     // let cost_function: Vec<i32> = vec![1,1,0,0,1,1];

//     // match solve_marking_equation(&initial_marking, &final_marking, &cost_function,&incidence_matrix) {
//     //     Ok(solution) => {
//     //         println!("Solution: {:?}", solution);
//     //     },
//     //     Err(e) => println!("Error: {}", e),
//     // }
    
//     let pn1 = import_pnml_from_path("./data/Rtf_5.pnml").unwrap();
//     let pn2 = import_pnml_from_path("./data/Rtf_5.pnml").unwrap();

//     println!("transitions in pn1: {:?}", pn1.transitions.len());
//     println!("places in pn1: {:?}", pn1.places.len());
//     println!("transitions in pn2: {:?}", pn2.transitions.len());
//     println!("places in pn2: {:?}", pn2.places.len());

//     let mut final_marking1 = Marking::new();
//     let mut final_marking2 = Marking::new();

//     let mut places4final = pn1.places.clone();

//     for arc in pn1.arcs.iter(){
//         let arc_type = &arc.from_to;
//         match arc_type{
//             // the arc is from a place to a transition
//             ArcType::PlaceTransition(from, to) => {
//                 places4final.remove(from);
//             },
//             // the arc is from a transition to a place, do nothing
//             ArcType::TransitionPlace(from, to)=>{
//             }
//         }
//     }
//     for p in places4final.iter(){
//         let p_uuid:PlaceID = PlaceID(*p.0);
//         final_marking1.insert(p_uuid,1);
//     }    
    
//     // first store all places from pn1
//     let mut places4final2 = pn2.places.clone();

//     for arc in pn2.arcs.iter(){
//         let arc_type = &arc.from_to;
//         match arc_type{
//             // the arc is from a place to a transition
//             ArcType::PlaceTransition(from, to) => {
//                 places4final2.remove(from);
//             },
//             // the arc is from a transition to a place, do nothing
//             ArcType::TransitionPlace(from, to)=>{
//             }
//         }
//     }
//     for p in places4final2.iter(){
//         let p_uuid:PlaceID = PlaceID(*p.0);
//         final_marking2.insert(p_uuid,1);
//     }    
// }

