// use fraction::{Fraction, One, Zero};
// use indexmap::{map::Entry::{Occupied, Vacant}, IndexMap};
// use num_traits::AsPrimitive;
// use process_mining::{import_xes_file, petri_net::import_pnml::import_pnml_from_path, petri_net::petri_net_struct::{ArcType, Marking, PlaceID}, PetriNet, XESImportOptions};
// use std::{borrow::Borrow, cmp::Ordering, collections::{HashMap, HashSet}, hash::BuildHasherDefault, ops::{Add, AddAssign, Div}};
// use std::hash::Hash;
// use std::fmt::Display;
// use std::fmt::Debug;
// use std::rc::Rc;
// use anyhow::Result;
// use rustc_hash::FxHasher;
// use std::collections::BinaryHeap;
// use crate::{activity_key::{self, Activity}, ebi_alignments::cross_product_search_state::AstarSearchState, ebi_objects::alignments::Alignments, ebi_traits::{ebi_semantics::Semantics, ebi_trait::EbiTrait, ebi_trait_finite_language::EbiTraitFiniteLanguage, ebi_trait_stochastic_semantics::{EbiTraitStochasticSemantics, StochasticSemantics, TransitionIndex}}, ActivityTrace, Trace};


// impl <M> dyn StochasticSemantics<State = M> {
//     /**
//  * 
//  * @param initialState
//  * @return the maximum probability of path
//  */
//  fn get_maximum_path_probability(&self) -> anyhow::Result<Fraction>{
    
//  }
// }


// pub fn cost_fn <C, M> (
//     model_state: &M, 
//     model_move: Option<TransitionIndex>, 
//     log_move: Option<usize>,
//     upper_cost: usize,
//     upper_probability: C) -> C
//     where 
//     C: Add + Zero + PartialOrd + Ord + Clone + One + AddAssign, 
//     M: Eq + Hash + Clone + Display + Ord + Copy {
//         let mut cost;
//         if log_move.is_some() {
//             cost = C::one();
//         }
//         else{
//             cost = C::one();
//         }
//         cost
// }


// pub fn astar_apply <C, M> (
//     trace: Vec<Activity>,
//     semantics: Box<dyn Semantics<State=M>>)
//     // -> Result<Vec<(Option<usize>,Option<TransitionIndex>)>>
//     -> String
//     where 
//     C: Add + Zero + PartialOrd + Ord + Clone + One + AddAssign, 
//     M: Eq + Hash + Clone + Display + Ord + Copy {
//     let alignment_result = astar::<C,M>(trace, semantics);
    
//     let result = String::from("result");
//     result
// }

// pub fn astar<C, M>(
//     trace: Vec<Activity>,
//     semantics: Box<dyn Semantics<State=M>>
// ) -> Result<Vec<(Option<usize>, Option<TransitionIndex>)>>
// where
//     C: Zero + PartialOrd + Add + Ord + Clone + One + AddAssign,
//     M: Display + Clone + Hash + Eq + Ord + Copy
// {
//     // get the cost and probability upper bound
//     let upper_cost = 100 as usize;
//     let upper_probability: usize = 1 as usize;

//     // insert the initial state into the binary heap
//     let mut to_see = BinaryHeap::new();

//     to_see.push(Rc::new(AstarSearchState::<C,M>::new(0, semantics.get_initial_state())));

//     while let Some(current) = to_see.pop() {

//         if current.is_final_state(semantics, trace.len()){
//             let mut alignment_vec = Vec::new();

//             let trace_length = trace.len();

//             while let Some(previous) = current.previous_state {
//                 alignment_vec.push((Some(current.log_state), previous.previous_model_move));
//             }
//             return Ok(alignment_vec);
//         }

//         // get all enabled model-move
//         let enabled_model_moves: Vec<TransitionIndex> = semantics.get_enabled_transitions(&current.model_state);

//         for model_move in enabled_model_moves {
//             let cost = cost_fn(&current.model_state, Some(model_move), None, upper_cost, upper_probability);
//             let next: AstarSearchState<C, M> = current.move_model(model_move, semantics, C::one())?;
//             to_see.push(Rc::new(next));

//             // in this way we get the label of model move
//             if let Some(activity) = semantics.get_transition_activity(model_move) {

//                 // in case we find the sync move
//                 if activity == trace[current.log_state] {
//                     let cost = cost_fn(&current.model_state, Some(model_move), Some(current.log_state),upper_cost, upper_probability);
//                     let next: AstarSearchState<C, M> = current.move_sync(model_move, semantics, C::one())?;
//                     to_see.push(Rc::new(next));
//                 }
//             }
//         }

//         // get all enabled log-move
//         let cost = cost_fn(&current.model_state, None, Some(current.log_state), upper_cost, upper_probability);
//         let next: AstarSearchState<C, M> = current.move_log(C::one());
//         to_see.push(Rc::new(next));
//     }
//     Ok(Vec::new())
// }
