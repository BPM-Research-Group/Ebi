// use std::cmp;
// use std::fmt::Display;
// use std::hash::Hash;
// use std::ops::{Add, AddAssign};
// use std::rc::Rc;
// use std::{cmp::Ordering, io, vec}; // prelude
// use std::collections::HashMap;
// use std::fs::File;
// use std::thread;
// use anyhow::Result;
// use core::time::Duration;
// use std::collections::BinaryHeap;

// use fraction::Zero;

// use crate::ebi_traits::ebi_semantics::Semantics;
// use crate::ebi_traits::ebi_trait_stochastic_semantics::{StochasticSemantics, TransitionIndex};

// #[derive(Clone)]
// pub struct AstarSearchState<C, M> {
//     pub log_state: usize,
//     pub model_state: M,
//     pub cost: C,
//     pub previous_log_move: bool,
//     pub previous_model_move: Option<TransitionIndex>,
//     pub previous_state: Option<Rc<Self>>,
// }

// impl <C, M> AstarSearchState<C, M> 
// where C: Zero + PartialOrd + Add + Ord + Clone, M: Display + Clone + Hash + Eq + Copy {
//     pub fn new(log_state: usize, model_state: M) -> Self {
//         Self {
//             log_state: log_state,
//             model_state: model_state,
//             cost: C::zero(),
//             previous_log_move: false,
//             previous_model_move: None,
//             previous_state: None,
//         }
//     }

//     pub fn move_model(self: Rc<Self>, model_move: TransitionIndex, semantics: Box<dyn Semantics<State=M>>, cost: C) -> Result<Self> {
//         Ok(Self {
//             log_state: self.log_state,
//             model_state: { 
//                 let mut new_state = self.model_state.clone(); 
//                 semantics.execute_transition(&mut new_state, model_move)?; 
//                 new_state },
//             cost: self.cost.clone() + cost,
//             previous_log_move: false,
//             previous_model_move: Some(model_move),
//             previous_state: Some(Rc::clone(&self)),
//         })
//     }

//     pub fn move_log(self: Rc<Self>, cost: C) -> Self {
//         Self {
//             log_state: self.log_state + 1,
//             model_state: self.model_state.clone(),
//             cost: self.cost.clone() + cost,
//             previous_log_move: true,
//             previous_model_move: None,
//             previous_state: Some(Rc::clone(&self)),
//         }
//     }

//     pub fn move_sync(self: Rc<Self>, model_move: TransitionIndex, semantics: Box<dyn Semantics<State=M>>, cost: C) -> Result<Self> {
//         Ok(Self {
//             log_state: self.log_state + 1,
//             model_state: { 
//                 let mut new_state = self.model_state.clone(); 
//                 semantics.execute_transition(&mut new_state, model_move)?; 
//                 new_state },
//             cost: self.cost + cost,
//             previous_log_move: true,
//             previous_model_move: Some(model_move),
//             previous_state: Some(Rc::clone(&self)),
//         })
//     }

//     pub fn is_final_state(&self, semantics: Box<dyn Semantics<State=M>>, trace_length: usize) -> bool {
//         semantics.is_final_state(&self.model_state) && self.log_state == trace_length
//     }
// }

// impl <C, M> PartialOrd for AstarSearchState<C, M> where C: PartialOrd + Ord {
//     fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
//         Some(self.cmp(other))
//     }
// }

// impl <C, M> Ord for AstarSearchState<C, M> where C: Ord {
//     fn cmp(&self, other: &Self) -> Ordering {
//         self.cost.cmp(&other.cost).then_with(|| self.log_state.cmp(&other.log_state))
//     }
// }

// impl <C, M> Eq for AstarSearchState<C, M> where C: Eq + PartialOrd {}

// impl <C, M> PartialEq for AstarSearchState<C, M> where C: PartialEq + PartialOrd {
//     fn eq(&self, other: &Self) -> bool {
//         self.cost == other.cost
//     }
// }

// // impl <C, M> Hash for AstarSearchState<C, M> {
// //     fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
// //         self.cost.hash(state);
// //     }
// // }