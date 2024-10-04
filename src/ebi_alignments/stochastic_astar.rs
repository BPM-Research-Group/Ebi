use fraction::{One, Zero};
use indexmap::{map::Entry::{Occupied, Vacant}, IndexMap};
use process_mining::{import_xes_file, petri_net::import_pnml::import_pnml_from_path, petri_net::petri_net_struct::{ArcType, Marking, PlaceID}, PetriNet, XESImportOptions};
use std::{cmp::Ordering, collections::{HashMap, HashSet}, hash::BuildHasherDefault, ops::Add};
use uuid::Uuid;
use std::hash::Hash;
use std::fmt::Display;
use std::rc::Rc;
use anyhow::Result;
use rustc_hash::FxHasher;
use std::collections::BinaryHeap;
use crate::{activity_key::Activity, ebi_alignments::cross_product_search_state::AstarSearchState, ebi_traits::{ebi_semantics::Semantics, ebi_trait::EbiTrait, ebi_trait_stochastic_semantics::{EbiTraitStochasticSemantics, StochasticSemantics, TransitionIndex}}};

type FxIndexMap<K, V> = IndexMap<K, V, BuildHasherDefault<FxHasher>>;


fn astar_apply <C, M> (
    trace: Vec<Activity>,
                semantics: Box<dyn Semantics<State=M>>,
                cost_map: fn(model_state: M, model_move: Option<TransitionIndex>, log_move: Option<usize>) -> C) 
                -> Result<Vec<(Option<usize>, Option<TransitionIndex>)>>
                where C: Add + Zero + PartialOrd + Ord, M: Eq + Hash + Clone + Display + Ord {
    // let last_state = astar(trace, semantics, cost_map)?;
    todo!()
}

pub fn astar<N, C, M, FN, IN, FH, FS>(
    trace: Vec<Activity>,
    semantics: Box<dyn Semantics<State=M>>,
    cost_map: fn(model_state: M, 
    model_move: Option<TransitionIndex>, 
    log_move: Option<usize>) -> C
) -> Result<(Vec<N>, C)>
where
    N: Eq + Hash + Clone,
    C: Zero + One + PartialOrd + Add + Ord,
    FN: FnMut(&N) -> IN,
    IN: IntoIterator<Item = (N, C)>,
    M: Display + Clone + Hash + Eq
{
    // insert the initial state into the binary heap
    let mut to_see = BinaryHeap::new();
    to_see.push(AstarSearchState::new(0, semantics.get_initial_state()));

    while let Some(current) = to_see.pop() {

        if current.is_final_state(semantics, trace.len()){
            let vec = vec![];
            let cost = current.cost;
            return Ok((vec, cost));
        }

        // get all enabled model-move
        let enabled_model_moves = semantics.get_enabled_transitions(&current.model_state);

        for log_move in enabled_model_moves {
            let cost = C::one();
            let next: AstarSearchState<C, M> = Rc::new(current).move_model(log_move, semantics, cost)?;
            to_see.push(next);
        }

        // get all enabled log-move
        let enabled_log_move = trace[current.log_state];
        let cost = C::one();
        let next: AstarSearchState<C, M> = Rc::new(current).move_log(cost);
        to_see.push(next);

        // how to handle the sync move? the transition are usize and transitionindex
        
    }
    None
}

#[allow(clippy::needless_collect)]
fn reverse_path<N, V, F>(parents: &FxIndexMap<N, V>, mut parent: F, start: usize) -> Vec<N>
where
    N: Eq + Hash + Clone,
    F: FnMut(&V) -> usize,
{
    let mut i = start;
    let path = std::iter::from_fn(|| {
        parents.get_index(i).map(|(node, value)| {
            i = parent(value);
            node
        })
    })
    .collect::<Vec<&N>>();
    // Collecting the going through the vector is needed to revert the path because the
    // unfold iterator is not double-ended due to its iterative nature.
    path.into_iter().rev().cloned().collect()
}


/// This structure is used to implement Rust's max-heap as a min-heap
/// version for A*. The smallest `estimated_cost` (which is the sum of
/// the `cost` and the heuristic) is preferred. For the same
/// `estimated_cost`, the highest `cost` will be favored, as it may
/// indicate that the goal is nearer, thereby requiring fewer
/// exploration steps.
struct SmallestCostHolder<K> {
    estimated_cost: K,
    cost: K,
    index: usize,
}

impl<K: PartialEq> PartialEq for SmallestCostHolder<K> {
    fn eq(&self, other: &Self) -> bool {
        self.estimated_cost.eq(&other.estimated_cost) && self.cost.eq(&other.cost)
    }
}

impl<K: PartialEq> Eq for SmallestCostHolder<K> {}

impl<K: Ord> PartialOrd for SmallestCostHolder<K> {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

impl<K: Ord> Ord for SmallestCostHolder<K> {
    fn cmp(&self, other: &Self) -> Ordering {
        match other.estimated_cost.cmp(&self.estimated_cost) {
            Ordering::Equal => self.cost.cmp(&other.cost),
            s => s,
        }
    }
}

fn main() {
    todo!()
}
