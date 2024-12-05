// This file contains code derived from pathfinding,
// which is dual-licensed under Apache 2.0 and MIT licenses.
// The original code has been modified to support the Fraction type in this project.
// For more information, see https://github.com/evenfurther/pathfinding?tab=readme-ov-file#license

use fraction::Zero;
use indexmap::map::Entry::{Occupied, Vacant};
use core::num;
use std::cmp::Ordering;
use std::collections::BinaryHeap;
use std::fmt::Debug;
use std::fmt::Display;
use std::hash::Hash;
use std::hash::BuildHasherDefault;
use std::ops::AddAssign;
use indexmap::IndexMap;
use rustc_hash::FxHasher;

use crate::ebi_traits::ebi_trait_stochastic_semantics::TransitionIndex;

type FxIndexMap<K, V> = IndexMap<K, V, BuildHasherDefault<FxHasher>>;


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

/// Compute a shortest path using the [A* search
/// algorithm](https://en.wikipedia.org/wiki/A*_search_algorithm).
///
/// The shortest path starting from `start` up to a node for which `success` returns `true` is
/// computed and returned along with its total cost, in a `Some`. If no path can be found, `None`
/// is returned instead.
///
/// - `start` is the starting node.
/// - `successors` returns a list of successors for a given node, along with the cost for moving
///   from the node to the successor. This cost must be non-negative.
/// - `heuristic` returns an approximation of the cost from a given node to the goal. The
///   approximation must not be greater than the real cost, or a wrong shortest path may be returned.
/// - `success` checks whether the goal has been reached. It is not a node as some problems require
///   a dynamic solution instead of a fixed node.
///
/// A node will never be included twice in the path as determined by the `Eq` relationship.
///
/// The returned path comprises both the start and end node.
///
/// # Example
///
/// We will search the shortest path on a chess board to go from (1, 1) to (4, 6) doing only knight
/// moves.
///
/// The first version uses an explicit type `Pos` on which the required traits are derived.
///
/// ```
/// use pathfinding::prelude::astar;
///
/// #[derive(Clone, Debug, Eq, Hash, Ord, PartialEq, PartialOrd)]
/// struct Pos(i32, i32);
///
/// impl Pos {
///   fn distance(&self, other: &Pos) -> u32 {
///     (self.0.abs_diff(other.0) + self.1.abs_diff(other.1)) as u32
///   }
///
///   fn successors(&self) -> Vec<(Pos, u32)> {
///     let &Pos(x, y) = self;
///     vec![Pos(x+1,y+2), Pos(x+1,y-2), Pos(x-1,y+2), Pos(x-1,y-2),
///          Pos(x+2,y+1), Pos(x+2,y-1), Pos(x-2,y+1), Pos(x-2,y-1)]
///          .into_iter().map(|p| (p, 1)).collect()
///   }
/// }
///
/// static GOAL: Pos = Pos(4, 6);
/// let result = astar(&Pos(1, 1), |p| p.successors(), |p| p.distance(&GOAL) / 3,
///                    |p| *p == GOAL);
/// assert_eq!(result.expect("no path found").1, 4);
/// ```
///
/// The second version does not declare a `Pos` type, makes use of more closures,
/// and is thus shorter.
///
/// ```
/// use pathfinding::prelude::astar;
///
/// static GOAL: (i32, i32) = (4, 6);
/// let result = astar(&(1, 1),
///                    |&(x, y)| vec![(x+1,y+2), (x+1,y-2), (x-1,y+2), (x-1,y-2),
///                                   (x+2,y+1), (x+2,y-1), (x-2,y+1), (x-2,y-1)]
///                               .into_iter().map(|p| (p, 1)),
///                    |&(x, y)| (GOAL.0.abs_diff(x) + GOAL.1.abs_diff(y)) / 3,
///                    |&p| p == GOAL);
/// assert_eq!(result.expect("no path found").1, 4);
/// ```
#[allow(clippy::missing_panics_doc)]
#[allow(clippy::missing_panics_doc)]
pub fn astar<'a, N, C, FN, IN, FH, FS>(
    start: &N,
    mut successors: FN,
    mut heuristic: FH,
    mut success: FS,
) -> Option<(Vec<N>, C, usize)>
where
    N: Eq + Hash + Clone + Debug,
    C: Zero + Ord + Clone + AddAssign + Debug,
    FN: FnMut(&N) -> IN,
    IN: IntoIterator<Item = (N, (usize,TransitionIndex), C)>,
    FH: FnMut(&N) -> C,
    FS: FnMut(&N) -> bool,
{
    let mut to_see = BinaryHeap::new();
    to_see.push(SmallestCostHolder {
        estimated_loss: Zero::zero(),
        cost: Zero::zero(),
        index: 0,
    });
    let mut parents: FxIndexMap<N, (usize, C)> = FxIndexMap::default();
    parents.insert(start.clone(), (usize::MAX, Zero::zero()));
    let mut count = 0;
    while let Some(SmallestCostHolder { estimated_loss, cost, index }) = to_see.pop() {

        count += 1;
        let successors = {
            let (node, &(_, ref c)) = parents.get_index(index).unwrap(); // Cannot fail
            if success(node) {
                let path = reverse_path(&parents, |&(p, _)| p, index);
                return Some((path, estimated_loss, count));
            }
            // We may have inserted a node several time into the binary heap if we found
            // a better way to access it. Ensure that we are currently dealing with the
            // best path and discard the others.    
            if &cost > c {
                continue;
            }
            successors(node)
        };

        for (successor, _, mut move_cost) in successors {
            move_cost += cost.clone();
            let new_cost = move_cost;
            let h; // heuristic(&successor)
            let n; // index for successor
            match parents.entry(successor) {
                Vacant(e) => {
                    h = heuristic(e.key());
                    n = e.index();
                    e.insert((index, new_cost.clone()));
                }
                Occupied(mut e) => {
                    if e.get().1 > new_cost {
                        h = heuristic(e.key());
                        n = e.index();
                        e.insert((index, new_cost.clone()));
                    } else {
                        continue;
                    }
                }
            }

            let mut estimated_loss = new_cost.clone();
            estimated_loss += h;
            to_see.push(SmallestCostHolder {
                estimated_loss: estimated_loss,
                cost: new_cost,
                index: n,
            });
        }
    }
    None
}


/// Compute a shortest path using the [A* search
/// algorithm](https://en.wikipedia.org/wiki/A*_search_algorithm).
///
/// The shortest path starting from `start` up to a node for which `success` returns `true` is
/// computed and returned along with its total cost, in a `Some`. If no path can be found, `None`
/// is returned instead.
///
/// - `start` is the starting node.
/// - `successors` returns a list of successors for a given node, along with the cost for moving
///   from the node to the successor. This cost must be non-negative.
/// - `heuristic` returns an approximation of the cost from a given node to the goal. The
///   approximation must not be greater than the real cost, or a wrong shortest path may be returned.
/// - `success` checks whether the goal has been reached. It is not a node as some problems require
///   a dynamic solution instead of a fixed node.
///
/// A node will never be included twice in the path as determined by the `Eq` relationship.
///
/// The returned path comprises both the start and end node.
///
/// # Example
///
/// We will search the shortest path on a chess board to go from (1, 1) to (4, 6) doing only knight
/// moves.
/// 
///
/// The first version uses an explicit type `Pos` on which the required traits are derived.
///
/// ```
/// use pathfinding::prelude::astar;
///
/// #[derive(Clone, Debug, Eq, Hash, Ord, PartialEq, PartialOrd)]
/// struct Pos(i32, i32);
///
/// impl Pos {
///   fn distance(&self, other: &Pos) -> u32 {
///     (self.0.abs_diff(other.0) + self.1.abs_diff(other.1)) as u32
///   }
///
///   fn successors(&self) -> Vec<(Pos, u32)> {
///     let &Pos(x, y) = self;
///     vec![Pos(x+1,y+2), Pos(x+1,y-2), Pos(x-1,y+2), Pos(x-1,y-2),
///          Pos(x+2,y+1), Pos(x+2,y-1), Pos(x-2,y+1), Pos(x-2,y-1)]
///          .into_iter().map(|p| (p, 1)).collect()
///   }
/// }
///
/// static GOAL: Pos = Pos(4, 6);
/// let result = astar(&Pos(1, 1), |p| p.successors(), |p| p.distance(&GOAL) / 3,
///                    |p| *p == GOAL);
/// assert_eq!(result.expect("no path found").1, 4);
/// ```
///
/// The second version does not declare a `Pos` type, makes use of more closures,
/// and is thus shorter.
///
/// ```
/// use pathfinding::prelude::astar;
///
/// static GOAL: (i32, i32) = (4, 6);
/// let result = astar(&(1, 1),
///                    |&(x, y)| vec![(x+1,y+2), (x+1,y-2), (x-1,y+2), (x-1,y-2),
///                                   (x+2,y+1), (x+2,y-1), (x-2,y+1), (x-2,y-1)]
///                               .into_iter().map(|p| (p, 1)),
///                    |&(x, y)| (GOAL.0.abs_diff(x) + GOAL.1.abs_diff(y)) / 3,
///                    |&p| p == GOAL);
/// assert_eq!(result.expect("no path found").1, 4);
/// ```
#[allow(clippy::missing_panics_doc)]
#[allow(clippy::missing_panics_doc)]
pub fn astar4slpn_without_reuse<N, C, FN, IN, FH, FS>(
    start: &N,
    mut successors: FN,
    mut heuristic: FH,
    mut success: FS,
) -> Option<(Vec<N>, C, usize)>
where
    N: Eq + Hash + Clone + Debug,
    C: Zero + Ord + Clone + AddAssign + Debug,
    FN: FnMut(&N) -> IN,
    IN: IntoIterator<Item = (N, (usize,TransitionIndex), C)>,
    FH: FnMut(&N,usize) -> (C, usize),
    FS: FnMut(&N) -> bool,
{
    let mut to_see = BinaryHeap::new();
    to_see.push(SmallestCostHolder4SLPNWithoutHeuristic {
        estimated_loss: Zero::zero(),
        cost: Zero::zero(),
        index: 0
    });
    let mut number_lp:usize = 0;
    let mut parents: FxIndexMap<N, (usize, C)> = FxIndexMap::default();
    parents.insert(start.clone(), (usize::MAX, Zero::zero()));
    let mut count = 0;
    while let Some(SmallestCostHolder4SLPNWithoutHeuristic { estimated_loss, cost, index}) = to_see.pop() {

        count += 1;
        let successors = {
            let (node, &(_, ref c)) = parents.get_index(index).unwrap(); // Cannot fail
            if success(node) {
                let path = reverse_path(&parents, |&(p, _)| p, index);
                println!("get total {} states and computes {} lp", count, number_lp);
                return Some((path, estimated_loss, count));
            }
            // We may have inserted a node several time into the binary heap if we found
            // a better way to access it. Ensure that we are currently dealing with the
            // best path and discard the others.    
            if &cost > c {
                continue;
            }
            successors(node)
        };

        for (successor, pre_transition, mut current_cost) in successors {
            current_cost += cost.clone();
            let new_cost = current_cost;
            let h; // heuristic(&successor)
            let n; // index for successor
            match parents.entry(successor) {
                Vacant(e) => {
                    h = heuristic(e.key(),number_lp);
                    n = e.index();
                    e.insert((index, new_cost.clone()));
                    number_lp += 2;
                }
                Occupied(mut e) => {
                    if e.get().1 > new_cost {
                        h = heuristic(e.key(), number_lp);
                        n = e.index();
                        e.insert((index, new_cost.clone()));
                        number_lp += 2;
                    } else {
                        continue;
                    }
                }
            }

            let mut estimated_loss = new_cost.clone();
            estimated_loss += h.0;

            to_see.push(SmallestCostHolder4SLPNWithoutHeuristic {
                estimated_loss: estimated_loss,
                cost: new_cost,
                index: n                
            });
        }
    }
    None
}



/// Compute a shortest path using the [A* search
/// algorithm](https://en.wikipedia.org/wiki/A*_search_algorithm).
///
/// The shortest path starting from `start` up to a node for which `success` returns `true` is
/// computed and returned along with its total cost, in a `Some`. If no path can be found, `None`
/// is returned instead.
///
/// - `start` is the starting node.
/// - `successors` returns a list of successors for a given node, along with the cost for moving
///   from the node to the successor. This cost must be non-negative.
/// - `heuristic` returns an approximation of the cost from a given node to the goal. The
///   approximation must not be greater than the real cost, or a wrong shortest path may be returned.
/// - `success` checks whether the goal has been reached. It is not a node as some problems require
///   a dynamic solution instead of a fixed node.
///
/// A node will never be included twice in the path as determined by the `Eq` relationship.
///
/// The returned path comprises both the start and end node.
///
/// # Example
///
/// We will search the shortest path on a chess board to go from (1, 1) to (4, 6) doing only knight
/// moves.
/// 
///
/// The first version uses an explicit type `Pos` on which the required traits are derived.
///
/// ```
/// use pathfinding::prelude::astar;
///
/// #[derive(Clone, Debug, Eq, Hash, Ord, PartialEq, PartialOrd)]
/// struct Pos(i32, i32);
///
/// impl Pos {
///   fn distance(&self, other: &Pos) -> u32 {
///     (self.0.abs_diff(other.0) + self.1.abs_diff(other.1)) as u32
///   }
///
///   fn successors(&self) -> Vec<(Pos, u32)> {
///     let &Pos(x, y) = self;
///     vec![Pos(x+1,y+2), Pos(x+1,y-2), Pos(x-1,y+2), Pos(x-1,y-2),
///          Pos(x+2,y+1), Pos(x+2,y-1), Pos(x-2,y+1), Pos(x-2,y-1)]
///          .into_iter().map(|p| (p, 1)).collect()
///   }
/// }
///
/// static GOAL: Pos = Pos(4, 6);
/// let result = astar(&Pos(1, 1), |p| p.successors(), |p| p.distance(&GOAL) / 3,
///                    |p| *p == GOAL);
/// assert_eq!(result.expect("no path found").1, 4);
/// ```
///
/// The second version does not declare a `Pos` type, makes use of more closures,
/// and is thus shorter.
///
/// ```
/// use pathfinding::prelude::astar;
///
/// static GOAL: (i32, i32) = (4, 6);
/// let result = astar(&(1, 1),
///                    |&(x, y)| vec![(x+1,y+2), (x+1,y-2), (x-1,y+2), (x-1,y-2),
///                                   (x+2,y+1), (x+2,y-1), (x-2,y+1), (x-2,y-1)]
///                               .into_iter().map(|p| (p, 1)),
///                    |&(x, y)| (GOAL.0.abs_diff(x) + GOAL.1.abs_diff(y)) / 3,
///                    |&p| p == GOAL);
/// assert_eq!(result.expect("no path found").1, 4);
/// ```
#[allow(clippy::missing_panics_doc)]
#[allow(clippy::missing_panics_doc)]
pub fn astar4slpn_with_reuse<N, C, FN, IN, FE, FC, FP, FS>(
    start: &N,
    mut successors: FN,
    mut heuristic_estimated: FE,
    mut cost_heuristic: FC,
    mut probability_heuristic: FP,
    mut success: FS,
    transition_num: usize,
) -> Option<(Vec<N>, C, usize)>
where
    N: Eq + Hash + Clone + Debug,
    C: Zero + Ord + Clone + AddAssign + Debug,
    FN: FnMut(&N) -> IN,
    IN: IntoIterator<Item = (N, (usize,TransitionIndex), C)>,
    FE: FnMut(&N, (usize, TransitionIndex), f64, f64, Vec<f64>, Vec<f64>) -> (C, f64, f64, Vec<f64>, Vec<f64>, bool, bool),
    FC: FnMut(&N) -> (f64, Vec<f64>, bool),
    FP: FnMut(&N) -> (f64, Vec<f64>, bool),
    FS: FnMut(&N) -> bool,
{
    let mut to_see = BinaryHeap::new();
    to_see.push(SmallestCostHolder4SLPN {
        state: start.clone(),
        estimated_loss: Zero::zero(),
        cost: Zero::zero(),
        index: 0,
        heuristic4cost_is_valid: false,
        heuristic4probability_is_valid: false,
        estimated_remaining_move_cost: 0.0,
        estimated_remaining_probability: 1.0,
        move_cost_vec: vec![0.0; transition_num],
        probability_vec: vec![0.0; transition_num],
    });
    let mut number_lp:usize = 0;
    let mut parents: FxIndexMap<N, (usize, C)> = FxIndexMap::default();
    parents.insert(start.clone(), (usize::MAX, Zero::zero()));
    let mut count = 0;
    while let Some(SmallestCostHolder4SLPN {state, estimated_loss, cost, index, mut heuristic4cost_is_valid, mut heuristic4probability_is_valid,
        mut estimated_remaining_move_cost, mut estimated_remaining_probability, mut move_cost_vec, mut  probability_vec }) = to_see.pop() {
        count += 1;
        let successors = {
            let (node, &(_, ref c)) = parents.get_index(index).unwrap(); // Cannot fail
            if success(node) {
                let path = reverse_path(&parents, |&(p, _)| p, index);
                println!("get total {} states and computes {} lp", count, number_lp);
                return Some((path, estimated_loss, count));
            }

            if !heuristic4cost_is_valid{
                //recompute heuristic for cost
                let cost_h = cost_heuristic(&state);
                estimated_remaining_move_cost = cost_h.0;
                move_cost_vec = cost_h.1;
                heuristic4cost_is_valid = cost_h.2;
                number_lp += 1;
                
            }
            if !heuristic4probability_is_valid{
                //recompute heuristic for probability
                let probability_h = probability_heuristic(&state);
                estimated_remaining_probability = probability_h.0;
                probability_vec = probability_h.1;
                heuristic4probability_is_valid = probability_h.2;
                number_lp += 1;
            }
            // We may have inserted a node several time into the binary heap if we found
            // a better way to access it. Ensure that we are currently dealing with the
            // best path and discard the others.    
            if &cost > c {
                continue;
            }
            successors(node)
        };

        for (successor, pre_transition, mut current_cost) in successors {
            current_cost += cost.clone();
            let new_cost = current_cost;
            let h; // heuristic(&successor)
            let n; // index for successor
            match parents.entry(successor) {
                Vacant(e) => {
                    h = heuristic_estimated(&e.key().clone(), pre_transition, estimated_remaining_move_cost, estimated_remaining_probability, move_cost_vec.clone(), probability_vec.clone());

                    let aState = e.key().clone();
                    n = e.index().clone();
                    e.insert((index, new_cost.clone()));
                    let mut estimated_loss = new_cost.clone();
                    estimated_loss += h.0;
        
                    to_see.push(SmallestCostHolder4SLPN {
                        state: aState,
                        estimated_loss: estimated_loss,
                        cost: new_cost,
                        index: n,
                        estimated_remaining_move_cost:h.1,
                        estimated_remaining_probability:h.2,
                        move_cost_vec:h.3,
                        probability_vec:h.4,
                        heuristic4cost_is_valid:h.5,
                        heuristic4probability_is_valid:h.6,
                    });
                }
                Occupied(mut e) => {
                    if e.get().1 > new_cost {
                        h = heuristic_estimated(e.key(), pre_transition, estimated_remaining_move_cost, estimated_remaining_probability, move_cost_vec.clone(), probability_vec.clone());
                        n = e.index();
                        e.insert((index, new_cost.clone()));
                        let mut estimated_loss = new_cost.clone();
                        estimated_loss += h.0;
            
                        to_see.push(SmallestCostHolder4SLPN {
                            state: e.key().clone(),
                            estimated_loss: estimated_loss,
                            cost: new_cost,
                            index: n,
                            estimated_remaining_move_cost:h.1,
                            estimated_remaining_probability:h.2,
                            move_cost_vec:h.3,
                            probability_vec:h.4,
                            heuristic4cost_is_valid:h.5,
                            heuristic4probability_is_valid:h.6,
                        });
                    } else {
                        continue;
                    }
                }
            }

        }
    }
    None
}



#[allow(clippy::missing_panics_doc)]
#[allow(clippy::missing_panics_doc)]
pub fn astar4slpn_without_heuristic<N, C, FN, IN, FH, FS>(
    start: &N,
    mut successors: FN,
    mut heuristic: FH,
    mut success: FS,
) -> Option<(Vec<N>, C, usize)>
where
    N: Eq + Hash + Clone + Debug,
    C: Zero + Ord + Clone + AddAssign + Debug,
    FN: FnMut(&N) -> IN,
    IN: IntoIterator<Item = (N, (usize,TransitionIndex), C)>,
    FH: FnMut(&N, (usize, TransitionIndex)) -> C,
    FS: FnMut(&N) -> bool,
{
    let mut to_see = BinaryHeap::new();
    to_see.push(SmallestCostHolder4SLPNWithoutHeuristic {
        estimated_loss: Zero::zero(),
        cost: Zero::zero(),
        index: 0
    });
    let mut number_lp:usize = 0;
    let mut parents: FxIndexMap<N, (usize, C)> = FxIndexMap::default();
    parents.insert(start.clone(), (usize::MAX, Zero::zero()));
    let mut count = 0;
    while let Some(SmallestCostHolder4SLPNWithoutHeuristic { estimated_loss, cost, index}) = to_see.pop() {

        count += 1;
        let successors = {
            let (node, &(_, ref c)) = parents.get_index(index).unwrap(); // Cannot fail
            if success(node) {
                let path = reverse_path(&parents, |&(p, _)| p, index);

                println!("get total {} states and computes {} lp", count, number_lp);
                println!("cost:{:?}", cost);
                return Some((path, estimated_loss, count));
            }
            // We may have inserted a node several time into the binary heap if we found
            // a better way to access it. Ensure that we are currently dealing with the
            // best path and discard the others.    
            if &cost > c {
                continue;
            }
            successors(node)
        };

        for (successor, pre_transition, mut current_cost) in successors {
            current_cost += cost.clone();
            let new_cost = current_cost;
            let h; // heuristic(&successor)
            let n; // index for successor
            match parents.entry(successor) {
                Vacant(e) => {
                    h = heuristic(e.key(), pre_transition);
                    n = e.index();
                    e.insert((index, new_cost.clone()));
                }
                Occupied(mut e) => {
                    if e.get().1 > new_cost {
                        h = heuristic(e.key(), pre_transition);
                        n = e.index();
                        e.insert((index, new_cost.clone()));
                    } else {
                        continue;
                    }
                }
            }

            let mut estimated_loss = new_cost.clone();
            estimated_loss += h;

            to_see.push(SmallestCostHolder4SLPNWithoutHeuristic {
                estimated_loss: estimated_loss,
                cost: new_cost,
                index: n,
            });
        }
    }
    None
}


/// This structure is used to implement Rust's max-heap as a min-heap
/// version for A*. The smallest `estimated_loss` (which is the sum of
/// the `cost` and the heuristic) is preferred. For the same
/// `estimated_loss`, the highest `cost` will be favored, as it may
/// indicate that the goal is nearer, thereby requiring fewer
/// exploration steps.
struct SmallestCostHolder<K> {
    estimated_loss: K,
    cost: K,
    index: usize,
}

impl<K: PartialEq> PartialEq for SmallestCostHolder<K> {
    fn eq(&self, other: &Self) -> bool {
        self.estimated_loss.eq(&other.estimated_loss)
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
        match other.estimated_loss.cmp(&self.estimated_loss) {
            Ordering::Equal => other.cost.cmp(&self.cost),
            s => s,
        }
    }
}

struct SmallestCostHolder4SLPN<K, FS> {
    state: FS,
    estimated_loss: K,
    cost: K,
    index: usize,
    heuristic4cost_is_valid: bool,
    heuristic4probability_is_valid: bool,
    estimated_remaining_move_cost:f64,
    estimated_remaining_probability: f64,
    move_cost_vec: Vec<f64>,
    probability_vec: Vec<f64>,
}

impl<K: PartialEq, FS: Eq + Hash + Clone + Debug> PartialEq for SmallestCostHolder4SLPN<K, FS> {
    fn eq(&self, other: &Self) -> bool {
        self.estimated_loss.eq(&other.estimated_loss)
    }
}

impl<K: PartialEq, FS: Eq + Hash + Clone + Debug> Eq for SmallestCostHolder4SLPN<K, FS> {}

impl<K: Ord, FS: Eq + Hash + Clone + Debug> PartialOrd for SmallestCostHolder4SLPN<K, FS> {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

impl<K: Ord, FS: Eq + Hash + Clone + Debug> Ord for SmallestCostHolder4SLPN<K, FS> {
    fn cmp(&self, other: &Self) -> Ordering {
        match other.estimated_loss.cmp(&self.estimated_loss) {
            Ordering::Equal => other.cost.cmp(&self.cost),
            s => s,
        }
    }
}



struct SmallestCostHolder4SLPNWithoutHeuristic<K> {
    estimated_loss: K,
    cost: K,
    index: usize,
}

impl<K: PartialEq> PartialEq for SmallestCostHolder4SLPNWithoutHeuristic<K> {
    fn eq(&self, other: &Self) -> bool {
        self.estimated_loss.eq(&other.estimated_loss)
    }
}

impl<K: PartialEq> Eq for SmallestCostHolder4SLPNWithoutHeuristic<K> {}

impl<K: Ord> PartialOrd for SmallestCostHolder4SLPNWithoutHeuristic<K> {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

impl<K: Ord> Ord for SmallestCostHolder4SLPNWithoutHeuristic<K> {
    fn cmp(&self, other: &Self) -> Ordering {
        match other.estimated_loss.cmp(&self.estimated_loss) {
            Ordering::Equal => other.cost.cmp(&self.cost),
            s => s,
        }
    }
}