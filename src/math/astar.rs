// This file contains code derived from pathfinding,
// which is dual-licensed under Apache 2.0 and MIT licenses.
// The original code has been modified to support the Fraction type in this project.
// For more information, see https://github.com/evenfurther/pathfinding?tab=readme-ov-file#license

use fraction::Zero;
use indexmap::map::Entry::{Occupied, Vacant};
use std::cmp::Ordering;
use std::collections::BinaryHeap;
use std::hash::Hash;
use std::hash::BuildHasherDefault;
use std::ops::AddAssign;
use indexmap::IndexMap;
use rustc_hash::FxHasher;
use std::fmt::Debug;

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
) -> Option<(Vec<N>, C)>
where
    N: Eq + Hash + Clone,
    C: Zero + Ord + Clone + AddAssign,
    FN: FnMut(&N) -> IN,
    IN: IntoIterator<Item = (N, C)>,
    FH: FnMut(&N) -> C,
    FS: FnMut(&N) -> bool,
{
    let mut to_see = BinaryHeap::new();
    to_see.push(SmallestCostHolder {
        estimated_cost: Zero::zero(),
        cost: Zero::zero(),
        index: 0,
    });
    let mut parents: FxIndexMap<N, (usize, C)> = FxIndexMap::default();
    parents.insert(start.clone(), (usize::MAX, Zero::zero()));
    while let Some(SmallestCostHolder { cost, index, .. }) = to_see.pop() {
        let successors = {
            let (node, &(_, ref c)) = parents.get_index(index).unwrap(); // Cannot fail
            if success(node) {
                let path = reverse_path(&parents, |&(p, _)| p, index);
                return Some((path, cost));
            }
            // We may have inserted a node several time into the binary heap if we found
            // a better way to access it. Ensure that we are currently dealing with the
            // best path and discard the others.
            if &cost > c {
                continue;
            }
            successors(node)
        };
        for (successor, mut move_cost) in successors {
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

            let mut estimated_cost = new_cost.clone();
            estimated_cost += h;
            to_see.push(SmallestCostHolder {
                estimated_cost: estimated_cost,
                cost: new_cost,
                index: n,
            });
        }
    }
    None
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

#[allow(clippy::missing_panics_doc)]
#[allow(clippy::missing_panics_doc)]
pub fn astar_with_reuse<N, FN, IN, FH, FS, FR>(
    start: &N,
    mut successors: FN,
    mut heuristic_reuse: FR,
    mut heuristic: FH,
    mut success: FS,
) -> Option<(Vec<N>, f64)>
where
    N: Eq + Hash + Clone + Debug,
    FN: FnMut(&N) -> IN,
    IN: IntoIterator<Item = (N, (usize,TransitionIndex), f64)>,
    FR: FnMut((usize, TransitionIndex), f64, Vec<f64>) -> (f64, Vec<f64>, bool),
    FH: FnMut(&N) -> (f64, Vec<f64>),
    FS: FnMut(&N) -> bool,
{
    let mut to_see = BinaryHeap::new();
    to_see.push(SmallestCostHolder4LPN {
        state: start.clone(),
        index: 0,
        cost_f: 0.0,
        cost_g: 0.0,
        cost_h: 0.0,
        cost_vec: Vec::new(),
        h_is_valid: false,
    });
    let mut parents: FxIndexMap<N, (usize, f64)> = FxIndexMap::default();
    parents.insert(start.clone(), (usize::MAX, 0.0));

    while let Some(SmallestCostHolder4LPN {state, index, cost_f, cost_g, mut cost_h, mut cost_vec, h_is_valid}) = to_see.pop() {
        let successors = {
            let (node, &(_, c)) = parents.get_index(index).unwrap(); // Cannot fail
            if success(node) {
                let path = reverse_path(&parents, |&(p, _)| p, index);
                return Some((path, cost_f));
            }
            // We may have inserted a node several time into the binary heap if we found
            // a better way to access it. Ensure that we are currently dealing with the
            // best path and discard the others.
            if cost_g > c {
                continue;
            }

            if !h_is_valid{
                //recompute heuristic for cost
                let cost_heuristic = heuristic(&state);
                cost_h = cost_heuristic.0;
                cost_vec = cost_heuristic.1;    
            }
            successors(node)
        };

        for (successor, pre_transition, mut move_cost) in successors {
            move_cost += cost_g;
            let h: (f64, Vec<f64>, bool); // heuristic(&successor)
            let n; // index for successor
            match parents.entry(successor.clone()) {
                Vacant(e) => {
                    h = heuristic_reuse(
                        pre_transition, 
                        cost_h, 
                        cost_vec.clone());
                    n = e.index().clone();
                    e.insert((index, move_cost));
                }
                Occupied(mut e) => {
                    if e.get().1 > move_cost {
                        h = heuristic_reuse(
                            pre_transition, 
                            cost_h, 
                            cost_vec.clone());
                        n = e.index().clone();
                        e.insert((index, move_cost));
                    } else {
                        continue;
                    }
                }
            }
            to_see.push(SmallestCostHolder4LPN {
                state: successor,
                index: n,
                cost_f: move_cost + h.0,
                cost_g: move_cost,
                cost_h: h.0,
                cost_vec: h.1,
                h_is_valid: h.2,
            });
        }
    }
    None
}

struct SmallestCostHolder4LPN<FS> {
    state: FS,
    index: usize,
    cost_f: f64,
    cost_g: f64,
    cost_h: f64,
    cost_vec: Vec<f64>,
    h_is_valid: bool,
}

// Implement PartialOrd for floating point comparison
impl<FS: Eq + Hash + Clone + Debug> PartialOrd for SmallestCostHolder4LPN<FS> {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        // First compare cost_f (smaller cost_f means bigger)
        match self.cost_f.partial_cmp(&other.cost_f) {
            Some(Ordering::Equal) => {
                // If cost_f is equal, compare cost_g (larger cost_g means bigger)
                other.cost_g.partial_cmp(&self.cost_g)
            }
            Some(ordering) => {
                // Reverse the ordering for cost_f since smaller means bigger
                Some(ordering.reverse())
            }
            None => None
        }
    }
}

impl<FS: Eq + Hash + Clone + Debug> PartialEq for SmallestCostHolder4LPN<FS> {
    fn eq(&self, other: &Self) -> bool {
        self.cost_f.eq(&other.cost_f) && self.cost_g.eq(&other.cost_g) && self.h_is_valid.eq(&other.h_is_valid)
    }
}

impl<FS: Eq + Hash + Clone + Debug> Eq for SmallestCostHolder4LPN<FS> {}

impl<FS: Eq + Hash + Clone + Debug> Ord for SmallestCostHolder4LPN<FS> {
    fn cmp(&self, other: &Self) -> Ordering {
        // Convert partial_cmp to cmp by handling NaN cases
        self.partial_cmp(other)
            .unwrap_or(Ordering::Less)  // You might want different NaN handling
    }
}