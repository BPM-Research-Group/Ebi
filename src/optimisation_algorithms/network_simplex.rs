use super::network_simplex_value_type::{MulWithFloat, ToBigInt};
use crate::math::traits::{One, Signed, Zero};
use crate::optimisation_algorithms::network_simplex_value_type::IsFloat;
use core::convert::From;
use fraction::BigInt;
use rand::{seq::SliceRandom, thread_rng};
use rayon::ThreadPool;
use std::sync::atomic::{AtomicUsize, Ordering};
use std::sync::Arc;
use std::{
    cmp::{PartialEq, PartialOrd},
    fmt::{Debug, Display},
    iter::Sum,
    ops::{AddAssign, MulAssign, Neg, SubAssign},
};

// Enums for representing various problem types, supply types, and arc states

/// Enum for representing the type of problem solved by the Network Simplex algorithm
/// - `Optimal`: The problem is feasible and bounded, and an optimal solution has been found
/// - `Infeasible`: The problem is infeasible, i.e., no feasible solution exists
/// - `Unbounded`: The problem is unbounded, i.e., the objective function can be made arbitrarily small
#[derive(Debug, PartialEq)]
pub enum ProblemType {
    Optimal,
    Infeasible,
    Unbounded,
}

/// Enum for representing the type of supply constraints in the network
/// - `GEQ`: The supply constraints are treated as "greater than or equal to" constraints
/// - `LEQ`: The supply constraints are treated as strict equality constraints
/// If the sum of the supplies is zero, both types are equivalent.
#[derive(Debug, PartialEq)]
pub enum SupplyType {
    GEQ,
    LEQ,
}

/// Enum for representing the state of an arc in the spanning tree representation
/// - `Upper`: The flow on the arc is currently equal to its capacity
/// - `Tree`: The arc is currently part of the spanning tree
/// - `Lower`: The arc is currently not part of the spanning tree
///
/// A i32 representation is used to enable conversion to generic type for calculations.
///
/// # Note
/// LEMON also uses an 'Upper' state, which handles arcs that max out their capacity.
/// This implementation does not use capacities, so the 'Upper' state is not needed.
#[derive(Debug, PartialEq, Clone)]
pub enum ArcState<T> {
    Upper(T),
    Tree(T),
    Lower(T),
}

impl<T> ArcState<T>
where
    T: From<i32>, // Need this to convert from our constants
{
    pub fn upper() -> Self {
        ArcState::Upper(T::from(-1))
    }

    pub fn tree() -> Self {
        ArcState::Tree(T::from(0))
    }

    pub fn lower() -> Self {
        ArcState::Lower(T::from(1))
    }

    pub fn value(&self) -> &T {
        match self {
            ArcState::Upper(v) => v,
            ArcState::Tree(v) => v,
            ArcState::Lower(v) => v,
        }
    }
}

/// Enum for representing the direction of an arc in the spanning tree
/// - `Down`: The arc is oriented away from the root node of the spanning tree
/// - `Up`: The arc is oriented towards the root node of the spanning tree
///
/// A i32 representation is used to enable conversion to generic type for calculations.
#[derive(Debug, PartialEq, Clone)]
pub enum ArcDirection<T> {
    Down(T),
    Up(T),
}

impl<T> ArcDirection<T>
where
    T: From<i32>,
{
    pub fn down() -> Self {
        ArcDirection::Down(T::from(-1))
    }

    pub fn up() -> Self {
        ArcDirection::Up(T::from(1))
    }

    pub fn value(&self) -> &T {
        match self {
            ArcDirection::Down(v) => v,
            ArcDirection::Up(v) => v,
        }
    }
}

/// Epislon value for floating point calculations - may require adjustment depending on problem
const EPSILON: f64 = 1e-15;

/// Authored by Leonhard Mühlmeyer (2024)
/// # Network Simplex Implementation
///
/// This Rust implementation of the Network Simplex algorithm is based on the
/// original implementation from the LEMON (Library for Efficient Modeling and Optimization in Networks) library.
///
/// # Acknowledgment
/// The original C++ code is part of the LEMON library:
///
/// ```text
/// Copyright (C) 2003-2013
/// Egervary Jeno Kombinatorikus Optimalizalasi Kutatocsoport
/// (Egervary Research Group on Combinatorial Optimization, EGRES).
///
/// Permission to use, modify, and distribute the software was granted under the
/// Boost Software License, Version 1.0.
/// ```
///
/// For more details, refer to the original LEMON documentation and license:
/// - [LEMON Library](http://lemon.cs.elte.hu)
/// - Boost Software License: <https://www.boost.org/LICENSE_1_0.txt>
///
/// # Changes to LEMON implementation
/// - This implementation always uses the block search pivot rule, while LEMON allows for different pivot rules. However, the block search pivot rule is the most efficient in practice and also LEMON defaults to it.
/// - This implementation works with floating point types, which is not the case for LEMON. However, this might also be instable in this implemenation (see Notes).
/// - This implementation simplifies the process of executing the algorithm over LEMON's implementation, which includes `reset, resetParams` methods and also uses individual methods for setting the digraph, its supplies and its costs. Here, the constructor method `new` is used to streamline this process.
/// - This implementation does not allow for setting arc capacities, as the original LEMON implementation does.
///
/// # Description
/// The Network Simplex algorithm is a network optimization algorithm that solves the minimum cost flow problem, where every node has a supply or demand and every arc has a cost.<br>
/// This implementation uses the generic type `T` to represent the costs, supplies, and flow values. The NetworkSimplexValueType enum provides three options that are ready to use with this implementation (`i64, BigInt, f64`).<br>
/// First, create a new instance of NetworkSimplex with the graph and costs, supply, and other parameters. Then call the `run` method to solve the problem. Finally, the `get_result` method provides the optimal cost if the problem is feasible and bounded.<br>
///
/// # Example
/// ```ignore
/// use NetworkSimplexValueType::Exact64;
/// NetworkSimplexValueType::set_mode_globally(1);
/// let supply: Vec<NetworkSimplexValueType> =
///     vec![20, 0, 0, -5, -14].into_iter().map(Exact64).collect();
/// let graph_and_costs: Vec<Vec<Option<NetworkSimplexValueType>>> = vec![
///     vec![None, Some(4), Some(4), None, None],
///     vec![None, None, Some(2), Some(2), Some(6)],
///     vec![None, None, None, Some(1), Some(3)],
///     vec![None, None, None, None, Some(2)],
///     vec![None, None, Some(3), None, None],
/// ]
/// .into_iter()
/// .map(|row| row.into_iter().map(|x| x.map(Exact64)).collect())
/// .collect();
/// let mut ns = NetworkSimplex::new(&graph_and_costs, &supply, false, false);
/// if ns.run(false) == ProblemType::Optimal {
///     result = ns.get_result().unwrap();
/// }
/// ```
///
/// # Notes
/// Using the Network Simplex algorithm on floating point types may result in issues caused by rounding errors. To deal with these, an EPSILON constant is used.
/// This may impair the result accuracy. Furthermore, correctness and termination are not guaranteed for all network inputs due to lack of extensive testing.
/// If any issues related to the use of floats come up, <https://pythonot.github.io> might be a helpful reference.
/// Pythonot internally uses an adjusted version of LEMON's Network Simplex algorithm, that is explicitely designed to work with floating point types.
pub struct NetworkSimplex<T> {
    // Data related to the underlying digraph
    node_num: usize,
    all_node_num: usize,
    arc_num: usize,
    all_arc_num: usize,
    search_arc_num: usize,
    node_id: Vec<usize>,
    source: Vec<usize>, // stores node_id of source nodes of arcs
    target: Vec<usize>, // stores node_id of target nodes of arcs
    cost: Vec<T>,       // Cost of each arc
    supply: Vec<T>,     // Supply of each node
    sum_supply: T,
    supply_type: SupplyType,
    flow: Vec<T>, // Flow values for arcs
    pi: Vec<T>,   // Potential (dual variable) for nodes

    // Data for storing the spanning tree structure
    parent: Vec<Option<usize>>, // Parent node in the spanning tree - parent(root) = None
    predecessor: Vec<Option<usize>>, // Predecessor arc in the tree - predecessor(root) = None
    thread: Vec<usize>,         // Threading order in the spanning tree
    reverse_thread: Vec<usize>, // Reverse threading order
    successor_num: Vec<usize>,  // Number of successors in the tree
    last_successor: Vec<usize>, // Last successor in the tree
    predecessor_direction: Vec<ArcDirection<T>>, // Direction of predecessor arc
    state: Vec<ArcState<T>>,    // State of arcs (upper, lower, or tree)
    dirty_revs: Vec<usize>,     // Dirty reverse edges (for edge revision tracking)
    root: usize,                // Root node of the spanning tree

    // Temporary data used in the current pivot iteration
    in_arc: usize,
    join: usize,
    u_in: usize,
    v_in: usize,
    u_out: usize,
    v_out: usize,
    delta: T, // Delta value for flow change
    max: T,   // Maximum value for delta

    // Block search pivot rule parameters
    block_size: usize,
    next_arc: usize,

    // Probem Type to reject get_result requests for infeasible or unbounded problems
    problem_type: Option<ProblemType>,
}

impl<T> NetworkSimplex<T>
where
    T: Zero
        + One
        + IsFloat // Custom trait to check if the type is a float during runtime
        + MulWithFloat
        + Clone
        + for<'a> AddAssign<&'a T>
        + for<'a> SubAssign<&'a T>
        + for<'a> MulAssign<&'a T>
        + Neg<Output = T>
        + Signed
        + PartialEq
        + PartialOrd
        + ?Sized
        + Display
        + Debug
        + From<i32>
        + Sum
        + Send
        + Sync
        + ToBigInt
        + 'static,
{
    /// Creates a new instance of `NetworkSimplex`.
    ///
    /// # Parameters
    /// - `graph_and_costs`: A reference to a 2D vector where each element represents the cost
    ///   of an arc in the graph. Each inner vector corresponds to a row in the adjacency matrix,
    ///   and `None` indicates the absence of an arc between nodes.
    /// - `supply`: A reference to a vector containing the supply or demand for each node.
    ///   Positive values indicate supply, negative values indicate demand, and zero indicates
    ///   a balanced node.
    /// - `arc_mixing`: A boolean flag that determines whether to apply arc mixing.
    ///   For certain network structures it might increase the stability of the algorithm.
    /// - `greater_eq_supply`: A boolean flag that specifies whether the supply constraints should
    ///   be treated as "greater than or equal to" (`true`) or strict equality (`false`). If `0 <= sum_supply` choose false.
    ///
    /// # Returns
    /// A new instance of `NetworkSimplex` initialized with the given graph structure and parameters.
    pub fn new(
        graph_and_costs: &Vec<Vec<Option<T>>>,
        supply: &Vec<T>,
        arc_mixing: bool,
        greater_eq_supply: bool,
    ) -> Self {
        let node_num = supply.len();

        // Ensure that the graph dimensions match the number of nodes
        assert!(
            graph_and_costs.len() == node_num,
            "Graph size and supply size mismatch"
        );

        // Ensure that the graph is square (all rows must be the same size)
        for row in graph_and_costs.iter() {
            assert!(row.len() == node_num, "Graph matrix not square");
        }

        let node_id: Vec<usize> = (0..node_num).collect();
        let supply = (*supply).clone(); // No need to change the supplies

        // Create arcs from the graph and costs matrix
        let mut source = vec![];
        let mut target = vec![];
        let mut cost = vec![];
        for i in 0..node_num {
            for j in 0..node_num {
                if let Some(c) = &graph_and_costs[i][j] {
                    // Could as well allow for self loops: cost>0 -> ignore, cost<0 -> Unbounded if connected to some supply
                    assert!(i != j, "Tried to add arc from node to itself");
                    source.push(i);
                    target.push(j);
                    cost.push((*c).clone());
                }
            }
        }
        let arc_num = cost.len();

        // Shuffle the arcs if arc_mixing is enabled -> might be beneficial for stability in some cases
        if arc_mixing {
            // Combine the data into a single collection
            let mut arcs: Vec<_> = source
                .iter()
                .zip(target.iter())
                .zip(cost.iter())
                .map(|((src, tgt), cst)| (*src, *tgt, cst.clone()))
                .collect();

            // Shuffle the combined data
            let mut rng = thread_rng();
            arcs.shuffle(&mut rng);

            // Unpack the data back into separate vectors
            source.clear();
            target.clear();
            cost.clear();

            for (src, tgt, cst) in arcs {
                source.push(src);
                target.push(tgt);
                cost.push(cst);
            }
        }

        let block_size_factor = 1.0;
        let min_block_size = 10;
        let block_size =
            ((block_size_factor * (arc_num as f64).sqrt()) as usize).max(min_block_size);

        let supply_type = if greater_eq_supply {
            SupplyType::GEQ
        } else {
            SupplyType::LEQ
        };

        let ns = NetworkSimplex {
            // Data related to the underlying digraph
            node_num,
            all_node_num: node_num,
            arc_num,
            all_arc_num: arc_num,
            search_arc_num: 0,

            // Parameters of the problem
            sum_supply: T::zero(),

            // Data structures for storing the digraph
            node_id,
            source,
            target,

            // Node and arc data
            cost,
            supply,
            flow: vec![],
            pi: vec![],

            // Data for storing the spanning tree structure
            parent: vec![],
            predecessor: vec![],
            thread: vec![],
            reverse_thread: vec![],
            successor_num: vec![],
            last_successor: vec![],
            predecessor_direction: vec![],
            state: vec![],
            dirty_revs: vec![],
            root: 0,

            // Temporary data used in the current pivot iteration
            in_arc: 0,
            join: 0,
            u_in: 0,
            v_in: 0,
            u_out: 0,
            v_out: 0,
            delta: T::zero(),

            // Maximum value for delta
            max: T::one(),

            // Block search pivot rule parameters
            block_size,
            next_arc: 0,

            problem_type: None,
            supply_type,
        };

        ns
    }

    /// DEBUG function
    /// Might be useful for debugging if unclear whether the network is set up correctly
    pub fn visualize_network(&self) {
        let mut nodes_output = String::new();
        for i in 0..self.node_id.len() {
            let node = self.node_id[i];
            let supply = &self.supply[i];
            nodes_output.push_str(&format!("{}({})", node, supply));
            if i < self.node_id.len() - 1 {
                nodes_output.push_str(", ");
            }
        }
        log::debug!("nodes: [{}]", nodes_output);
        let mut arcs_output = String::new();
        for i in 0..self.all_arc_num {
            let source = self.source[i];
            let target = self.target[i];
            let cost = &self.cost[i];
            arcs_output.push_str(&format!("{}--({})-->{}", source, cost, target));
            if i < self.all_arc_num - 1 {
                arcs_output.push_str(", ");
            }
        }
        log::debug!("arcs: {}", arcs_output);
    }

    /// DEBUG function
    /// Might be useful for debugging if suspected that tree update is not working correctly
    pub fn visualize_tree_graphviz(&self) -> String {
        let mut graphviz_code = String::new();
        graphviz_code.push_str("digraph Tree {\n");

        // Label the root node
        graphviz_code.push_str(&format!(
            "    {} [label=\"{} (Root)\", shape=box];\n",
            self.root, self.root
        ));

        for i in 0..self.all_node_num {
            if self.parent[i] != None {
                let parent = self.parent[i].unwrap();
                let direction = &self.predecessor_direction[i];
                let flow = &self.flow[self.predecessor[i].unwrap()];
                if direction.value() == &T::from(1) {
                    graphviz_code
                        .push_str(&format!("    {} -> {} [label=\"{}\"];\n", i, parent, *flow));
                } else {
                    graphviz_code
                        .push_str(&format!("    {} -> {} [label=\"{}\"];\n", parent, i, *flow));
                }
            }
        }
        graphviz_code.push_str("}\n");
        graphviz_code
    }

    /// Central function performing the primal network simplex algorithm
    ///
    /// # Parameters
    /// - `guarantee_network_feasibility`: if true the algorithm will ignore the final sanity feasibility check whether any flow is left on artificial arcs
    ///
    /// # Returns
    /// The problem type of the network: Optimal, Infeasible, or Unbounded
    ///
    /// # Algorithm
    /// 1. Create initial basic solution (see `initialize_feasible_solution`). If this fails, return Infeasible
    /// 2. WHILE Find entering arc using block search pivot rule is successful (closes a circle within the spanning tree; see `find_entering_arc`)
    ///    - Find join node (node on closed cycle that is closest to the root node; see `find_join_node`)
    ///    - Identify arc that should leave the basis (see `find_leaving_arc`)
    ///    - Update the flow along the cycle (see `change_flow`)
    ///    - Adjust the spanning tree representation (see `update_tree_structure`)
    ///    - Update the potentials where necessary (see `update_potential`)
    /// 3. Check feasibility: any remaining flow on artificial arcs? (only if `guarantee_network_feasibility` is false)
    ///    - If so, return Infeasible
    ///    - Otherwise, return Optimal
    pub fn run(&mut self, guarantee_network_feasibility: bool) -> ProblemType {
        if !self.initialize_feasible_solution() {
            self.problem_type = Some(ProblemType::Infeasible);
            log::info!("Could not initialize feasible solution");
            return ProblemType::Infeasible;
        }
        log::debug!("{}", self.visualize_tree_graphviz());
        log::debug!("Potential: {:?}", self.pi);
        let mut iter = 1;

        let num_threads = rayon::current_num_threads();
        let pool = rayon::ThreadPoolBuilder::new()
            .num_threads(num_threads)
            .build()
            .unwrap();

        //while self.find_entering_arc() {
        while self.find_entering_arc_par(&pool) {
            log::debug!("_____________________________\nIteration: {}", iter);
            iter += 1;

            log::debug!(
                "Entering arc: {}-->{}",
                self.source[self.in_arc],
                self.target[self.in_arc]
            );

            self.find_join_node();
            let change = self.find_leaving_arc();
            // if a cycle with negative cost is found, the network is unbounded
            if self.delta >= self.max {
                self.problem_type = Some(ProblemType::Unbounded);
                log::info!("The current Network is unbounded");
                return ProblemType::Unbounded;
            }

            self.change_flow(change);
            if change {
                log::debug!(
                    "Leaving arc: {}-->{} with delta {}",
                    self.source[self.predecessor[self.u_out].unwrap()],
                    self.target[self.predecessor[self.u_out].unwrap()],
                    self.delta
                );

                self.update_tree_structure();
                self.update_potential(); // update the dual solution for the next iteration
                log::debug!("Potential updated");
                log::debug!("Potential: {:?}", self.pi);
                log::debug!("{}", self.visualize_tree_graphviz());
            }
        }
        log::info!("Network Simplex finished in {} iterations", iter);

        // check feasibility: any remaining flow on artificial arcs?
        if !guarantee_network_feasibility {
            // for floating point types T, check if flow is close to zero; for integer types, check if flow is zero
            if T::is_float(&self.sum_supply) {
                for e in self.search_arc_num..self.all_arc_num {
                    // there might be some rounding errors. Increase/scale the epsilon if necessary
                    if self.flow[e] > T::one().mul_with_float(&EPSILON) {
                        self.problem_type = Some(ProblemType::Infeasible);
                        log::info!(
                            "The current Network is infeasible, flow remains on artificial arcs"
                        );
                        return ProblemType::Infeasible;
                    }
                }
            } else {
                for e in self.search_arc_num..self.all_arc_num {
                    if self.flow[e] != T::zero() {
                        self.problem_type = Some(ProblemType::Infeasible);
                        log::info!(
                            "The current Network is infeasible, flow remains on artificial arcs"
                        );
                        return ProblemType::Infeasible;
                    }
                }
            }
        }

        self.problem_type = Some(ProblemType::Optimal);
        log::info!("Optimal solution found");
        return ProblemType::Optimal;
    }

    /// Internal function:
    /// Uses Block Search Pivot Rule to find the entering arc
    /// For each arc in the current block (block_size), the potential deterioration is calculated.
    /// The arc with the most negative deterioration (biggest improvement) is selected as the entering arc.
    /// If the block is exhausted and no improving arc has been found, the next block is started.
    fn find_entering_arc(&mut self) -> bool {
        let mut cost: T;
        let mut min_cost = T::zero();
        let mut count = self.block_size;

        // First loop from next_arc to _search_arc_num
        for e in self.next_arc..self.search_arc_num {
            cost = self.cost[e].clone();
            cost += &self.pi[self.source[e]];
            cost -= &self.pi[self.target[e]];
            cost *= self.state[e].value();

            log::trace!(
                "{}-->{}, cost: {} = {} * ({} + {} - {})",
                self.source[e],
                self.target[e],
                cost,
                self.state[e].value(),
                self.cost[e],
                self.pi[self.source[e]],
                self.pi[self.target[e]]
            );
            if cost < min_cost {
                min_cost = cost;
                self.in_arc = e;
            }
            count -= 1;
            // block exhausted, check if a valid arc was found
            if count == 0 {
                if T::is_float(&min_cost) {
                    // Floating-point specific logic
                    let source_value = self.pi[self.source[self.in_arc]].abs();
                    let target_value = self.pi[self.target[self.in_arc]].abs();
                    let cost_value = self.cost[self.in_arc].abs();

                    let mut a = if source_value > target_value {
                        source_value
                    } else {
                        target_value
                    };
                    a = if a > cost_value { a } else { cost_value };

                    if min_cost < -a.mul_with_float(&EPSILON) {
                        self.next_arc = e;
                        return true;
                    }
                } else {
                    // Integer logic
                    if min_cost < T::zero() {
                        self.next_arc = e;
                        return true;
                    }
                }
                // reset count for next block
                count = self.block_size;
            }
        }

        // Second loop from 0 to next_arc. Only used if the end of the arc vector is reached before the block is exhausted.
        // -> continue search from start
        for e in 0..self.next_arc {
            cost = self.cost[e].clone();
            cost += &self.pi[self.source[e]];
            cost -= &self.pi[self.target[e]];
            cost *= self.state[e].value();
            log::trace!(
                "{}-->{}, cost: {} = {} * ({} + {} - {})",
                self.source[e],
                self.target[e],
                cost,
                self.state[e].value(),
                self.cost[e],
                self.pi[self.source[e]],
                self.pi[self.target[e]]
            );
            if cost < min_cost {
                min_cost = cost;
                self.in_arc = e;
            }
            count -= 1;
            if count == 0 {
                if min_cost < T::zero() {
                    self.next_arc = e;
                    return true;
                }
                count = self.block_size;
            }
            // block exhausted, check if a valid arc was found
            if count == 0 {
                if T::is_float(&min_cost) {
                    // Floating-point specific logic
                    let source_value = self.pi[self.source[self.in_arc]].abs();
                    let target_value = self.pi[self.target[self.in_arc]].abs();
                    let cost_value = self.cost[self.in_arc].abs();

                    let mut a = if source_value > target_value {
                        source_value
                    } else {
                        target_value
                    };
                    a = if a > cost_value { a } else { cost_value };

                    if min_cost < -a.mul_with_float(&EPSILON) {
                        self.next_arc = e;
                        return true;
                    }
                } else {
                    // Integer logic
                    if min_cost < T::zero() {
                        self.next_arc = e;
                        return true;
                    }
                }
                // reset count for next block
                count = self.block_size;
            }
        }

        // Check if a valid arc was found

        if T::is_float(&min_cost) {
            // Floating-point specific logic
            let source_value = self.pi[self.source[self.in_arc]].abs();
            let target_value = self.pi[self.target[self.in_arc]].abs();
            let cost_value = self.cost[self.in_arc].abs();

            let mut a = if source_value > target_value {
                source_value
            } else {
                target_value
            };
            a = if a > cost_value { a } else { cost_value };

            if min_cost >= -a.mul_with_float(&EPSILON) {
                return false;
            }
        } else {
            // Integer logic
            if min_cost >= T::zero() {
                return false;
            }
        }

        true
    }

    fn find_entering_arc_par(&mut self, pool: &ThreadPool) -> bool {
        self.find_entering_arc_par_recursive(pool, 0)
    }

    fn find_entering_arc_par_recursive(&mut self, pool: &ThreadPool, arcs_visited: usize) -> bool {
        let num_threads = pool.current_num_threads();
        let block_size_per_thread = self.block_size / num_threads;
        let arcs_per_thread = self.search_arc_num / num_threads;
        if block_size_per_thread == 0 || arcs_per_thread < 500 {
            return self.find_entering_arc();
        }

        // Shared state between threads
        let min_cost = Arc::new(parking_lot::Mutex::new(T::zero()));
        let min_arc = Arc::new(AtomicUsize::new(0));

        let cost = &self.cost;
        let pi = &self.pi;
        let source = &self.source;
        let target = &self.target;
        let state = &self.state;
        let next_arc = self.next_arc;
        let search_arc_num = self.search_arc_num;

        pool.install(|| {
            pool.scope(|scope| {
                for thread_idx in 0..num_threads {
                    let min_cost = Arc::clone(&min_cost);
                    let min_arc = Arc::clone(&min_arc);

                    scope.spawn(move |_| {
                        let start = next_arc + thread_idx * arcs_per_thread;
                        let end = start + arcs_per_thread;

                        let mut thread_min_cost = T::zero();
                        let mut thread_min_arc = start;
                        let mut first_iteration = true;

                        let mut current = start;
                        while current < end {
                            let e = if current >= search_arc_num {
                                current - search_arc_num
                            } else {
                                current
                            };

                            let mut cost = cost[e].clone();
                            cost += &pi[source[e]];
                            cost -= &pi[target[e]];
                            cost *= state[e].value();

                            if first_iteration || cost < thread_min_cost {
                                thread_min_cost = cost;
                                thread_min_arc = e;
                                first_iteration = false;
                            }

                            current += 1;
                        }

                        if thread_min_cost < T::zero() {
                            let mut global_min = min_cost.lock();
                            if first_iteration || thread_min_cost < *global_min {
                                *global_min = thread_min_cost;
                                min_arc.store(thread_min_arc, Ordering::Relaxed);
                            }
                        }
                    });
                }
            });
        });

        let final_min_cost = min_cost.lock().clone();
        let final_min_arc = min_arc.load(Ordering::Relaxed);

        // Calculate how many arcs we processed in this block
        let arcs_in_block = num_threads * arcs_per_thread;
        let new_arcs_visited = arcs_visited + arcs_in_block;

        // Update struct fields
        self.next_arc = (next_arc + arcs_in_block) % search_arc_num;
        self.in_arc = final_min_arc;

        // Check if a valid arc was found
        let valid_arc_found = if T::is_float(&final_min_cost) {
            let source_value = self.pi[self.source[self.in_arc]].abs();
            let target_value = self.pi[self.target[self.in_arc]].abs();
            let cost_value = self.cost[self.in_arc].abs();

            let mut a = if source_value > target_value {
                source_value
            } else {
                target_value
            };
            a = if a > cost_value { a } else { cost_value };

            final_min_cost < -a.mul_with_float(&EPSILON)
        } else {
            final_min_cost < T::zero()
        };

        if valid_arc_found {
            true
        } else if new_arcs_visited >= search_arc_num {
            // We've checked at least as many arcs as exist in total
            false
        } else {
            // Continue searching with the next block
            self.find_entering_arc_par_recursive(pool, new_arcs_visited)
        }
    }

    /// Internal function:
    /// find arc that should leave the basis
    /// i.e. the arc with the minimum flow (primal solution) that is oriented against the closed cycle
    /// returns true iff a leaving arc could be identified
    fn find_leaving_arc(&mut self) -> bool {
        let first;
        let second;
        if self.state[self.in_arc].value() == &T::from(1) {
            first = self.source[self.in_arc];
            second = self.target[self.in_arc];
        } else {
            first = self.target[self.in_arc];
            second = self.source[self.in_arc];
        }

        self.delta = self.max.clone();
        let mut result = 0;
        let mut d;
        let mut e;

        // search tree from first node to join node
        let mut u = Some(first);
        while let Some(u_node) = u {
            if u_node == self.join {
                break;
            }
            e = self.predecessor[u_node].unwrap();
            d = &self.flow[e];
            if self.predecessor_direction[u_node].value() == &T::from(-1) {
                d = &self.max;
            }
            if *d < self.delta {
                self.delta = d.clone();
                self.u_out = u_node;
                result = 1;
            }
            u = self.parent[u_node];
        }

        // search tree from second node to join node
        let mut u = Some(second);
        while let Some(u_node) = u {
            if u_node == self.join {
                break;
            }
            e = self.predecessor[u_node].unwrap();
            d = &self.flow[e];
            if self.predecessor_direction[u_node].value() == &T::from(1) {
                d = &self.max;
            }
            if *d < self.delta {
                self.delta = d.clone();
                self.u_out = u_node;
                result = 2;
            }
            u = self.parent[u_node];
        }
        if result == 1 {
            self.u_in = first;
            self.v_in = second;
        } else {
            self.u_in = second;
            self.v_in = first;
        }
        return result != 0;
    }

    /// Internal function:
    /// Function to update potentials after flow changes
    /// All potentials of the successors of u_in are updated
    fn update_potential(&mut self) {
        let mut sigma = -self.cost[self.in_arc].clone();
        sigma *= &(self.predecessor_direction[self.u_in].value());
        sigma += &self.pi[self.v_in];
        sigma -= &self.pi[self.u_in];

        let end = self.thread[self.last_successor[self.u_in]];
        log::debug!("u_in: {}, end: {}", self.u_in, end);
        let mut u = self.u_in;
        while u != end {
            log::trace!("Potential updated, u: {}, end: {}", u, end);
            self.pi[u] += &sigma;
            u = self.thread[u];
        }
    }

    /// Internal function:
    /// Initializes flows and potentials
    /// adds artificial root node, connects all nodes to it (orienation based on supply)
    /// this is the initial basis (feasible solution)
    fn initialize_feasible_solution(&mut self) -> bool {
        // no nodes in the graph
        if self.node_num == 0 {
            log::info!("No nodes in the graph");
            return false;
        }
        // check if sum of supply is valid
        self.sum_supply = T::zero();
        for i in 0..self.node_num {
            self.sum_supply += &self.supply[i];

            if self.supply[i].is_positive() {
                self.max += &self.supply[i]
            }
        }
        if !((self.supply_type == SupplyType::GEQ && self.sum_supply <= T::zero())
            || (self.supply_type == SupplyType::LEQ && self.sum_supply >= T::zero()))
        {
            log::info!("Sum of supply is invalid, try changing supply type");
            return false;
        }

        let mut max_cost = self.find_max_cost();
        max_cost += &T::one();
        max_cost *= &T::from(self.node_num as i32);
        let art_cost: T = max_cost;

        log::debug!("art_cost identified as: {}", art_cost);

        // resize all vectors
        self.all_node_num = self.node_num + 1;
        let max_arc_num = self.arc_num + 2 * self.node_num;

        self.all_arc_num = self.arc_num + self.node_num;
        self.source.resize(max_arc_num, 0);
        self.target.resize(max_arc_num, 0);
        self.flow.resize(max_arc_num, T::zero());
        self.state.resize(max_arc_num, ArcState::lower());
        self.cost.resize(max_arc_num, T::zero());
        self.supply.resize(self.all_node_num, T::zero());
        self.pi.resize(self.all_node_num, T::zero());
        self.parent.resize(self.all_node_num, Some(0));
        self.predecessor.resize(self.all_node_num, Some(0));
        self.predecessor_direction
            .resize(self.all_node_num, ArcDirection::up());
        self.thread.resize(self.all_node_num, 0);
        self.reverse_thread.resize(self.all_node_num, 0);
        self.successor_num.resize(self.all_node_num, 0);
        self.last_successor.resize(self.all_node_num, 0);

        // initialize arc network arcs
        for i in 0..self.node_num {
            self.flow[i] = T::zero();
            self.state[i] = ArcState::lower();
        }

        // set up artificial root node
        self.root = self.node_num;
        self.node_id.push(self.root);
        self.parent[self.root] = None;
        self.predecessor[self.root] = None;
        self.thread[self.root] = 0;
        self.reverse_thread[0] = self.root;
        self.successor_num[self.root] = self.node_num + 1; // including root
        self.last_successor[self.root] = self.node_num - 1;
        self.supply[self.root] = -self.sum_supply.clone();
        self.pi[self.root] = T::zero();

        // set up aticficial arcs (i, root node) for b_i >= 0, (root node, i) for b_i < 0
        if self.sum_supply == T::zero() {
            self.search_arc_num = self.arc_num;
            let mut e = self.arc_num;
            for u in 0..self.node_num {
                self.parent[u] = Some(self.root);
                self.predecessor[u] = Some(e);
                self.thread[u] = u + 1;
                self.reverse_thread[u + 1] = u;
                self.successor_num[u] = 1;
                self.last_successor[u] = u;
                self.state[e] = ArcState::tree();
                if !self.supply[u].is_negative() {
                    self.predecessor_direction[u] = ArcDirection::up();
                    self.pi[u] = T::zero();
                    self.source[e] = u;
                    self.target[e] = self.root;
                    self.flow[e] = self.supply[u].clone();
                    self.cost[e] = T::zero();
                } else {
                    self.predecessor_direction[u] = ArcDirection::down();
                    self.pi[u] = art_cost.clone();
                    self.source[e] = self.root;
                    self.target[e] = u;
                    self.flow[e] = -self.supply[u].clone();
                    self.cost[e] = art_cost.clone();
                }
                e += 1;
            }
        } else if self.sum_supply > T::zero() {
            // LEQ supply constraints
            self.search_arc_num = self.arc_num + self.node_num;
            let mut f = self.arc_num + self.node_num;
            log::debug!("node num: {}", self.node_num);
            for u in 0..self.node_num {
                self.parent[u] = Some(self.root);
                self.thread[u] = u + 1;
                self.reverse_thread[u + 1] = u;
                self.successor_num[u] = 1;
                self.last_successor[u] = u;
                if !self.supply[u].is_negative() {
                    self.predecessor_direction[u] = ArcDirection::up();
                    self.pi[u] = T::zero();
                    self.predecessor[u] = Some(self.arc_num + u);
                    self.source[self.arc_num + u] = u;
                    self.target[self.arc_num + u] = self.root;
                    self.state[self.arc_num + u] = ArcState::tree();
                    self.flow[self.arc_num + u] = self.supply[u].clone();
                    self.cost[self.arc_num + u] = T::zero();
                } else {
                    self.predecessor_direction[u] = ArcDirection::down();
                    self.pi[u] = art_cost.clone();
                    self.predecessor[u] = Some(f);
                    self.source[f] = self.root;
                    self.target[f] = u;
                    self.state[f] = ArcState::tree();
                    self.flow[f] = -self.supply[u].clone();
                    self.cost[f] = art_cost.clone();
                    self.source[self.arc_num + u] = u;
                    self.target[self.arc_num + u] = self.root;
                    self.state[self.arc_num + u] = ArcState::lower();
                    self.flow[self.arc_num + u] = T::zero();
                    self.cost[self.arc_num + u] = T::zero();
                    f += 1;
                    log::debug!("f increased by 1");
                }
            }
            self.all_arc_num = f;
        } else {
            // GEQ supply constraints
            self.search_arc_num = self.arc_num + self.node_num;
            let mut f = self.arc_num + self.node_num;
            for u in 0..self.node_num {
                self.parent[u] = Some(self.root);
                self.thread[u] = u + 1;
                self.reverse_thread[u + 1] = u;
                self.successor_num[u] = 1;
                self.last_successor[u] = u;
                if !self.supply[u].is_positive() {
                    self.predecessor_direction[u] = ArcDirection::down();
                    self.pi[u] = T::zero();
                    self.predecessor[u] = Some(self.arc_num + u);
                    self.source[self.arc_num + u] = self.root;
                    self.target[self.arc_num + u] = u;
                    self.state[self.arc_num + u] = ArcState::tree();
                    self.flow[self.arc_num + u] = -self.supply[u].clone();
                    self.cost[self.arc_num + u] = T::zero();
                } else {
                    self.predecessor_direction[u] = ArcDirection::up();
                    self.pi[u] = -art_cost.clone();
                    self.predecessor[u] = Some(f);
                    self.source[f] = u;
                    self.target[f] = self.root;
                    self.state[f] = ArcState::tree();
                    self.flow[f] = self.supply[u].clone();
                    self.cost[f] = art_cost.clone();
                    self.source[self.arc_num + u] = self.root;
                    self.target[self.arc_num + u] = u;
                    self.state[self.arc_num + u] = ArcState::lower();
                    self.flow[self.arc_num + u] = T::zero();
                    self.cost[self.arc_num + u] = T::zero();
                    f += 1;
                }
            }
            self.all_arc_num = f;
        }
        return true;
    }

    /// Internal function:
    /// Function that identifies the node in the tree where the cycle is closed, i.e. the deepest node that is both a path to v_in and u_in.
    /// In the basis tree, trace down from nodes adjacent to the entering arc to first node closing the circle
    fn find_join_node(&mut self) {
        let mut u = self.source[self.in_arc];
        let mut v = self.target[self.in_arc];
        while u != v {
            // successor number is used to measure the depth of the node in the tree
            // for the u = v = join node the successor number will be the same
            if self.successor_num[u] < self.successor_num[v] {
                u = self.parent[u].unwrap();
            } else {
                v = self.parent[v].unwrap();
            }
        }
        self.join = u;
    }

    /// Internal function:
    /// Function that updates the flow along the cycle with the identified delta value.
    /// For arcs that are oriented against the cycle, the flow is decreased by delta.
    /// For arcs that are oriented with the cycle, the flow is increased by delta.
    /// This leads to a resulting flow of zero on the leaving arc (for floating point types, the flow is close to zero -> could lead to errors).
    fn change_flow(&mut self, change: bool) {
        if self.delta > T::zero() {
            let mut value = self.state[self.in_arc].value().clone();
            value *= &self.delta;
            self.flow[self.in_arc] += &value;
            let mut u = self.source[self.in_arc];
            while u != self.join {
                let mut reduce_by = self.predecessor_direction[u].value().clone();
                reduce_by *= &value;
                self.flow[self.predecessor[u].unwrap()] -= &reduce_by;
                u = self.parent[u].unwrap();
            }
            u = self.target[self.in_arc];
            while u != self.join {
                let mut increase_by = self.predecessor_direction[u].value().clone();
                increase_by *= &value;
                self.flow[self.predecessor[u].unwrap()] += &increase_by;
                u = self.parent[u].unwrap();
            }
        }
        if change {
            self.state[self.in_arc] = ArcState::tree();
            if self.flow[self.predecessor[self.u_out].unwrap()] == T::zero() {
                self.state[self.predecessor[self.u_out].unwrap()] = ArcState::lower();
            } else {
                self.state[self.predecessor[self.u_out].unwrap()] = ArcState::upper();
            }
        } else {
            if self.state[self.in_arc] == ArcState::lower() {
                self.state[self.in_arc] = ArcState::upper();
            } else {
                self.state[self.in_arc] = ArcState::lower();
            }
        }
    }

    /// Internal function:
    /// Function to update the tree structure when arcs are swapped in/out of the basis
    /// Depending on whether the leaving arc is on the branch from u_in to to the root or on the branch from v_in to the root,
    /// the respective branch is shifted and restructured s.t. u_in or v_in becomes the root of the subtree.
    /// This subtree is then inserted into the spanning tree.
    fn update_tree_structure(&mut self) {
        let old_reverse_thread = self.reverse_thread[self.u_out];
        let old_successor_num = self.successor_num[self.u_out];
        let old_last_successor = self.last_successor[self.u_out];
        self.v_out = self.parent[self.u_out].unwrap();

        // check if u_in and u_out coincide
        if self.u_in == self.u_out {
            // update parent, predecessor, predecessor_direction
            self.parent[self.u_in] = Some(self.v_in);
            self.predecessor[self.u_in] = Some(self.in_arc);
            self.predecessor_direction[self.u_in] = if self.u_in == self.source[self.in_arc] {
                ArcDirection::up()
            } else {
                ArcDirection::down()
            };

            // update thread and reverse_thread
            if self.thread[self.v_in] != self.u_out {
                let mut after = self.thread[old_last_successor];
                self.thread[old_reverse_thread] = after;
                self.reverse_thread[after] = old_reverse_thread;
                after = self.thread[self.v_in];
                self.thread[self.v_in] = self.u_out;
                self.reverse_thread[self.u_out] = self.v_in;
                self.thread[old_last_successor] = after;
                self.reverse_thread[after] = old_last_successor;
            }
        } else {
            // Handle the case when old_rev_thread equals to v_in
            // (it also means that join and v_out coincide)
            let thread_continue = if old_reverse_thread == self.v_in {
                self.thread[old_last_successor]
            } else {
                self.thread[self.v_in]
            };
            // update thread and parent along the stem nodes (i.e. the nodes between u_in and u_out, whose parents need adjustment)
            let mut stem = self.u_in;
            let mut stem_parent = self.v_in;
            let mut next_stem;
            let mut last = self.last_successor[self.u_in];
            let mut before;
            let mut after = self.thread[last];
            self.thread[self.v_in] = self.u_in;
            self.dirty_revs.clear();
            self.dirty_revs.push(self.v_in);
            while stem != self.u_out {
                // insert the next stem node into the thread list
                next_stem = self.parent[stem].unwrap();
                self.thread[last] = next_stem;
                self.dirty_revs.push(last);
                // remove the subtree of stem from the thread list
                before = self.reverse_thread[stem];
                self.thread[before] = after;
                self.reverse_thread[after] = before;
                // change the parent node and shift the stem nodes
                self.parent[stem] = Some(stem_parent);
                stem_parent = stem;
                stem = next_stem;
                // update last and after
                last = if self.last_successor[stem] == self.last_successor[stem_parent] {
                    self.reverse_thread[stem_parent]
                } else {
                    self.last_successor[stem]
                };
                after = self.thread[last];
            }
            self.parent[self.u_out] = Some(stem_parent);
            self.thread[last] = thread_continue;
            self.reverse_thread[thread_continue] = last;
            self.last_successor[self.u_out] = last;
            // remove the subtree of u_out from the thread list
            // except for the case when old_rev_thread equals to v_in
            if old_reverse_thread != self.v_in {
                self.thread[old_reverse_thread] = after;
                self.reverse_thread[after] = old_reverse_thread;
            }

            // update reverse_thread using the new thread values
            for i in 0..self.dirty_revs.len() {
                let u = self.dirty_revs[i];
                self.reverse_thread[self.thread[u]] = u;
            }

            // update predecessor, predecessor_direction, last_successor, and successor_num along the stem nodes
            let mut temp_successor_num = 0;
            let temp_last_successor = self.last_successor[self.u_out];
            let mut u = self.u_out;
            let mut p = self.parent[u];
            while u != self.u_in {
                self.predecessor[u] = self.predecessor[p.unwrap()];
                self.predecessor_direction[u] =
                    if self.predecessor_direction[p.unwrap()] == ArcDirection::up() {
                        ArcDirection::down()
                    } else {
                        ArcDirection::up()
                    };
                temp_successor_num += self.successor_num[u] - self.successor_num[p.unwrap()];
                self.successor_num[u] = temp_successor_num;
                self.last_successor[p.unwrap()] = temp_last_successor;

                u = p.unwrap();
                p = self.parent[u];
            }
            self.predecessor[self.u_in] = Some(self.in_arc);
            self.predecessor_direction[self.u_in] = if self.u_in == self.source[self.in_arc] {
                ArcDirection::up()
            } else {
                ArcDirection::down()
            };
            self.successor_num[self.u_in] = old_successor_num;
        }

        // update last_successor from v_in towards the root
        let up_limit_out = if self.last_successor[self.join] == self.v_in {
            Some(self.join)
        } else {
            None
        };
        let last_successor_out = self.last_successor[self.u_out];
        let mut u = Some(self.v_in);
        while u != None && self.last_successor[u.unwrap()] == self.v_in {
            self.last_successor[u.unwrap()] = last_successor_out;
            u = self.parent[u.unwrap()];
        }

        // update last_successor from v_out towards the root
        if self.join != old_reverse_thread && self.v_in != old_reverse_thread {
            u = Some(self.v_out);
            while u != None
                && u != up_limit_out
                && self.last_successor[u.unwrap()] == old_last_successor
            {
                self.last_successor[u.unwrap()] = old_reverse_thread;
                u = self.parent[u.unwrap()];
            }
        } else if last_successor_out != old_last_successor {
            u = Some(self.v_out);
            while u != None
                && u != up_limit_out
                && self.last_successor[u.unwrap()] == old_last_successor
            {
                self.last_successor[u.unwrap()] = last_successor_out;
                u = self.parent[u.unwrap()];
            }
        }
        // update successor_num from v_in to join
        let mut u = self.v_in;
        while u != self.join {
            self.successor_num[u] += old_successor_num;
            u = self.parent[u].unwrap();
        }
        // update successor_num from v_out to join
        u = self.v_out;
        while u != self.join {
            self.successor_num[u] -= old_successor_num;
            u = self.parent[u].unwrap();
        }
    }

    /// Retrieves the total cost of the flow if the problem is optimal.
    ///
    /// # Returns
    /// - `Some(T)`: The total cost of the flow if the problem type is `Optimal`.
    /// - `None`: If the problem type is not optimal or undefined.
    ///
    /// **Calculation**  
    /// The total cost is calculated as:
    /// Total Cost = Σ (flow_i × cost_i) for i = 1 to n <br>
    /// Where:
    /// - `flow_i` is the flow value on arc `i`.
    /// - `cost_i` is the cost associated with arc `i`.
    ///
    /// The result is accumulated over all arcs in the network.
    ///
    /// # Examples
    /// ```ignore
    /// // Assuming `simplex` is an instance of `NetworkSimplex` with optimal flow.
    /// if let Some(total_cost) = simplex.get_result() {
    ///     println!("The total flow cost is: {}", total_cost);
    /// } else {
    ///     println!("The problem is not in an optimal state.");
    /// }
    /// ```
    pub fn get_result(&self) -> Option<T> {
        if let Some(problem_type) = &self.problem_type {
            if problem_type == &ProblemType::Optimal {
                let flow_cost = self.flow.iter().zip(self.cost.iter());
                let mut result = T::zero();
                for (flow, cost) in flow_cost {
                    let mut arc_result = flow.clone();
                    arc_result *= cost;
                    result += &arc_result;
                }
                return Some(result);
            }
        }

        return None;
    }

    pub fn get_bigint_result(&self) -> Option<BigInt> {
        if let Some(problem_type) = &self.problem_type {
            if problem_type == &ProblemType::Optimal {
                let flow_cost = self.flow.iter().zip(self.cost.iter());
                let mut result = BigInt::zero();
                for (flow, cost) in flow_cost {
                    let mut arc_result = flow.to_big_int();
                    arc_result *= cost.to_big_int();
                    result += arc_result;
                }
                return Some(result);
            }
        }

        None
    }

    /// Retrieves the flow values of the network.
    ///
    /// # Returns
    /// A vector of flow values for each arc in the network.
    pub fn get_flow(&self) -> Vec<T> {
        self.flow.clone()
    }

    /// Retrieves the cost values of the network.
    ///
    /// # Returns
    /// A vector of cost values for each arc in the network.
    pub fn get_cost(&self) -> Vec<T> {
        self.cost.clone()
    }

    /// Internal function: Retrieves the supply values of the network.
    fn find_max_cost(&self) -> T {
        select_max(&self.cost).expect("Cost vector cannot be empty")
    }
}

/// Selects the maximum value from a slice of values.
/// Compared to standard max() function, this only requires the PartialOrd trait.
pub fn select_max<T>(values: &[T]) -> Option<T>
where
    T: PartialOrd + Clone,
{
    values
        .iter()
        .filter(|x| x.partial_cmp(x).is_some()) // Handles NaN if T is f64
        .max_by(|a, b| a.partial_cmp(b).unwrap())
        .cloned()
}

#[cfg(test)]
mod tests {
    use crate::optimisation_algorithms::network_simplex::NetworkSimplex;

    
    #[test]
    fn network_simplex_int() {
        let supply: Vec<i64> = vec![20, 0, 0, -5, -14];

        let graph_and_costs: Vec<Vec<Option<i64>>> = vec![
            vec![None, Some(4), Some(4), None, None],
            vec![None, None, Some(2), Some(2), Some(6)],
            vec![None, None, None, Some(1), Some(3)],
            vec![None, None, None, None, Some(2)],
            vec![None, None, Some(3), None, None],
        ];
        let mut ns = NetworkSimplex::new(&graph_and_costs, &supply, true, false);
        _ = ns.run(false);
        assert_eq!(ns.get_result().unwrap(), 123);
    }

    #[test]
    fn network_simplex_bigint() {
        use num::BigInt;

        let supply: Vec<BigInt> = vec![20, 0, 0, -5, -14]
            .into_iter()
            .map(|s| BigInt::from(s))
            .collect();

        let graph_and_costs: Vec<Vec<Option<BigInt>>> = vec![
            vec![None, Some(4), Some(4), None, None],
            vec![None, None, Some(2), Some(2), Some(6)],
            vec![None, None, None, Some(1), Some(3)],
            vec![None, None, None, None, Some(2)],
            vec![None, None, Some(3), None, None],
        ]
        .into_iter()
        .map(|row| {
            row.into_iter()
                .map(|x| x.map(|cost| BigInt::from(cost)))
                .collect()
        })
        .collect();

        let mut ns = NetworkSimplex::new(&graph_and_costs, &supply, true, false);
        _ = ns.run(false);
        assert_eq!(ns.get_result().unwrap(), BigInt::from(123));
    }

    #[test]
    fn network_simplex_float() {
        let supply: Vec<f64> = vec![20, 0, 0, -5, -14]
            .into_iter()
            .map(|s| s.into())
            .collect();

        let graph_and_costs: Vec<Vec<Option<f64>>> = vec![
            vec![None, Some(4), Some(4), None, None],
            vec![None, None, Some(2), Some(2), Some(6)],
            vec![None, None, None, Some(1), Some(3)],
            vec![None, None, None, None, Some(2)],
            vec![None, None, Some(3), None, None],
        ]
        .into_iter()
        .map(|row| row.into_iter().map(|x| x.map(|cost| cost.into())).collect())
        .collect();

        let mut ns = NetworkSimplex::new(&graph_and_costs, &supply, true, false);
        _ = ns.run(false);
        let result = ns.get_result().unwrap();
        assert_eq!(result, 123.0);
    }
}