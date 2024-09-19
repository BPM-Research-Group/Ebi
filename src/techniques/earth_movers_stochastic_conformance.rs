use fraction::{One, ToPrimitive, Zero};
use std::{collections::HashMap, ops::{Index, Mul, Neg}};
use num_integer::Roots;

use anyhow::{anyhow, Ok, Result};
use crate::{distances::{self, DistanceMatrix}, ebi_traits::{ebi_trait_finite_language::EbiTraitFiniteLanguage, ebi_trait_finite_stochastic_language::EbiTraitFiniteStochasticLanguage}, math::fraction::Fraction, net::Net};

pub fn emsc<A, B>(log: &A, model: &B) -> Result<Fraction> where A: EbiTraitFiniteLanguage + ?Sized, B: EbiTraitFiniteLanguage + ?Sized {
    let distances = DistanceMatrix::new(log, model);
    todo!()
}


//The remainder of this file has been adapted from https://github.com/nbonneel/network_simplex/blob/master/network_simplex_simple.h
// https://raw.githubusercontent.com/nbonneel/network_simplex/master/network_simplex_simple.h
//
/* -*- mode: C++; indent-tabs-mode: nil; -*-
*
*
* This file has been adapted by Nicolas Bonneel (2013),
* from network_simplex.h from LEMON, a generic C++ optimization library,
* to implement a lightweight network simplex for mass transport, more
* memory efficient than the original file. A previous version of this file
* is used as part of the Displacement Interpolation project,
* Web: http://www.cs.ubc.ca/labs/imager/tr/2011/DisplacementInterpolation/
*
* Revisions:
* March 2015: added OpenMP parallelization
* March 2017: included Antoine Rolet's trick to make it more robust
* April 2018: IMPORTANT bug fix + uses 64bit integers (slightly slower but less risks of overflows), updated to a newer version of the algo by LEMON, sparse flow by default + minor edits.
*
*
**** Original file Copyright Notice :
*
* Copyright (C) 2003-2010
* Egervary Jeno Kombinatorikus Optimalizalasi Kutatocsoport
* (Egervary Research Group on Combinatorial Optimization, EGRES).
*
* Permission to use, modify and distribute this software is granted
* provided that this copyright notice appears in all copies. For
* precise terms see the accompanying LICENSE file.
*
* This software is provided "AS IS" with no warranty of any kind,
* express or implied, and with no claim as to its suitability for any
* purpose.
*
*/

type ArcsType = usize;
type V = Fraction; /// The type of the flow amounts, capacity bounds and supply values
type C = Fraction; /// The type of the arc costs
type Cost = C;
type Value = V;
type IntVector = Vec<usize>;
type MaybeIntVector = Vec<Option<usize>>;
type ArcVector = Vec<ArcsType>;
type MaybeArcVector = Vec<Option<ArcsType>>;
type ValueVector = Vec<Value>;
type CostVector = Vec<Cost>;
//	typedef SparseValueVector<Cost> CostVector;
type BoolVector = Vec<bool>; // Note: vector<char> is used instead of vector<bool> for efficiency reasons
type StateVector = Vec<ArcState>; // Note: vector<signed char> is used instead of vector<ArcState> for efficiency reasons
type Node = usize;
type Arc = usize;


/// \addtogroup min_cost_flow_algs
/// @{

/// \brief Implementation of the primal Network Simplex algorithm
/// for finding a \ref min_cost_flow "minimum cost flow".
///
/// \ref NetworkSimplexSimple implements the primal Network Simplex algorithm
/// for finding a \ref min_cost_flow "minimum cost flow"
/// \ref amo93networkflows, \ref dantzig63linearprog,
/// \ref kellyoneill91netsimplex.
/// This algorithm is a highly efficient specialized version of the
/// linear programming simplex method directly for the minimum cost
/// flow problem.
///
/// In general, %NetworkSimplexSimple is the fastest implementation available
/// in LEMON for this problem.
/// Moreover, it supports both directions of the supply/demand inequality
/// constraints. For more information, see \ref SupplyType.
///
/// Most of the parameters of the problem (except for the digraph)
/// can be given using separate functions, and the algorithm can be
/// executed using the \ref run() function. If some parameters are not
/// specified, then default values will be used.
///
/// \tparam GR The digraph type the algorithm runs on.
/// \tparam V The number type used for flow amounts, capacity bounds
/// and supply values in the algorithm. By default, it is \c int.
/// \tparam C The number type used for costs and potentials in the
/// algorithm. By default, it is the same as \c V.
///
/// \warning Both number types must be signed and all input data must
/// be integer.
///
/// \note %NetworkSimplexSimple provides five different pivot rule
/// implementations, from which the most efficient one is used
/// by default. For more information, see \ref PivotRule.
struct NetworkSimplexSimple<'a>	{
	max_iter: usize,

    // Data related to the underlying digraph
    _graph: &'a FullBipartiteDigraphBase,
    _node_num: usize,
    _arc_num: ArcsType,
    _all_arc_num: ArcsType,
    _search_arc_num: ArcsType,

    // Parameters of the problem
	_stype: SupplyType,
	_sum_supply: Value,

    //IntArcMap _arc_id;
    _source: IntVector,  // keep nodes as integers
    _target: IntVector,
    _arc_mixing: bool,
    
    // Node and arc data
    _cost: CostVector,
    _supply: ValueVector,
    _flow: ValueVector,

    _pi: CostVector,
    
    // Data for storing the spanning tree structure
    _parent: MaybeIntVector,
    _pred: MaybeArcVector,
    _thread: IntVector,
    _rev_thread: IntVector,
    _succ_num: IntVector,
    _last_succ: MaybeIntVector,
    _dirty_revs: IntVector,
    _forward: BoolVector,
    _state: Vec<ArcState>,
    _root: ArcsType,

    // Temporary data used in the current pivot iteration
    in_arc: ArcsType, 
    join: ArcsType, 
    u_in: ArcsType, 
    v_in: Option<ArcsType>, 
    u_out: ArcsType, 
    v_out: ArcsType,
    first: ArcsType, 
    second: ArcsType, 
    right: ArcsType, 
    last: ArcsType,
    stem: ArcsType, 
    par_stem: ArcsType, 
    new_stem: ArcsType,
    delta: Value,

    MAX: Value,

    mixingCoeff: ArcsType,

    /// \brief Constant for infinite upper bounds (capacities).
    ///
    /// Constant for infinite upper bounds (capacities).
    /// It is \c std::numeric_limits<Value>::infinity() if available,
    /// \c std::numeric_limits<Value>::max() otherwise.
    INF: Value,

    subsequence_length: ArcsType,
    num_big_subsequences: ArcsType,
    num_total_big_subsequence_numbers: ArcsType,

    _init_nb_nodes: usize,
    _init_nb_arcs: ArcsType,
}

impl <'a> NetworkSimplexSimple<'a> {
    /// \brief Constructor.
    ///
    /// The constructor of the class.
    ///
    /// \param graph The digraph the algorithm runs on.
    /// \param arc_mixing Indicate if the arcs have to be stored in a
    /// mixed order in the internal data structure.
    /// In special cases, it could lead to better overall performance,
    /// but it is usually slower. Therefore it is disabled by default.
    pub fn new(graph: &FullBipartiteDigraphBase, arc_mixing: bool, nbnodes: usize, nb_arcs: ArcsType, maxiters: usize) -> Self {
        let result = Self {
            max_iter: 0,
            _graph: graph,
            _node_num: todo!(),
            _arc_num: todo!(),
            _all_arc_num: todo!(),
            _search_arc_num: todo!(),
            _stype: todo!(),
            _sum_supply: todo!(),
            _source: todo!(),
            _target: todo!(),
            _arc_mixing: arc_mixing,
            _cost: todo!(),
            _supply: todo!(),
            _flow: vec![],
            _pi: vec![],
            _parent: vec![],
            _pred: vec![],
            _thread: vec![],
            _rev_thread: vec![],
            _succ_num: vec![],
            _last_succ: vec![],
            _dirty_revs: vec![],
            _forward: vec![],
            _state: vec![],
            _root: todo!(),
            in_arc: todo!(),
            join: todo!(),
            u_in: todo!(),
            v_in: todo!(),
            u_out: todo!(),
            v_out: todo!(),
            first: todo!(),
            second: todo!(),
            right: todo!(),
            last: todo!(),
            stem: todo!(),
            par_stem: todo!(),
            new_stem: todo!(),
            delta: todo!(),
            MAX: Fraction::infinity(),
            mixingCoeff: todo!(),
            INF: Fraction::infinity(),
            subsequence_length: todo!(),
            num_big_subsequences: todo!(),
            num_total_big_subsequence_numbers: todo!(),
            _init_nb_nodes: nbnodes,
            _init_nb_arcs: nb_arcs,
        };

        // Reset data structures
        result.reset();
        result.max_iter = maxiters;

        result
    }

    fn _node_id(&self, n: usize) -> usize { 
        return self._node_num - n - 1; 
    }

        // thank you to DVK and MizardX from StackOverflow for this function!
    fn sequence(&self, mut k: ArcsType) -> ArcsType {
        let smallv: ArcsType = if k > self.num_total_big_subsequence_numbers { 1 } else { 0 };

        k -= self.num_total_big_subsequence_numbers * smallv;
        let subsequence_length2: ArcsType = self.subsequence_length - smallv;
        let subsequence_num: ArcsType = (k / subsequence_length2) + self.num_big_subsequences * smallv;
        let subsequence_offset: ArcsType = (k % subsequence_length2) * self.mixingCoeff;

        return subsequence_offset + subsequence_num;
    }

    fn getArcID(&self, arc: &Arc) -> Option<ArcsType> {
        //int n = _arc_num-arc._id-1;
        let id = FullBipartiteDigraphBase::id_arc(arc);
        if id >= self._arc_num {
            return None;
        }

        let n: ArcsType = (self._arc_num - id) - 1;

        //ArcsType a = mixingCoeff*(n%mixingCoeff) + n/mixingCoeff; 
        //ArcsType b = _arc_id[arc];
        if self._arc_mixing {
            return Some(self.sequence(n));
        } else {
            return Some(n);
        }
    }

    /// \brief Set the type of the supply constraints.
		///
		/// This function sets the type of the supply/demand constraints.
		/// If it is not used before calling \ref run(), the \ref GEQ supply
		/// type will be used.
		///
		/// For more information, see \ref SupplyType.
		///
		/// \return <tt>(*this)</tt>
	pub fn supplyType(&mut self, supply_type: SupplyType) {
		self._stype = supply_type;
	}

    /// \brief Run the algorithm.
    ///
    /// This function runs the algorithm.
    /// The paramters can be specified using functions \ref lowerMap(),
    /// \ref upperMap(), \ref costMap(), \ref supplyMap(), \ref stSupply(),
    /// \ref supplyType().
    /// For example,
    /// \code
    ///   NetworkSimplexSimple<ListDigraph> ns(graph);
    ///   ns.lowerMap(lower).upperMap(upper).costMap(cost)
    ///     .supplyMap(sup).run();
    /// \endcode
    ///
    /// This function can be called more than once. All the given parameters
    /// are kept for the next call, unless \ref resetParams() or \ref reset()
    /// is used, thus only the modified parameters have to be set again.
    /// If the underlying digraph was also modified after the construction
    /// of the class (or the last \ref reset() call), then the \ref reset()
    /// function must be called.
    ///
    /// \param pivot_rule The pivot rule that will be used during the
    /// algorithm. For more information, see \ref PivotRule.
    ///
    /// \return \c INFEASIBLE if no feasible flow exists,
    /// \n \c OPTIMAL if the problem has optimal solution
    /// (i.e. it is feasible and bounded), and the algorithm has found
    /// optimal flow and node potentials (primal and dual solutions),
    /// \n \c UNBOUNDED if the objective function of the problem is
    /// unbounded, i.e. there is a directed cycle having negative total
    /// cost and infinite upper bound.
    ///
    /// \see ProblemType, PivotRule
    /// \see resetParams(), reset()
    pub fn run(&mut self) -> Result<ProblemType> {
        if !self.init() {
            return Ok(ProblemType::INFEASIBLE);
        }
        self.start()
    }

    /// \brief Reset all the parameters that have been given before.
    ///
    /// This function resets all the paramaters that have been given
    /// before using functions \ref lowerMap(), \ref upperMap(),
    /// \ref costMap(), \ref supplyMap(), \ref stSupply(), \ref supplyType().
    ///
    /// It is useful for multiple \ref run() calls. Basically, all the given
    /// parameters are kept for the next \ref run() call, unless
    /// \ref resetParams() or \ref reset() is used.
    /// If the underlying digraph was also modified after the construction
    /// of the class or the last \ref reset() call, then the \ref reset()
    /// function must be used, otherwise \ref resetParams() is sufficient.
    ///
    /// For example,
    /// \code
    ///   NetworkSimplexSimple<ListDigraph> ns(graph);
    ///
    ///   // First run
    ///   ns.lowerMap(lower).upperMap(upper).costMap(cost)
    ///     .supplyMap(sup).run();
    ///
    ///   // Run again with modified cost map (resetParams() is not called,
    ///   // so only the cost map have to be set again)
    ///   cost[e] += 100;
    ///   ns.costMap(cost).run();
    ///
    ///   // Run again from scratch using resetParams()
    ///   // (the lower bounds will be set to zero on all arcs)
    ///   ns.resetParams();
    ///   ns.upperMap(capacity).costMap(cost)
    ///     .supplyMap(sup).run();
    /// \endcode
    ///
    /// \return <tt>(*this)</tt>
    ///
    /// \see reset(), run()
    pub fn resetParams(&mut self) {
        for i in 0..self._node_num {
            self._supply[i] = Fraction::zero();
        }
        for i in 0..self._arc_num {
            self._cost[i] = Fraction::one();
        }
        self._stype = SupplyType::GEQ;
    }

    /// \brief Reset the internal data structures and all the parameters
    /// that have been given before.
    ///
    /// This function resets the internal data structures and all the
    /// paramaters that have been given before using functions \ref lowerMap(),
    /// \ref upperMap(), \ref costMap(), \ref supplyMap(), \ref stSupply(),
    /// \ref supplyType().
    ///
    /// It is useful for multiple \ref run() calls. Basically, all the given
    /// parameters are kept for the next \ref run() call, unless
    /// \ref resetParams() or \ref reset() is used.
    /// If the underlying digraph was also modified after the construction
    /// of the class or the last \ref reset() call, then the \ref reset()
    /// function must be used, otherwise \ref resetParams() is sufficient.
    ///
    /// See \ref resetParams() for examples.
    ///
    /// \return <tt>(*this)</tt>
    ///
    /// \see resetParams(), run()
    pub fn reset(&mut self) {
        // Resize vectors
        self._node_num = self._init_nb_nodes;
        self._arc_num = self._init_nb_arcs;
        let all_node_num = self._node_num + 1;
        let max_arc_num: ArcsType = self._arc_num + 2 * self._node_num;

        self._source.resize(max_arc_num, 0);
        self._target.resize(max_arc_num, 0);

        self._cost.resize(max_arc_num, Fraction::zero());
        self._supply.resize(all_node_num, Fraction::zero());
        self._flow.resize(max_arc_num, Fraction::zero());
        self._pi.resize(all_node_num, Fraction::zero());

        self._parent.resize(all_node_num, None);
        self._pred.resize(all_node_num, None);
        self._forward.resize(all_node_num, false);
        self._thread.resize(all_node_num, 0);
        self._rev_thread.resize(all_node_num, 0);
        self._succ_num.resize(all_node_num, 0);
        self._last_succ.resize(all_node_num, None);
        self._state.resize(max_arc_num, ArcState::STATE_LOWER);


        //_arc_mixing=false;
        if self._arc_mixing && self._node_num > 1 {
            // Store the arcs in a mixed order
            //ArcsType k = std::max(ArcsType(std::sqrt(double(_arc_num))), ArcsType(10));
            let k: ArcsType = (self._arc_num / self._node_num).max(3);
            self.mixingCoeff = k;
            self.subsequence_length = self._arc_num / self.mixingCoeff + 1;
            self.num_big_subsequences = self._arc_num % self.mixingCoeff;
            self.num_total_big_subsequence_numbers = self.subsequence_length * self.num_big_subsequences;

            //#pragma omp parallel for schedule(static)
            for a in 0..self._graph.maxArcId() { //for (Arc a = 0; a <= _graph.maxArcId(); a++) {   // --a <=> _graph.next(a)  , -1 == INVALID 
                let i: ArcsType = self.sequence(self._graph.maxArcId()-a);
                self._source[i] = self._node_id(self._graph.source(a));
                self._target[i] = self._node_id(self._graph.target(a));
            }
        } else {
            // Store the arcs in the original order
            let mut i: ArcsType = 0;
            let mut ax = self._graph.first_arc();
            while let Some(a) = ax {
                self._source[i] = self._node_id(self._graph.source(a));
                self._target[i] = self._node_id(self._graph.target(a));
                //_arc_id[a] = i;

                i += 1;
                ax = self._graph.next_arc(a);
            }
        }

        // Reset parameters
        self.resetParams();
    }

    // Initialize internal data structures
    fn init(&mut self) -> bool {
        if self._node_num == 0 {
            return false;
        }

        // Check the sum of supply values
        self._sum_supply = Cost::zero();
        for i in 0..self._node_num {
            self._sum_supply += &self._supply[i];
        }
        /*if (!((_stype == GEQ && _sum_supply <= 0) ||
        (_stype == LEQ && _sum_supply >= 0))) return false;*/


        // Initialize artifical cost
        let mut ART_COST = Cost::zero();
        for i in 0..self._arc_num {
            if self._cost[i] > ART_COST {
                ART_COST = self._cost[i].clone();
            }
        }
        ART_COST += Fraction::one();
        ART_COST *= self._node_num;

        // Initialize arc maps
        for i in 0..self._arc_num {
            self._flow[i] = Cost::zero(); //by default, the sparse matrix is empty
            self._state[i] = ArcState::STATE_LOWER;
        }

        // Set data for the artificial root node
        self._root = self._node_num;
        self._parent[self._root] = None;
        self._pred[self._root] = None;
        self._thread[self._root] = 0;
        self._rev_thread[0] = self._root;
        self._succ_num[self._root] = self._node_num + 1;
        self._last_succ[self._root] = Some(self._root - 1);
        self._supply[self._root] = -&self._sum_supply;
        self._pi[self._root] = Cost::zero();

        // Add artificial arcs and initialize the spanning tree data structure
        if self._sum_supply.is_zero() {
            // EQ supply constraints
            self._search_arc_num = self._arc_num;
            self._all_arc_num = self._arc_num + self._node_num;

            for (u, e) in (0..self._node_num).zip(self._arc_num..) {
                self._parent[u] = Some(self._root);
                self._pred[u] = Some(e);
                self._thread[u] = u + 1;
                self._rev_thread[u + 1] = u;
                self._succ_num[u] = 1;
                self._last_succ[u] = Some(u);
                self._state[e] = ArcState::STATE_TREE;
                if !self._supply[u].is_negative() {
                    self._forward[u] = true;
                    self._pi[u] = Cost::zero();
                    self._source[e] = u;
                    self._target[e] = self._root;
                    self._flow[e] = self._supply[u].clone();
                    self._cost[e] = Cost::zero();
                } else {
                    self._forward[u] = false;
                    self._pi[u] = ART_COST.clone();
                    self._source[e] = self._root;
                    self._target[e] = u;
                    self._flow[e] = -&self._supply[u];
                    self._cost[e] = ART_COST.clone();
                }
            }
        } else if self._sum_supply.is_positive() {
            // LEQ supply constraints
            self._search_arc_num = self._arc_num + self._node_num;
            let mut f = self._arc_num + self._node_num;
            for (u, e) in (0..self._node_num).zip(self._arc_num..) {
                self._parent[u] = Some(self._root);
                self._thread[u] = u + 1;
                self._rev_thread[u + 1] = u;
                self._succ_num[u] = 1;
                self._last_succ[u] = Some(u);
                if !self._supply[u].is_negative() {
                    self._forward[u] = true;
                    self._pi[u] = Cost::zero();
                    self._pred[u] = Some(e);
                    self._source[e] = u;
                    self._target[e] = self._root;
                    self._flow[e] = self._supply[u].clone();
                    self._cost[e] = Cost::zero();
                    self._state[e] = ArcState::STATE_TREE;
                } else {
                    self._forward[u] = false;
                    self._pi[u] = ART_COST.clone();
                    self._pred[u] = Some(f);
                    self._source[f] = self._root;
                    self._target[f] = u;
                    self._flow[f] = -&self._supply[u];
                    self._cost[f] = ART_COST.clone();
                    self._state[f] = ArcState::STATE_TREE;
                    self._source[e] = u;
                    self._target[e] = self._root;
                    //_flow[e] = 0;  //by default, the sparse matrix is empty
                    self._cost[e] = Cost::zero();
                    self._state[e] = ArcState::STATE_LOWER;
                    f += 1;
                }
            }
            self._all_arc_num = f;
        } else {
            // GEQ supply constraints
            self._search_arc_num = self._arc_num + self._node_num;
            let mut f = self._arc_num + self._node_num;

            for (u, e) in (0..self._node_num).zip(self._arc_num..) {
                self._parent[u] = Some(self._root);
                self._thread[u] = u + 1;
                self._rev_thread[u + 1] = u;
                self._succ_num[u] = 1;
                self._last_succ[u] = Some(u);
                if !self._supply[u].is_positive() {
                    self._forward[u] = false;
                    self._pi[u] = Cost::zero();
                    self._pred[u] = Some(e);
                    self._source[e] = self._root;
                    self._target[e] = u;
                    self._flow[e] = -&self._supply[u];
                    self._cost[e] = Cost::zero();
                    self._state[e] = ArcState::STATE_TREE;
                } else {
                    self._forward[u] = true;
                    self._pi[u] = -ART_COST.clone();
                    self._pred[u] = Some(f);
                    self._source[f] = u;
                    self._target[f] = self._root;
                    self._flow[f] = self._supply[u].clone();
                    self._state[f] = ArcState::STATE_TREE;
                    self._cost[f] = ART_COST.clone();
                    self._source[e] = self._root;
                    self._target[e] = u;
                    //_flow[e] = 0; //by default, the sparse matrix is empty
                    self._cost[e] = Cost::zero();
                    self._state[e] = ArcState::STATE_LOWER;
                    f += 1;
                }
            }
            self._all_arc_num = f;
        }

        return true;
    }

    // Find the join node
	fn findJoinNode(&mut self) -> Result<()> {
        let mut u = self._source[self.in_arc];
        let mut v = self._target[self.in_arc];
        while u != v {
            if self._succ_num[u] < self._succ_num[v] {
                u = self._parent[u].u()?;
            } else {
                v = self._parent[v].u()?;
            }
        }
        self.join = u;
        Ok(())
    }

        // Find the leaving arc of the cycle and returns true if the
        // leaving arc is not the same as the entering arc
    fn findLeavingArc(&mut self) -> Result<bool> {
        // Initialize first and second nodes according to the direction
        // of the cycle
        if self._state[self.in_arc] == ArcState::STATE_LOWER {
            self.first = self._source[self.in_arc];
            self.second = self._target[self.in_arc];
        } else {
            self.first = self._target[self.in_arc];
            self.second = self._source[self.in_arc];
        }
        self.delta = Fraction::infinity();
        let mut result = 0;
        let d: Value;
        let e: ArcsType;

        // Search the cycle along the path form the first node to the root
        let mut u = self.first;
        while u != self.join {
            let e = self._pred[u].u()?;
            let i = Cost::infinity();
            let d = if self._forward[u] { &self._flow[e] } else { &i };
            if d < &self.delta {
                self.delta = d.clone();
                self.u_out = u;
                result = 1;
            }

            u = self._parent[u].u()?;
        }
        // Search the cycle along the path form the second node to the root
        let mut u = self.second;
        while u != self.join {
            let e = self._pred[u].u()?;
            let i = Cost::infinity();
            let d = if self._forward[u] { &i } else { &self._flow[e] };
            if d <= &self.delta {
                self.delta = d.clone();
                self.u_out = u;
                result = 2;
            }
            u = self._parent[u].u()?;
        }

        if result == 1 {
            self.u_in = self.first;
            self.v_in = Some(self.second);
        } else {
            self.u_in = self.second;
            self.v_in = Some(self.first);
        }
        return Ok(result != 0);
    }


    // Change _flow and _state vectors
	fn changeFlow(&mut self, change: bool) -> Result<()> {
        // Augment along the cycle
        if self.delta.is_positive() {
            let val = self._state[self.in_arc].clone() * &self.delta;
            self._flow[self.in_arc] += &val;
            
            let mut u = self._source[self.in_arc];
            while u != self.join {
                if self._forward[u] {
                    self._flow[self._pred[u].u()?] -= &val;
                } else { 
                    self._flow[self._pred[u].u()?] += &val;
                };

                u = self._parent[u].u()?;
            }

            let mut u = self._target[self.in_arc];
            while u != self.join {
                if self._forward[u] {
                    self._flow[self._pred[u].u()?] += &val;
                } else { 
                    self._flow[self._pred[u].u()?] -= &val;
                }

                u = self._parent[u].u()?;
            }
        }
        // Update the state of the entering and leaving arcs
        if change {
            self._state[self.in_arc] = ArcState::STATE_TREE;
            self._state[self._pred[self.u_out].u()?] =
                if self._flow[self._pred[self.u_out].u()?].is_zero() { ArcState::STATE_LOWER } else { ArcState::STATE_UPPER };
        } else {
            self._state[self.in_arc] = -self._state[self.in_arc];
        }

        Ok(())
    }

    // Update the tree structure
    fn updateTreeStructure(&mut self) -> Result<()> {
        let old_rev_thread = self._rev_thread[self.u_out];
        let old_succ_num = self._succ_num[self.u_out];
        let old_last_succ = self._last_succ[self.u_out];
        self.v_out = self._parent[self.u_out].u()?;

        // Check if u_in and u_out coincide
        if self.u_in == self.u_out {
            // Update _parent, _pred, _pred_dir
            self._parent[self.u_in] = self.v_in;
            self._pred[self.u_in] = Some(self.in_arc);
            self._forward[self.u_in] = (self.u_in == self._source[self.in_arc]);

            // Update _thread and _rev_thread
            if self._thread[self.v_in.u()?] != self.u_out {
                let mut after = self._thread[old_last_succ.u()?];
                self._thread[old_rev_thread] = after;
                self._rev_thread[after] = old_rev_thread;
                after = self._thread[self.v_in.u()?];
                self._thread[self.v_in.u()?] = self.u_out;
                self._rev_thread[self.u_out] = self.v_in.u()?;
                self._thread[old_last_succ.u()?] = after;
                self._rev_thread[after] = old_last_succ.u()?;
            }
        } else {
            // Handle the case when old_rev_thread equals to v_in
            // (it also means that join and v_out coincide)
            let thread_continue = if old_rev_thread == self.v_in.u()? {self._thread[old_last_succ.u()?] } else {self._thread[self.v_in.u()?] };

            // Update _thread and _parent along the stem nodes (i.e. the nodes
            // between u_in and u_out, whose parent have to be changed)
            let mut stem = self.u_in;              // the current stem node
            let mut par_stem = self.v_in;          // the new parent of stem
            let mut next_stem;                // the next stem node
            let mut last = self._last_succ[self.u_in];  // the last successor of stem
            let mut before = self._thread[last.u()?];
            let mut after = self._thread[last.u()?];
            self._thread[self.v_in.u()?] = self.u_in;
            self._dirty_revs.clear();
            self._dirty_revs.push(self.v_in.u()?);
            while stem != self.u_out {
                // Insert the next stem node into the thread list
                next_stem = self._parent[stem].u()?;
                self._thread[last.u()?] = next_stem;
                self._dirty_revs.push(last.u()?);

                // Remove the subtree of stem from the thread list
                before = self._rev_thread[stem];
                self._thread[before] = after;
                self._rev_thread[after] = before;

                // Change the parent node and shift stem nodes
                self._parent[stem] = Some(par_stem.u()?);
                par_stem = Some(stem);
                stem = next_stem;

                // Update last and after
                last = if self._last_succ[stem] == self._last_succ[par_stem.u()?] {Some(self._rev_thread[par_stem.u()?]) } else {self._last_succ[stem]};
                after = self._thread[last.u()?];
            }
            self._parent[self.u_out] = par_stem;
            self._thread[self.last] = thread_continue;
            self._rev_thread[thread_continue] = last.u()?;
            self._last_succ[self.u_out] = last;

            // Remove the subtree of u_out from the thread list except for
            // the case when old_rev_thread equals to v_in
            if old_rev_thread != self.v_in.u()? {
                self._thread[old_rev_thread] = after;
                self._rev_thread[after] = old_rev_thread;
            }

            // Update _rev_thread using the new _thread values
            for u in &self._dirty_revs {
                self._rev_thread[self._thread[*u]] = *u;
            }

            // Update _pred, _pred_dir, _last_succ and _succ_num for the
            // stem nodes from u_out to u_in
            let mut tmp_sc = 0;
            let tmp_ls = self._last_succ[self.u_out];

            let mut u = self.u_out;
            while u != self.u_in {
                let p = self._parent[u].u()?;

                self._pred[u] = self._pred[p];
                self._forward[u] = !self._forward[p];
                tmp_sc += self._succ_num[u] - self._succ_num[p];
                self._succ_num[u] = tmp_sc;
                self._last_succ[p] = tmp_ls;

                u = p;
            }
            self._pred[self.u_in] = Some(self.in_arc);
            self._forward[self.u_in] = (self.u_in == self._source[self.in_arc]);
            self._succ_num[self.u_in] = old_succ_num;
        }

        // Update _last_succ from v_in towards the root
        let up_limit_out = if self._last_succ[self.join] == self.v_in { Some(self.join) } else { None };
        let last_succ_out = self._last_succ[self.u_out];

        let mut u = self.v_in;
        while u.is_some() && self._last_succ[u.u()?] == self.v_in {
            self._last_succ[u.u()?] = last_succ_out;

            u = self._parent[u.u()?];
        }

        // Update _last_succ from v_out towards the root
        if self.join != old_rev_thread && self.v_in.u()? != old_rev_thread {

            let mut u = self.v_out;
            while u != up_limit_out.u()? && self._last_succ[u] == old_last_succ {
                self._last_succ[u] = Some(old_rev_thread);

                u = self._parent[u].u()?
            }
        } else if last_succ_out != old_last_succ {

            let mut u = self.v_out;
            while Some(u) != up_limit_out && self._last_succ[u] == old_last_succ {
                self._last_succ[u] = last_succ_out;

                u = self._parent[u].u()?;
            }
        }

        // Update _succ_num from v_in to join
        let mut u = self.v_in;
        while u != Some(self.join) {
            self._succ_num[u.u()?] += old_succ_num;

            u = self._parent[u.u()?];
        }
        // Update _succ_num from v_out to join
        let mut u = self.v_out;
        while u != self.join {
            self._succ_num[u] -= old_succ_num;

            u = self._parent[u].u()?;
        }

        Ok(())
    }

    fn updatePotential(&mut self) -> Result<()> {
        let x = &self._pi[self.v_in.u()?] - &self._pi[self.u_in];
        let sigma = if (self._forward[self.u_in]) {
            x - &self._cost[self.in_arc] 
        } else {
            x + &self._cost[self.in_arc]
        };
        let end = self._thread[self._last_succ[self.u_in].u()?];
        let mut u = self.u_in;
        while u != end {
            self._pi[u] += &sigma;

            u = self._thread[u]
        }
        Ok(())
    }

    // Heuristic initial pivots
    fn initialPivots(&mut self) -> Result<bool> {
        let mut curr = Cost::zero();
        let mut total = Cost::zero();
        let mut supply_nodes = vec![];
        let mut demand_nodes = vec![];
        
        let mut ux = self._graph.first_node();
        while let Some(u) = ux {
            curr = self._supply[self._node_id(u)].clone();
            if curr.is_positive() {
                total += curr;
                supply_nodes.push(u);
            } else if curr.is_negative() {
                demand_nodes.push(u);
            }

            ux = self._graph.next_node(u);
        }
        if self._sum_supply.is_positive() {
            total -= &self._sum_supply;
        }
        if !total.is_positive() {
            return Ok(true);
        }

        let mut arc_vector;
        if !self._sum_supply.is_negative() {
            if supply_nodes.len() == 1 && demand_nodes.len() == 1 {
                let mut arc_vector2 = vec![];
                // Perform a reverse graph search from the sink to the source
                //typename GR::template NodeMap<bool> reached(_graph, false);
                let mut reached = vec![false; self._node_num];
                let s = supply_nodes[0];
                let t = demand_nodes[0];
                let mut stack = vec![];
                reached[t] = true;
                stack.push(t);
                while let Some(v) = stack.pop() {
                    let u: Node;
                    if v == s {
                        break;
                    }
                    let mut ai = self._graph.firstIn(&v);
                    while let Some(a) = ai {
                        let u = self._graph.source(a);
                        if reached[u] {
                            continue;
                        }
                        let j = self.getArcID(&a);
                        arc_vector2.push(j);
                        reached[u] = true;
                        stack.push(u);

                        ai = self._graph.nextIn(a);
                    }
                }
                arc_vector = arc_vector2.into_iter().filter_map(|e| e).collect::<Vec<_>>();
            } else {
                let mut arc_vector2 = Vec::with_capacity(supply_nodes.len());
                // Find the min. cost incomming arc for each demand node
                // #pragma omp parallel for
                for i in 0..demand_nodes.len() {
                    let v = demand_nodes[i];
                    let mut min_cost = &Cost::infinity();
                    let mut min_arc: Arc = usize::MAX;
                    let mut ax = self._graph.firstIn(&v);
                    while let Some(a) = ax {
                        let c = &self._cost[self.getArcID(&a).u()?];
                        if c < min_cost {
                            min_cost = c;
                            min_arc = a;
                        }

                        ax = self._graph.nextIn(a);
                    }
                    arc_vector2.push(self.getArcID(&min_arc));
                }
                arc_vector = arc_vector2.into_iter().filter_map(|e| e).collect();
            }
        } else {
            let mut arc_vector2 = Vec::with_capacity(supply_nodes.len());
            // Find the min. cost outgoing arc for each supply node
            // #pragma omp parallel for
            for i in 0..supply_nodes.len() {
                let u = supply_nodes[i];
                let mut min_cost = &Cost::infinity();
                let mut min_arc = usize::MAX;
                let mut ax = self._graph.firstOut(&u);
                while let Some(a) = ax {
                    let c = &self._cost[self.getArcID(&a).u()?];
                    if c < min_cost {
                        min_cost = c;
                        min_arc = a;
                    }

                    ax = self._graph.nextOut(&a);
                }
                arc_vector2.push(self.getArcID(&min_arc));
            }
            arc_vector = arc_vector2.into_iter().filter_map(|e| e).collect();
        }

        // Perform heuristic initial pivots
        for i in 0..arc_vector.len() {
            let in_arc = arc_vector[i];

            if !(self._state[self.in_arc] * (&self._cost[self.in_arc] + &self._pi[self._source[self.in_arc]] -
                &self._pi[self._target[self.in_arc]])).is_negative() {
                continue 
            }
            self.findJoinNode();
            let change = self.findLeavingArc()?;
            if self.delta.is_infinite() {
                return Ok(false);
            }
            self.changeFlow(change)?;
            if change {
                self.updateTreeStructure()?;
                self.updatePotential()?;
            }
        }
        return Ok(true);
    }

    // Execute the algorithm
    fn start(&mut self) -> Result<ProblemType> {
        let mut pivot = BlockSearchPivotRule::new(self);
        // PivotRuleImpl pivot(*this);

        // Perform heuristic initial pivots
        if !self.initialPivots()? {
            return Ok(ProblemType::UNBOUNDED);
        }

        let mut iter_number = 0;
        // Execute the Network Simplex algorithm
        while pivot.findEnteringArc() {
            if (iter_number <= self.max_iter && self.max_iter > 0) || self.max_iter <= 0 {
                iter_number += 1;
                self.findJoinNode();
                let change = self.findLeavingArc()?;
                if self.delta.is_infinite() {
                    return Ok(ProblemType::UNBOUNDED);
                }
                self.changeFlow(change)?;
                if change {
                    self.updateTreeStructure()?;
                    self.updatePotential()?;
                }
            } else {
                break;
            }
        }

        // Check feasibility
        for e in self._search_arc_num..self._all_arc_num {
            if !self._flow[e].is_zero() {
                return Ok(ProblemType::INFEASIBLE);
            }
        }

        // Shift potentials to meet the requirements of the GEQ/LEQ type
        // optimality conditions
        if self._sum_supply.is_zero() {
            if self._stype == SupplyType::GEQ {
                let mut max_pot = Cost::neg_infinity();
                for i in 0..self._node_num {
                    if self._pi[i] > max_pot {
                        max_pot = self._pi[i].clone();
                    }
                }
                if max_pot.is_positive() {
                    for i in 0..self._node_num {
                        self._pi[i] -= &max_pot;
                    }
                }
            } else {
                let mut min_pot = Cost::infinity();
                for i in 0..self._node_num {
                    if self._pi[i] < min_pot {
                        min_pot = self._pi[i].clone();
                    }
                }
                if min_pot.is_negative() {
                    for i in 0..self._node_num {
                        self._pi[i] -= &min_pot;
                    }
                }
            }
        }

        return Ok(ProblemType::OPTIMAL);
    }
}

/// \brief Problem type constants for the \c run() function.
///
/// Enum type containing the problem type constants that can be
/// returned by the \ref run() function of the algorithm.
enum ProblemType {
    /// The problem has no feasible solution (flow).
    INFEASIBLE,
    /// The problem has optimal solution (i.e. it is feasible and
    /// bounded), and the algorithm has found optimal flow and node
    /// potentials (primal and dual solutions).
    OPTIMAL,
    /// The objective function of the problem is unbounded, i.e.
    /// there is a directed cycle having negative total cost and
    /// infinite upper bound.
    UNBOUNDED
}

/// \brief Constants for selecting the type of the supply constraints.
///
/// Enum type containing constants for selecting the supply type,
/// i.e. the direction of the inequalities in the supply/demand
/// constraints of the \ref min_cost_flow "minimum cost flow problem".
///
/// The default supply type is \c GEQ, the \c LEQ type can be
/// selected using \ref supplyType().
/// The equality form is a special case of both supply types.
#[derive(PartialEq)]
enum SupplyType {
    /// This option means that there are <em>"greater or equal"</em>
    /// supply/demand constraints in the definition of the problem.
    GEQ,
    /// This option means that there are <em>"less or equal"</em>
    /// supply/demand constraints in the definition of the problem.
    LEQ
}

// State constants for arcs
#[derive(Clone,Copy,PartialEq)]
enum ArcState {
    STATE_UPPER,
    STATE_TREE,
    STATE_LOWER,
}

impl Mul<Fraction> for ArcState {
    type Output = Fraction;

    fn mul(self, rhs: Fraction) -> Self::Output {
        match self {
            ArcState::STATE_UPPER => -rhs,
            ArcState::STATE_TREE => Fraction::zero(),
            ArcState::STATE_LOWER => rhs,
        }
    }
}

impl Mul<&Fraction> for ArcState {
    type Output = Fraction;

    fn mul(self, rhs: &Fraction) -> Self::Output {
        match self {
            ArcState::STATE_UPPER => -rhs,
            ArcState::STATE_TREE => Fraction::zero(),
            ArcState::STATE_LOWER => rhs.clone(),
        }
    }
}

impl Neg for ArcState {
    type Output = ArcState;

    fn neg(self) -> Self::Output {
        match self {
            ArcState::STATE_UPPER => ArcState::STATE_LOWER,
            ArcState::STATE_TREE => ArcState::STATE_TREE,
            ArcState::STATE_LOWER => ArcState::STATE_UPPER,
        }
    }
}

// Implementation of the Block Search pivot rule
struct BlockSearchPivotRule<'a> {
    // References to the NetworkSimplexSimple class
    _source: &'a IntVector,
    _target: &'a IntVector,
    _cost: &'a CostVector,
    _state: &'a StateVector,
    _pi: &'a CostVector,
    _in_arc: &'a mut ArcsType,
    _search_arc_num: ArcsType,

    // Pivot rule data
    _block_size: ArcsType,
    _next_arc: ArcsType,
    _ns: &'a NetworkSimplexSimple<'a>,
}

const BLOCK_SIZE_FACTOR: usize = 1;
const MIN_BLOCK_SIZE: ArcsType = 10;

impl <'a> BlockSearchPivotRule<'a> {
    pub fn new(ns: &'a mut NetworkSimplexSimple) -> Self {
        Self {
            _source: &ns._source,
            _target: &ns._target,
            _cost: &ns._cost,
            _state: &ns._state,
            _pi: &ns._pi,
            _in_arc: &mut ns.in_arc,
            _search_arc_num: ns._search_arc_num,
            _next_arc: 0,
            _ns: ns,
            _block_size: (BLOCK_SIZE_FACTOR * ns._search_arc_num.sqrt()).max(MIN_BLOCK_SIZE)
        }
    }

    fn omp_get_max_threads(&self) -> usize {
        return 1;
    }

    fn omp_get_thread_num(&self) -> usize {
        return 0;
    }

    // Find next entering arc
    pub fn findEnteringArc(&mut self) -> bool {
        let mut min_val = Cost::zero();

        let N: ArcsType = self.omp_get_max_threads();
        let mut minArray = vec![Cost::zero(); N];
        let mut arcId = vec![ArcsType::zero(); N];
        let bs: ArcsType = self._block_size.div_ceil(N);

        for i in (0..self._search_arc_num).step_by(self._block_size) {
            let mut e: ArcsType = ArcsType::MAX;
            let j: ArcsType;
            // #pragma omp parallel
            {
                let t = self.omp_get_thread_num();

                // #pragma omp for schedule(static, bs) lastprivate(e)
                for j in 0..(i + self._block_size).min(self._search_arc_num) - i {
                    e = (self._next_arc + i + j); 
                    if (e >= self._search_arc_num) {
                        e -= self._search_arc_num
                    };
                    let c = self._state[e] * (&self._cost[e] + &self._pi[self._source[e]] - &self._pi[self._target[e]]);
                    if c < minArray[t] {
                        minArray[t] = c;
                        arcId[t] = e;
                    }
                }
            }
            for j in 0..N {
                if (minArray[j] < min_val) {
                    min_val = minArray[j].clone();
                    *self._in_arc = arcId[j];
                }
            }

            let a = self._pi[self._source[*self._in_arc]].abs().max(self._pi[self._target[*self._in_arc]].abs()).max(self._cost[*self._in_arc].abs());

            if min_val.is_negative() {
                self._next_arc = e;
                return true;
            }
        }

        let a = self._pi[self._source[*self._in_arc]].abs().max(self._pi[self._target[*self._in_arc]].abs()).max(self._cost[*self._in_arc].abs());
        
        !min_val.is_negative()
    }
}

pub struct FullBipartiteDigraphBase {  
      _node_num: usize,
      _arc_num: usize,
      _n1: usize,
      _n2: usize,
}
      
impl FullBipartiteDigraphBase {
    pub fn new(n1: usize, n2: usize) -> Self {
        Self {
            _node_num: n1 + n2,
            _arc_num: n1 * n2,
            _n1: n1,
            _n2: n2,
        }
    }

    pub fn index(node: &Node) -> usize {
        return *node;
    }

    //Node operator()(int ix) const { return Node(ix); }

    pub fn arc(&self, s: &Node, t: &Node) -> Option<Arc> {
        if s < &self._n1 && t >= &self._n1 {
            return Some(s * self._n2 + (t - self._n1));
        } else {
            return None;
        }
    }

    pub fn nodeNum(&self) -> usize { 
        return self._node_num; 
    }
    
    pub fn arcNum(&self) -> usize {
        return self._arc_num;
    }

    pub fn maxNodeId(&self) -> usize { 
        return self._node_num - 1; 
    }

    pub fn maxArcId(&self) -> usize {
        return self._arc_num - 1; 
    }

    pub fn source(&self, arc: Arc) -> usize { 
        return arc / self._n2; 
    }

    pub fn target(&self, arc: Arc) -> usize {
        return (arc % self._n2) + self._n1; 
    }

    pub fn id_node(node: Node) -> usize { 
        return node; 
    }

    pub fn id_arc(arc: &Arc) -> usize { 
        return *arc; 
    }

    pub fn nodeFromId(id: usize) -> Node {
        return id;
    }

    pub fn arcFromId(id: usize) -> Arc { 
        return id;
    }

    pub fn findArc(&self, s: &Node, t: &Node, prev: Option<Arc>) -> Option<Arc> {
        match prev {
            Some(_) => None,
            None => self.arc(s, t),
        }
    }

    pub fn first_node(&self) -> Option<Node> {
        if self._node_num < 0 {
            Some(self._node_num - 1)
        } else {
            None
        }
    }

    pub fn next_node(&self, node: Node) -> Option<Node> {
        if node > 0 {
            Some(node - 1)
        } else {
            None
        }
    }

    pub fn first_arc(&self) -> Option<Arc> {
        if self._arc_num >= 1 {
            Some(self._arc_num - 1)
        } else {
            None
        }
    }

    pub fn next_arc(&self, arc: Arc) -> Option<Arc> {
        if arc > 0 {
            Some(arc - 1)
        } else {
            None
        }
    }

    pub fn firstOut(&self, node: &Node) -> Option<Arc> {
        if node >= &self._n1 {
            None
        } else {
            Some((node + 1) * self._n2 - 1)
        }
    }

    pub fn nextOut(&self, arc: &Arc) -> Option<Arc> {
        if arc % self._n2 == 0 {
            None
        } else {
            Some(arc - 1)
        }
    }

    pub fn firstIn(&self, node: &Node) -> Option<Arc> {
        if node < &self._n1 {
            return None;
        } else {
            return Some(self._arc_num + node - self._node_num);
        }
    }

    pub fn nextIn(&self, arc: Arc) -> Option<Arc> {
        if arc < self._n2 {
            None
        } else {
            Some(arc - self._n2)
        }
    }
}

trait X<T> {
    fn u(self) -> Result<T>;
}

impl <T> X<T> for Option<T> {

    /**
     * Returns the value or gives a useless error message.
     */
    fn u(self) -> Result<T> {
        self.ok_or_else(|| anyhow!("Error"))
    }
}