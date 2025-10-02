use ebi_arithmetic::{Fraction, Zero};
use std::collections::{HashMap, VecDeque};
/// A transition in a stochastic non-deterministic finite automaton
#[derive(Clone, Debug)]
pub struct Transition {
    pub target: usize,
    pub label: String,
    pub probability: Fraction,
}

/// A state in a stochastic non-deterministic finite automaton
#[derive(Clone, Debug)]
pub struct State {
    pub transitions: Vec<Transition>,
    /// Probability of terminating in this state. This could be implied by missing
    /// probability mass on outgoing edges, but we keep it explicit because we
    /// often redirect the full final probability to a sink state.
    pub p_final: Fraction,
}

/// The semantics are: starting in the `initial` state we either terminate with
/// probability `p_final` or follow one of the outgoing transitions chosen
/// according to their probabilities and move to the target state.
#[derive(Clone, Debug)]
pub struct StochasticNondeterministicFiniteAutomaton {
    pub states: Vec<State>,
    pub initial: usize,
}

impl StochasticNondeterministicFiniteAutomaton {
    /// Create an empty SNFA with a single initial / final state
    pub fn new() -> Self {
        Self {
            states: vec![State {
                transitions: vec![],
                p_final: Fraction::from((1, 1)),
            }],
            initial: 0,
        }
    }

    /// Ensures that a state with index `idx` exists, extending the vector if necessary.
    fn ensure_state(&mut self, idx: usize) {
        while self.states.len() <= idx {
            self.states.push(State { transitions: vec![], p_final: Fraction::from((0, 1)) });
        }
    }

    /// Adds a transition. The caller must make sure that outgoing probabilities of each state sum up to <= 1.
    pub fn add_transition(&mut self, source: usize, label: String, target: usize, probability: Fraction) {
        self.ensure_state(source);
        self.ensure_state(target);
        self.states[source].transitions.push(Transition { target, label, probability });
    }

    /// Sets the final-probability of a state.
    pub fn set_final_probability(&mut self, state: usize, p_final: Fraction) {
        self.ensure_state(state);
        self.states[state].p_final = p_final;
    }

    /// Returns the number of states.
    pub fn len(&self) -> usize { self.states.len() }
    pub fn is_empty(&self) -> bool { self.states.is_empty() }

    /// Removes all tau-transitions (those whose label is the empty string "")
    /// from the automaton while preserving its stochastic behaviour.
    ///
    /// The implementation follows Mohri's two–phase epsilon-removal scheme
    /// adapted to the probabilistic semiring.
    pub fn remove_tau_transitions(&mut self) {
        let n = self.len();
        if n == 0 {
            return;
        }

        let one = Fraction::from((1, 1));
        let zero = Fraction::from((0, 1));

        // Build tau-only adjacency with weights
        let mut tau_adj: Vec<Vec<(usize, Fraction)>> = vec![vec![]; n];
        for p in 0..n {
            for t in &self.states[p].transitions {
                if t.label.is_empty() {
                    tau_adj[p].push((t.target, t.probability.clone()));
                }
            }
        }

        // Compute tau-closure matrix d[p,q]
        // Initially d = I (identity)
        let mut d: Vec<Vec<Fraction>> = vec![vec![zero.clone(); n]; n];
        // Track rows that already contain non-zero entries -> lets us skip all-zero rows later
        let mut row_nonzero: Vec<bool> = vec![false; n];
        for i in 0..n {
            d[i][i] = one.clone();
        }

        // Compute SCCs of tau-graph and their reverse topological order
        let sccs = compute_scc(n, &tau_adj);
        // Create map state -> component id
        let mut comp_of = vec![0usize; n];
        for (cid, comp) in sccs.iter().enumerate() {
            for &v in comp {
                comp_of[v] = cid;
            }
        }

        // Build DAG edges from each component to its successor components
        let mut dag_succ: Vec<Vec<usize>> = vec![vec![]; sccs.len()];
        for u in 0..n {
            for &(v, _) in &tau_adj[u] {
                let cu = comp_of[u];
                let cv = comp_of[v];
                if cu != cv && !dag_succ[cu].contains(&cv) {
                    dag_succ[cu].push(cv);
                }
            }
        }

        // Obtain reverse topological order via DFS on DAG
        let mut visited = vec![false; sccs.len()];
        let mut rev_topo: Vec<usize> = Vec::new();
        fn dfs(u: usize, dag: &Vec<Vec<usize>>, vis: &mut Vec<bool>, out: &mut Vec<usize>) {
            if vis[u] { return; }
            vis[u] = true;
            for &v in &dag[u] {
                dfs(v, dag, vis, out);
            }
            out.push(u);
        }
        for cid in 0..sccs.len() {
            dfs(cid, &dag_succ, &mut visited, &mut rev_topo);
        }

        // Process SCCs in reverse topological order
        for &cid in &rev_topo {
            let comp = &sccs[cid];
            let m = comp.len();
            if m == 1 && tau_adj[comp[0]].is_empty() {
                // single vertex with no self-loop -> nothing to invert, d already has identity
            } else {
                // Build (I-W) for this component
                let mut mat: Vec<Vec<Fraction>> = vec![vec![zero.clone(); m]; m];
                for (i_idx, &i) in comp.iter().enumerate() {
                    mat[i_idx][i_idx] = one.clone();
                    for &(j, ref w) in &tau_adj[i] {
                        if comp_of[j] == cid {
                            let j_idx = comp.iter().position(|&x| x == j).unwrap();
                            mat[i_idx][j_idx] = &mat[i_idx][j_idx] - &w.clone();
                        }
                    }
                }
                let d_local = invert_matrix(mat).expect("tau-removal: singular (I-W) in SCC");
                // store intra-component closure
                for (i_idx, &i) in comp.iter().enumerate() {
                    for (j_idx, &j) in comp.iter().enumerate() {
                        d[i][j] = d_local[i_idx][j_idx].clone();
                    }
                    row_nonzero[i] = true;
                }
            }

            // Propagate to successor components (inter-SCC tau-edges)
            for &q in comp {
                for &(r, ref w_qr) in &tau_adj[q] {
                    if comp_of[r] == cid { continue; }
                    // For every s with d[r][s] > 0 (s already processed)
                    for p in 0..n {
                        if !row_nonzero[p] { continue; }
                        if d[p][q].is_zero() { continue; }
                        let prefix = &d[p][q] * &w_qr.clone();
                        if prefix.is_zero() { continue; }
                        
                        for s in 0..n {
                            let d_rs = d[r][s].clone();
                            if d_rs.is_zero() { continue; }
                            let add = &prefix * &d_rs;
                            if add.is_zero() { continue; }

                            d[p][s] += add;
                        }
                    }
                }
            }
        }

        // Transform graph
        let mut new_transitions: Vec<Vec<Transition>> = vec![vec![]; n];
        let mut new_p_final: Vec<Fraction> = vec![zero.clone(); n];

        for q in 0..n {
            for t in &self.states[q].transitions {
                if !t.label.is_empty() {
                    let tprob = t.probability.clone();
                    for p in 0..n {
                        if d[p][q].is_zero() { continue; }
                        let prob = &d[p][q] * &tprob;
                        new_transitions[p].push(Transition { target: t.target, label: t.label.clone(), probability: prob });
                    }
                }
            }
            if !self.states[q].p_final.is_zero() {
                for p in 0..n {
                    if d[p][q].is_zero() { continue; }
                    new_p_final[p] = &new_p_final[p] + &(&d[p][q] * &self.states[q].p_final.clone());
                }
            }
        }

        // Merge parallel edges
        for p in 0..n {

            let mut map: HashMap<(usize, String), Fraction> = HashMap::new();
            for tr in new_transitions[p].drain(..) {
                *map.entry((tr.target, tr.label.clone()))
                    .or_insert_with(Fraction::zero) += tr.probability.clone();
            }
            new_transitions[p] = map.into_iter()
                .map(|((tgt, lbl), prob)| Transition { target: tgt, label: lbl, probability: prob })
                .collect();
        }

        // Write back
        for p in 0..n {
            self.states[p].transitions = new_transitions[p].clone();
            self.states[p].p_final = new_p_final[p].clone();
        }

        // Trim unreachable states (reachable over visible edges)
        {

            let mut reachable = vec![false; self.states.len()];
            let mut queue = VecDeque::new();
            queue.push_back(self.initial);
            while let Some(u) = queue.pop_front() {
                if reachable[u] { continue; }
                reachable[u] = true;
                for tr in &self.states[u].transitions {
                    if tr.label.is_empty() { continue; }
                    queue.push_back(tr.target);
                }
            }
            if reachable.iter().any(|&r| !r) {
                
                let mut map = vec![None; self.states.len()];
                let mut new_states: Vec<State> = Vec::new();
                for (old_idx, st) in self.states.iter().enumerate() {
                    if reachable[old_idx] {
                        let new_idx = new_states.len();
                        map[old_idx] = Some(new_idx);
                        new_states.push(State { transitions: Vec::new(), p_final: st.p_final.clone() });
                    }
                }
                for (old_idx, st) in self.states.iter().enumerate() {
                    if let Some(new_src) = map[old_idx] {
                        for tr in &st.transitions {
                            if let Some(new_tgt) = map[tr.target] {
                                new_states[new_src].transitions.push(Transition { target: new_tgt, label: tr.label.clone(), probability: tr.probability.clone() });
                            }
                        }
                    }
                }
                self.states = new_states;
                self.initial = map[self.initial].unwrap();
            }
        }

        // Check row sums
        for (i, st) in self.states.iter().enumerate() {
            let mut row = st.p_final.clone();
            for tr in &st.transitions { row += tr.probability.clone(); }
            debug_assert!(row == Fraction::from((1,1)),
                          "row sum ≠ 1 after tau-removal in state {}", i);
        }
    }
}

/// Inverts a square matrix of Fractions using Gauss-Jordan elimination.
/// Returns None if the matrix is singular.
fn invert_matrix(mut a: Vec<Vec<Fraction>>) -> Option<Vec<Vec<Fraction>>> {
    let n = a.len();
    let one = Fraction::from((1, 1));
    let zero = Fraction::from((0, 1));

    // Attach the identity matrix to the right side of the current matrix
    for i in 0..n {
        a[i].extend((0..n).map(|j| if i == j { one.clone() } else { zero.clone() }));
    }

    // Gauss-Jordan elimination
    for i in 0..n {
        // Find pivot
        let mut pivot = i;
        while pivot < n && a[pivot][i].is_zero() {
            pivot += 1;
        }
        if pivot == n {
            return None; // singular
        }
        if pivot != i {
            a.swap(i, pivot);
        }

        // Scale pivot row to make pivot = 1
        let pivot_val = a[i][i].clone();
        for j in 0..2 * n {
            a[i][j] = &a[i][j] / &pivot_val;
        }

        // Eliminate other rows
        for r in 0..n {
            if r == i {
                continue;
            }
            if a[r][i].is_zero() {
                continue;
            }
            let factor = a[r][i].clone();
            for c in 0..2 * n {
                let val = &a[r][c] - &(&factor * &a[i][c]);
                a[r][c] = val;
            }
        }
    }

    // Extract inverse matrix
    let inv: Vec<Vec<Fraction>> = a
        .into_iter()
        .map(|row| row.into_iter().skip(n).take(n).collect())
        .collect();
    Some(inv)
}

/// Kosaraju algorithm to compute SCCs of a directed weighted graph (adjacency list ignores weights).
fn compute_scc(n: usize, adj: &Vec<Vec<(usize, Fraction)>>) -> Vec<Vec<usize>> {
    // 1st pass: order by finish time using DFS on original graph
    let mut visited = vec![false; n];
    let mut order: Vec<usize> = Vec::with_capacity(n);
    fn dfs1(u: usize, adj: &Vec<Vec<(usize, Fraction)>>, vis: &mut Vec<bool>, order: &mut Vec<usize>) {
        if vis[u] { return; }
        vis[u] = true;
        for &(v, _) in &adj[u] {
            dfs1(v, adj, vis, order);
        }
        order.push(u);
    }
    for v in 0..n {
        dfs1(v, adj, &mut visited, &mut order);
    }

    // Build reverse graph
    let mut radj: Vec<Vec<usize>> = vec![vec![]; n];
    for u in 0..n {
        for &(v, _) in &adj[u] {
            radj[v].push(u);
        }
    }

    // 2nd pass: DFS on reverse graph in reverse finish order
    let mut comp_id = vec![None; n];
    let mut comps: Vec<Vec<usize>> = Vec::new();
    fn dfs2(u: usize, radj: &Vec<Vec<usize>>, comp: &mut Vec<usize>, comp_id: &mut Vec<Option<usize>>, cid: usize) {
        if comp_id[u].is_some() { return; }
        comp_id[u] = Some(cid);
        comp.push(u);
        for &v in &radj[u] {
            dfs2(v, radj, comp, comp_id, cid);
        }
    }
    let mut cid = 0;
    while let Some(u) = order.pop() {
        if comp_id[u].is_none() {
            comps.push(Vec::new());
            dfs2(u, &radj, comps.last_mut().unwrap(), &mut comp_id, cid);
            cid += 1;
        }
    }
    comps
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::collections::HashMap;

    /// frac!(n,d)  ->  Fraction::from((n,d))
    macro_rules! frac { ($n:expr , $d:expr) => { Fraction::from(($n as u32, $d as u32)) }; }

    /// Collapse parallel visible edges into (src,label,dst) -> probability.
    fn collect(states: &[State]) -> HashMap<(usize, String, usize), Fraction> {
        let mut m = HashMap::new();
        for (src, st) in states.iter().enumerate() {
            for tr in &st.transitions {
                *m.entry((src, tr.label.clone(), tr.target))
                    .or_insert_with(Fraction::zero) += tr.probability.clone();
            }
        }
        m
    }

    /// Ensure every state row sums exactly to 1.
    fn assert_rows_sum_to_one(states: &[State]) {
        for (i, st) in states.iter().enumerate() {
            let mut row = st.p_final.clone();
            for tr in &st.transitions { row += tr.probability.clone(); }
            assert_eq!(row, frac!(1,1), "row sum != 1 in state {}", i);
        }
    }

    // 1. detailed self-loop + cycle test
    #[test]
    fn tau_removal_self_loop_and_cycle_rows_sum_to_one() {
        // tau self-loop on 0 (1/5) and cycle between 1 and 2 (1/4,1/2)
        let mut a = StochasticNondeterministicFiniteAutomaton::new();
        for s in 0..3 { a.ensure_state(s); }
        a.set_final_probability(0, Fraction::zero());

        a.add_transition(0, "".into(), 0, frac!(1,5));
        a.add_transition(1, "".into(), 2, frac!(1,4));
        a.add_transition(2, "".into(), 1, frac!(1,2));

        a.add_transition(0, "a".into(), 0, frac!(3,5));
        a.add_transition(0, "b".into(), 1, frac!(1,5));
        a.add_transition(1, "c".into(), 1, frac!(3,4));
        a.add_transition(2, "d".into(), 2, frac!(1,4));

        a.set_final_probability(2, frac!(1,4));

        a.remove_tau_transitions();

        // no tau labels
        for st in &a.states {
            assert!(st.transitions.iter().all(|t| !t.label.is_empty()));
        }

        // expected exact result
        let mut expect = HashMap::new();
        expect.insert((0, "a".into(), 0), frac!(3,4));
        expect.insert((0, "b".into(), 1), frac!(1,4));
        expect.insert((1, "c".into(), 1), frac!(6,7));
        expect.insert((1, "d".into(), 2), frac!(1,14));
        expect.insert((2, "c".into(), 1), frac!(3,7));
        expect.insert((2, "d".into(), 2), frac!(2,7));

        let expect_final = [frac!(0,1), frac!(1,14), frac!(2,7)];

        assert_eq!(collect(&a.states), expect, "visible multiset differs");
        for (i, st) in a.states.iter().enumerate() {
            assert_eq!(st.p_final, expect_final[i], "p_final mismatch {}", i);
        }
        assert_rows_sum_to_one(&a.states);
    }

    // 2. inter-SCC tau-edge test (row = 1)
    #[test]
    fn tau_removal_inter_scc_path() {
        // SCC_A: 0 self-loop 1/2
        // SCC_B: 1,2 (1/4,1/3)
        // cross A -> B: 0 -> 1 1/2
        let mut a = StochasticNondeterministicFiniteAutomaton::new();
        for s in 0..3 { a.ensure_state(s); }
        a.set_final_probability(0, Fraction::zero());

        a.add_transition(0, "".into(), 0, frac!(1,2));
        a.add_transition(0, "".into(), 1, frac!(1,2));
        a.add_transition(1, "".into(), 2, frac!(1,4));
        a.add_transition(2, "".into(), 1, frac!(1,3));

        // visible edges
        a.add_transition(2, "v".into(), 2, frac!(1,2));
        a.add_transition(1, "w".into(), 1, frac!(1,4));   // completes row of state 1

        // finals to complete rows
        a.set_final_probability(1, frac!(1,2));
        a.set_final_probability(2, frac!(1,6));

        a.remove_tau_transitions();

        // "v" must be reachable from 0 and 1 now
        assert!(a.states[0].transitions.iter().any(|t| t.label == "v"));
        assert!(a.states[1].transitions.iter().any(|t| t.label == "v"));
        assert_rows_sum_to_one(&a.states);
    }

    // 3. empty automaton
    #[test]
    fn tau_removal_empty_automaton() {
        let mut a = StochasticNondeterministicFiniteAutomaton { states: vec![], initial: 0 };
        a.remove_tau_transitions();   // should not panic
        assert!(a.states.is_empty());
    }

    // 4. identity when no tau-edges
    #[test]
    fn tau_removal_no_tau_edges_is_identity() {
        let mut a = StochasticNondeterministicFiniteAutomaton::new();
        a.add_transition(0, "x".into(), 0, frac!(2,3));
        a.set_final_probability(0, frac!(1,3));

        let before = collect(&a.states);
        let before_final = a.states[0].p_final.clone();

        a.remove_tau_transitions();

        assert_eq!(collect(&a.states), before);
        assert_eq!(a.states[0].p_final, before_final);
    }

    // 5. single-state trivial case
    #[test]
    fn tau_removal_single_state_self_final() {
        let mut a = StochasticNondeterministicFiniteAutomaton::new();
        a.remove_tau_transitions();
        assert_eq!(a.states.len(), 1);
        assert_eq!(a.states[0].p_final, frac!(1,1));
    }

    // 6. simple acyclic tau-chain
    #[test]
    fn tau_removal_tau_chain() {
        // 0 -(1/5)->1 -(1/2)->2 , visible 2 -a->2
        let mut a = StochasticNondeterministicFiniteAutomaton::new();
        for s in 0..3 { a.ensure_state(s); }

        a.add_transition(0, "".into(), 1, frac!(1,5));
        a.add_transition(1, "".into(), 2, frac!(1,2));
        a.add_transition(2, "a".into(), 2, frac!(1,2));

        a.set_final_probability(0, frac!(4,5));
        a.set_final_probability(1, frac!(1,2));
        a.set_final_probability(2, frac!(1,2));

        a.remove_tau_transitions();

        // expect new a-edge from 0 with prob 1/20
        let w = a.states[0].transitions.iter()
                         .find(|t| t.label == "a")
                         .unwrap().probability.clone();
        assert_eq!(w, frac!(1,20));
        assert_rows_sum_to_one(&a.states);
    }

    // 7. singular matrix must panic
    #[test]
    #[should_panic(expected = "singular")]
    fn tau_removal_singular_matrix_panics() {
        // single state with tau-self-loop weight 1  ->  I-W singular
        let mut a = StochasticNondeterministicFiniteAutomaton::new();
        a.add_transition(0, "".into(), 0, frac!(1,1));
        a.remove_tau_transitions();   // must panic
    }

    // 8. automaton with only tau-edges
    #[test]
    fn tau_removal_only_tau_edges() {
        let mut a = StochasticNondeterministicFiniteAutomaton::new();
        a.ensure_state(1);
        a.add_transition(0, "".into(), 1, frac!(1,2));
        a.add_transition(1, "".into(), 0, frac!(1,2));
        a.set_final_probability(0, frac!(1,2));
        a.set_final_probability(1, frac!(1,2));

        a.remove_tau_transitions();

        for st in &a.states { assert!(st.transitions.is_empty()); }
        assert_rows_sum_to_one(&a.states);
    }

    // 9. size 1 / no tau-edge
    #[test]
    fn tau_removal_trivial_source_state() {
        // 0 has only visible edges, 1 has tau-edges
        let mut a = StochasticNondeterministicFiniteAutomaton::new();
        a.ensure_state(1);

        a.add_transition(0, "x".into(), 0, frac!(1,2));
        a.set_final_probability(0, frac!(1,2));

        a.add_transition(1, "".into(), 1, frac!(1,2)); // tau self-loop
        a.add_transition(1, "y".into(), 1, frac!(1,2));

        a.remove_tau_transitions();
        assert_rows_sum_to_one(&a.states);
    }
}
