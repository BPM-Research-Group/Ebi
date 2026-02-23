use anyhow::{Context, Ok, Result};
use ebi_objects::{
    Activity, StochasticNondeterministicFiniteAutomaton,
    ebi_arithmetic::{EbiMatrix, Fraction, FractionMatrix, Inversion, One, Zero},
};
use std::collections::{HashMap, VecDeque};

pub trait TauRemoval {
    fn remove_tau_transitions(&mut self) -> Result<()>;
}

impl TauRemoval for StochasticNondeterministicFiniteAutomaton {
    fn remove_tau_transitions(&mut self) -> Result<()> {
        let n = self.number_of_states();
        if n == 0 {
            return Ok(());
        }

        let one = Fraction::one();
        let zero = Fraction::zero();

        //chech whether the SLPN has an initial state
        let initial_state = if let Some(initial) = self.initial_state {
            initial
        } else {
            return Ok(());
        };

        // Build tau-only adjacency with weights
        let mut tau_adj: Vec<Vec<(usize, Fraction)>> = vec![vec![]; n];
        for (source, target, label, probability) in self.into_iter() {
            if label.is_none() {
                tau_adj[*source].push((*target, probability.clone()));
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
            if vis[u] {
                return;
            }
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
                let mut mat = FractionMatrix::new(m, m);
                for (i_idx, &i) in comp.iter().enumerate() {
                    mat.set(i_idx, i_idx, one.clone());
                    for &(j, ref w) in &tau_adj[i] {
                        if comp_of[j] == cid {
                            let j_idx = comp.iter().position(|&x| x == j).unwrap();
                            mat.set(i_idx, j_idx, &mat.get(i_idx, j_idx).unwrap() - w);
                        }
                    }
                }
                let d_local = mat
                    .invert()
                    .with_context(|| "tau-removal: singular (I-W) in SCC")?;
                // store intra-component closure
                for (i_idx, &i) in comp.iter().enumerate() {
                    for (j_idx, &j) in comp.iter().enumerate() {
                        d[i][j] = d_local.get(i_idx, j_idx).unwrap().clone();
                    }
                    row_nonzero[i] = true;
                }
            }

            // Propagate to successor components (inter-SCC tau-edges)
            for &q in comp {
                for &(r, ref w_qr) in &tau_adj[q] {
                    if comp_of[r] == cid {
                        continue;
                    }
                    // For every s with d[r][s] > 0 (s already processed)
                    for p in 0..n {
                        if !row_nonzero[p] {
                            continue;
                        }
                        if d[p][q].is_zero() {
                            continue;
                        }
                        let prefix = &d[p][q] * &w_qr.clone();
                        if prefix.is_zero() {
                            continue;
                        }

                        for s in 0..n {
                            let d_rs = d[r][s].clone();
                            if d_rs.is_zero() {
                                continue;
                            }
                            let add = &prefix * &d_rs;
                            if add.is_zero() {
                                continue;
                            }

                            d[p][s] += add;
                        }
                    }
                }
            }
        }

        // Transform graph
        let mut new_transitions: Vec<Vec<Transition>> = vec![vec![]; n];

        for q in 0..n {
            for (_, target, label, probability) in self.outgoing_edges(q) {
                if !label.is_none() {
                    for p in 0..n {
                        if d[p][q].is_zero() {
                            continue;
                        }
                        let prob = &d[p][q] * probability;
                        new_transitions[p].push(Transition {
                            target: *target,
                            label: *label,
                            probability: prob,
                        });
                    }
                }
            }
        }

        // Merge parallel edges
        for p in 0..n {
            let mut map: HashMap<(usize, Option<Activity>), Fraction> = HashMap::new();
            for tr in new_transitions[p].drain(..) {
                *map.entry((tr.target, tr.label.clone()))
                    .or_insert_with(Fraction::zero) += tr.probability.clone();
            }
            new_transitions[p] = map
                .into_iter()
                .map(|((tgt, lbl), prob)| Transition {
                    target: tgt,
                    label: lbl,
                    probability: prob,
                })
                .collect();
        }

        // Write back
        replace_states(self, new_transitions)?;

        // Trim unreachable states (reachable over visible edges)
        {
            let mut reachable = vec![false; self.number_of_states()];
            let mut queue = VecDeque::new();
            queue.push_back(initial_state);
            while let Some(u) = queue.pop_front() {
                if reachable[u] {
                    continue;
                }
                reachable[u] = true;
                for (_, target, label, _) in self.outgoing_edges(u) {
                    if label.is_none() {
                        continue;
                    }
                    queue.push_back(*target);
                }
            }
            if reachable.iter().any(|&r| !r) {
                let mut map = vec![None; self.number_of_states()];
                let mut new_states: Vec<State> = Vec::new();
                for old_idx in 0..self.number_of_states() {
                    if reachable[old_idx] {
                        let new_idx = new_states.len();
                        map[old_idx] = Some(new_idx);
                        new_states.push(State {
                            transitions: Vec::new(),
                        });
                    }
                }
                for old_idx in 0..self.number_of_states() {
                    if let Some(new_src) = map[old_idx] {
                        for (_, target, label, probability) in self.outgoing_edges(old_idx) {
                            if let Some(new_tgt) = map[*target] {
                                new_states[new_src].transitions.push(Transition {
                                    target: new_tgt,
                                    label: *label,
                                    probability: probability.clone(),
                                });
                            }
                        }
                    }
                }
                replace_everything(self, new_states)?;
                self.initial_state = Some(map[initial_state].unwrap());
            }
        }
        Ok(())
    }
}

/// Kosaraju algorithm to compute SCCs of a directed weighted graph (adjacency list ignores weights).
fn compute_scc(n: usize, adj: &Vec<Vec<(usize, Fraction)>>) -> Vec<Vec<usize>> {
    // 1st pass: order by finish time using DFS on original graph
    let mut visited = vec![false; n];
    let mut order: Vec<usize> = Vec::with_capacity(n);
    fn dfs1(
        u: usize,
        adj: &Vec<Vec<(usize, Fraction)>>,
        vis: &mut Vec<bool>,
        order: &mut Vec<usize>,
    ) {
        if vis[u] {
            return;
        }
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
    fn dfs2(
        u: usize,
        radj: &Vec<Vec<usize>>,
        comp: &mut Vec<usize>,
        comp_id: &mut Vec<Option<usize>>,
        cid: usize,
    ) {
        if comp_id[u].is_some() {
            return;
        }
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

#[derive(Clone)]
struct Transition {
    target: usize,
    label: Option<Activity>,
    probability: Fraction,
}

#[derive(Clone)]
struct State {
    transitions: Vec<Transition>,
}

fn replace_everything(
    snfa: &mut StochasticNondeterministicFiniteAutomaton,
    states: Vec<State>,
) -> Result<()> {
    snfa.sources.clear();
    snfa.targets.clear();
    snfa.activities.clear();
    snfa.probabilities.clear();
    snfa.terminating_probabilities.clear();

    for _ in 0..states.len() {
        snfa.add_state();
    }

    for (source, state) in states.into_iter().enumerate() {
        for transition in state.transitions.into_iter() {
            snfa.add_transition(
                source,
                transition.label,
                transition.target,
                transition.probability,
            )?;
        }
    }
    Ok(())
}

fn replace_states(
    snfa: &mut StochasticNondeterministicFiniteAutomaton,
    transitionss: Vec<Vec<Transition>>,
) -> Result<()> {
    snfa.sources.clear();
    snfa.targets.clear();
    snfa.activities.clear();
    snfa.probabilities.clear();
    snfa.terminating_probabilities.fill(Fraction::one());

    for (source, transitions) in transitionss.into_iter().enumerate() {
        for transition in transitions.into_iter() {
            snfa.add_transition(
                source,
                transition.label,
                transition.target,
                transition.probability,
            )?;
        }
    }
    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;
    use ebi_objects::{
        Activity, StochasticNondeterministicFiniteAutomaton,
        ebi_arithmetic::{Fraction, f0, f1},
    };
    use std::collections::HashMap;

    /// frac!(n,d)  ->  Fraction::from((n,d))
    macro_rules! frac {
        ($n:expr , $d:expr) => {
            Fraction::from(($n, $d))
        };
    }

    /// Collapse parallel visible edges into (src,label,dst) -> probability.
    fn collect(
        snfa: &StochasticNondeterministicFiniteAutomaton,
    ) -> HashMap<(usize, Option<Activity>, usize), Fraction> {
        let mut m = HashMap::new();
        for transition in 0..snfa.number_of_transitions() {
            let source = snfa.sources[transition];
            let target = snfa.targets[transition];
            let probability = &snfa.probabilities[transition];
            let label = snfa.activities[transition];
            *m.entry((source, label, target))
                .or_insert_with(Fraction::zero) += probability;
        }
        m
    }

    // 1. detailed self-loop + cycle test
    #[test]
    fn tau_removal_self_loop_and_cycle_rows_sum_to_one() {
        // tau self-loop on 0 (1/5) and cycle between 1 and 2 (1/4,1/2)
        let mut snfa = StochasticNondeterministicFiniteAutomaton::new();
        for _ in 0..3 {
            snfa.add_state();
        }

        snfa.add_transition(0, None, 0, frac!(1, 5)).unwrap();
        snfa.add_transition(1, None, 2, frac!(1, 4)).unwrap();
        snfa.add_transition(2, None, 1, frac!(1, 2)).unwrap();

        let acta = Some(snfa.activity_key.process_activity("a"));
        let actb = Some(snfa.activity_key.process_activity("b"));
        let actc = Some(snfa.activity_key.process_activity("c"));
        let actd = Some(snfa.activity_key.process_activity("d"));

        snfa.add_transition(0, acta, 0, frac!(3, 5)).unwrap();
        snfa.add_transition(0, actb, 1, frac!(1, 5)).unwrap();
        snfa.add_transition(1, actc, 1, frac!(3, 4)).unwrap();
        snfa.add_transition(2, actd, 2, frac!(1, 4)).unwrap();

        snfa.remove_tau_transitions().unwrap();

        // no tau labels
        assert!(snfa.activities.iter().all(|label| label.is_some()));

        // expected exact result
        let mut expect = HashMap::new();
        expect.insert((0, acta, 0), frac!(3, 4));
        expect.insert((0, actb, 1), frac!(1, 4));
        expect.insert((1, actc, 1), frac!(6, 7));
        expect.insert((1, actd, 2), frac!(1, 14));
        expect.insert((2, actc, 1), frac!(3, 7));
        expect.insert((2, actd, 2), frac!(2, 7));

        let expect_final = [f0!(), frac!(1, 14), frac!(2, 7)];

        assert_eq!(collect(&snfa), expect, "visible multiset differs");
        for (state, termination_probability) in snfa.terminating_probabilities.iter().enumerate() {
            assert_eq!(
                termination_probability, &expect_final[state],
                "p_final mismatch state {}",
                state
            );
        }
        snfa.check_consistency().unwrap();
    }

    // 2. inter-SCC tau-edge test (row = 1)
    #[test]
    fn tau_removal_inter_scc_path() {
        // SCC_A: 0 self-loop 1/2
        // SCC_B: 1,2 (1/4,1/3)
        // cross A -> B: 0 -> 1 1/2
        let mut snfa = StochasticNondeterministicFiniteAutomaton::new();
        for _ in 0..3 {
            snfa.add_state();
        }

        snfa.add_transition(0, None, 0, frac!(1, 2)).unwrap();
        snfa.add_transition(0, None, 1, frac!(1, 2)).unwrap();
        snfa.add_transition(1, None, 2, frac!(1, 4)).unwrap();
        snfa.add_transition(2, None, 1, frac!(1, 3)).unwrap();

        let actv = Some(snfa.activity_key.process_activity("v"));
        let actw = Some(snfa.activity_key.process_activity("w"));

        // visible edges
        snfa.add_transition(2, actv, 2, frac!(1, 2)).unwrap();
        snfa.add_transition(1, actw, 1, frac!(1, 4)).unwrap(); // completes row of state 1

        snfa.remove_tau_transitions().unwrap();

        // "v" must be reachable from 0 and 1 now
        assert!(snfa.activities.iter().any(|label| *label == actv));
        assert!(snfa.activities.iter().any(|label| *label == actv));

        snfa.check_consistency().unwrap();
    }

    // 3. empty automaton
    #[test]
    fn tau_removal_empty_automaton() {
        let mut snfa = StochasticNondeterministicFiniteAutomaton::new();
        snfa.terminating_probabilities.clear();
        snfa.remove_tau_transitions().unwrap(); // should not panic
        assert_eq!(snfa.number_of_states(), 0);
    }

    // 4. identity when no tau-edges
    #[test]
    fn tau_removal_no_tau_edges_is_identity() {
        let mut snfa = StochasticNondeterministicFiniteAutomaton::new();
        let actx = Some(snfa.activity_key.process_activity("x"));
        snfa.add_transition(0, actx, 0, frac!(2, 3)).unwrap();

        let before = collect(&snfa);
        let before_final = snfa.terminating_probabilities[0].clone();

        snfa.remove_tau_transitions().unwrap();

        assert_eq!(collect(&snfa), before);
        assert_eq!(snfa.terminating_probabilities[0], before_final);
    }

    // 5. single-state trivial case
    #[test]
    fn tau_removal_single_state_self_final() {
        let mut snfa = StochasticNondeterministicFiniteAutomaton::new();
        snfa.remove_tau_transitions().unwrap();
        assert_eq!(snfa.number_of_states(), 1);
        assert_eq!(snfa.terminating_probabilities[0], f1!());
    }

    // 6. simple acyclic tau-chain
    #[test]
    fn tau_removal_tau_chain() {
        // 0 -(1/5)->1 -(1/2)->2 , visible 2 -a->2
        let mut snfa = StochasticNondeterministicFiniteAutomaton::new();
        for _ in 0..3 {
            snfa.add_state();
        }

        let acta = Some(snfa.activity_key.process_activity("a"));

        snfa.add_transition(0, None, 1, frac!(1, 5)).unwrap();
        snfa.add_transition(1, None, 2, frac!(1, 2)).unwrap();
        snfa.add_transition(2, acta, 2, frac!(1, 2)).unwrap();

        snfa.remove_tau_transitions().unwrap();

        snfa.check_consistency().unwrap();

        // expect new a-edge from 0 with prob 1/20
        for transition in 0..snfa.number_of_transitions() {
            if snfa.activities[transition] == acta && snfa.probabilities[transition] == frac!(1, 20)
            {
                return;
            }
        }
        panic!("doesn't match");
    }

    // 7. singular matrix must panic
    #[test]
    #[should_panic(expected = "singular")]
    fn tau_removal_singular_matrix_panics() {
        // single state with tau-self-loop weight 1  ->  I-W singular
        let mut a = StochasticNondeterministicFiniteAutomaton::new();
        a.add_transition(0, None, 0, f1!()).unwrap();
        a.remove_tau_transitions().unwrap(); // must panic
    }

    // 8. automaton with only tau-edges
    #[test]
    fn tau_removal_only_tau_edges() {
        let mut snfa = StochasticNondeterministicFiniteAutomaton::new();
        snfa.add_state();
        snfa.add_transition(0, None, 1, frac!(1, 2)).unwrap();
        snfa.add_transition(1, None, 0, frac!(1, 2)).unwrap();

        snfa.remove_tau_transitions().unwrap();

        assert_eq!(snfa.number_of_transitions(), 0);
        snfa.check_consistency().unwrap();
    }

    // 9. size 1 / no tau-edge
    #[test]
    fn tau_removal_trivial_source_state() {
        // 0 has only visible edges, 1 has tau-edges
        let mut snfa = StochasticNondeterministicFiniteAutomaton::new();
        snfa.add_state();

        let actx = Some(snfa.activity_key.process_activity("x"));
        let acty = Some(snfa.activity_key.process_activity("y"));

        snfa.add_transition(0, actx, 0, frac!(1, 2)).unwrap();

        snfa.add_transition(1, None, 1, frac!(1, 2)).unwrap(); // tau self-loop
        snfa.add_transition(1, acty, 1, frac!(1, 2)).unwrap();

        snfa.remove_tau_transitions().unwrap();
        snfa.check_consistency().unwrap();
    }
}
