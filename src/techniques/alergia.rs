use crate::ebi_traits::ebi_trait_finite_stochastic_language::EbiTraitFiniteStochasticLanguage;
use ebi_objects::{
    Activity, ActivityKey, AutomatonState, StochasticDeterministicFiniteAutomaton,
    ebi_arithmetic::{
        Fraction, Log, One, Recip, Signed, Sqrt, Zero, f, fraction::approximate::Approximate,
    },
};
use ebi_optimisation::anyhow::{Ok, Result};
use std::collections::HashMap;

/// Learns a stochastic deterministic finite automaton from an event log
/// using the Alergia state-merging algorithm (Carrasco & Oncina, 1994).
pub trait Alergia {
    fn alergia(&self, alpha: Fraction) -> Result<StochasticDeterministicFiniteAutomaton>;
}

impl Alergia for dyn EbiTraitFiniteStochasticLanguage {
    fn alergia(&self, alpha: Fraction) -> Result<StochasticDeterministicFiniteAutomaton> {
        let mut fpta = FrequencyPrefixTree::from_log(self);

        //TODO: here, exact arithmetic is not yet up to the task
        let gamma_approx = (0.5 * (2.0 / alpha.approximate()?).ln()).sqrt();
        let confidence_factor = format!("{}", gamma_approx).parse::<Fraction>()?;

        let sdfa: StochasticDeterministicFiniteAutomaton =
            alergia_algorithm(&confidence_factor, &mut fpta, 0);

        Ok(sdfa)
    }
}

fn alergia_algorithm(
    confidence_factor: &Fraction,
    fpta: &mut FrequencyPrefixTree,
    min_visits: usize,
) -> StochasticDeterministicFiniteAutomaton {
    let mut parents: HashMap<usize, usize> = HashMap::new();
    let mut red: Vec<usize> = vec![0];
    let mut blue: Vec<usize> = fpta.get_immediate_reachable_states(0);
    for &b in &blue {
        parents.insert(b, 0);
    }

    loop {
        blue.retain(|&q| fpta.alive[q]);

        blue.sort_by(|&a, &b| {
            fpta.paths[a]
                .len()
                .cmp(&fpta.paths[b].len())
                .then_with(|| fpta.paths[a].cmp(&fpta.paths[b]))
        });
        blue.dedup();

        let pos: usize = match blue
            .iter()
            .position(|&q| fpta.visit_counts[q] >= min_visits)
        {
            Some(pos) => pos,
            None => break,
        };
        let q_blue: usize = blue.remove(pos);

        let mut found_merge: bool = false;
        for &q_red in &red {
            if alergia_compatible(&confidence_factor, fpta, q_blue, q_red) {
                found_merge = true;
                //log::debug!("[step {}] MERGE   qb={:?}  into qr={:?}", step, fpta.paths[q_blue], fpta.paths[q_red]);
                stochastic_merge(fpta, &mut parents, q_red, q_blue);
                break;
            }
        }
        if !found_merge {
            //log::debug!("[step {}] PROMOTE qb={:?}  (no compatible red state found)", step, fpta.paths[q_blue]);
            red.push(q_blue);
        }

        blue.clear();
        for &r in &red {
            for neighbour in fpta.get_immediate_reachable_states(r) {
                if !red.contains(&neighbour) && fpta.alive[neighbour] {
                    blue.push(neighbour);
                    parents.insert(neighbour, r);
                }
            }
        }
    }

    fpta.convert_to_sdfa()
}

/// A frequency-annotated prefix tree built from an event log's traces.
///
/// Each node tracks how many traces pass through it (`visit_counts`), how many
/// terminate there (`termination_counts`) and its outgoing transitions with
/// per-edge frequencies
#[derive(Debug)]
pub struct FrequencyPrefixTree {
    pub transitions: Vec<Vec<(Activity, usize, usize)>>,
    pub visit_counts: Vec<usize>,
    pub termination_counts: Vec<usize>,
    pub alive: Vec<bool>,
    pub activity_key: ActivityKey,
    pub paths: Vec<Vec<Activity>>,
}

impl FrequencyPrefixTree {
    /// Builds the prefix tree from the distinct traces of `log`, with edge
    /// and node counts weighted by trace frequency.
    pub fn from_log(log: &dyn EbiTraitFiniteStochasticLanguage) -> Self {
        let mut trace_counts: HashMap<Vec<Activity>, usize> = HashMap::new();
        for t in log.iter_traces() {
            *trace_counts.entry(t.clone()).or_insert(0) += 1;
        }

        let mut transitions: Vec<Vec<(Activity, usize, usize)>> = vec![vec![]];
        let mut visit_counts: Vec<usize> = vec![0];
        let mut termination_counts: Vec<usize> = vec![0];
        let mut child_map: Vec<HashMap<Activity, usize>> = vec![HashMap::new()];
        let mut paths: Vec<Vec<Activity>> = vec![vec![]]; // root path = []
        let mut trace_list: Vec<(Vec<Activity>, usize)> = trace_counts.into_iter().collect();
        trace_list.sort_by_key(|(trace, _)| trace.clone());

        for (trace, count) in &trace_list {
            let mut node = 0usize;
            visit_counts[node] += count;

            for &act in trace {
                if !child_map[node].contains_key(&act) {
                    let new_id = transitions.len();
                    transitions.push(vec![]);
                    visit_counts.push(0);
                    termination_counts.push(0);
                    child_map.push(HashMap::new());

                    // new node's path = parent's path + this activity
                    let mut child_path = paths[node].clone();
                    child_path.push(act);
                    paths.push(child_path);

                    child_map[node].insert(act, new_id);
                    let pos = transitions[node].partition_point(|&(a, _, _)| a < act);
                    transitions[node].insert(pos, (act, new_id, 0));
                }

                let next = child_map[node][&act];
                let pos = transitions[node].partition_point(|&(a, _, _)| a < act);
                transitions[node][pos].2 += count;
                visit_counts[next] += count;
                node = next;
            }
            termination_counts[node] += count;
        }

        let n: usize = transitions.len();

        Self {
            transitions,
            visit_counts,
            termination_counts,
            alive: vec![true; n],
            activity_key: log.activity_key().clone(),
            paths,
        }
    }

    pub fn get_immediate_reachable_states(&self, q: usize) -> Vec<usize> {
        self.transitions[q]
            .iter()
            .map(|(_, q_prime, _)| *q_prime)
            .collect()
    }

    pub fn get_edge_count(&self, a: Activity, q: usize) -> Option<&usize> {
        self.transitions[q]
            .iter()
            .find(|(a_prime, _, _)| *a_prime == a)
            .map(|(_, _, count)| count)
    }

    /// Converts the (possibly merged) prefix tree into a stochastic
    /// deterministic finite automaton, keeping only states marked `alive`
    pub fn convert_to_sdfa(&mut self) -> StochasticDeterministicFiniteAutomaton {
        let live_states: Vec<usize> = self
            .alive
            .iter()
            .enumerate()
            .filter(|(_, alive)| **alive)
            .map(|(i, _)| i)
            .collect();
        let mut remap = vec![0usize; self.alive.len()];
        for (new_idx, &old_idx) in live_states.iter().enumerate() {
            remap[old_idx] = new_idx;
        }
        let mut sources: Vec<AutomatonState> = Vec::new();
        let mut targets: Vec<AutomatonState> = Vec::new();
        let mut activities: Vec<Activity> = Vec::new();
        let mut probabilities: Vec<Fraction> = Vec::new();
        for &old_src in &live_states {
            let new_src = AutomatonState::of(remap[old_src]);
            let visit: &usize = &self.visit_counts[old_src];
            for &(act, old_tgt, ref count) in &self.transitions[old_src] {
                if !self.alive[old_tgt] {
                    continue;
                }
                sources.push(new_src);
                targets.push(AutomatonState::of(remap[old_tgt]));
                activities.push(act);
                probabilities.push(if visit.is_zero() {
                    f!(0)
                } else {
                    f!(*count as u64) / f!(*visit as u64)
                });
            }
        }
        let terminating_probabilities: Vec<Fraction> = live_states
            .iter()
            .map(|&old| {
                let visit: &usize = &self.visit_counts[old];
                if visit.is_zero() {
                    f!(0)
                } else {
                    f!(self.termination_counts[old] as u64) / f!(*visit as u64)
                }
            })
            .collect();
        StochasticDeterministicFiniteAutomaton {
            initial_state: Some(AutomatonState::zero()),
            activity_key: self.activity_key.clone(),
            sources,
            targets,
            activities,
            probabilities,
            terminating_probabilities,
        }
    }
}

fn stochastic_merge(
    fpta: &mut FrequencyPrefixTree,
    parents: &mut HashMap<usize, usize>,
    q: usize,
    q_prime: usize,
) {
    if let Some(&parent) = parents.get(&q_prime) {
        if let Some((_, next_q, _)) = fpta.transitions[parent]
            .iter_mut()
            .find(|(_, next_q, _)| *next_q == q_prime)
        {
            *next_q = q;
        }
    }

    stochastic_fold(fpta, parents, q, q_prime);
}

fn stochastic_fold(
    fpta: &mut FrequencyPrefixTree,
    parents: &mut HashMap<usize, usize>,
    q: usize,
    q_prime: usize,
) {
    let term_prime: usize = fpta.termination_counts[q_prime];
    let visit_prime: usize = fpta.visit_counts[q_prime];
    fpta.termination_counts[q] += term_prime;
    fpta.visit_counts[q] += visit_prime;
    fpta.termination_counts[q_prime] = 0;
    fpta.visit_counts[q_prime] = 0;

    let q_prime_edges: Vec<(Activity, usize, usize)> = fpta.transitions[q_prime].clone();
    for (activity, q_prime_child, count) in q_prime_edges {
        let q_existing = fpta.transitions[q]
            .iter()
            .find(|(a, _, _)| *a == activity)
            .map(|(_, child, _)| *child);

        if let Some(q_child) = q_existing {
            if let Some((_, _, q_count)) = fpta.transitions[q]
                .iter_mut()
                .find(|(a, _, _)| *a == activity)
            {
                *q_count += count;
            }
            stochastic_fold(fpta, parents, q_child, q_prime_child);
        } else {
            let pos = fpta.transitions[q].partition_point(|(a, _, _)| *a < activity);
            fpta.transitions[q].insert(pos, (activity, q_prime_child, count));
            parents.insert(q_prime_child, q);
        }
    }

    fpta.transitions[q_prime].clear();
    fpta.alive[q_prime] = false;
}

/// Checks whether `q_red` and `q_blue` (and all their corresponding
/// outgoing edges) are statistically compatible under the Hoeffding-bound
/// test, with confidence by `confidence_factor`.
fn alergia_compatible(
    confidence_factor: &Fraction,
    fpta: &FrequencyPrefixTree,
    q_blue: usize,
    q_red: usize,
) -> bool {
    if !alergia_test(
        &confidence_factor,
        *fpta.termination_counts.get(q_blue).unwrap_or(&0),
        *fpta.termination_counts.get(q_red).unwrap_or(&0),
        *fpta.visit_counts.get(q_blue).unwrap_or(&0),
        *fpta.visit_counts.get(q_red).unwrap_or(&0),
    ) {
        return false;
    }

    let mut activities: Vec<Activity> = fpta.activity_key.name2activity.values().copied().collect();
    activities.sort();

    for activity in &activities {
        let freq_blue = *fpta.get_edge_count(*activity, q_blue).unwrap_or(&0);
        let freq_red = *fpta.get_edge_count(*activity, q_red).unwrap_or(&0);
        let total_blue = *fpta.visit_counts.get(q_blue).unwrap_or(&0);
        let total_red = *fpta.visit_counts.get(q_red).unwrap_or(&0);

        if !alergia_test(
            &confidence_factor,
            freq_blue,
            freq_red,
            total_blue,
            total_red,
        ) {
            return false;
        }
    }

    true
}

/// Hoeffding-bound compatibility test between two empirical frequencies
/// (`freq_red`/`total_red` and `freq_blue`/`total_blue`), at confidence
/// level `confidence_factor`.
const SQRT_PRECISION: u32 = 16;

fn alergia_test(
    confidence_factor: &Fraction,
    freq_blue: usize,
    freq_red: usize,
    total_blue: usize,
    total_red: usize,
) -> bool {
    if freq_red == 0 && freq_blue == 0 {
        return true;
    }

    let f_freq_red = f!(freq_red);
    let f_total_red = f!(total_red);
    let f_freq_blue = f!(freq_blue);
    let f_total_blue = f!(total_blue);

    let dist = ((&f_freq_red / &f_total_red) - (&f_freq_blue / &f_total_blue)).abs();

    let inv_red = &Fraction::one() / &f_total_red;
    let inv_blue = &Fraction::one() / &f_total_blue;

    let root_red = inv_red.approx_abs_sqrt(SQRT_PRECISION);
    let root_blue = inv_blue.approx_abs_sqrt(SQRT_PRECISION);

    let bound = &(&root_red + &root_blue) * confidence_factor;

    dist < bound
}

#[cfg(test)]
mod tests {
    use crate::{
        ebi_traits::ebi_trait_finite_stochastic_language::EbiTraitFiniteStochasticLanguage,
        techniques::alergia::Alergia,
    };
    use ebi_objects::{
        FiniteStochasticLanguage, ebi_arithmetic::{Fraction, f}, ebi_objects::scalable_vector_graphics::ToSVG,
    };
    use std::fs;

    #[test]
    fn carrasco_paper() {
        let fin = fs::read_to_string("testfiles/carrasco.slang").unwrap();
        let slang = fin.parse::<FiniteStochasticLanguage>().unwrap();
        let slang: Box<dyn EbiTraitFiniteStochasticLanguage> = Box::new(slang);
        let sdfa = slang.alergia(f!(8, 10)).unwrap();

        //test fails for now, result is not equal to paper
        assert!(false);
    }
}
