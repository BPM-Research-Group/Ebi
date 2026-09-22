use crate::ebi_traits::{
    ebi_trait_event_log::EbiTraitEventLog,
    ebi_trait_finite_stochastic_language::EbiTraitFiniteStochasticLanguage,
    ebi_trait_queriable_stochastic_language::EbiTraitQueriableStochasticLanguage,
};
use crate::techniques::{
    alergia::FrequencyPrefixTree,
    earth_movers_stochastic_conformance::EarthMoversStochasticConformance,
    entropic_relevance::EntropicRelvance, sample::Sampler, select::Preference, select::select,
};
use ebi_objects::ebi_arithmetic::{ConstFraction, ToNative};
use ebi_objects::{
    Activity, AutomatonState, EventLog, FiniteStochasticLanguage,
    StochasticDeterministicFiniteAutomaton, StochasticDirectlyFollowsModel,
    ebi_arithmetic::{Fraction, One, Signed, f, fraction::approximate::Approximate},
};
use ebi_optimisation::anyhow::Result;
use rand::{RngExt, prelude::SliceRandom};
use rayon::prelude::*;
use rustc_hash::FxHashMap;
use std::collections::{HashMap, HashSet};

const CONFIDENCE_UPPER_BOUND: u64 = 15;
const CONFIDENCE_LOWER_BOUND: u64 = 0;
const FILTER_FREQUENCY_LOWER_NUMER: u64 = 10; // 0.00001 * 10^6
const MUTATION_STEP_FRACTION: ConstFraction = ConstFraction::of(2, 100);

const CONFIDENCE_FACTOR_RESOLUTION_DIGITS: u32 = 6;
const FILTER_FACTOR_RESOLUTION_DIGITS: u32 = 6;

/// Same algorithm as Alergia, but with the confidence factor, minimum
/// visit threshold, and trace-filtering frequency exposed as parameters
/// (used by Gaspd).
pub fn alergia_gaspd(
    confidence_factor: &Fraction,
    filter_frequency: &Fraction,
    log: &dyn EbiTraitFiniteStochasticLanguage,
    min_visits: usize,
) -> Result<StochasticDeterministicFiniteAutomaton> {
    let filtered_log = filter_log(filter_frequency.clone(), log);
    let mut fpta: FrequencyPrefixTree = FrequencyPrefixTree::from_log(&filtered_log);

    let sdfa: StochasticDeterministicFiniteAutomaton =
        alergia_algorithm(confidence_factor, &mut fpta, min_visits);
    Ok(sdfa)
}

/// Keeps only the most frequent traces, retaining a `filter_frequency`
/// fraction (rounded up) of distinct trace variants.
pub fn filter_log(
    filter_frequency: Fraction,
    log: &mut dyn EbiTraitFiniteStochasticLanguage,
) -> impl EbiTraitEventLog {
    let mut sorted_traces = log.iter_traces_probabilities().collect::<Vec<_>>();
    sorted_traces.sort_by(|a, b| b.1.cmp(&a.1).then(a.0.cmp(&b.0)));

    let len = sorted_traces.len();
    let target = f!(log.number_of_traces()) * filter_frequency;

    // Smallest `keep` such that Fraction::from(keep) >= target,
    let mut lo = 0;
    let mut hi = len;
    while lo < hi {
        let mid = lo + (hi - lo) / 2;
        if Fraction::from(mid) >= target {
            hi = mid;
        } else {
            lo = mid + 1;
        }
    }
    let keep = lo;

    log.retain_traces(Box::new(|trace, prob| true));

    let filtered_traces: Vec<Vec<Activity>> = sorted_traces
        .into_iter()
        .take(keep)
        .flat_map(|(trace, count)| std::iter::repeat(trace).take(count))
        .collect();

    EventLog {
        traces: filtered_traces,
        activity_key: log.activity_key().clone(),
    }
}

/// One candidate Alergia parameter setting plus its stochastic conformance scores.
/// `relevance == -1` means "not yet evaluated".
#[derive(Debug, Clone, PartialEq)]
pub struct Entry {
    confidence_factor: Fraction,
    pub relevance: Fraction,
    pub adhesion: Fraction,
    filter_frequency: Fraction,
    generation: usize,
    min_visits: usize,
    pub simplicity: usize,
}

impl Entry {
    /// Creates a random, unevaluated starting population.
    fn generate_initial_population(
        most_frequent_fpta_branch: usize,
        population_size: usize,
    ) -> Vec<Entry> {
        let mut population: Vec<Entry> = vec![];
        let mut rng = rand::rng();

        let denom: u64 = 10u64.pow(CONFIDENCE_FACTOR_RESOLUTION_DIGITS);
        let max_number: u64 = CONFIDENCE_UPPER_BOUND * denom;

        let filter_denom: u64 = 10u64.pow(FILTER_FACTOR_RESOLUTION_DIGITS);
        let filter_upper_numer: u64 = filter_denom; // 1.0 at this resolution

        for _ in 0..population_size {
            let confidence_choice: u64 = rng.random_range(0..=max_number);
            let confidence_factor: Fraction = &f!(confidence_choice) / &f!(denom);

            let filter_choice: u64 =
                rng.random_range(FILTER_FREQUENCY_LOWER_NUMER..=filter_upper_numer);
            let filter_frequency: Fraction = &f!(filter_choice) / &f!(filter_denom);

            let min_visits: usize = rng.random_range(1..=most_frequent_fpta_branch);

            population.push(Entry {
                confidence_factor,
                adhesion: f!(0),
                relevance: f!(-1),
                filter_frequency,
                generation: 0,
                min_visits,
                simplicity: 0,
            });
        }
        population
    }
}

pub trait Gaspd {
    fn gaspd(
        &mut self,
        generation_limit: usize,
        number_of_parents: usize,
        population_size: usize,
        weight_simplicity: Fraction,
        weight_relevance: Fraction,
        weight_a: Fraction,
    ) -> Result<StochasticDirectlyFollowsModel>;
}

/// Runs the Gaspd genetic search and returns the final Pareto-optimal
/// model according to the specified weights.
impl Gaspd for dyn EbiTraitEventLog {
    fn gaspd(
        &mut self,
        generation_limit: usize,
        number_of_parents: usize,
        population_size: usize,
        weight_simplicity: Fraction,
        weight_relevance: Fraction,
        weight_a: Fraction,
    ) -> Result<StochasticDirectlyFollowsModel> {
        let preference = Preference::new(weight_simplicity, weight_relevance, weight_a);
        let event_log = convert_trait_to_log(self);
        let test_lang = convert_trait_to_finite_stochastic_language(self);
        //test_log.translate_using_activity_key(&mut event_log.activity_key);
        let fpta: FrequencyPrefixTree = FrequencyPrefixTree::from_log(self);

        let most_frequent_fpta_branch = fpta
            .get_immediate_reachable_states(0)
            .iter()
            .map(|&neighbour| fpta.visit_counts[neighbour])
            .max()
            .unwrap_or(0);

        let mut population =
            Entry::generate_initial_population(most_frequent_fpta_branch, population_size);

        let mut archive: Vec<Entry> = vec![];

        select_parallel(&event_log, &test_lang, &mut population, &mut archive);

        for i in 1..generation_limit + 1 {
            let frontier = pareto_frontier(&population);
            let mut offspring = crossover_mutation(
                &archive,
                &frontier,
                i,
                most_frequent_fpta_branch,
                number_of_parents,
            );

            retain_elite_parallel(
                &event_log,
                &test_lang,
                &mut offspring,
                &mut population,
                &mut archive,
            );
        }

        let unique_frontier = dedup_frontier(population);
        let model_list: Vec<StochasticDirectlyFollowsModel> =
            create_concrete_models(&unique_frontier, &event_log);
        //let content = archive_to_csv_for_sdfm(&unique_frontier, &Some(model_list));

        let model = select(model_list, unique_frontier, preference)
            .unwrap_or_else(|| panic!("Error: no candidates to select from"));

        Ok(model)
    }
}

/// Rebuilds a trait-object log as a concrete `EventLog`, sorted by
/// descending trace frequency.
pub fn convert_trait_to_log(log: &mut dyn EbiTraitEventLog) -> EventLog {
    let mut trace_counts: HashMap<Vec<Activity>, usize> = HashMap::new();
    for t in log.iter_traces() {
        *trace_counts.entry(t.clone()).or_insert(0) += 1;
    }
    let mut sorted: Vec<(Vec<Activity>, usize)> = trace_counts.into_iter().collect();
    sorted.sort_by(|a, b| b.1.cmp(&a.1));
    let traces: Vec<Vec<Activity>> = sorted
        .into_iter()
        .flat_map(|(trace, count)| std::iter::repeat(trace).take(count))
        .collect();

    EventLog {
        traces,
        activity_key: log.activity_key().clone(),
    }
}

/// Converts a trait-object log into a normalized `FiniteStochasticLanguage`,
/// used as the reference distribution for scoring candidates.
pub fn convert_trait_to_finite_stochastic_language(
    log: &mut dyn EbiTraitEventLog,
) -> FiniteStochasticLanguage {
    let mut fslang = FiniteStochasticLanguage::new_with_activity_key(log.activity_key().clone());
    for trace in log.iter_traces() {
        fslang
            .push_raw(trace.clone(), &Fraction::one())
            .expect("weight is always positive");
    }
    fslang.normalise();

    fslang
}
/*
/// Standard CSV field escaping (wraps in quotes, doubles embedded quotes).
fn csv_escape(field: &str) -> String {
    format!("\"{}\"", field.replace('"', "\"\""))
}

/// Serializes the archive to CSV, one row per entry, with an optional
/// serialized SDFM per row.
fn archive_to_csv_for_sdfm(archive: &[Entry], models: &Option<Vec<StochasticDirectlyFollowsModel>>) -> String {
    if let Some(m) = models {
        assert_eq!(
            archive.len(),
            m.len(),
            "archive and models must be the same length and index-aligned"
        );
    }

    let mut out = String::new();
    let header = match models {
        Some(_) => "simplicity,relevance,adhesion,sdfm\n",
        None => "simplicity,relevance,adhesion,sdfm\n",
    };
    out.push_str(header);

    for (i, e) in archive.iter().enumerate() {

        out.push_str(&format!(
            "{},{},{}",
            e.simplicity, e.relevance, e.adhesion, 
        ));

        if let Some(m) = models {
            out.push(',');
            out.push_str(&csv_escape(&m[i].to_string()));
        }
        out.push('\n');
    }

    out
}
*/

/// Hashable key for a candidate's parameters, used for dedup/`seen` sets.
fn param_key(
    confidence_factor: &Fraction,
    filter_frequency: &Fraction,
    min_visits: usize,
) -> (u64, u64, u64) {
    (
        confidence_factor.clone().approximate().unwrap().to_bits(),
        filter_frequency.clone().approximate().unwrap().to_bits(),
        min_visits as u64,
    )
}

/// Converts an SDFA into its SDAG: one SDFM node per
/// transition, same-activity occurrences aren't merged.
fn convert_sdfa_to_sdfm(
    sdfa: &StochasticDeterministicFiniteAutomaton,
) -> Result<StochasticDirectlyFollowsModel> {
    let s0 = sdfa
        .initial_state
        .expect("SDFA must have an initial state to build its SDAG");

    let mut sdfm = StochasticDirectlyFollowsModel::new(sdfa.activity_key.clone());

    let nodes: Vec<AutomatonState> = sdfa
        .activities
        .iter()
        .map(|&activity| sdfm.add_node(activity))
        .collect();

    let mut by_source: FxHashMap<AutomatonState, Vec<usize>> = FxHashMap::default();
    for (t, &src) in sdfa.sources.iter().enumerate() {
        by_source.entry(src).or_default().push(t);
    }

    for (t1, &target_y) in sdfa.targets.iter().enumerate() {
        if let Some(outgoing) = by_source.get(&target_y) {
            for &t2 in outgoing {
                let weight = sdfa.probabilities[t2].clone();
                sdfm.add_edge(nodes[t1], nodes[t2], weight);
            }
        }
    }
    if let Some(starting) = by_source.get(&s0) {
        for &t in starting {
            let weight = sdfa.probabilities[t].clone();
            sdfm.start_node_weights[nodes[t].0] += weight;
        }
    }
    for (t, &target_y) in sdfa.targets.iter().enumerate() {
        let term = sdfa.terminating_probabilities[target_y.0].clone();
        if term.is_positive() {
            sdfm.end_node_weights[nodes[t].0] += term;
        }
    }
    let empty_weight = sdfa.terminating_probabilities[s0.0].clone();
    if empty_weight.is_positive() {
        sdfm.add_empty_trace(&empty_weight);
    }

    Ok(sdfm)
}

/// Drops near-duplicate frontier entries
fn dedup_frontier(frontier: Vec<Entry>) -> Vec<Entry> {
    let mut seen = HashSet::new();

    frontier
        .into_iter()
        .filter(|e| {
            let key = (f!(e.relevance.clone()) * f!(1000), e.simplicity);
            seen.insert(key)
        })
        .collect()
}

/// Rebuilds an SDFA + SDFM for each frontier entry, in parallel.
fn create_concrete_models(
    frontier: &Vec<Entry>,
    log: &EventLog,
) -> Vec<StochasticDirectlyFollowsModel> {
    frontier
        .par_iter()
        .map(|entry| {
            let temp_sdfa = alergia_gaspd(
                &entry.confidence_factor,
                &entry.filter_frequency,
                log.clone(),
                entry.min_visits,
            )
            .expect("alergia_gaspd failed for a frontier candidate");

            let res = convert_sdfa_to_sdfm(&temp_sdfa)
                .expect("convert_sdfa_to_dfg failed for a frontier candidate");
            res
        })
        .collect()
}

/// Scores each not-yet-seen population entry in parallel (simplicity,
/// relevance, adhesion) and records newly seen ones in `archive`.
fn select_parallel(
    log: &EventLog,
    lang: &FiniteStochasticLanguage,
    population: &mut Vec<Entry>,
    archive: &mut Vec<Entry>,
) {
    let to_evaluate: Vec<usize> = population
        .iter()
        .enumerate()
        .filter(|(_, entry)| entry.relevance == f!(-1))
        .map(|(i, _)| i)
        .collect();

    let results: Vec<(usize, usize, Fraction, Fraction)> = to_evaluate
        .par_iter()
        .map(|&i| {
            let entry = &population[i];
            let (size, er, em) = evaluate_entry(entry, log, lang);
            (i, size, er, em)
        })
        .collect();

    for (i, size, er, em) in results {
        population[i].simplicity = size;
        population[i].relevance = er.clone();
        population[i].adhesion = em.clone();
    }
    population.retain(|e| e.relevance != f!(-1));
    archive.extend(population.clone());
}
/// Computes an SDFM's "simplicity" size of the ouput type SDFM directly from the SDFA, without
/// building it.
fn derived_sdfm_detail(sdfa: &StochasticDeterministicFiniteAutomaton) -> usize {
    let s0 = sdfa
        .initial_state
        .expect("empty language has no meaningful SDFM size");

    let sdfm_nodes = sdfa.terminating_probabilities.len() - 1;

    let mut sdfm_edges = 0;
    let mut sdfm_start_edges = 0;
    for (source, prob) in sdfa.sources.iter().zip(sdfa.probabilities.iter()) {
        if !prob.is_positive() {
            continue;
        }
        if *source == s0 {
            sdfm_start_edges += 1;
        } else {
            sdfm_edges += 1;
        }
    }

    let sdfm_end_nodes = sdfa
        .terminating_probabilities
        .iter()
        .enumerate()
        .filter(|&(state, p)| state != s0.0 && p.is_positive())
        .count();

    sdfm_nodes + sdfm_edges + sdfm_start_edges + sdfm_end_nodes
}

/// Scores offspring in parallel, then merges them into `population` via the
/// Pareto frontier.
fn retain_elite_parallel(
    log: &EventLog,
    lang: &FiniteStochasticLanguage,
    offspring: &mut Vec<Entry>,
    population: &mut Vec<Entry>,
    archive: &mut Vec<Entry>,
) {
    let results: Vec<(usize, usize, Fraction, Fraction)> = offspring
        .par_iter()
        .enumerate()
        .map(|(i, entry)| {
            let (size, er, em) = evaluate_entry(entry, log, lang);
            (i, size, er, em)
        })
        .collect();

    for (i, simplicity, er, em) in results {
        offspring[i].simplicity = simplicity;
        offspring[i].relevance = er.clone();
        offspring[i].adhesion = em.clone();
    }

    let evaluated_u: Vec<Entry> = offspring
        .iter()
        .filter(|e| e.relevance != f!(-1))
        .cloned()
        .collect();

    let mut combined = population.clone();
    combined.extend(evaluated_u);
    archive.extend(combined.clone());

    *population = pareto_frontier(&combined);
}

/// Scores one candidate by rebuilding its SDFA via Alergia and computing
/// its simplicity, entropic relevance, and earth movers' stochastic
/// conformance against the reference language.
fn evaluate_entry(
    entry: &Entry,
    log: &EventLog,
    lang: &FiniteStochasticLanguage,
) -> (usize, Fraction, Fraction) {
    let sdfa = alergia_gaspd(
        &entry.confidence_factor,
        &entry.filter_frequency,
        log.clone(),
        entry.min_visits,
    )
    .unwrap();
    let mut trait_fslang: Box<dyn EbiTraitFiniteStochasticLanguage> = Box::new(lang.clone());
    let size = derived_sdfm_detail(&sdfa);
    let model: Box<dyn EbiTraitQueriableStochasticLanguage> = Box::new(sdfa.clone());
    let er = trait_fslang
        .entropic_relevance(model)
        .ok()
        .and_then(|lp| lp.approximate().ok())
        .and_then(|v| v.to_string().parse::<Fraction>().ok())
        .unwrap_or(f!(-1));
    let mut slang: FiniteStochasticLanguage = sdfa.sample(200).unwrap();
    let em = trait_fslang
        .earth_movers_stochastic_conformance(&mut slang)
        .unwrap();
    (size, er, em)
}
/// Builds the next generation: samples parents from the frontier, applies
/// pairwise crossover, then mutation.
fn crossover_mutation(
    archive: &Vec<Entry>,
    frontier: &Vec<Entry>,
    generation: usize,
    most_frequent_fpta_branch: usize,
    number_of_parents: usize,
) -> Vec<Entry> {
    let mut rng = rand::rng();

    let mut selected = frontier.clone();
    selected.shuffle(&mut rng);
    selected.truncate(number_of_parents);

    if selected.len() < number_of_parents {
        let needed = number_of_parents - selected.len();
        let selected_keys: HashSet<(u64, u64, u64)> = selected
            .iter()
            .map(|e| param_key(&e.confidence_factor, &e.filter_frequency, e.min_visits))
            .collect();

        let mut archive_candidates: Vec<&Entry> = archive
            .iter()
            .filter(|e| {
                !selected_keys.contains(&param_key(
                    &e.confidence_factor,
                    &e.filter_frequency,
                    e.min_visits,
                ))
            })
            .collect();

        archive_candidates.shuffle(&mut rng);
        archive_candidates.truncate(needed);
        selected.extend(archive_candidates.iter().map(|e| (*e).clone()));
    }

    let mut crossover_children: Vec<Entry> = vec![];
    let pairs: Vec<(usize, usize)> = (0..selected.len())
        .flat_map(|i| (i + 1..selected.len()).map(move |j| (i, j)))
        .collect();

    for (i, j) in &pairs {
        let p1 = &selected[*i];
        let p2 = &selected[*j];

        crossover_children.push(Entry {
            confidence_factor: p1.confidence_factor.clone(),
            relevance: f!(-1),
            adhesion: f!(0),
            filter_frequency: p2.filter_frequency.clone(),
            generation,
            min_visits: p2.min_visits,
            simplicity: 0,
        });
        crossover_children.push(Entry {
            adhesion: f!(0),
            confidence_factor: p2.confidence_factor.clone(),
            relevance: f!(-1),
            filter_frequency: p1.filter_frequency.clone(),
            generation,
            min_visits: p1.min_visits,
            simplicity: 0,
        });

        crossover_children.push(Entry {
            adhesion: f!(0),
            confidence_factor: p1.confidence_factor.clone(),
            relevance: f!(-1),
            filter_frequency: p2.filter_frequency.clone(),
            generation,
            min_visits: p1.min_visits,
            simplicity: 0,
        });
        crossover_children.push(Entry {
            adhesion: f!(0),
            confidence_factor: p2.confidence_factor.clone(),
            relevance: f!(-1),
            filter_frequency: p1.filter_frequency.clone(),
            generation,
            min_visits: p2.min_visits,
            simplicity: 0,
        });

        crossover_children.push(Entry {
            adhesion: f!(0),
            confidence_factor: p1.confidence_factor.clone(),
            relevance: f!(-1),
            filter_frequency: p1.filter_frequency.clone(),
            generation,
            min_visits: p2.min_visits,
            simplicity: 0,
        });
        crossover_children.push(Entry {
            adhesion: f!(0),
            confidence_factor: p2.confidence_factor.clone(),
            relevance: f!(-1),
            filter_frequency: p2.filter_frequency.clone(),
            generation,
            min_visits: p1.min_visits,
            simplicity: 0,
        });
    }

    let mutated: Vec<Entry> = crossover_children
        .iter()
        .map(|e| Entry {
            confidence_factor: mutate_confidence_factor(&e.confidence_factor, &mut rng),
            relevance: f!(-1),
            adhesion: f!(0),
            filter_frequency: mutate_filter_frequency(&e.filter_frequency, &mut rng),
            generation,
            min_visits: mutate_min_visits(e.min_visits, most_frequent_fpta_branch, &mut rng),
            simplicity: 0,
        })
        .collect();
    let mut offspring = selected;
    offspring.extend(crossover_children);
    offspring.extend(mutated);
    offspring
}

/// Perturbs `current` by a random rational step of at most `half_range_units`
/// with `1 / 10^resolution_digits` resolution, clamped to `[lower_bound, upper_bound]`.
fn mutate_fraction(
    current: &Fraction,
    resolution_digits: u32,
    half_range_units: i64,
    lower_bound: &Fraction,
    upper_bound: &Fraction,
    rng: &mut impl rand::Rng,
) -> Fraction {
    let denom: u64 = 10u64.pow(resolution_digits);
    let perturb_numer: i64 = rng.random_range(-half_range_units..=half_range_units);
    let perturbation: Fraction = if perturb_numer >= 0 {
        f!(perturb_numer.to_usize(), denom)
    } else {
        f!((-perturb_numer).to_usize(), denom)
    };

    let mut result: Fraction = current + &perturbation;

    if &result < lower_bound {
        result = lower_bound.clone();
    } else if &result > upper_bound {
        result = upper_bound.clone();
    }

    result
}
/// Randomly perturbs a confidence factor within MUTATION_STEP_FRACTION of [0, 15], clamped.
fn mutate_confidence_factor(current: &Fraction, rng: &mut impl rand::Rng) -> Fraction {
    let denom: u64 = 10u64.pow(CONFIDENCE_FACTOR_RESOLUTION_DIGITS);
    let range = CONFIDENCE_UPPER_BOUND - CONFIDENCE_LOWER_BOUND; // 15
    let half_range: i64 =
        (MUTATION_STEP_FRACTION.to_fraction() * f!(denom * range)).to_usize() as i64;
    let zero = f!(CONFIDENCE_LOWER_BOUND);
    let fifteen = f!(15, 1);
    mutate_fraction(
        current,
        CONFIDENCE_FACTOR_RESOLUTION_DIGITS,
        half_range,
        &zero,
        &fifteen,
        rng,
    )
}

/// Randomly perturbs a filter frequency within MUTATION_STEP_FRACTION of [FILTER_LOWER_BOUND, 1],
/// clamped.
fn mutate_filter_frequency(current: &Fraction, rng: &mut impl rand::Rng) -> Fraction {
    let denom: u64 = 10u64.pow(FILTER_FACTOR_RESOLUTION_DIGITS);
    let range = Fraction::one(); // FILTER_UPPER_BOUND - FILTER_LOWER_BOUND, ≈1.0
    let half_range: i64 =
        (MUTATION_STEP_FRACTION.to_fraction() * f!(range) * f!(denom)).to_usize() as i64;
    mutate_fraction(
        current,
        FILTER_FACTOR_RESOLUTION_DIGITS,
        half_range,
        &filter_lower_bound(),
        &f!(1),
        rng,
    )
}

/// Randomly perturbs `min_visits` by up to MUTATION_STEP_FRACTION of `most_frequent_fpta_branch`,
/// clamped to `[1, most_frequent_fpta_branch]`.
fn mutate_min_visits(
    current: usize,
    most_frequent_fpta_branch: usize,
    rng: &mut impl rand::Rng,
) -> usize {
    let half_range: i64 = (MUTATION_STEP_FRACTION.to_fraction() * f!(most_frequent_fpta_branch))
        .to_usize()
        .max(1) as i64;
    let perturbation: i64 = rng.random_range(-half_range..=half_range);
    current
        .saturating_add_signed(perturbation as isize)
        .clamp(1, most_frequent_fpta_branch)
}

/// Lower bound for `filter_frequency`, as an exact `Fraction`
/// (`FILTER_FREQUENCY_LOWER_NUMER / 10^FILTER_FACTOR_RESOLUTION_DIGITS`).
fn filter_lower_bound() -> Fraction {
    &f!(FILTER_FREQUENCY_LOWER_NUMER) / &f!(10u64.pow(FILTER_FACTOR_RESOLUTION_DIGITS))
}
/// Returns the non-dominated (Pareto-optimal) entries in `population`.
fn pareto_frontier(population: &[Entry]) -> Vec<Entry> {
    if population.is_empty() {
        return vec![];
    }

    let mut frontier = Vec::new();
    for i in 0..population.len() {
        let mut is_dominated = false;
        for j in 0..population.len() {
            if i == j {
                continue;
            }
            if dominates_entry(&population[j], &population[i]) {
                is_dominated = true;
                break;
            }
        }
        if !is_dominated {
            frontier.push(population[i].clone());
        }
    }

    frontier
}

/// True if `a` dominates `b` on all objectives, strictly on at least one.
fn dominates_entry(a: &Entry, b: &Entry) -> bool {
    (a.relevance <= b.relevance && a.simplicity <= b.simplicity && a.adhesion >= b.adhesion)
        && (a.relevance < b.relevance || a.simplicity < b.simplicity || a.adhesion > b.adhesion)
}
