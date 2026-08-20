use std::collections::HashMap;

use ebi_objects::{
    Activity, Attribute, DataType, FiniteStochasticLanguage,
    anyhow::Result,
    ebi_arithmetic::{Fraction, One, Zero},
};
use rand::seq::SliceRandom;
use rayon::iter::{IntoParallelIterator, ParallelIterator};
use fnv::FnvBuildHasher;

use crate::{
    ebi_framework::ebi_command::EbiCommand,
    ebi_traits::{
        ebi_trait_event_log_trace_attributes::{EbiTraitEventLogTraceAttributes, ATTRIBUTE_TIME},
        ebi_trait_finite_stochastic_language::EbiTraitFiniteStochasticLanguage,
    },
    math::{distances::WeightedDistances, distances_matrix::WeightedDistanceMatrix, levenshtein},
    techniques::earth_movers_stochastic_conformance::EarthMoversStochasticConformance,
};

// Number of random shuffles for the baseline correction (paper recommends 25).
// Set to 0 to skip baseline and report raw EMSC.
const PHI: usize = 10;

// Minimum fraction of total cases a cohort must have to be included.
// Cohorts below this threshold are statistically too small to be meaningful.
// 0.05 = 5%, based on suggestions for this analysis.
const ALPHA: f64 = 0.05;

/// For each categorical trace attribute-value pair, partition the log into a
/// target cohort and the rest, compute their EMSC, optionally subtract a
/// random baseline to correct for size bias, and return a ranked leaderboard.
/// EMSC = 0 means identical behaviour, 1 means maximally different.
pub fn cohort_analysis(log: &dyn EbiTraitEventLogTraceAttributes) -> Result<String> {
    // Collect categorical attributes upfront so the attribute_key borrow is
    // dropped before we borrow `log` again for iteration.
    let attrs: Vec<(Attribute, String)> = {
        let ak = log.attribute_key();
        let mut result: Vec<(Attribute, String)> = Vec::new();
        let mut id = 0usize;
        loop {
            let attr = ak.id_to_attribute(id);
            match ak.attribute_to_label(attr) {
                None => break,
                Some(name) => {
                    if name != ATTRIBUTE_TIME {
                        if let Some(DataType::Categorical) = ak.attribute_to_data_type(attr) {
                            result.push((attr, name.clone()));
                        }
                    }
                }
            }
            id += 1;
        }
        result
    };

    if attrs.is_empty() {
        return Ok("No categorical trace attributes found in this log.".to_string());
    }

    // Shared activity key: must be the same for both cohorts when computing EMSC.
    let activity_key = log.activity_key().clone();

    // Pre-scan each attribute with a cheap frequency count so we can apply
    // the alpha filter before building the pairs list. This avoids collecting
    // tens of thousands of pairs for high-cardinality attributes (e.g. case IDs
    // or activity names) that would all be filtered anyway.
    let mut all_pairs: Vec<(String, String, Attribute)> = Vec::new();
    for (attribute, attr_name) in &attrs {
        let mut value_counts: HashMap<String, usize> = HashMap::new();
        let mut total = 0usize;
        for opt_val in log.iter_categorical(*attribute) {
            total += 1;
            if let Some(val) = opt_val {
                *value_counts.entry(val).or_insert(0) += 1;
            }
        }
        for (value, count) in value_counts {
            if count >= 2 && (count as f64) >= ALPHA * total as f64 {
                all_pairs.push((attr_name.clone(), value, *attribute));
            }
        }
    }

    let progress_bar = EbiCommand::get_progress_bar_ticks(all_pairs.len());

    // Results store (attribute_name, value, raw_emsc, corrected_emsc, cohort_size).
    let mut results: Vec<(String, String, Fraction, Fraction, usize)> = Vec::new();

    for (attr_name, value, attribute) in &all_pairs {
        let mut cohort_a: HashMap<Vec<Activity>, Fraction, FnvBuildHasher> = HashMap::default();
        let mut cohort_b: HashMap<Vec<Activity>, Fraction, FnvBuildHasher> = HashMap::default();
        // Flat instance lists are needed for the shuffle baseline.
        let mut instances_a: Vec<Vec<Activity>> = Vec::new();
        let mut instances_b: Vec<Vec<Activity>> = Vec::new();

        for (trace, opt_val) in log.iter_categorical_and_traces(*attribute) {
            if opt_val.as_deref() == Some(value.as_str()) {
                *cohort_a
                    .entry(trace.clone())
                    .or_insert_with(Fraction::zero) += Fraction::one();
                instances_a.push(trace.clone());
            } else {
                *cohort_b
                    .entry(trace.clone())
                    .or_insert_with(Fraction::zero) += Fraction::one();
                instances_b.push(trace.clone());
            }
        }

        // Skip if one side is empty (can happen if the log has missing values).
        if instances_a.is_empty() || cohort_b.is_empty() {
            progress_bar.inc(1);
            continue;
        }

        let cohort_size = instances_a.len();

        // Normalise both cohorts into proper probability distributions.
        let mut lang_a =
            FiniteStochasticLanguage::from((activity_key.clone(), cohort_a));
        let mut lang_b =
            FiniteStochasticLanguage::from((activity_key.clone(), cohort_b));

        // EMSC = 1 means identical distributions (zero transport cost), 0 means maximally different.
        let raw_emsc = {
            let t: &mut dyn EbiTraitFiniteStochasticLanguage = &mut lang_a;
            t.earth_movers_stochastic_conformance(&mut lang_b)?
        };
        let raw_emsc_display = raw_emsc.clone();

        // Baseline: average EMSC over PHI random splits of the same size.
        // Algorithm 1, line 7: d = real_dist - avg_random_dist (subtraction formula per paper).
        // Optimisation: precompute the N×N Levenshtein distance matrix once in
        // parallel, then only run the transport solver for each of the PHI shuffles.
        let corrected = if PHI == 0 {
            raw_emsc
        } else {
            // Assign every unique trace an index so shuffle iterations work with
            // cheap usize copies instead of cloning Vec<Activity>.
            let mut trace_to_idx: HashMap<Vec<Activity>, usize> = HashMap::new();
            let mut unique_traces: Vec<Vec<Activity>> = Vec::new();
            for trace in instances_a.iter().chain(instances_b.iter()) {
                if !trace_to_idx.contains_key(trace) {
                    trace_to_idx.insert(trace.clone(), unique_traces.len());
                    unique_traces.push(trace.clone());
                }
            }
            let n_unique = unique_traces.len();

            // Precompute all pairwise Levenshtein distances in parallel, paid once.
            let precomputed: Vec<Vec<Fraction>> = (0..n_unique)
                .into_par_iter()
                .map(|i| {
                    (0..n_unique)
                        .map(|j| levenshtein::normalised(&unique_traces[i], &unique_traces[j]))
                        .collect()
                })
                .collect();

            let n_a = instances_a.len();
            let n_b = instances_b.len();
            let mut all_indices: Vec<usize> = instances_a
                .iter()
                .chain(instances_b.iter())
                .map(|t| trace_to_idx[t])
                .collect();

            let mut rng = rand::rng();
            let mut baseline_sum = Fraction::zero();

            for _ in 0..PHI {
                all_indices.shuffle(&mut rng);

                // Count how often each unique trace appears in each random cohort.
                let mut counts_a = vec![0usize; n_unique];
                let mut counts_b = vec![0usize; n_unique];
                for &idx in &all_indices[..n_a] {
                    counts_a[idx] += 1;
                }
                for &idx in &all_indices[n_a..] {
                    counts_b[idx] += 1;
                }

                // Only keep traces that actually appear in this split, avoids
                // passing a large sparse matrix to the transport solver.
                let active_a: Vec<usize> =
                    (0..n_unique).filter(|&i| counts_a[i] > 0).collect();
                let active_b: Vec<usize> =
                    (0..n_unique).filter(|&j| counts_b[j] > 0).collect();

                let weights_a: Vec<Fraction> = active_a
                    .iter()
                    .map(|&i| {
                        let mut f = Fraction::from(counts_a[i]);
                        f /= n_a;
                        f
                    })
                    .collect();
                let weights_b: Vec<Fraction> = active_b
                    .iter()
                    .map(|&j| {
                        let mut f = Fraction::from(counts_b[j]);
                        f /= n_b;
                        f
                    })
                    .collect();

                // Build a compact sub-matrix using precomputed distances.
                let distances: Vec<Vec<Fraction>> = active_a
                    .iter()
                    .map(|&i| {
                        active_b.iter().map(|&j| precomputed[i][j].clone()).collect()
                    })
                    .collect();

                let shuffle_matrix =
                    WeightedDistanceMatrix::from_precomputed(weights_a, weights_b, distances);
                let rand_emsc = {
                    let d: &dyn WeightedDistances = &shuffle_matrix;
                    d.earth_movers_stochastic_conformance()?
                };
                baseline_sum += rand_emsc;
            }

            // Algorithm 1, line 7: d = real_distance - avg_random_distance (subtraction).
            // Ebi returns EMSC as similarity (1=identical), so distance = 1 - EMSC.
            // real_dist = 1 - raw_emsc; avg_random_dist = 1 - baseline_avg_similarity
            // d > 0: cohort deviates more than random (interesting, ranked first)
            let baseline_avg_sim = &baseline_sum / PHI as u64;
            let real_dist = Fraction::one() - raw_emsc.clone();
            let avg_random_dist = Fraction::one() - baseline_avg_sim;
            real_dist - avg_random_dist
        };

        results.push((attr_name.clone(), value.clone(), raw_emsc_display, corrected, cohort_size));
        progress_bar.inc(1);
    }

    progress_bar.finish_and_clear();

    if results.is_empty() {
        return Ok(
            "No behaviorally distinct cohorts found (all attribute partitions are trivial)."
                .to_string(),
        );
    }

    // PHI == 0: sort ascending by raw EMSC (lower similarity means more divergent).
    // PHI > 0: sort descending by corrected (higher means more behaviorally distinct).
    results.sort_by(|a, b| {
        if PHI == 0 {
            a.2.partial_cmp(&b.2).unwrap_or(std::cmp::Ordering::Equal)
        } else {
            b.3.partial_cmp(&a.3).unwrap_or(std::cmp::Ordering::Equal)
        }
    });

    let w_attr = results.iter().map(|(a, _, _, _, _)| a.len()).max().unwrap_or(9).max(9);
    let w_val = results.iter().map(|(_, v, _, _, _)| v.len()).max().unwrap_or(5).max(5);

    let mut out = String::new();

    if PHI == 0 {
        out.push_str(&format!(
            "{:<4}  {:<w_a$}  {:<w_v$}  {:>6}  {}\n",
            "Rank", "Attribute", "Value", "Cases", "EMSC (raw)",
            w_a = w_attr, w_v = w_val
        ));
        out.push_str(&format!(
            "{}  {}  {}  {}  {}\n",
            "-".repeat(4), "-".repeat(w_attr), "-".repeat(w_val),
            "-".repeat(6), "-".repeat(10)
        ));
        for (rank, (attr, val, raw, _corrected, size)) in results.iter().enumerate() {
            out.push_str(&format!(
                "{:<4}  {:<w_a$}  {:<w_v$}  {:>6}  {}\n",
                rank + 1, attr, val, size, raw,
                w_a = w_attr, w_v = w_val
            ));
        }
    } else {
        out.push_str(&format!(
            "{:<4}  {:<w_a$}  {:<w_v$}  {:>6}  {:>14}  {}\n",
            "Rank", "Attribute", "Value", "Cases", "EMSC (raw)", "d",
            w_a = w_attr, w_v = w_val
        ));
        out.push_str(&format!(
            "{}  {}  {}  {}  {}  {}\n",
            "-".repeat(4), "-".repeat(w_attr), "-".repeat(w_val),
            "-".repeat(6), "-".repeat(14), "-".repeat(9)
        ));
        for (rank, (attr, val, raw, corrected, size)) in results.iter().enumerate() {
            out.push_str(&format!(
                "{:<4}  {:<w_a$}  {:<w_v$}  {:>6}  {:>14}  {}\n",
                rank + 1, attr, val, size, raw, corrected,
                w_a = w_attr, w_v = w_val
            ));
        }
    }

    Ok(out)
}
