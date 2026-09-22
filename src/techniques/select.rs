use super::gaspd::Entry;
use ebi_objects::{
    StochasticDirectlyFollowsModel,
    ebi_arithmetic::{Fraction, One, Zero, f},
};
use std::fmt::Write;

/// Holds the normalised objectives for one candidate; 1.0 is best on every objective.
#[derive(Debug, Clone)]
pub struct NormalisedEntry {
    pub norm_entropic_relevance: Fraction,
    pub norm_size: Fraction,
    pub norm_emsc: Fraction,
}

#[derive(Debug, Clone)]
pub struct Preference {
    pub weight_simplicity: Fraction, // simplicity
    pub weight_relevance: Fraction,  // relevance
    pub weight_a: Fraction,          // remainder, derived: 1 - w_s - w_r
}

impl Preference {
    /// Checks that simplicity + relevance is at most 1, and derives the
    /// remainder weight so all three sum to 1.
    pub fn new(
        mut weight_simplicity: Fraction,
        mut weight_relevance: Fraction,
        mut weight_a: Fraction,
    ) -> Self {
        let sum = &(&weight_simplicity + &weight_relevance) + &weight_a;

        if sum.is_zero() {
            return Self {
                weight_simplicity,
                weight_relevance,
                weight_a: Fraction::one(),
            };
        }

        weight_simplicity /= &sum;
        weight_relevance /= &sum;
        weight_a /= &sum;

        Self {
            weight_simplicity,
            weight_relevance,
            weight_a,
        }
    }
}

fn augmentation_rho() -> Fraction {
    &f!(1u64) / &f!(100u64)
}

pub fn select(
    models: Vec<StochasticDirectlyFollowsModel>,
    entries: Vec<Entry>,
    preference: Preference,
) -> Option<StochasticDirectlyFollowsModel> {
    let normalised = preprocess_objectives(&entries);

    let best = match select_best(&normalised, &preference) {
        Some(i) => i,
        None => return None,
    };
    let score = chebyshev_score(&normalised[best], &preference);
    log::info!(
        "{}",
        format_trade_off_report(&entries[best], &normalised[best], &preference, score)
    );

    Some(models[best].clone())
}

fn format_trade_off_report(
    best: &Entry,
    normalised: &NormalisedEntry,
    preference: &Preference,
    score: Fraction,
) -> String {
    let mut out = String::new();

    let _ = writeln!(out, "trade-off report");
    let _ = writeln!(
        out,
        "  weights     simplicity={:.3}  relevance={:.3}  remainder={:.3}",
        preference.weight_simplicity, preference.weight_relevance, preference.weight_a
    );
    let _ = writeln!(out);
    let _ = writeln!(
        out,
        "  {:<20} {:>12} {:>12}",
        "objective", "raw", "normalized"
    );
    let _ = writeln!(
        out,
        "  {:<20} {:>12.4} {:>12.4}",
        "entropic_relevance", best.relevance, normalised.norm_entropic_relevance
    );
    let _ = writeln!(
        out,
        "  {:<20} {:>12.0} {:>12.4}",
        "size", best.simplicity, normalised.norm_size
    );
    let _ = writeln!(
        out,
        "  {:<20} {:>12.4} {:>12.4}",
        "earth_movers", best.adhesion, normalised.norm_emsc
    );
    let _ = writeln!(out);
    let _ = write!(out, "  chebyshev_score: {:.6}", score);

    out
}

/// Min-max normalizes entropic relevance and size across all entries (inverted,
/// so 1.0 is best), and copies earth_movers straight through as its own
/// normalized value.
fn preprocess_objectives(entries: &[Entry]) -> Vec<NormalisedEntry> {
    if entries.is_empty() {
        return vec![];
    }

    let mut min_er = entries[0].relevance.clone();
    let mut max_er = min_er.clone();
    let mut min_size = f!(entries[0].simplicity as u64);
    let mut max_size = min_size.clone();

    for e in entries.iter() {
        let size = f!(e.simplicity as u64);

        if e.relevance < min_er {
            min_er = e.relevance.clone();
        }
        if e.relevance > max_er {
            max_er = e.relevance.clone();
        }
        if size < min_size {
            min_size = size.clone();
        }
        if size > max_size {
            max_size = size;
        }
    }

    let scale_inverted = |v: &Fraction, min: &Fraction, max: &Fraction| -> Fraction {
        if min == max {
            Fraction::one()
        } else {
            &(max - v) / &(max - min)
        }
    };

    entries
        .iter()
        .map(|e| NormalisedEntry {
            norm_entropic_relevance: scale_inverted(&e.relevance, &min_er, &max_er),
            norm_size: scale_inverted(&f!(e.simplicity as u64), &min_size, &max_size),
            norm_emsc: e.adhesion.clone(),
        })
        .collect()
}

/// Computes the augmented weighted Chebyshev distance of `row` from the
/// ideal point (1.0 on every normalized objective); lower is better.
fn chebyshev_score(row: &NormalisedEntry, pref: &Preference) -> Fraction {
    let one = Fraction::one();

    // Deviation from the ideal point (1.0 = best) for each objective.
    let dev_r = &one - &row.norm_entropic_relevance;
    let dev_s = &one - &row.norm_size;
    let dev_a = &one - &row.norm_emsc;

    let weighted = [
        &pref.weight_relevance * &dev_r,
        &pref.weight_simplicity * &dev_s,
        &pref.weight_a * &dev_a,
    ];

    let mut max_term = weighted[0].clone();
    let mut sum_term = weighted[0].clone();
    for w in &weighted[1..] {
        if w > &max_term {
            max_term = w.clone();
        }
        sum_term = &sum_term + w;
    }

    &max_term + &(&augmentation_rho() * &sum_term)
}

/// Returns the index of the entry with the lowest (best) Chebyshev score,
/// or None if there are no entries.
fn select_best(rows: &[NormalisedEntry], pref: &Preference) -> Option<usize> {
    rows.iter()
        .enumerate()
        .map(|(i, row)| (i, chebyshev_score(row, pref)))
        .min_by(|(_, a), (_, b)| a.partial_cmp(b).unwrap_or(std::cmp::Ordering::Equal))
        .map(|(i, _)| i)
}
