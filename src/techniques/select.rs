use csv::ReaderBuilder;
use std::collections::HashMap;
use std::error::Error;
use std::fs::File;
use std::fmt::Write;

/// Holds the metrics for one row of the CSV, plus the associated sdfm
#[derive(Debug, Clone)]
pub struct ModelRow {
    pub entropic_relevance: f64,
    pub earth_movers: f64,
    pub size: f64,
    pub dfg: String,
    pub norm_entropic_relevance: Option<f64>,
    pub norm_size: Option<f64>,
    pub norm_emsc: Option<f64>,
}
#[derive(Debug, Clone, Copy)]
pub struct Preference {
    pub w_s: f64, // simplicity
    pub w_r: f64, // relevance
    pub w_a: f64, // remainder, derived: 1 - w_s - w_r
}
const AUGMENTATION_RHO: f64 = 0.01;

const REQUIRED_FIELDS: [&str; 4] = [
    "relevance",
    "adhesion",
    "simplicity",
    "sdfm",
];

pub fn select(path: String, w_s: String, w_r: String) -> String {
    let mut rows = match load_csv(&path) {
        Ok(rows) => rows,
        Err(e) => return format!("Error: {e}"),
    };
    preprocess_objectives(&mut rows);
    let preference = match validate_preference(&w_s, &w_r) {
        Ok(p) => p,
        Err(e) => return format!("Error: {e}"),
    };
    let best = match select_best(&rows, &preference) {
        Some(row) => row,
        None => return "Error: no candidates to select from".to_string(),
    };
    let score = chebyshev_score(best, &preference);
    eprintln!("{}", format_trade_off_report(best, &preference, score)); 
    best.dfg.to_string()
}

fn format_trade_off_report(best: &ModelRow, preference: &Preference, score: f64) -> String {
    let mut out = String::new();

    let _ = writeln!(out, "trade-off report");
    let _ = writeln!(out, "  weights     simplicity={:.3}  relevance={:.3}  remainder={:.3}",
        preference.w_s, preference.w_r, preference.w_a);
    let _ = writeln!(out);
    let _ = writeln!(out, "  {:<20} {:>12} {:>12}", "objective", "raw", "normalized");
    let _ = writeln!(out, "  {:<20} {:>12.4} {:>12.4}", "entropic_relevance",
        best.entropic_relevance, best.norm_entropic_relevance.unwrap());
    let _ = writeln!(out, "  {:<20} {:>12.0} {:>12.4}", "size",
        best.size, best.norm_size.unwrap());
    let _ = writeln!(out, "  {:<20} {:>12.4} {:>12.4}", "earth_movers",
        best.earth_movers, best.norm_emsc.unwrap());
    let _ = writeln!(out);
    let _ = write!(out, "  chebyshev_score: {:.6}", score);

    out
}

/// Min-max normalizes entropic relevance and size across all rows (inverted,
/// so 1.0 is best), and copies earth_movers straight through as its own
/// normalized value.
fn preprocess_objectives(rows: &mut [ModelRow]) {
    let (mut min_er, mut max_er) = (f64::INFINITY, f64::NEG_INFINITY);
    let (mut min_sdfa, mut max_sdfa) = (f64::INFINITY, f64::NEG_INFINITY);

    for r in rows.iter() {
        min_er = min_er.min(r.entropic_relevance);
        max_er = max_er.max(r.entropic_relevance);
        min_sdfa = min_sdfa.min(r.size);
        max_sdfa = max_sdfa.max(r.size);
    }

    let scale_inverted = |v: f64, min: f64, max: f64| -> f64 {
        if (max - min).abs() < f64::EPSILON {
            1.0
        } else {
            (max - v) / (max - min)
        }
    };

    for r in rows.iter_mut() {
        r.norm_entropic_relevance = Some(scale_inverted(r.entropic_relevance, min_er, max_er));
        r.norm_size = Some(scale_inverted(r.size, min_sdfa, max_sdfa));
        r.norm_emsc = Some(r.earth_movers);
    }
}

/// Reads the CSV at `path`, checks it has all `REQUIRED_FIELDS`, and parses
/// each row into a `ModelRow`.
fn load_csv(path: &str) -> Result<Vec<ModelRow>, Box<dyn Error>> {
    let file = File::open(path)?;
    let mut rdr = ReaderBuilder::new().has_headers(true).from_reader(file);

    let headers = rdr.headers()?.clone();
    let mut index_of: HashMap<&str, usize> = HashMap::new();
    for (i, h) in headers.iter().enumerate() {
        index_of.insert(h, i);
    }

    for field in REQUIRED_FIELDS {
        if !index_of.contains_key(field) {
            return Err(format!("missing required column: {field}").into());
        }
    }

    let (idx_er, idx_em, idx_sdfa, idx_dfg) = (
        index_of["relevance"],
        index_of["adhesion"],
        index_of["simplicity"],
        index_of["sdfm"],
    );

    let mut rows = Vec::new();
    for result in rdr.records() {
        let record = result?;
        rows.push(ModelRow {
            entropic_relevance: record.get(idx_er).ok_or("row missing relevance")?.parse()?,
            earth_movers: record.get(idx_em).ok_or("row missing adhesion")?.parse()?,
            size: record.get(idx_sdfa).ok_or("row missing simplicity")?.parse()?,
            dfg: record.get(idx_dfg).ok_or("row missing sdfm")?.to_string(),
            norm_entropic_relevance: None,
            norm_size: None,
            norm_emsc: None,
        });
    }

    if rows.is_empty() {
        return Err("csv had no data rows".into());
    }

    Ok(rows)
}

/// Parses and validates the simplicity/relevance weight strings, deriving
/// the remainder weight so all three sum to 1.
fn validate_preference(w_s: &str, w_r: &str) -> Result<Preference, Box<dyn Error>> {
    let w_s: f64 = w_s.trim().parse().map_err(|_| format!("w_s is not a valid number: '{w_s}'"))?;
    let w_r: f64 = w_r.trim().parse().map_err(|_| format!("w_r is not a valid number: '{w_r}'"))?;

    if !w_s.is_finite() || !w_r.is_finite() {
        return Err("weights must be finite numbers".into());
    }
    if w_s < 0.0 {
        return Err(format!("Weight for simplicity must be >= 0, got {w_s}").into());
    }
    if w_r < 0.0 {
        return Err(format!("Weight for relevance must be >= 0, got {w_r}").into());
    }

    let sum = w_s + w_r;
    if sum > 1.0 {
        return Err(format!("simplicity + relevance must be <= 1, instead {sum}").into());
    }

    let w_a = 1.0 - sum;

    Ok(Preference { w_s, w_r, w_a })
}

/// Computes the augmented weighted Chebyshev distance of `row` from the
/// ideal point (1.0 on every normalized objective); lower is better.
fn chebyshev_score(row: &ModelRow, pref: &Preference) -> f64 {
    let norm_er = row
        .norm_entropic_relevance
        .expect("row must be preprocessed before scoring");
    let norm_sdfa = row
        .norm_size
        .expect("row must be preprocessed before scoring");
    let norm_emsc = row.norm_emsc.expect("row must be preprocessed before scoring");

    // Deviation from the ideal point (1.0 = best) for each objective.
    let dev_r = 1.0 - norm_er;
    let dev_s = 1.0 - norm_sdfa;
    let dev_a = 1.0 - norm_emsc;

    let weighted = [
        pref.w_r * dev_r,
        pref.w_s * dev_s,
        pref.w_a * dev_a,
    ];

    let max_term = weighted.iter().cloned().fold(f64::NEG_INFINITY, f64::max);
    let sum_term: f64 = weighted.iter().sum();
    max_term + AUGMENTATION_RHO * sum_term
}    

/// Returns the row with the lowest (best) Chebyshev score.
fn select_best<'a>(rows: &'a [ModelRow], pref: &Preference) -> Option<&'a ModelRow> {
    rows.iter()
        .map(|row| (row, chebyshev_score(row, pref)))
        .min_by(|(_, a), (_, b)| a.partial_cmp(b).unwrap_or(std::cmp::Ordering::Equal))
        .map(|(row, _)| row)
}