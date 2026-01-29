use ebi_objects::{
    Activity,
    ebi_arithmetic::{Fraction, Zero},
};

pub fn normalised(trace1: &Vec<Activity>, trace2: &Vec<Activity>) -> Fraction {
    if !trace1.is_empty() || !trace2.is_empty() {
        let dist = strsim::generic_levenshtein(trace1, trace2);
        Fraction::from((dist, trace1.len().max(trace2.len())))
    } else {
        Fraction::zero()
    }
}

pub fn distance(trace1: &Vec<Activity>, trace2: &Vec<Activity>) -> usize {
    strsim::generic_levenshtein(trace1, trace2)
}
