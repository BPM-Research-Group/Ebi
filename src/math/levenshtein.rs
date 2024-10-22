use crate::ebi_framework::activity_key::Activity;

use super::fraction::Fraction;



pub fn normalised(trace1: &Vec<Activity>, trace2: &Vec<Activity>) -> Fraction {
    let dist = strsim::generic_levenshtein(trace1, trace2);

    Fraction::from((dist, trace1.len().max(trace2.len())))
}

pub fn distance(trace1: &Vec<Activity>, trace2: &Vec<Activity>) -> usize {
    strsim::generic_levenshtein(trace1, trace2)
}