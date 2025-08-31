use std::collections::HashMap;

use ebi_arithmetic::{f, Fraction, One, Zero};

pub trait Completeness {
    /*
     * computes the completeness of the sample data. A value of '1' indicates full completeness,
     * whereas as value of '0' indicates total incompleteness
     * :param obs_species_counts: the species with corresponding incidence counts
     * :return: the estimated completeness
     */
    fn estimate_completeness(&self) -> Fraction;
}

impl<T> Completeness for HashMap<T, usize> {
    fn estimate_completeness(&self) -> Fraction {
        //from https://github.com/MartinKabierski/process-completeness-estimation/blob/main/src/estimation/metrics.py

        let s_p = estimate_species_richness_chao(self);
        if s_p.is_zero() {
            Fraction::zero()
        } else {
            let obs_species_count: Fraction = get_number_observed_species(self).into();
            &obs_species_count / &s_p
        }
    }
}

fn get_singletons<T>(multiset: &HashMap<T, usize>) -> usize {
    multiset.iter().filter(|&(_, c)| c == &1).count()
}

fn get_doubletons<T>(multiset: &HashMap<T, usize>) -> usize {
    multiset.iter().filter(|&(_, c)| c == &2).count()
}

fn get_number_observed_species<T>(multiset: &HashMap<T, usize>) -> usize {
    multiset.len()
}

/**
 * computes the asymptotic(=estimated) species richness using the Chao1 estimator(for abundance data)
 * or Chao2 estimator (for incidence data)
 * :param obs_species_counts: the species with corresponding incidence counts
 * :return: the estimated species richness
 **/
fn estimate_species_richness_chao<T>(multiset: &HashMap<T, usize>) -> Fraction {
    let mut obs_species_count: Fraction = get_number_observed_species(multiset).into();
    let f_1: Fraction = get_singletons(multiset).into();
    let f_2: Fraction = get_doubletons(multiset).into();

    if !f_2.is_zero() {
        obs_species_count += &(&f_1 * &f_1) / &(&f!(2) * &f_2);
    } else {
        obs_species_count += &(&f_1 * &(&f_1 - &Fraction::one())) / &f!(2);
    }
    obs_species_count
}
