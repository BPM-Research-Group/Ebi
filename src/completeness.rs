use std::collections::{HashMap, HashSet};

use fraction::{One, Zero};

use crate::math::fraction::Fraction;

pub fn estimate_completeness <T> (multiset: &HashMap<T, usize>) -> Fraction {
    //from https://github.com/MartinKabierski/process-completeness-estimation/blob/main/src/estimation/metrics.py

    /**
     * computes the completeness of the sample data. A value of '1' indicates full completeness,
     * whereas as value of '0' indicates total incompleteness
     * :param obs_species_counts: the species with corresponding incidence counts
     * :return: the estimated completeness
    */
    let s_p = estimate_species_richness_chao(multiset);
    if s_p.is_zero() {
        Fraction::zero()
    } else {
        let obs_species_count: Fraction = get_number_observed_species(multiset).into();
        &obs_species_count / &s_p
    }
}

fn get_singletons <T> (multiset: &HashMap<T, usize>) -> usize {
    multiset.iter().filter(|&(_, c)| c == &1).count()
}

fn get_doubletons <T> (multiset: &HashMap<T, usize>) -> usize {
    multiset.iter().filter(|&(_, c)| c == &2).count()
}

fn get_number_observed_species <T> (multiset: &HashMap<T, usize>) -> usize {
    multiset.len()
}

fn estimate_species_richness_chao <T> (multiset: &HashMap<T, usize>) -> Fraction {
    /**
    * computes the asymptotic(=estimated) species richness using the Chao1 estimator(for abundance data)
     * or Chao2 estimator (for incidence data)
     * :param obs_species_counts: the species with corresponding incidence counts
     * :return: the estimated species richness
    **/
    let mut obs_species_count: Fraction = get_number_observed_species(multiset).into();
    let f_1: Fraction = get_singletons(multiset).into();
    let f_2: Fraction = get_doubletons(multiset).into();

    if !f_2.is_zero() {
        obs_species_count += &(&f_1 * &f_1) / &(&Fraction::two() * &f_2);
    } else {
        obs_species_count += &(&f_1 * &(&f_1 - &Fraction::one())) / &Fraction::two();
    }
    obs_species_count
}