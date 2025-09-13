use anyhow::{Result, anyhow};
use ebi_arithmetic::{Fraction, One, Signed, Zero, f};
use ebi_objects::FiniteLanguage;
use std::sync::Arc;

use crate::{
    ebi_traits::ebi_trait_finite_language::EbiTraitFiniteLanguage,
    math::distances::TriangularDistanceMatrix,
};

pub trait MedoidNonStochastic {
    fn medoid(&self, number_of_traces: usize) -> Result<FiniteLanguage>;

    /**
     * Applies an adaption of the FasterPAM algorithm from Fast and Eager k-Medoids Clustering: O(k) Runtime Improvement of the PAM, CLARA, and CLARANS Algorithms?
     */
    fn k_medoids_clustering(&self, number_of_clusters: usize) -> Result<FiniteLanguage>;
}

impl<T: ?Sized> MedoidNonStochastic for T
where
    T: EbiTraitFiniteLanguage,
{
    fn medoid(&self, number_of_traces: usize) -> Result<FiniteLanguage> {
        let activity_key = self.activity_key().clone();
        let mut result = FiniteLanguage::new_hashmap();

        let distances = TriangularDistanceMatrix::new(self);

        if number_of_traces.is_one() {
            let trace_number = medoid_single(self, &distances);
            if trace_number.is_none() {
                return Err(anyhow!(
                    "1 trace was requested, but the stochastic language contains none."
                ));
            }
            result.insert(self.get_trace(trace_number.unwrap()).unwrap().to_owned());
            return Ok((activity_key, result).into());
        }

        if self.number_of_traces() < number_of_traces {
            return Err(anyhow!(
                "{} traces were requested, but the stochastic language contains only {} traces.",
                number_of_traces,
                self.number_of_traces()
            ));
        }

        let mut sum_distance = sum_distances(self, &distances);

        let mut list = Vec::new();
        while list.len() < number_of_traces {
            //find the position of the minimum value
            let mut min_pos = 0;
            for i in 1..sum_distance.len() {
                if sum_distance[i] < sum_distance[min_pos] {
                    min_pos = i;
                }
            }

            //report the minimum value
            list.push(min_pos);
            sum_distance[min_pos] = f!(2);
        }
        list.sort();

        //put in the output format
        let mut list_i = 0;
        for (i1, trace1) in self.iter().enumerate() {
            if list_i < list.len() && i1 == list[list_i] {
                result.insert(trace1.to_vec());
                list_i += 1;
            }
        }

        Ok((activity_key, result).into())
    }

    /**
     * Applies an adaption of the FasterPAM algorithm from Fast and Eager k-Medoids Clustering: O(k) Runtime Improvement of the PAM, CLARA, and CLARANS Algorithms?
     */
    fn k_medoids_clustering(&self, number_of_clusters: usize) -> Result<FiniteLanguage> {
        //there is a Rust k-medoids crate, but that does not support exact arithmetic

        if self.number_of_traces() < number_of_clusters {
            return Err(anyhow!(
                "Language contains only {} different traces, and {} clusters were requested.",
                self.number_of_traces(),
                number_of_clusters
            ));
        }
        if number_of_clusters <= 0 {
            return Err(anyhow!("No clusters were requested"));
        }

        let distances = TriangularDistanceMatrix::new(self);
        let mut rng = rand::rng();
        let mut medoids =
            random_initialization(self.number_of_traces(), number_of_clusters, &mut rng);

        fasterpam(&distances, &mut medoids, 500);

        //create output (the medoids of the clusters)
        let mut result = FiniteLanguage::new_hashmap();
        medoids.sort();
        let mut list_i = 0;
        for (i1, trace1) in self.iter().enumerate() {
            if list_i < medoids.len() && i1 == medoids[list_i] {
                result.insert(trace1.to_vec());
                list_i += 1;
            }
        }
        Ok((self.activity_key().clone(), result).into())
    }
}

/**
 * Returns the index of the weighted medoid, if there is one.
 */
pub fn medoid_single<T>(log: &T, distances: &TriangularDistanceMatrix) -> Option<usize>
where
    T: EbiTraitFiniteLanguage + ?Sized,
{
    let sum_distance = sum_distances(log, distances);

    //report the minimum value
    let mut min_pos = 0;
    for (pos, _) in sum_distance.iter().enumerate() {
        if sum_distance[pos] < sum_distance[min_pos] {
            min_pos = pos;
        }
    }

    return Some(min_pos);
}

pub fn sum_distances<T>(log: &T, distances: &TriangularDistanceMatrix) -> Vec<Fraction>
where
    T: EbiTraitFiniteLanguage + ?Sized,
{
    let mut sum_distance = vec![Fraction::zero(); log.number_of_traces()];

    for (i, j, _, distance) in distances {
        sum_distance[i] += distance.as_ref();
        sum_distance[j] += distance.as_ref();
    }

    sum_distance
}

/**
 * All functions below are GPL-3, and adapted from https://github.com/kno10/rust-kmedoids/tree/main.
 *
/// ## Example
/// Given a dissimilarity matrix of size 4 x 4, use:
/// ```
/// let data = ndarray::arr2(&[[0,1,2,3],[1,0,4,5],[2,4,0,6],[3,5,6,0]]);
/// let mut meds = kmedoids::random_initialization(4, 2, &mut rand::thread_rng());
/// let (loss, assi, n_iter, n_swap): (f64, _, _, _) = kmedoids::fasterpam(&data, &mut meds, 100);
/// log::debug!("Loss is: {}", loss);
/// ```
 */
pub fn fasterpam(
    distances: &TriangularDistanceMatrix,
    medoids: &mut Vec<usize>,
    maxiter: usize,
) -> (Fraction, Vec<usize>, usize, usize) {
    let (n, k) = (distances.len(), medoids.len());
    if k == 1 {
        let assi = vec![0; n];
        let (swapped, loss) = choose_medoid_within_partition(distances, &assi, medoids, 0);
        return (loss, assi, 1, if swapped { 1 } else { 0 });
    }
    let (mut loss, mut data) = initial_assignment(distances, medoids);
    debug_assert_assignment(distances, medoids, &data);
    let mut removal_loss = vec![Fraction::zero(); k];
    update_removal_loss(&data, &mut removal_loss);
    let (mut lastswap, mut n_swaps, mut iter) = (n, 0, 0);
    while iter < maxiter {
        iter += 1;
        let (swaps_before, lastloss) = (n_swaps, loss.clone());
        for j in 0..n {
            if j == lastswap {
                break;
            }
            if j == medoids[data[j].near.index] {
                continue; // This already is a medoid
            }
            let (change, b) = find_best_swap(distances, &removal_loss, &data, j);
            if !change.is_negative() {
                continue; // No improvement
            }
            n_swaps += 1;
            lastswap = j;
            // perform the swap
            loss = do_swap(distances, medoids, &mut data, b, j);
            update_removal_loss(&data, &mut removal_loss);
        }
        if n_swaps == swaps_before || loss >= lastloss {
            break; // converged
        }
    }
    let assi = data.iter().map(|x| x.near.index).collect();
    (loss, assi, iter, n_swaps)
}

#[inline]
pub fn random_initialization(n: usize, k: usize, rng: &mut impl rand::Rng) -> Vec<usize> {
    rand::seq::index::sample(rng, n, k).into_vec()
}

/// Perform the initial assignment to medoids
#[inline]
pub(crate) fn initial_assignment(
    distances: &TriangularDistanceMatrix,
    medoids: &[usize],
) -> (Fraction, Vec<Rec>) {
    let (n, k) = (distances.len(), medoids.len());
    assert!(n <= usize::MAX, "N is too large");
    assert!(k > 0 && k < usize::MAX, "invalid N");
    assert!(k <= n, "k must be at most N");
    let mut data = vec![Rec::empty(); distances.len()];

    let firstcenter = medoids[0];
    let loss = data
        .iter_mut()
        .enumerate()
        .map(|(i, cur)| {
            *cur = Rec::new(
                0,
                distances.get(i, firstcenter).clone(),
                usize::MAX,
                distances.get_zero(),
            );
            for (m, &me) in medoids.iter().enumerate().skip(1) {
                let d = distances.get(i, me);
                if d < &cur.near.distance || i == me {
                    cur.seco = cur.near.clone();
                    cur.near = DistancePair {
                        index: m,
                        distance: d.clone(),
                    };
                } else if cur.seco.index == usize::MAX || d < &cur.seco.distance {
                    cur.seco = DistancePair {
                        index: m,
                        distance: d.clone(),
                    };
                }
            }
            cur.near.distance.clone()
        })
        .fold(Fraction::zero(), |acc, b| &acc + b.as_ref());
    (loss, data)
}

/// Find the best swap for object j - FastPAM version
#[inline]
pub(crate) fn find_best_swap(
    distances: &TriangularDistanceMatrix,
    removal_loss: &[Fraction],
    data: &[Rec],
    j: usize,
) -> (Fraction, usize) {
    let mut ploss = removal_loss.to_vec();
    // Improvement from the journal version:
    let mut acc = Fraction::zero();
    for (o, reco) in data.iter().enumerate() {
        let distance_o_j = distances.get(o, j);
        // New medoid is closest:
        if distance_o_j < &reco.near.distance {
            acc += distance_o_j.clone();
            acc -= reco.near.distance.clone();
            // loss already includes ds - dn, remove
            ploss[reco.near.index] += reco.near.distance.clone();
            ploss[reco.near.index] -= reco.seco.distance.clone();
        } else if distance_o_j < &reco.seco.distance {
            // loss already includes ds - dn, adjust to d(xo) - dn
            ploss[reco.near.index] += distance_o_j.clone();
            ploss[reco.near.index] -= reco.seco.distance.clone();
        }
    }
    let (b, bloss) = find_min(&mut ploss.iter());
    acc += bloss;
    (acc, b) // add the shared accumulator
}

/// Update the loss when removing each medoid
pub(crate) fn update_removal_loss(data: &[Rec], loss: &mut Vec<Fraction>) {
    loss.fill(Fraction::zero()); // stable since 1.50
    for rec in data.iter() {
        loss[rec.near.index] += rec.seco.distance.as_ref();
        loss[rec.near.index] -= rec.near.distance.as_ref();
        // as N might be unsigned
    }
}

/// Update the second nearest medoid information
/// Called after each swap.
#[inline]
pub(crate) fn update_second_nearest(
    distances: &TriangularDistanceMatrix,
    medoids: &[usize],
    n: usize,
    b: usize,
    o: usize,
    doj: Arc<Fraction>,
) -> DistancePair {
    let mut s = DistancePair::new(b, doj);
    for (i, &mi) in medoids.iter().enumerate() {
        if i == n || i == b {
            continue;
        }
        let d = distances.get(o, mi);
        if d < &s.distance {
            s = DistancePair::new(i, d.clone());
        }
    }
    s
}

/// Perform a single swap
#[inline]
pub(crate) fn do_swap(
    distances: &TriangularDistanceMatrix,
    medoids: &mut Vec<usize>,
    data: &mut Vec<Rec>,
    b: usize,
    j: usize,
) -> Fraction {
    let n = distances.len();
    assert!(b < medoids.len(), "invalid medoid number");
    assert!(j < n, "invalid object number");
    medoids[b] = j;
    data.iter_mut()
        .enumerate()
        .map(|(o, reco)| {
            if o == j {
                if reco.near.index != b {
                    reco.seco = reco.near.clone();
                }
                reco.near = DistancePair::new(b, distances.get_zero());
                return distances.get_zero();
            }
            let doj = distances.get(o, j);
            // Nearest medoid is gone:
            if reco.near.index == b {
                if doj < &reco.seco.distance {
                    reco.near = DistancePair::new(b, doj.clone());
                } else {
                    reco.near = reco.seco.clone();
                    reco.seco = update_second_nearest(
                        distances,
                        medoids,
                        reco.near.index,
                        b,
                        o,
                        doj.clone(),
                    );
                }
            } else {
                // nearest not removed
                if doj < &reco.near.distance {
                    reco.seco = reco.near.clone();
                    reco.near = DistancePair::new(b, doj.clone());
                } else if doj < &reco.seco.distance {
                    reco.seco = DistancePair::new(b, doj.clone());
                } else if reco.seco.index == b {
                    // second nearest was replaced
                    reco.seco = update_second_nearest(
                        distances,
                        medoids,
                        reco.near.index,
                        b,
                        o,
                        doj.clone(),
                    );
                }
            }
            reco.near.distance.clone()
        })
        .fold(Fraction::zero(), |acc, b| &acc + b.as_ref())
}

/// Object id and distance pair
#[derive(Debug, Clone)]
pub(crate) struct DistancePair {
    pub(crate) index: usize,
    pub(crate) distance: Arc<Fraction>,
}
impl DistancePair {
    pub(crate) fn new(i: usize, d: Arc<Fraction>) -> Self {
        DistancePair {
            index: i,
            distance: d,
        }
    }
}
impl DistancePair {
    pub(crate) fn empty() -> Self {
        DistancePair {
            index: usize::MAX,
            distance: Arc::new(Fraction::zero()),
        }
    }
}

/// Information kept for each point: two such pairs
#[derive(Debug, Clone)]
pub(crate) struct Rec {
    pub(crate) near: DistancePair,
    pub(crate) seco: DistancePair,
}

impl Rec {
    pub(crate) fn new(i1: usize, d1: Arc<Fraction>, i2: usize, d2: Arc<Fraction>) -> Rec {
        Rec {
            near: DistancePair {
                index: i1,
                distance: d1,
            },
            seco: DistancePair {
                index: i2,
                distance: d2,
            },
        }
    }
}
impl Rec {
    pub(crate) fn empty() -> Self {
        Rec {
            near: DistancePair::empty(),
            seco: DistancePair::empty(),
        }
    }
}

/// Choose the best medoid within a partition
/// Used by ther alternating algorithm, or when a single cluster is requested.
pub(crate) fn choose_medoid_within_partition(
    distances: &TriangularDistanceMatrix,
    assi: &[usize],
    medoids: &mut [usize],
    m: usize,
) -> (bool, Fraction) {
    let first = medoids[m];
    let mut best = first;
    let mut sumb = Fraction::zero();
    for (i, &a) in assi.iter().enumerate() {
        if first != i && a == m {
            sumb += distances.get(first, i).clone();
        }
    }
    for (j, &aj) in assi.iter().enumerate() {
        if j != first && aj == m {
            let mut sumj = Fraction::zero();
            for (i, &ai) in assi.iter().enumerate() {
                if i != j && ai == m {
                    sumj += distances.get(j, i).clone();
                }
            }
            if sumj < sumb {
                best = j;
                sumb = sumj;
            }
        }
    }
    medoids[m] = best;
    (best != first, sumb)
}

/// Debug helper function
pub(crate) fn debug_assert_assignment(
    distances: &TriangularDistanceMatrix,
    medoids: &[usize],
    _data: &[Rec],
) {
    for o in 0..distances.len() {
        debug_assert!(
            distances.get(o, medoids[_data[o].near.index]) == &_data[o].near.distance,
            "primary assignment inconsistent"
        );
        debug_assert!(
            distances.get(o, medoids[_data[o].seco.index]) == &_data[o].seco.distance,
            "secondary assignment inconsistent"
        );
        debug_assert!(
            _data[o].near.distance <= _data[o].seco.distance,
            "nearest is farther than second nearest"
        );
    }
}

/// Find the minimum (index and value)
#[inline]
pub(crate) fn find_min<'a, I: 'a>(a: &mut I) -> (usize, &'a Fraction)
where
    I: std::iter::Iterator<Item = &'a Fraction>,
{
    let mut a = a.enumerate();
    let mut best = (0, a.next().unwrap().1);
    for (ik, iv) in a {
        if iv < &best.1 {
            best = (ik, iv);
        }
    }
    best
}

#[cfg(test)]
mod tests {
    use std::fs;

    use ebi_objects::FiniteStochasticLanguage;

    use crate::techniques::medoid_non_stochastic::MedoidNonStochastic;

    #[test]
    fn non_stochastic_clustering() {
        let fin = fs::read_to_string("testfiles/aa-ab-ba.slang").unwrap();
        let slang = fin.parse::<FiniteStochasticLanguage>().unwrap();
        let cluster = slang.k_medoids_clustering(1).unwrap();
        let fout = fs::read_to_string("testfiles/aa.lang").unwrap();
        assert_eq!(fout, cluster.to_string())
    }

    #[test]
    fn non_stochastic_medoid() {
        let fin = fs::read_to_string("testfiles/aa-ab-ba.slang").unwrap();
        let slang = fin.parse::<FiniteStochasticLanguage>().unwrap();
        let fout = fs::read_to_string("testfiles/aa.lang").unwrap();
        let medoid = slang.medoid(1).unwrap();
        assert_eq!(fout, medoid.to_string())
    }
}
