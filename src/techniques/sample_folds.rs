use rand_chacha::rand_core::{RngCore, SeedableRng};

use crate::ebi_objects::event_log::EventLog;

pub trait FoldsSampler {
    /// Peform fold splitting:
    /// (i) create a number of random folds, using the random seed.
    /// The split must be reproducible in the same version of Ebi, independent of platform.
    /// (ii) remove all traces from the log that do not belong to the selected fold.
    fn sample_folds(&mut self, number_of_folds: u32, seed: u64, select_fold: u32);
}

impl FoldsSampler for EventLog {
    fn sample_folds(&mut self, number_of_folds: u32, seed: u64, select_fold: u32) {
        let mut rng: rand_chacha::ChaCha8Rng = rand_chacha::ChaCha8Rng::seed_from_u64(seed);
        self.retain_traces_mut(&mut |_| select_fold == rng.next_u32() % number_of_folds);
    }
}

#[cfg(test)]
mod tests {
    use std::fs;

    use crate::{
        ebi_objects::event_log::EventLog, ebi_traits::ebi_trait_event_log::IndexTrace,
        techniques::sample_folds::FoldsSampler,
    };

    #[test]
    fn sample_folds() {
        let fin = fs::read_to_string("testfiles/a-b.xes").unwrap();
        let mut log = fin.parse::<EventLog>().unwrap();
        log.sample_folds(2, 10, 0);
        assert_eq!(log.len(), 0);

        let fin = fs::read_to_string("testfiles/a-b.xes").unwrap();
        let mut log = fin.parse::<EventLog>().unwrap();
        log.sample_folds(2, 10, 1);
        assert_eq!(log.len(), 2);
    }
}
