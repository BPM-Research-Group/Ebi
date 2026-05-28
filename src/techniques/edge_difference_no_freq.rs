use ebi_objects::{
    DirectlyFollowsGraph, TranslateActivityKey,
    ebi_arithmetic::{Fraction, One, Zero},
};

/// DFG-difference ignoring frequency, counts differences in empty-trace, start, end, and edge presence.
pub trait EdgeDifferenceNoFrequencies {
    fn edge_difference_no_freq(&mut self, other: &mut Self) -> Fraction;
}

impl EdgeDifferenceNoFrequencies for DirectlyFollowsGraph {
    fn edge_difference_no_freq(&mut self, other: &mut Self) -> Fraction {
        // Align activity keys between the two graphs
        other.translate_using_activity_key(&mut self.activity_key);

        let zero = Fraction::zero();
        let one = Fraction::one();

        let is_present = |w: &Fraction| !w.is_zero();

        let mut result = Fraction::zero();

        //empty traces
        let empty1 = is_present(&self.empty_traces_weight);
        let empty2 = is_present(&other.empty_traces_weight);
        if empty1 != empty2 {
            log::debug!(
                "empty trace presence differs: first={} second={}",
                empty1,
                empty2
            );
            result += &one;
        }

        //start activities
        for activity in self.activity_key.get_activities() {
            let start1 = &self.start_activity_weight(*activity);
            let start2 = &other.start_activity_weight(*activity);

            let p1 = is_present(start1);
            let p2 = is_present(start2);

            if p1 != p2 {
                log::debug!(
                    "start presence different: {} first={} second={}",
                    self.activity_key.get_activity_label(activity),
                    p1,
                    p2
                );
                result += &one;
            }
        }

        //end activities
        for activity in self.activity_key.get_activities() {
            let end1 = &self.end_activity_weight(*activity);
            let end2 = &other.end_activity_weight(*activity);

            let p1 = is_present(end1);
            let p2 = is_present(end2);

            if p1 != p2 {
                log::debug!(
                    "end presence different: {} first={} second={}",
                    self.activity_key.get_activity_label(activity),
                    p1,
                    p2
                );
                result += &one;
            }
        }

        //edges
        let activities = self.activity_key.get_activities().to_vec();

        for &a in &activities {
            for &b in &activities {
                let e1 = self.edge_weight(*a, *b).unwrap_or(&zero);
                let e2 = other.edge_weight(*a, *b).unwrap_or(&zero);

                let p1 = is_present(e1);
                let p2 = is_present(e2);

                if p1 != p2 {
                    log::debug!(
                        "edge presence different: {}->{} first={} second={}",
                        self.activity_key.get_activity_label(a),
                        self.activity_key.get_activity_label(b),
                        p1,
                        p2,
                    );
                    result += &one;
                }
            }
        }

        result
    }
}
