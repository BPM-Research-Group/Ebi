use ebi_objects::{
    DirectlyFollowsGraph, TranslateActivityKey,
    ebi_arithmetic::{Fraction, Signed},
};

pub trait EdgeDifference {
    fn edge_difference(&mut self, other: &mut Self) -> Fraction;
}

impl EdgeDifference for DirectlyFollowsGraph {
    fn edge_difference(&mut self, other: &mut Self) -> Fraction {
        other.translate_using_activity_key(&mut self.activity_key);

        //empty traces
        let mut result = (&self.empty_traces_weight - &other.empty_traces_weight).abs();

        //start activities
        for activity in self.activity_key.get_activities() {
            let start1 = self.start_activity_weight(*activity);
            let start2 = other.start_activity_weight(*activity);

            if start1 != start2 {
                log::debug!(
                    "start different: {} first {} second {}",
                    self.activity_key.get_activity_label(activity),
                    start1,
                    start2
                );
            }

            result += (start1 - start2).abs();
        }

        //end activities
        for activity in self.activity_key.get_activities() {
            let end1 = self.end_activity_weight(*activity);
            let end2 = other.end_activity_weight(*activity);

            if end1 != end2 {
                log::debug!(
                    "end different: {} first {} second {}",
                    self.activity_key.get_activity_label(activity),
                    end1,
                    end2
                );
            }

            result += (end1 - end2).abs();
        }

        //edges
        for (a, b) in self.activities().zip(self.activities()) {
            let e1 = self.edge_weight(a, b);

            let e2 = other.edge_weight(a, b);

            if e1 != e2 {
                log::debug!(
                    "edge different: {}->{} first {} second {}",
                    self.activity_key.get_activity_label(&a),
                    self.activity_key.get_activity_label(&b),
                    e1,
                    e2,
                );
            }
            result += (e1 - e2).abs();
        }

        result
    }
}
