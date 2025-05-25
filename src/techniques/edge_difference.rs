use crate::{
    ebi_framework::activity_key::TranslateActivityKey,
    ebi_objects::directly_follows_graph::DirectlyFollowsGraph,
    math::{
        fraction::Fraction,
        traits::{Signed, Zero},
    },
};

pub trait EdgeDifference {
    fn edge_difference(&mut self, other: &mut Self) -> Fraction;
}

impl EdgeDifference for DirectlyFollowsGraph {
    fn edge_difference(&mut self, other: &mut Self) -> Fraction {
        other.translate_using_activity_key(&mut self.activity_key);
        let zero = Fraction::zero();

        //empty traces
        let mut result = (&self.empty_traces_weight - &other.empty_traces_weight).abs();

        //start activities
        for activity in self.activity_key.get_activities() {
            let start1 = self
                .start_activities
                .get(activity)
                .unwrap_or(&zero);
            let start2 = other
                .start_activities
                .get(activity)
                .unwrap_or(&zero);

            result += (start1 - start2).abs();
        }

        //end activities
        for activity in self.activity_key.get_activities() {
            let start1 = self
                .end_activities
                .get(activity)
                .unwrap_or(&zero);
            let start2 = other
                .end_activities
                .get(activity)
                .unwrap_or(&zero);

            result += (start1 - start2).abs();
        }

        //edges
        for (a, b) in self
            .activity_key
            .get_activities()
            .iter()
            .zip(self.activity_key.get_activities().iter())
        {
            let e1 = self
                .edge_weight(**a, **b)
                .unwrap_or(&zero);
            let e2 = other
                .edge_weight(**a, **b)
                .unwrap_or(&zero);
            result += (e1 - e2).abs();
        }

        result
    }
}
