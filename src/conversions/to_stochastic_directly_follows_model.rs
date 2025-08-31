use ebi_arithmetic::{Fraction, Signed, Zero, f};

use crate::{
    ebi_framework::activity_key::HasActivityKey,
    ebi_objects::{
        directly_follows_graph::DirectlyFollowsGraph,
        stochastic_directly_follows_model::StochasticDirectlyFollowsModel,
    },
};

impl From<DirectlyFollowsGraph> for StochasticDirectlyFollowsModel {
    fn from(value: DirectlyFollowsGraph) -> Self {
        log::info!("Convert directly follows graph into stochastic directly follows model");

        let DirectlyFollowsGraph {
            activity_key,
            empty_traces_weight,
            sources,
            targets,
            weights,
            start_activities,
            end_activities,
        } = value;

        let zero = f!(0);
        let mut node_2_activity =
            vec![activity_key.get_activities()[0].clone(); activity_key.get_number_of_activities()];
        let mut start_node_weights = vec![Fraction::zero(); node_2_activity.len()];
        let mut end_node_weights = vec![Fraction::zero(); node_2_activity.len()];

        for activity in activity_key.get_activities() {
            let node = activity_key.get_id_from_activity(activity);

            node_2_activity[node] = activity.clone();
            start_node_weights[node] = start_activities.get(activity).unwrap_or(&zero).clone();
            end_node_weights[node] = end_activities.get(activity).unwrap_or(&zero).clone();
        }

        let mut result = Self {
            activity_key: activity_key,
            node_2_activity: node_2_activity,
            empty_traces_weight: empty_traces_weight.into(),
            sources: vec![],
            targets: vec![],
            weights: vec![],
            start_node_weights,
            end_node_weights,
        };

        //edges
        for (source, (target, weight)) in sources
            .into_iter()
            .zip(targets.into_iter().zip(weights.into_iter()))
        {
            if weight.is_positive() {
                let source_index = result.get_activity_key().get_id_from_activity(source);
                let target_index = result.get_activity_key().get_id_from_activity(target);
                result.add_edge(source_index, target_index, weight.into())
            }
        }

        result
    }
}
