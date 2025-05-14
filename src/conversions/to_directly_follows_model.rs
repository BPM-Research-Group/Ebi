use std::collections::HashSet;

use crate::{
    ebi_objects::{
        directly_follows_model::DirectlyFollowsModel,
        stochastic_directly_follows_model::StochasticDirectlyFollowsModel,
    },
    math::traits::Signed,
};

impl From<StochasticDirectlyFollowsModel> for DirectlyFollowsModel {
    fn from(value: StochasticDirectlyFollowsModel) -> Self {
        log::info!("Convert stochastic directly follows model into directly follows model");

        let StochasticDirectlyFollowsModel {
            activity_key,
            empty_traces_weight,
            sources,
            targets,
            weights,
            start_activities,
            end_activities,
        } = value;

        let node_2_activity = activity_key
            .get_activities()
            .iter()
            .cloned()
            .cloned()
            .collect::<Vec<_>>();

        //edges
        let mut edges = vec![vec![false; node_2_activity.len()]; node_2_activity.len()];
        for (source, (target, weight)) in sources.iter().zip(targets.iter().zip(weights.iter())) {
            if weight.is_positive() {
                let source_index = activity_key.get_id_from_activity(source);
                let target_index = activity_key.get_id_from_activity(target);
                edges[source_index][target_index] = true;
            }
        }

        let start_nodes = start_activities
            .into_iter()
            .filter_map(|(activity, weight)| {
                if weight.is_positive() {
                    Some(activity_key.get_id_from_activity(activity))
                } else {
                    None
                }
            })
            .collect::<HashSet<_>>();

        let end_nodes = end_activities
            .into_iter()
            .filter_map(|(activity, weight)| {
                if weight.is_positive() {
                    Some(activity_key.get_id_from_activity(activity))
                } else {
                    None
                }
            })
            .collect::<HashSet<_>>();

        Self {
            activity_key: activity_key,
            empty_traces: empty_traces_weight.is_positive(),
            edges: edges,
            node_2_activity: node_2_activity,
            start_nodes: start_nodes,
            end_nodes: end_nodes,
        }
    }
}
