use ebi_arithmetic::ebi_number::Signed;

use crate::{
    ebi_framework::activity_key::HasActivityKey,
    ebi_objects::{
        directly_follows_graph::DirectlyFollowsGraph, directly_follows_model::DirectlyFollowsModel,
        stochastic_directly_follows_model::StochasticDirectlyFollowsModel,
    },
};

impl From<DirectlyFollowsGraph> for DirectlyFollowsModel {
    fn from(value: DirectlyFollowsGraph) -> Self {
        log::info!("Convert directly follows graph into directly follows model");

        let DirectlyFollowsGraph {
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

        let mut start_nodes = vec![false; node_2_activity.len()];
        start_activities.into_iter().for_each(|(activity, weight)| {
            if weight.is_positive() {
                start_nodes[activity_key.get_id_from_activity(activity)] = true;
            }
        });

        let mut end_nodes = vec![false; node_2_activity.len()];
        end_activities.into_iter().for_each(|(activity, weight)| {
            if weight.is_positive() {
                end_nodes[activity_key.get_id_from_activity(activity)] = true;
            }
        });

        let mut result = Self {
            activity_key: activity_key,
            empty_traces: empty_traces_weight.is_positive(),
            node_2_activity: node_2_activity,
            sources: vec![],
            targets: vec![],
            start_nodes,
            end_nodes,
        };

        //edges
        for (source, (target, weight)) in sources.iter().zip(targets.iter().zip(weights.iter())) {
            if weight.is_positive() {
                let source_index = result.get_activity_key().get_id_from_activity(source);
                let target_index = result.get_activity_key().get_id_from_activity(target);
                result.add_edge(source_index, target_index)
            }
        }

        result
    }
}

impl From<StochasticDirectlyFollowsModel> for DirectlyFollowsModel {
    fn from(value: StochasticDirectlyFollowsModel) -> Self {
        Self {
            activity_key: value.activity_key,
            node_2_activity: value.node_2_activity,
            empty_traces: value.empty_traces_weight.is_positive(),
            sources: value.sources,
            targets: value.targets,
            start_nodes: value
                .start_node_weights
                .into_iter()
                .map(|w| w.is_positive())
                .collect(),
            end_nodes: value
                .end_node_weights
                .into_iter()
                .map(|w| w.is_positive())
                .collect(),
        }
    }
}
