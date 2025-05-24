use crate::{
    ebi_framework::activity_key::HasActivityKey,
    ebi_objects::{
        directly_follows_graph::DirectlyFollowsGraph,
        stochastic_directly_follows_model::StochasticDirectlyFollowsModel,
    },
    math::{
        fraction::Fraction,
        traits::{Signed, Zero},
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

        let node_2_activity = activity_key
            .get_activities()
            .iter()
            .cloned()
            .cloned()
            .collect::<Vec<_>>();

        let mut start_node_weights = vec![Fraction::zero(); node_2_activity.len()];
        start_activities.into_iter().for_each(|(activity, weight)| {
            if weight.is_positive() {
                start_node_weights[activity_key.get_id_from_activity(activity)] = weight.into();
            }
        });

        let mut end_node_weights = vec![Fraction::zero(); node_2_activity.len()];
        end_activities.into_iter().for_each(|(activity, weight)| {
            if weight.is_positive() {
                end_node_weights[activity_key.get_id_from_activity(activity)] = weight.into();
            }
        });

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
