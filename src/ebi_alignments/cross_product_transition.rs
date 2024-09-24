// use serde::{Deserialize, Serialize};
// use uuid::Uuid;
// use crate::ebi_alignments::cross_product_arc::CrossProductNetArc;

// #[derive(Debug, Deserialize, Serialize, Clone)]
// pub enum CrossProductNetTransitionType{
//     LogMove,
//     ModelMove,
//     SyncMove,
// }


// #[derive(Debug, Deserialize, Serialize, Clone)]
// pub enum CrossProductNetTransitionCost{
//     Zero,
//     One
// }

// /// Transition in a synchronous product net
// #[derive(Debug, Clone, Deserialize, Serialize)]
// pub struct CrossProductNetTransition {
//     pub id: Uuid,
//     pub label: String,
//     pub cost: CrossProductNetTransitionCost,
//     pub transtion_type: CrossProductNetTransitionType,
//     pub has_model_transition: bool,
//     pub in_arcs: Vec<CrossProductNetArc>,
//     pub out_arcs: Vec<CrossProductNetArc>
// }