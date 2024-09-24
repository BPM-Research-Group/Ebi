// use serde::{Deserialize, Serialize};
// use uuid::Uuid;


// #[derive(Debug, Deserialize, Serialize, Clone)]
// pub enum CrossProductNetArcType{
//     CrossProductNetPlace2CrossProductNetTransition,
//     CrossProductNetTransition2CrossProductNetPlace
// }

// /// Transition2Place arc in a synchronous product net
// #[derive(Debug, Deserialize, Serialize, Clone)]
// pub struct CrossProductNetArc{
//     pub from: Uuid,
//     pub to: Uuid,
//     pub arc_type: CrossProductNetArcType
// }