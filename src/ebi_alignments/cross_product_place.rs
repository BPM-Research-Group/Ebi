// use serde::{Deserialize, Serialize};
// use uuid::Uuid;
// use crate::ebi_alignments::cross_product_arc::CrossProductNetArc;
// use std::collections::HashMap;

// /// Place in a synchronous product net
// #[derive(Debug, Clone, Deserialize, Serialize)]
// pub struct CrossProductNetPlace {
//     pub place_id: Uuid,
//     pub in_arcs: Vec<CrossProductNetArc>,
//     pub out_arcs: Vec<CrossProductNetArc>
// }

// /// Marking of a cross product net: Assigning the place id to a number of tokens
// pub type CrossProductNetMarking = HashMap<Uuid, u64>;