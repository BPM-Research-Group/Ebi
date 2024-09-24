// use bitvec::vec;
// use serde::{Deserialize, Serialize};
// use process_mining::petri_net::{import_pnml::import_pnml_from_path, petri_net_struct::{Marking, ArcType, PlaceID}, PetriNet};
// use uuid::Uuid;
// use std::collections::HashMap;
// use crate::ebi_alignments::{cross_product_arc::{CrossProductNetArc, CrossProductNetArcType}, 
// cross_product_place::{CrossProductNetPlace,CrossProductNetMarking}, 
// cross_product_transition::{CrossProductNetTransition, CrossProductNetTransitionCost, CrossProductNetTransitionType}};


// #[derive(Debug, Deserialize, Serialize)]
// pub struct CrossProductNet {
//     /// Places (Uuid come from source net)
//     pub places: HashMap<Uuid, CrossProductNetPlace>,
//     /// CrossProductNetTransitions
//     pub transitions: HashMap<Uuid, CrossProductNetTransition>,
//     /// Map transition to transition in the original source net
//     pub cross_transition2source_transition: HashMap<Uuid, Uuid>,
//     /// Arcs
//     pub arcs: Vec<CrossProductNetArc>,
//     /// Initial marking
//     pub initial_marking: CrossProductNetMarking,
//     /// Final markings (any of them are accepted as a final marking)
//     pub final_markings: Vec<CrossProductNetMarking>,
// }

// impl Default for CrossProductNet {
//     fn default() -> Self {
//         Self::new()
//     }
// }

// impl CrossProductNet {
//     /// Create new [`CrossProductNet`] with no places or transitions
//     pub fn new() -> Self {
//         Self {
//             places: HashMap::new(),
//             transitions: HashMap::new(),
//             cross_transition2source_transition: HashMap::new(),
//             arcs: Vec::new(),
//             initial_marking: CrossProductNetMarking::new(),
//             final_markings: Vec::new(),
//         }
//     }

//     /// Add a place (with an optional passed UUID)
//     pub fn add_place(&mut self, place_id: Uuid) {
//         let mut place = CrossProductNetPlace { place_id: place_id, in_arcs: Vec::new(), out_arcs: Vec::new() };
//         self.places.insert(place_id, place);
//     }

//     /// Add a transition (with an optional passed UUID)
//     pub fn add_transition(&mut self, 
//         transition_id: Uuid,
//         transition_label: String,
//         transitioin_cost: CrossProductNetTransitionCost,
//         transtion_type: CrossProductNetTransitionType,
//         has_model_transition: bool)
//         {
//         let mut transition = CrossProductNetTransition { id: transition_id, label: transition_label, cost: transitioin_cost, transtion_type: transtion_type, has_model_transition: has_model_transition, in_arcs: Vec::new(), out_arcs: Vec::new() };
//         self.transitions.insert(transition_id, transition);
//     }

//     // add arc to cross net
//     pub fn add_arc(
//         &mut self,
//         from_id:Uuid,
//         to_id:Uuid,
//         arc_type:CrossProductNetArcType){
//         // setup the arc
//         let arc = CrossProductNetArc {
//             from: from_id, 
//             to: to_id, 
//             arc_type: arc_type};
//         // add arc to the cross net
//         self.arcs.push(arc);                
//     }

//     /// Construct a synchronous product net of two input petri nets
//     ///
//     /// # Arguments
//     ///
//     /// * `petri_net1` - The first net
//     /// * `initial_marking1` - The initial marking of the first net
//     /// * `final_marking1` - The final marking of the first net
//     /// * `petri_net2` - The second net
//     /// * `initial_marking2` - The initial marking of the second net 
//     /// * `final_marking2` - The final marking of the second net
//     ///
//     /// # Returns 
//     /// 
//     /// a synchronous cross product net, its initial marking and final marking
//     pub fn construct_cross_product(
//         &mut self,
//         petri_net1: &PetriNet, 
//         initial_marking1: &Marking,
//         final_marking1: &Marking,
//         petri_net2: &PetriNet,
//         initial_marking2: &Marking,
//         final_marking2: &Marking,
//         ) {
//         // copy the log net into cross net
//         Self::copy_net_into_cross(self, petri_net1, true);

//         // copy the model net into cross net
//         Self::copy_net_into_cross(self, petri_net2, false);
    
//         // construct the sync transition for cross net
//         for (t1_uuid, t1) in petri_net1.transitions.iter() {
//             for (t2_uuid, t2) in petri_net2.transitions.iter() {
//                 if t1.label == t2.label && t1.label != None {
//                     let label_vec = vec![t1.label.clone().unwrap(), t2.label.clone().unwrap()];
//                     let new_label = label_vec.join("");
//                     let new_uuid = Uuid::new_v4();

//                     self.add_transition(
//                         new_uuid, 
//                         new_label, 
//                         CrossProductNetTransitionCost::Zero,
//                         CrossProductNetTransitionType::SyncMove,
//                         true
//                     );
    
//                     // add arc from petri_net1 and petri net2 to the new transition
//                     for arc in petri_net1.arcs.iter() {
//                         let arc_type = &arc.from_to;
//                         match arc_type{
//                             // the arc is from a place to a transition
//                             ArcType::PlaceTransition(from, to) => {
//                                 if to == t1_uuid {
//                                     self.add_arc(
//                                         *from, 
//                                         *to, 
//                                         CrossProductNetArcType::CrossProductNetPlace2CrossProductNetTransition);
//                                 }
//                             },
//                             // the arc is from a transition to a place
//                             ArcType::TransitionPlace(from, to)=>{
//                                 if to == t1_uuid {
//                                     self.add_arc(
//                                         *from, 
//                                         *to, 
//                                         CrossProductNetArcType::CrossProductNetTransition2CrossProductNetPlace);
//                                 }
//                             }
//                         }
//                     }
//                 }
//             }
//         }
    
//         // construct the initial marking for cross net
//         let mut sync_initial_marking = Marking::new();
//         sync_initial_marking.extend(initial_marking1);
//         sync_initial_marking.extend(initial_marking2);
    
//         // construct the final marking for corss net
//         let mut sync_final_marking = Marking::new();
//         sync_final_marking.extend(final_marking1);
//         sync_final_marking.extend(final_marking2);
//     }
    
    
//     // Copy the source net into cross product net
//     // The lower flag to true is used to indicate the source net is the model net
//     // The upper flag to false is used to indicate the source net is the trace net
//     fn copy_net_into_cross(&mut self, 
//                             source_net:&PetriNet,
//                            upper: bool)
//         {
//             let skip = String::from(">>");
//             let tau: String = String::from("tau");
    
//             for t in source_net.transitions.iter() {
//                 if upper{
//                     match t.1.label.clone(){
//                         Some(label) => {
//                             let label_vec = vec![label, skip.clone()];
//                             let new_label= label_vec.join("");
//                             self.add_transition(
//                             *t.0, 
//                             new_label, 
//                             CrossProductNetTransitionCost::One,
//                             CrossProductNetTransitionType::ModelMove,
//                             true);
//                         },
//                         None => {
//                             let label_vec = vec![tau.clone(), skip.clone()];
//                             let new_label= label_vec.join("");
//                             self.add_transition(
//                                 *t.0, 
//                                 new_label, 
//                                 CrossProductNetTransitionCost::Zero,
//                                 CrossProductNetTransitionType::ModelMove, 
//                                 true);
//                         }
//                     }
//                 } else {
//                     let label_vec = vec![skip.clone(), t.1.label.clone().unwrap()];
//                     let new_label= label_vec.join("");
//                     self.add_transition(
//                     *t.0,
//                     new_label, 
//                     CrossProductNetTransitionCost::One,
//                     CrossProductNetTransitionType::LogMove, 
//                     false);
//                 }
//             }
    
//             // add places to cross net
//             for p in source_net.places.iter() {
//                 self.add_place(*p.0);
//             }
    
//             // add arcs to cross net
//             for arc in source_net.arcs.iter() {
//                 let arc_type = &arc.from_to;
//                 match arc_type{
//                     // the arc is from a place to a transition
//                     ArcType::PlaceTransition(uuid1, uuid2) => {
//                         self.add_arc(*uuid1, *uuid2, CrossProductNetArcType::CrossProductNetPlace2CrossProductNetTransition);
//                     },
//                     // the arc is from a transition to a place
//                     ArcType::TransitionPlace(uuid1, uuid2) => {
//                         self.add_arc(*uuid1, *uuid2, CrossProductNetArcType::CrossProductNetTransition2CrossProductNetPlace);
//                     },
//                 }
//             }
//         }
        

//     pub fn construct_incidence_matrix(&mut self) -> (Vec<Vec<u32>>, HashMap<Uuid, usize>, HashMap<Uuid,usize>){
        
//         let mut t_idx: HashMap<Uuid, usize> = HashMap::new();
//         let mut p_idx = HashMap::new();

//         let mut idx = 0;
//         for place in self.places.iter(){
//             p_idx.insert(*place.0, idx);
//             idx += 1;
//         }

//         idx = 0;
//         for transition in self.transitions.iter(){
//             t_idx.insert(*transition.0, idx);
//             idx += 1;
//         }

//         let mut incidence_matrix = vec![vec![0; self.transitions.len()]; self.places.len()];

//         for place in self.places.iter(){
//             for arc in place.1.in_arcs.iter(){
//                 let t_idx = t_idx.get(&arc.from).unwrap();
//                 let p_idx = p_idx.get(&arc.to).unwrap();
//                 incidence_matrix[*p_idx][*t_idx] += 1;
//             }
//             for arc in place.1.out_arcs.iter(){
//                 let t_idx = t_idx.get(&arc.to).unwrap();
//                 let p_idx = p_idx.get(&arc.from).unwrap();
//                 incidence_matrix[*p_idx][*t_idx] -= 1;
//             }
//         }
        
//         (incidence_matrix, p_idx, t_idx)
//     }

//     pub fn constuct_ini_vec(&mut self, p_idx: HashMap<Uuid, usize>) -> Vec<u64>{
//         let mut ini_vec:Vec<u64> = vec![0; self.places.len()];
//         for marking in self.initial_marking.iter(){
//             let p_idx = p_idx.get(&marking.0).unwrap();
//             ini_vec[*p_idx] = *marking.1;
//         }
//         ini_vec
//     }

//     pub fn constuct_fin_vec(&mut self,p_idx: HashMap<Uuid, usize>) -> Vec<u64>{
//         let mut fin_vec:Vec<u64> = vec![0; self.places.len()];
//         for marking in self.initial_marking.iter(){
//             let p_idx = p_idx.get(&marking.0).unwrap();
//             fin_vec[*p_idx] = *marking.1;
//         }
//         fin_vec
//     }
// }