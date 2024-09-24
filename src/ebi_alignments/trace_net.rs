// use process_mining::{import_xes_file, petri_net::petri_net_struct::{Transition,ArcType, Marking, PlaceID}, PetriNet, XESImportOptions};
// use std::collections::HashSet;

// pub type Trace = Vec<String>;

// fn main() {

//     let log_res = import_xes_file("./data/rtf3.xes", XESImportOptions::default());

//     match log_res {
//         Ok(log) => {
//             println!("Imported event log with {} traces", log.traces.len());
//             let mut trace_map = HashSet::new();
//             for t in log.traces.iter() {
//                 let mut new_t: Vec<String> = Trace::new();
//                 for e in t.events.iter() {
//                     new_t.push(e.attributes.iter().find(|&s| s.key == "concept:name").unwrap().value.try_as_string().unwrap().to_string());
//                 }
//                 trace_map.insert(new_t.clone());

//                 // convert trace to trace net
//                 let mut trace_net: PetriNet = PetriNet::new();
//                 let mut place_id = Some(Uuid::new_v4());
//                 let mut transition_id;

//                 trace_net.add_place(place_id);

//                 for label in new_t{
//                     transition_id = Some(Uuid::new_v4());
//                     trace_net.add_transition(Some(label), transition_id);
                    
//                     // add arc from previous place to transition
//                     trace_net.add_arc(ArcType::PlaceTransition(place_id.unwrap(), transition_id.unwrap()), None);

//                     // add a place
//                     place_id = Some(Uuid::new_v4());
//                     trace_net.add_place(place_id);

//                     // add arc from transition to place
//                     trace_net.add_arc(ArcType::TransitionPlace(transition_id.unwrap(), place_id.unwrap()), None);
//                 }

//                 println!("{:?}",trace_net);
//                 trace_net.export_png("./data/rtf3.png");
//             }                
//         },
//         Err(e) => {
//         eprintln!("XES Import failed: {:?}", e)
//         },
//     }

// }

