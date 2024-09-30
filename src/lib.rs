pub mod ebi_framework {
    pub mod ebi_input;
    pub mod ebi_output;
    pub mod ebi_file_handler;
    pub mod ebi_object;
    pub mod ebi_command;
    pub mod ebi_trait;
    pub mod dottable;
    pub mod displayable;
    pub mod infoable;
    pub mod importable;
    pub mod exportable;
    pub mod activity_key;
    mod prom_link;
}
pub mod ebi_commands {
    pub mod ebi_command_analyse_non_stochastic;
    pub mod ebi_command_conformance;
    pub mod ebi_command_discover;
    pub mod ebi_command_probability;
    pub mod ebi_command_visualise;
    pub mod ebi_command_validate;
    pub mod ebi_command_convert;
    pub mod ebi_command_analyse;
    pub mod ebi_command_latex;
    pub mod ebi_command_association;
    pub mod ebi_command_info;
    pub mod ebi_command_sample;
    pub mod ebi_command_test;
}
pub mod ebi_objects {
    pub mod finite_stochastic_language_semantics;
    pub mod stochastic_deterministic_finite_automaton;
    pub mod stochastic_deterministic_finite_automaton_semantics;
    pub mod event_log;
    pub mod finite_language;
    pub mod compressed_event_log;
    pub mod labelled_petri_net;
    pub mod labelled_petri_net_semantics;
    pub mod stochastic_labelled_petri_net;
    pub mod stochastic_labelled_petri_net_semantics;
    pub mod finite_stochastic_language;
    pub mod directly_follows_model;
    pub mod petri_net_markup_language;
    pub mod alignments;
    pub mod deterministic_finite_automaton;
    pub mod deterministic_finite_automaton_semantics;
}
pub mod ebi_traits {
    pub mod ebi_trait_stochastic_deterministic_semantics;
    pub mod ebi_trait_queriable_stochastic_language;
    pub mod ebi_trait_finite_stochastic_language;
    pub mod ebi_trait_iterable_stochastic_language;
    pub mod ebi_trait_iterable_language;
    pub mod ebi_trait_finite_language;
    pub mod ebi_trait_event_log;
    pub mod ebi_trait_stochastic_semantics;
    pub mod ebi_trait_semantics;
}
pub mod math {
    pub mod astar;
    pub mod average;
    pub mod fraction;
    pub mod fraction_matched;
    pub mod fraction_raw;
    pub mod log_div;
    pub mod matrix;
    pub mod root;
    pub mod root_log_div;
    pub mod correlation;
    pub mod levenshtein;
}
pub mod techniques {
    pub mod align;
    pub mod alignment_stochastic_miner;
    pub mod association;
    pub mod probabilistic_queries;
    pub mod completeness;
    pub mod jensen_shannon_stochastic_conformance;
    pub mod uniform_stochastic_miner;
    pub mod unit_earth_movers_stochastic_conformance;
    pub mod entropic_relevance;
    pub mod occurrences_stochastic_miner;
    pub mod trace_probability;
    pub mod statistical_test;
    pub mod medoid_non_stochastic;
    pub mod sample;
    pub mod explain_trace;
}
pub mod deterministic_semantics_for_stochastic_semantics;
pub mod medoid;
pub mod distances;
pub mod tests;
pub mod text;
pub mod multiple_reader;
pub mod json;
pub mod line_reader;
pub mod follower_semantics;
pub mod marking;

use jni::JNIEnv;

// These objects are what you should use as arguments to your native
// function. They carry extra lifetime information to prevent them escaping
// this context and getting used after being GC'd.
use jni::objects::{JClass, JString};

// This is just a pointer. We'll be returning it from our function. We
// can't return one of the objects with lifetime information because the
// lifetime checker won't let us.
use jni::sys::jstring;

// This keeps Rust from "mangling" the name and making it unique for this
// crate.
#[no_mangle]
pub extern "system" fn Java_org_processmining_ebi_plugins_EbiPlugin_call_1ebi<'local>(mut env: JNIEnv<'local>,
// This is the class that owns our static method. It's not going to be used,
// but still must be present to match the expected signature of a static
// native method.
    _class: JClass<'local>,
    input: JString<'local>)
    -> jstring {
                                                        
    // First, we have to get the string out of Java. Check out the `strings`
    // module for more info on how this works.
    let input: String = env.get_string(&input).expect("Couldn't get java string!").into();

    // Then we have to create a new Java string to return. Again, more info
    // in the `strings` module.
    let output = env.new_string(format!("Hello, {}!", input)).expect("Couldn't create java string!");

    // Finally, extract the raw pointer to return.
    output.into_raw()
}