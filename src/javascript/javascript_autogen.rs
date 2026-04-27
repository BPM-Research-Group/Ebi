// This file has been automatically generated. Manual changes will be overridden.

use crate::{
    ebi_framework::ebi_command::EbiCommand, javascript::javascript_link::execute_javascript_command,
};
use wasm_bindgen::prelude::*;

#[wasm_bindgen]
pub fn analyse_all_traces(string_inputs: Vec<String>) {
    ebi_objects::ebi_arithmetic::exact::set_exact_globally(true);
    let command: &&EbiCommand = 
        &&crate::ebi_commands::ebi_command_analyse::EBI_ANALYSE_ALL;
    execute_javascript_command(command, string_inputs, "analyse_all_traces");
}

#[wasm_bindgen]
pub fn analyse_completeness(string_inputs: Vec<String>) {
    ebi_objects::ebi_arithmetic::exact::set_exact_globally(true);
    let command: &&EbiCommand = 
        &&crate::ebi_commands::ebi_command_analyse::EBI_ANALYSE_COMPLETENESS;
    execute_javascript_command(command, string_inputs, "analyse_completeness");
}

#[wasm_bindgen]
pub fn analyse_coverage(string_inputs: Vec<String>) {
    ebi_objects::ebi_arithmetic::exact::set_exact_globally(true);
    let command: &&EbiCommand = 
        &&crate::ebi_commands::ebi_command_analyse::EBI_ANALYSE_COVERAGE;
    execute_javascript_command(command, string_inputs, "analyse_coverage");
}

#[wasm_bindgen]
pub fn analyse_directly_follows_edge_difference(string_inputs: Vec<String>) {
    ebi_objects::ebi_arithmetic::exact::set_exact_globally(true);
    let command: &&EbiCommand = 
        &&crate::ebi_commands::ebi_command_analyse::EBI_ANALYSE_DIRECTLY_FOLLOWS_EDGE_DIFFERENCE;
    execute_javascript_command(command, string_inputs, "analyse_directly_follows_edge_difference");
}

#[wasm_bindgen]
pub fn analyse_medoid(string_inputs: Vec<String>) {
    ebi_objects::ebi_arithmetic::exact::set_exact_globally(true);
    let command: &&EbiCommand = 
        &&crate::ebi_commands::ebi_command_analyse::EBI_ANALYSE_MEDOID;
    execute_javascript_command(command, string_inputs, "analyse_medoid");
}

#[wasm_bindgen]
pub fn analyse_minimum_probability_traces(string_inputs: Vec<String>) {
    ebi_objects::ebi_arithmetic::exact::set_exact_globally(true);
    let command: &&EbiCommand = 
        &&crate::ebi_commands::ebi_command_analyse::EBI_ANALYSE_MINPROB;
    execute_javascript_command(command, string_inputs, "analyse_minimum_probability_traces");
}

#[wasm_bindgen]
pub fn analyse_mode(string_inputs: Vec<String>) {
    ebi_objects::ebi_arithmetic::exact::set_exact_globally(true);
    let command: &&EbiCommand = 
        &&crate::ebi_commands::ebi_command_analyse::EBI_ANALYSE_MODE;
    execute_javascript_command(command, string_inputs, "analyse_mode");
}

#[wasm_bindgen]
pub fn analyse_most_likely_traces(string_inputs: Vec<String>) {
    ebi_objects::ebi_arithmetic::exact::set_exact_globally(true);
    let command: &&EbiCommand = 
        &&crate::ebi_commands::ebi_command_analyse::EBI_ANALYSE_MOSTLIKELY;
    execute_javascript_command(command, string_inputs, "analyse_most_likely_traces");
}

#[wasm_bindgen]
pub fn analyse_variety(string_inputs: Vec<String>) {
    ebi_objects::ebi_arithmetic::exact::set_exact_globally(true);
    let command: &&EbiCommand = 
        &&crate::ebi_commands::ebi_command_analyse::EBI_ANALYSE_VARIETY;
    execute_javascript_command(command, string_inputs, "analyse_variety");
}

#[wasm_bindgen]
pub fn analyse_non_stochastic_activities(string_inputs: Vec<String>) {
    ebi_objects::ebi_arithmetic::exact::set_exact_globally(true);
    let command: &&EbiCommand = 
        &&crate::ebi_commands::ebi_command_analyse_non_stochastic::EBI_ANALYSE_NON_STOCHASTIC_ACTIVITIES;
    execute_javascript_command(command, string_inputs, "analyse_non_stochastic_activities");
}

#[wasm_bindgen]
pub fn analyse_non_stochastic_any_traces(string_inputs: Vec<String>) {
    ebi_objects::ebi_arithmetic::exact::set_exact_globally(true);
    let command: &&EbiCommand = 
        &&crate::ebi_commands::ebi_command_analyse_non_stochastic::EBI_ANALYSE_NON_STOCHASTIC_ANY_TRACES;
    execute_javascript_command(command, string_inputs, "analyse_non_stochastic_any_traces");
}

#[wasm_bindgen]
pub fn analyse_non_stochastic_bounded(string_inputs: Vec<String>) {
    ebi_objects::ebi_arithmetic::exact::set_exact_globally(true);
    let command: &&EbiCommand = 
        &&crate::ebi_commands::ebi_command_analyse_non_stochastic::EBI_ANALYSE_NON_STOCHASTIC_BOUNDED;
    execute_javascript_command(command, string_inputs, "analyse_non_stochastic_bounded");
}

#[wasm_bindgen]
pub fn analyse_non_stochastic_cluster(string_inputs: Vec<String>) {
    ebi_objects::ebi_arithmetic::exact::set_exact_globally(true);
    let command: &&EbiCommand = 
        &&crate::ebi_commands::ebi_command_analyse_non_stochastic::EBI_ANALYSE_NON_STOCHASTIC_CLUSTER;
    execute_javascript_command(command, string_inputs, "analyse_non_stochastic_cluster");
}

#[wasm_bindgen]
pub fn analyse_non_stochastic_executions(string_inputs: Vec<String>) {
    ebi_objects::ebi_arithmetic::exact::set_exact_globally(true);
    let command: &&EbiCommand = 
        &&crate::ebi_commands::ebi_command_analyse_non_stochastic::EBI_ANALYSE_NON_STOCHASTIC_EXECUTIONS;
    execute_javascript_command(command, string_inputs, "analyse_non_stochastic_executions");
}

#[wasm_bindgen]
pub fn analyse_non_stochastic_infinitely_many_traces(string_inputs: Vec<String>) {
    ebi_objects::ebi_arithmetic::exact::set_exact_globally(true);
    let command: &&EbiCommand = 
        &&crate::ebi_commands::ebi_command_analyse_non_stochastic::EBI_ANALYSE_NON_STOCHASTIC_INFINITELY_MANY_TRACES;
    execute_javascript_command(command, string_inputs, "analyse_non_stochastic_infinitely_many_traces");
}

#[wasm_bindgen]
pub fn analyse_non_stochastic_medoid(string_inputs: Vec<String>) {
    ebi_objects::ebi_arithmetic::exact::set_exact_globally(true);
    let command: &&EbiCommand = 
        &&crate::ebi_commands::ebi_command_analyse_non_stochastic::EBI_ANALYSE_NON_STOCHASTIC_MEDOID;
    execute_javascript_command(command, string_inputs, "analyse_non_stochastic_medoid");
}

#[wasm_bindgen]
pub fn association_all_trace_attributes(string_inputs: Vec<String>) {
    ebi_objects::ebi_arithmetic::exact::set_exact_globally(true);
    let command: &&EbiCommand = 
        &&crate::ebi_commands::ebi_command_association::ASSOCIATION_ATTRIBUTES;
    execute_javascript_command(command, string_inputs, "association_all_trace_attributes");
}

#[wasm_bindgen]
pub fn association_trace_attribute(string_inputs: Vec<String>) {
    ebi_objects::ebi_arithmetic::exact::set_exact_globally(true);
    let command: &&EbiCommand = 
        &&crate::ebi_commands::ebi_command_association::ASSOCIATION_ATTRIBUTE;
    execute_javascript_command(command, string_inputs, "association_trace_attribute");
}

#[wasm_bindgen]
pub fn conformance_chi_squared(string_inputs: Vec<String>) {
    ebi_objects::ebi_arithmetic::exact::set_exact_globally(true);
    let command: &&EbiCommand = 
        &&crate::ebi_commands::ebi_command_conformance::CONFORMANCE_CSSC;
    execute_javascript_command(command, string_inputs, "conformance_chi_squared");
}

#[wasm_bindgen]
pub fn conformance_chi_squared_sample(string_inputs: Vec<String>) {
    ebi_objects::ebi_arithmetic::exact::set_exact_globally(true);
    let command: &&EbiCommand = 
        &&crate::ebi_commands::ebi_command_conformance::CONFORMANCE_CSSC_SAMPLE;
    execute_javascript_command(command, string_inputs, "conformance_chi_squared_sample");
}

#[wasm_bindgen]
pub fn conformance_earth_movers(string_inputs: Vec<String>) {
    ebi_objects::ebi_arithmetic::exact::set_exact_globally(true);
    let command: &&EbiCommand = 
        &&crate::ebi_commands::ebi_command_conformance::CONFORMANCE_EMSC;
    execute_javascript_command(command, string_inputs, "conformance_earth_movers");
}

#[wasm_bindgen]
pub fn conformance_earth_movers_sample(string_inputs: Vec<String>) {
    ebi_objects::ebi_arithmetic::exact::set_exact_globally(true);
    let command: &&EbiCommand = 
        &&crate::ebi_commands::ebi_command_conformance::CONFORMANCE_EMSC_SAMPLE;
    execute_javascript_command(command, string_inputs, "conformance_earth_movers_sample");
}

#[wasm_bindgen]
pub fn conformance_entropic_relevance(string_inputs: Vec<String>) {
    ebi_objects::ebi_arithmetic::exact::set_exact_globally(false);
    let command: &&EbiCommand = 
        &&crate::ebi_commands::ebi_command_conformance::CONFORMANCE_ER;
    execute_javascript_command(command, string_inputs, "conformance_entropic_relevance");
}

#[wasm_bindgen]
pub fn conformance_hellinger(string_inputs: Vec<String>) {
    ebi_objects::ebi_arithmetic::exact::set_exact_globally(true);
    let command: &&EbiCommand = 
        &&crate::ebi_commands::ebi_command_conformance::CONFORMANCE_HSC;
    execute_javascript_command(command, string_inputs, "conformance_hellinger");
}

#[wasm_bindgen]
pub fn conformance_hellinger_sample(string_inputs: Vec<String>) {
    ebi_objects::ebi_arithmetic::exact::set_exact_globally(true);
    let command: &&EbiCommand = 
        &&crate::ebi_commands::ebi_command_conformance::CONFORMANCE_HSC_SAMPLE;
    execute_javascript_command(command, string_inputs, "conformance_hellinger_sample");
}

#[wasm_bindgen]
pub fn conformance_jensen_shannon(string_inputs: Vec<String>) {
    ebi_objects::ebi_arithmetic::exact::set_exact_globally(false);
    let command: &&EbiCommand = 
        &&crate::ebi_commands::ebi_command_conformance::CONFORMANCE_JSSC;
    execute_javascript_command(command, string_inputs, "conformance_jensen_shannon");
}

#[wasm_bindgen]
pub fn conformance_jensen_shannon_sample(string_inputs: Vec<String>) {
    ebi_objects::ebi_arithmetic::exact::set_exact_globally(false);
    let command: &&EbiCommand = 
        &&crate::ebi_commands::ebi_command_conformance::CONFORMANCE_JSSC_SAMPLE;
    execute_javascript_command(command, string_inputs, "conformance_jensen_shannon_sample");
}

#[wasm_bindgen]
pub fn conformance_markovian(string_inputs: Vec<String>) {
    ebi_objects::ebi_arithmetic::exact::set_exact_globally(true);
    let command: &&EbiCommand = 
        &&crate::ebi_commands::ebi_command_conformance::CONFORMANCE_MARKOVIAN;
    execute_javascript_command(command, string_inputs, "conformance_markovian");
}

#[wasm_bindgen]
pub fn conformance_unit_earth_movers(string_inputs: Vec<String>) {
    ebi_objects::ebi_arithmetic::exact::set_exact_globally(true);
    let command: &&EbiCommand = 
        &&crate::ebi_commands::ebi_command_conformance::CONFORMANCE_UEMSC;
    execute_javascript_command(command, string_inputs, "conformance_unit_earth_movers");
}

#[wasm_bindgen]
pub fn conformance_unit_earth_movers_sample(string_inputs: Vec<String>) {
    ebi_objects::ebi_arithmetic::exact::set_exact_globally(false);
    let command: &&EbiCommand = 
        &&crate::ebi_commands::ebi_command_conformance::CONFORMANCE_UEMSC_SAMPLE;
    execute_javascript_command(command, string_inputs, "conformance_unit_earth_movers_sample");
}

#[wasm_bindgen]
pub fn conformance_non_stochastic_alignments(string_inputs: Vec<String>) {
    ebi_objects::ebi_arithmetic::exact::set_exact_globally(true);
    let command: &&EbiCommand = 
        &&crate::ebi_commands::ebi_command_conformance_non_stochastic::EBI_CONFORMANCE_NON_STOCHASTIC_ALIGNMENTS;
    execute_javascript_command(command, string_inputs, "conformance_non_stochastic_alignments");
}

#[wasm_bindgen]
pub fn conformance_non_stochastic_escaping_edges_precision(string_inputs: Vec<String>) {
    ebi_objects::ebi_arithmetic::exact::set_exact_globally(true);
    let command: &&EbiCommand = 
        &&crate::ebi_commands::ebi_command_conformance_non_stochastic::EBI_CONFORMANCE_NON_STOCHASTIC_ESCAPING_EDGES_PRECISION;
    execute_javascript_command(command, string_inputs, "conformance_non_stochastic_escaping_edges_precision");
}

#[wasm_bindgen]
pub fn conformance_non_stochastic_set_alignments(string_inputs: Vec<String>) {
    ebi_objects::ebi_arithmetic::exact::set_exact_globally(true);
    let command: &&EbiCommand = 
        &&crate::ebi_commands::ebi_command_conformance_non_stochastic::EBI_CONFORMANCE_NON_STOCHASTIC_SET_ALIGNMENTS;
    execute_javascript_command(command, string_inputs, "conformance_non_stochastic_set_alignments");
}

#[wasm_bindgen]
pub fn conformance_non_stochastic_trace_fitness(string_inputs: Vec<String>) {
    ebi_objects::ebi_arithmetic::exact::set_exact_globally(true);
    let command: &&EbiCommand = 
        &&crate::ebi_commands::ebi_command_conformance_non_stochastic::EBI_CONFORMANCE_NON_STOCHASTIC_TRACE_FITNESS;
    execute_javascript_command(command, string_inputs, "conformance_non_stochastic_trace_fitness");
}

#[wasm_bindgen]
pub fn convert_business_process_model_and_notation(string_inputs: Vec<String>) {
    ebi_objects::ebi_arithmetic::exact::set_exact_globally(true);
    let command: &&EbiCommand = 
        &&crate::ebi_commands::ebi_command_convert::EBI_CONVERT_BPMN;
    execute_javascript_command(command, string_inputs, "convert_business_process_model_and_notation");
}

#[wasm_bindgen]
pub fn convert_finite_language(string_inputs: Vec<String>) {
    ebi_objects::ebi_arithmetic::exact::set_exact_globally(true);
    let command: &&EbiCommand = 
        &&crate::ebi_commands::ebi_command_convert::EBI_CONVERT_LANG;
    execute_javascript_command(command, string_inputs, "convert_finite_language");
}

#[wasm_bindgen]
pub fn convert_finite_stochastic_language(string_inputs: Vec<String>) {
    ebi_objects::ebi_arithmetic::exact::set_exact_globally(true);
    let command: &&EbiCommand = 
        &&crate::ebi_commands::ebi_command_convert::EBI_CONVERT_SLANG;
    execute_javascript_command(command, string_inputs, "convert_finite_stochastic_language");
}

#[wasm_bindgen]
pub fn convert_labelled_petri_net(string_inputs: Vec<String>) {
    ebi_objects::ebi_arithmetic::exact::set_exact_globally(true);
    let command: &&EbiCommand = 
        &&crate::ebi_commands::ebi_command_convert::EBI_CONVERT_LPN;
    execute_javascript_command(command, string_inputs, "convert_labelled_petri_net");
}

#[wasm_bindgen]
pub fn convert_log(string_inputs: Vec<String>) {
    ebi_objects::ebi_arithmetic::exact::set_exact_globally(true);
    let command: &&EbiCommand = 
        &&crate::ebi_commands::ebi_command_convert::EBI_CONVERT_LOG;
    execute_javascript_command(command, string_inputs, "convert_log");
}

#[wasm_bindgen]
pub fn convert_stochastic_deterministic_finite_automaton(string_inputs: Vec<String>) {
    ebi_objects::ebi_arithmetic::exact::set_exact_globally(true);
    let command: &&EbiCommand = 
        &&crate::ebi_commands::ebi_command_convert::EBI_CONVERT_SDFA;
    execute_javascript_command(command, string_inputs, "convert_stochastic_deterministic_finite_automaton");
}

#[wasm_bindgen]
pub fn convert_stochastic_labelled_petri_net(string_inputs: Vec<String>) {
    ebi_objects::ebi_arithmetic::exact::set_exact_globally(true);
    let command: &&EbiCommand = 
        &&crate::ebi_commands::ebi_command_convert::EBI_CONVERT_SLPN;
    execute_javascript_command(command, string_inputs, "convert_stochastic_labelled_petri_net");
}

#[wasm_bindgen]
pub fn convert_stochastic_nondeterministic_finite_automaton(string_inputs: Vec<String>) {
    ebi_objects::ebi_arithmetic::exact::set_exact_globally(true);
    let command: &&EbiCommand = 
        &&crate::ebi_commands::ebi_command_convert::EBI_CONVERT_SNFA;
    execute_javascript_command(command, string_inputs, "convert_stochastic_nondeterministic_finite_automaton");
}

#[wasm_bindgen]
pub fn discover_alignments_stochastic_business_process_model_and_notation(string_inputs: Vec<String>) {
    ebi_objects::ebi_arithmetic::exact::set_exact_globally(true);
    let command: &&EbiCommand = 
        &&crate::ebi_commands::ebi_command_discover::EBI_DISCOVER_ALIGNMENTS_BPMN;
    execute_javascript_command(command, string_inputs, "discover_alignments_stochastic_business_process_model_and_notation");
}

#[wasm_bindgen]
pub fn discover_alignments_stochastic_labelled_petri_nets(string_inputs: Vec<String>) {
    ebi_objects::ebi_arithmetic::exact::set_exact_globally(true);
    let command: &&EbiCommand = 
        &&crate::ebi_commands::ebi_command_discover::EBI_DISCOVER_ALIGNMENTS_SLPN;
    execute_javascript_command(command, string_inputs, "discover_alignments_stochastic_labelled_petri_nets");
}

#[wasm_bindgen]
pub fn discover_directly_follows_graph(string_inputs: Vec<String>) {
    ebi_objects::ebi_arithmetic::exact::set_exact_globally(true);
    let command: &&EbiCommand = 
        &&crate::ebi_commands::ebi_command_discover::EBI_DISCOVER_DIRECTLY_FOLLOWS;
    execute_javascript_command(command, string_inputs, "discover_directly_follows_graph");
}

#[wasm_bindgen]
pub fn discover_occurrence_stochastic_business_process_model_and_notation(string_inputs: Vec<String>) {
    ebi_objects::ebi_arithmetic::exact::set_exact_globally(true);
    let command: &&EbiCommand = 
        &&crate::ebi_commands::ebi_command_discover::EBI_DISCOVER_OCCURRENCE_SBPMN;
    execute_javascript_command(command, string_inputs, "discover_occurrence_stochastic_business_process_model_and_notation");
}

#[wasm_bindgen]
pub fn discover_occurrence_stochastic_labelled_petri_net(string_inputs: Vec<String>) {
    ebi_objects::ebi_arithmetic::exact::set_exact_globally(true);
    let command: &&EbiCommand = 
        &&crate::ebi_commands::ebi_command_discover::EBI_DISCOVER_OCCURRENCE_SLPN;
    execute_javascript_command(command, string_inputs, "discover_occurrence_stochastic_labelled_petri_net");
}

#[wasm_bindgen]
pub fn discover_occurrence_stochastic_process_tree(string_inputs: Vec<String>) {
    ebi_objects::ebi_arithmetic::exact::set_exact_globally(true);
    let command: &&EbiCommand = 
        &&crate::ebi_commands::ebi_command_discover::EBI_DISCOVER_OCCURRENCE_SPTREE;
    execute_javascript_command(command, string_inputs, "discover_occurrence_stochastic_process_tree");
}

#[wasm_bindgen]
pub fn discover_random_stochastic_business_process_model_and_notation(string_inputs: Vec<String>) {
    ebi_objects::ebi_arithmetic::exact::set_exact_globally(true);
    let command: &&EbiCommand = 
        &&crate::ebi_commands::ebi_command_discover::EBI_DISCOVER_RANDOM_SBPMN;
    execute_javascript_command(command, string_inputs, "discover_random_stochastic_business_process_model_and_notation");
}

#[wasm_bindgen]
pub fn discover_random_stochastic_labelled_petri_net(string_inputs: Vec<String>) {
    ebi_objects::ebi_arithmetic::exact::set_exact_globally(true);
    let command: &&EbiCommand = 
        &&crate::ebi_commands::ebi_command_discover::EBI_DISCOVER_RANDOM_SLPN;
    execute_javascript_command(command, string_inputs, "discover_random_stochastic_labelled_petri_net");
}

#[wasm_bindgen]
pub fn discover_random_stochastic_process_tree(string_inputs: Vec<String>) {
    ebi_objects::ebi_arithmetic::exact::set_exact_globally(true);
    let command: &&EbiCommand = 
        &&crate::ebi_commands::ebi_command_discover::EBI_DISCOVER_RANDOM_SPTREE;
    execute_javascript_command(command, string_inputs, "discover_random_stochastic_process_tree");
}

#[wasm_bindgen]
pub fn discover_uniform_stochastic_business_process_model_and_notation(string_inputs: Vec<String>) {
    ebi_objects::ebi_arithmetic::exact::set_exact_globally(true);
    let command: &&EbiCommand = 
        &&crate::ebi_commands::ebi_command_discover::EBI_DISCOVER_UNIFORM_SBPMN;
    execute_javascript_command(command, string_inputs, "discover_uniform_stochastic_business_process_model_and_notation");
}

#[wasm_bindgen]
pub fn discover_uniform_stochastic_labelled_petri_net(string_inputs: Vec<String>) {
    ebi_objects::ebi_arithmetic::exact::set_exact_globally(true);
    let command: &&EbiCommand = 
        &&crate::ebi_commands::ebi_command_discover::EBI_DISCOVER_UNIFORM_SLPN;
    execute_javascript_command(command, string_inputs, "discover_uniform_stochastic_labelled_petri_net");
}

#[wasm_bindgen]
pub fn discover_uniform_stochastic_process_tree(string_inputs: Vec<String>) {
    ebi_objects::ebi_arithmetic::exact::set_exact_globally(true);
    let command: &&EbiCommand = 
        &&crate::ebi_commands::ebi_command_discover::EBI_DISCOVER_UNIFORM_SPTREE;
    execute_javascript_command(command, string_inputs, "discover_uniform_stochastic_process_tree");
}

#[wasm_bindgen]
pub fn discover_non_stochastic_flower_deterministic_finite_automaton(string_inputs: Vec<String>) {
    ebi_objects::ebi_arithmetic::exact::set_exact_globally(true);
    let command: &&EbiCommand = 
        &&crate::ebi_commands::ebi_command_discover_non_stochastic::EBI_DISCOVER_NON_STOCHASTIC_FLOWER_DFA;
    execute_javascript_command(command, string_inputs, "discover_non_stochastic_flower_deterministic_finite_automaton");
}

#[wasm_bindgen]
pub fn discover_non_stochastic_flower_process_tree(string_inputs: Vec<String>) {
    ebi_objects::ebi_arithmetic::exact::set_exact_globally(true);
    let command: &&EbiCommand = 
        &&crate::ebi_commands::ebi_command_discover_non_stochastic::EBI_DISCOVER_NON_STOCHASTIC_FLOWER_TREE;
    execute_javascript_command(command, string_inputs, "discover_non_stochastic_flower_process_tree");
}

#[wasm_bindgen]
pub fn discover_non_stochastic_prefix_tree_deterministic_finite_automaton(string_inputs: Vec<String>) {
    ebi_objects::ebi_arithmetic::exact::set_exact_globally(true);
    let command: &&EbiCommand = 
        &&crate::ebi_commands::ebi_command_discover_non_stochastic::EBI_DISCOVER_NON_STOCHASTIC_TREE_DFA;
    execute_javascript_command(command, string_inputs, "discover_non_stochastic_prefix_tree_deterministic_finite_automaton");
}

#[wasm_bindgen]
pub fn discover_non_stochastic_prefix_tree_process_tree(string_inputs: Vec<String>) {
    ebi_objects::ebi_arithmetic::exact::set_exact_globally(true);
    let command: &&EbiCommand = 
        &&crate::ebi_commands::ebi_command_discover_non_stochastic::EBI_DISCOVER_NON_STOCHASTIC_TREE_TREE;
    execute_javascript_command(command, string_inputs, "discover_non_stochastic_prefix_tree_process_tree");
}

#[wasm_bindgen]
pub fn filter_traces_empty(string_inputs: Vec<String>) {
    ebi_objects::ebi_arithmetic::exact::set_exact_globally(true);
    let command: &&EbiCommand = 
        &&crate::ebi_commands::ebi_command_filter::EBI_FILTER_TRACES_EMPTY;
    execute_javascript_command(command, string_inputs, "filter_traces_empty");
}

#[wasm_bindgen]
pub fn filter_traces_event_activity(string_inputs: Vec<String>) {
    ebi_objects::ebi_arithmetic::exact::set_exact_globally(true);
    let command: &&EbiCommand = 
        &&crate::ebi_commands::ebi_command_filter::EBI_FILTER_TRACES_EVENT_ACTIVITY;
    execute_javascript_command(command, string_inputs, "filter_traces_event_activity");
}

#[wasm_bindgen]
pub fn filter_traces_length(string_inputs: Vec<String>) {
    ebi_objects::ebi_arithmetic::exact::set_exact_globally(true);
    let command: &&EbiCommand = 
        &&crate::ebi_commands::ebi_command_filter::EBI_FILTER_TRACES_LENGTH;
    execute_javascript_command(command, string_inputs, "filter_traces_length");
}

#[wasm_bindgen]
pub fn information(string_inputs: Vec<String>) {
    ebi_objects::ebi_arithmetic::exact::set_exact_globally(true);
    let command: &&EbiCommand = 
        &&crate::ebi_commands::ebi_command_info::EBI_INFO;
    execute_javascript_command(command, string_inputs, "information");
}

#[wasm_bindgen]
pub fn probability_log(string_inputs: Vec<String>) {
    ebi_objects::ebi_arithmetic::exact::set_exact_globally(true);
    let command: &&EbiCommand = 
        &&crate::ebi_commands::ebi_command_probability::EBI_PROBABILITY_LOG;
    execute_javascript_command(command, string_inputs, "probability_log");
}

#[wasm_bindgen]
pub fn sample_folds(string_inputs: Vec<String>) {
    ebi_objects::ebi_arithmetic::exact::set_exact_globally(true);
    let command: &&EbiCommand = 
        &&crate::ebi_commands::ebi_command_sample::EBI_SAMPLE_FOLDS;
    execute_javascript_command(command, string_inputs, "sample_folds");
}

#[wasm_bindgen]
pub fn sample_partially_ordered_traces(string_inputs: Vec<String>) {
    ebi_objects::ebi_arithmetic::exact::set_exact_globally(true);
    let command: &&EbiCommand = 
        &&crate::ebi_commands::ebi_command_sample::EBI_SAMPLE_PARTIALLY_ORDERED_TRACES;
    execute_javascript_command(command, string_inputs, "sample_partially_ordered_traces");
}

#[wasm_bindgen]
pub fn sample_traces(string_inputs: Vec<String>) {
    ebi_objects::ebi_arithmetic::exact::set_exact_globally(true);
    let command: &&EbiCommand = 
        &&crate::ebi_commands::ebi_command_sample::EBI_SAMPLE_TRACES;
    execute_javascript_command(command, string_inputs, "sample_traces");
}

#[wasm_bindgen]
pub fn test_bootstrap_test(string_inputs: Vec<String>) {
    ebi_objects::ebi_arithmetic::exact::set_exact_globally(true);
    let command: &&EbiCommand = 
        &&crate::ebi_commands::ebi_command_test::EBI_BOOTSTRAP_TEST;
    execute_javascript_command(command, string_inputs, "test_bootstrap_test");
}

#[wasm_bindgen]
pub fn test_log_categorical_attribute(string_inputs: Vec<String>) {
    ebi_objects::ebi_arithmetic::exact::set_exact_globally(true);
    let command: &&EbiCommand = 
        &&crate::ebi_commands::ebi_command_test::EBI_TEST_LOG_ATTRIBUTE;
    execute_javascript_command(command, string_inputs, "test_log_categorical_attribute");
}

#[wasm_bindgen]
pub fn visualise_graph(string_inputs: Vec<String>) {
    ebi_objects::ebi_arithmetic::exact::set_exact_globally(true);
    let command: &&EbiCommand = 
        &&crate::ebi_commands::ebi_command_visualise::EBI_VISUALISE_GRAPH;
    execute_javascript_command(command, string_inputs, "visualise_graph");
}

#[wasm_bindgen]
pub fn visualise_text(string_inputs: Vec<String>) {
    ebi_objects::ebi_arithmetic::exact::set_exact_globally(false);
    let command: &&EbiCommand = 
        &&crate::ebi_commands::ebi_command_visualise::EBI_VISUALISE_TEXT;
    execute_javascript_command(command, string_inputs, "visualise_text");
}
