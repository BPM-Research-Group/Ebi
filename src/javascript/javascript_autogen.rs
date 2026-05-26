// This file has been automatically generated. Manual changes will be overridden.

use crate::{
    ebi_framework::ebi_command::EbiCommand, javascript::javascript_link::{execute_javascript_command, JavascriptInput}
};
use wasm_bindgen::prelude::*;

#[wasm_bindgen]
pub fn analyse_all_traces(javascript_inputs: Vec<JavascriptInput>, exporter_file_extension: &str) {
    ebi_objects::ebi_arithmetic::exact::set_exact_globally(true);
    let command: &&EbiCommand = 
        &&crate::ebi_commands::ebi_command_analyse::EBI_ANALYSE_ALL;
    execute_javascript_command(command, javascript_inputs, "analyse_all_traces", exporter_file_extension);
}

#[wasm_bindgen]
pub fn analyse_completeness(javascript_inputs: Vec<JavascriptInput>, exporter_file_extension: &str) {
    ebi_objects::ebi_arithmetic::exact::set_exact_globally(true);
    let command: &&EbiCommand = 
        &&crate::ebi_commands::ebi_command_analyse::EBI_ANALYSE_COMPLETENESS;
    execute_javascript_command(command, javascript_inputs, "analyse_completeness", exporter_file_extension);
}

#[wasm_bindgen]
pub fn analyse_coverage(javascript_inputs: Vec<JavascriptInput>, exporter_file_extension: &str) {
    ebi_objects::ebi_arithmetic::exact::set_exact_globally(true);
    let command: &&EbiCommand = 
        &&crate::ebi_commands::ebi_command_analyse::EBI_ANALYSE_COVERAGE;
    execute_javascript_command(command, javascript_inputs, "analyse_coverage", exporter_file_extension);
}

#[wasm_bindgen]
pub fn analyse_directly_follows_edge_difference(javascript_inputs: Vec<JavascriptInput>, exporter_file_extension: &str) {
    ebi_objects::ebi_arithmetic::exact::set_exact_globally(true);
    let command: &&EbiCommand = 
        &&crate::ebi_commands::ebi_command_analyse::EBI_ANALYSE_DIRECTLY_FOLLOWS_EDGE_DIFFERENCE;
    execute_javascript_command(command, javascript_inputs, "analyse_directly_follows_edge_difference", exporter_file_extension);
}

#[wasm_bindgen]
pub fn analyse_entropy(javascript_inputs: Vec<JavascriptInput>, exporter_file_extension: &str) {
    ebi_objects::ebi_arithmetic::exact::set_exact_globally(true);
    let command: &&EbiCommand = 
        &&crate::ebi_commands::ebi_command_analyse::EBI_ANALYSE_ENTROPY;
    execute_javascript_command(command, javascript_inputs, "analyse_entropy", exporter_file_extension);
}

#[wasm_bindgen]
pub fn analyse_medoid(javascript_inputs: Vec<JavascriptInput>, exporter_file_extension: &str) {
    ebi_objects::ebi_arithmetic::exact::set_exact_globally(true);
    let command: &&EbiCommand = 
        &&crate::ebi_commands::ebi_command_analyse::EBI_ANALYSE_MEDOID;
    execute_javascript_command(command, javascript_inputs, "analyse_medoid", exporter_file_extension);
}

#[wasm_bindgen]
pub fn analyse_minimum_probability_traces(javascript_inputs: Vec<JavascriptInput>, exporter_file_extension: &str) {
    ebi_objects::ebi_arithmetic::exact::set_exact_globally(true);
    let command: &&EbiCommand = 
        &&crate::ebi_commands::ebi_command_analyse::EBI_ANALYSE_MINPROB;
    execute_javascript_command(command, javascript_inputs, "analyse_minimum_probability_traces", exporter_file_extension);
}

#[wasm_bindgen]
pub fn analyse_mode(javascript_inputs: Vec<JavascriptInput>, exporter_file_extension: &str) {
    ebi_objects::ebi_arithmetic::exact::set_exact_globally(true);
    let command: &&EbiCommand = 
        &&crate::ebi_commands::ebi_command_analyse::EBI_ANALYSE_MODE;
    execute_javascript_command(command, javascript_inputs, "analyse_mode", exporter_file_extension);
}

#[wasm_bindgen]
pub fn analyse_most_likely_traces(javascript_inputs: Vec<JavascriptInput>, exporter_file_extension: &str) {
    ebi_objects::ebi_arithmetic::exact::set_exact_globally(true);
    let command: &&EbiCommand = 
        &&crate::ebi_commands::ebi_command_analyse::EBI_ANALYSE_MOSTLIKELY;
    execute_javascript_command(command, javascript_inputs, "analyse_most_likely_traces", exporter_file_extension);
}

#[wasm_bindgen]
pub fn analyse_variety(javascript_inputs: Vec<JavascriptInput>, exporter_file_extension: &str) {
    ebi_objects::ebi_arithmetic::exact::set_exact_globally(true);
    let command: &&EbiCommand = 
        &&crate::ebi_commands::ebi_command_analyse::EBI_ANALYSE_VARIETY;
    execute_javascript_command(command, javascript_inputs, "analyse_variety", exporter_file_extension);
}

#[wasm_bindgen]
pub fn analyse_non_stochastic_activities(javascript_inputs: Vec<JavascriptInput>, exporter_file_extension: &str) {
    ebi_objects::ebi_arithmetic::exact::set_exact_globally(true);
    let command: &&EbiCommand = 
        &&crate::ebi_commands::ebi_command_analyse_non_stochastic::EBI_ANALYSE_NON_STOCHASTIC_ACTIVITIES;
    execute_javascript_command(command, javascript_inputs, "analyse_non_stochastic_activities", exporter_file_extension);
}

#[wasm_bindgen]
pub fn analyse_non_stochastic_any_traces(javascript_inputs: Vec<JavascriptInput>, exporter_file_extension: &str) {
    ebi_objects::ebi_arithmetic::exact::set_exact_globally(true);
    let command: &&EbiCommand = 
        &&crate::ebi_commands::ebi_command_analyse_non_stochastic::EBI_ANALYSE_NON_STOCHASTIC_ANY_TRACES;
    execute_javascript_command(command, javascript_inputs, "analyse_non_stochastic_any_traces", exporter_file_extension);
}

#[wasm_bindgen]
pub fn analyse_non_stochastic_bounded(javascript_inputs: Vec<JavascriptInput>, exporter_file_extension: &str) {
    ebi_objects::ebi_arithmetic::exact::set_exact_globally(true);
    let command: &&EbiCommand = 
        &&crate::ebi_commands::ebi_command_analyse_non_stochastic::EBI_ANALYSE_NON_STOCHASTIC_BOUNDED;
    execute_javascript_command(command, javascript_inputs, "analyse_non_stochastic_bounded", exporter_file_extension);
}

#[wasm_bindgen]
pub fn analyse_non_stochastic_cluster(javascript_inputs: Vec<JavascriptInput>, exporter_file_extension: &str) {
    ebi_objects::ebi_arithmetic::exact::set_exact_globally(true);
    let command: &&EbiCommand = 
        &&crate::ebi_commands::ebi_command_analyse_non_stochastic::EBI_ANALYSE_NON_STOCHASTIC_CLUSTER;
    execute_javascript_command(command, javascript_inputs, "analyse_non_stochastic_cluster", exporter_file_extension);
}

#[wasm_bindgen]
pub fn analyse_non_stochastic_executions(javascript_inputs: Vec<JavascriptInput>, exporter_file_extension: &str) {
    ebi_objects::ebi_arithmetic::exact::set_exact_globally(true);
    let command: &&EbiCommand = 
        &&crate::ebi_commands::ebi_command_analyse_non_stochastic::EBI_ANALYSE_NON_STOCHASTIC_EXECUTIONS;
    execute_javascript_command(command, javascript_inputs, "analyse_non_stochastic_executions", exporter_file_extension);
}

#[wasm_bindgen]
pub fn analyse_non_stochastic_infinitely_many_traces(javascript_inputs: Vec<JavascriptInput>, exporter_file_extension: &str) {
    ebi_objects::ebi_arithmetic::exact::set_exact_globally(true);
    let command: &&EbiCommand = 
        &&crate::ebi_commands::ebi_command_analyse_non_stochastic::EBI_ANALYSE_NON_STOCHASTIC_INFINITELY_MANY_TRACES;
    execute_javascript_command(command, javascript_inputs, "analyse_non_stochastic_infinitely_many_traces", exporter_file_extension);
}

#[wasm_bindgen]
pub fn analyse_non_stochastic_medoid(javascript_inputs: Vec<JavascriptInput>, exporter_file_extension: &str) {
    ebi_objects::ebi_arithmetic::exact::set_exact_globally(true);
    let command: &&EbiCommand = 
        &&crate::ebi_commands::ebi_command_analyse_non_stochastic::EBI_ANALYSE_NON_STOCHASTIC_MEDOID;
    execute_javascript_command(command, javascript_inputs, "analyse_non_stochastic_medoid", exporter_file_extension);
}

#[wasm_bindgen]
pub fn analyse_non_stochastic_timestamps_ordered(javascript_inputs: Vec<JavascriptInput>, exporter_file_extension: &str) {
    ebi_objects::ebi_arithmetic::exact::set_exact_globally(true);
    let command: &&EbiCommand = 
        &&crate::ebi_commands::ebi_command_analyse_non_stochastic::EBI_ANALYSE_NON_STOCHASTIC_TIMESTAMPS_ORDERED;
    execute_javascript_command(command, javascript_inputs, "analyse_non_stochastic_timestamps_ordered", exporter_file_extension);
}

#[wasm_bindgen]
pub fn association_all_trace_attributes(javascript_inputs: Vec<JavascriptInput>, exporter_file_extension: &str) {
    ebi_objects::ebi_arithmetic::exact::set_exact_globally(true);
    let command: &&EbiCommand = 
        &&crate::ebi_commands::ebi_command_association::ASSOCIATION_ATTRIBUTES;
    execute_javascript_command(command, javascript_inputs, "association_all_trace_attributes", exporter_file_extension);
}

#[wasm_bindgen]
pub fn association_trace_attribute(javascript_inputs: Vec<JavascriptInput>, exporter_file_extension: &str) {
    ebi_objects::ebi_arithmetic::exact::set_exact_globally(true);
    let command: &&EbiCommand = 
        &&crate::ebi_commands::ebi_command_association::ASSOCIATION_ATTRIBUTE;
    execute_javascript_command(command, javascript_inputs, "association_trace_attribute", exporter_file_extension);
}

#[wasm_bindgen]
pub fn conformance_chi_squared(javascript_inputs: Vec<JavascriptInput>, exporter_file_extension: &str) {
    ebi_objects::ebi_arithmetic::exact::set_exact_globally(true);
    let command: &&EbiCommand = 
        &&crate::ebi_commands::ebi_command_conformance::CONFORMANCE_CSSC;
    execute_javascript_command(command, javascript_inputs, "conformance_chi_squared", exporter_file_extension);
}

#[wasm_bindgen]
pub fn conformance_chi_squared_sample(javascript_inputs: Vec<JavascriptInput>, exporter_file_extension: &str) {
    ebi_objects::ebi_arithmetic::exact::set_exact_globally(true);
    let command: &&EbiCommand = 
        &&crate::ebi_commands::ebi_command_conformance::CONFORMANCE_CSSC_SAMPLE;
    execute_javascript_command(command, javascript_inputs, "conformance_chi_squared_sample", exporter_file_extension);
}

#[wasm_bindgen]
pub fn conformance_earth_movers(javascript_inputs: Vec<JavascriptInput>, exporter_file_extension: &str) {
    ebi_objects::ebi_arithmetic::exact::set_exact_globally(true);
    let command: &&EbiCommand = 
        &&crate::ebi_commands::ebi_command_conformance::CONFORMANCE_EMSC;
    execute_javascript_command(command, javascript_inputs, "conformance_earth_movers", exporter_file_extension);
}

#[wasm_bindgen]
pub fn conformance_earth_movers_sample(javascript_inputs: Vec<JavascriptInput>, exporter_file_extension: &str) {
    ebi_objects::ebi_arithmetic::exact::set_exact_globally(true);
    let command: &&EbiCommand = 
        &&crate::ebi_commands::ebi_command_conformance::CONFORMANCE_EMSC_SAMPLE;
    execute_javascript_command(command, javascript_inputs, "conformance_earth_movers_sample", exporter_file_extension);
}

#[wasm_bindgen]
pub fn conformance_entropic_relevance(javascript_inputs: Vec<JavascriptInput>, exporter_file_extension: &str) {
    ebi_objects::ebi_arithmetic::exact::set_exact_globally(true);
    let command: &&EbiCommand = 
        &&crate::ebi_commands::ebi_command_conformance::CONFORMANCE_ER;
    execute_javascript_command(command, javascript_inputs, "conformance_entropic_relevance", exporter_file_extension);
}

#[wasm_bindgen]
pub fn conformance_gain_precision(javascript_inputs: Vec<JavascriptInput>, exporter_file_extension: &str) {
    ebi_objects::ebi_arithmetic::exact::set_exact_globally(false);
    let command: &&EbiCommand = 
        &&crate::ebi_commands::ebi_command_conformance::CONFORMANCE_GAIN_PRECISION;
    execute_javascript_command(command, javascript_inputs, "conformance_gain_precision", exporter_file_extension);
}

#[wasm_bindgen]
pub fn conformance_gain_recall(javascript_inputs: Vec<JavascriptInput>, exporter_file_extension: &str) {
    ebi_objects::ebi_arithmetic::exact::set_exact_globally(false);
    let command: &&EbiCommand = 
        &&crate::ebi_commands::ebi_command_conformance::CONFORMANCE_GAIN_RECALL;
    execute_javascript_command(command, javascript_inputs, "conformance_gain_recall", exporter_file_extension);
}

#[wasm_bindgen]
pub fn conformance_hellinger(javascript_inputs: Vec<JavascriptInput>, exporter_file_extension: &str) {
    ebi_objects::ebi_arithmetic::exact::set_exact_globally(true);
    let command: &&EbiCommand = 
        &&crate::ebi_commands::ebi_command_conformance::CONFORMANCE_HSC;
    execute_javascript_command(command, javascript_inputs, "conformance_hellinger", exporter_file_extension);
}

#[wasm_bindgen]
pub fn conformance_hellinger_sample(javascript_inputs: Vec<JavascriptInput>, exporter_file_extension: &str) {
    ebi_objects::ebi_arithmetic::exact::set_exact_globally(true);
    let command: &&EbiCommand = 
        &&crate::ebi_commands::ebi_command_conformance::CONFORMANCE_HSC_SAMPLE;
    execute_javascript_command(command, javascript_inputs, "conformance_hellinger_sample", exporter_file_extension);
}

#[wasm_bindgen]
pub fn conformance_jensen_shannon(javascript_inputs: Vec<JavascriptInput>, exporter_file_extension: &str) {
    ebi_objects::ebi_arithmetic::exact::set_exact_globally(false);
    let command: &&EbiCommand = 
        &&crate::ebi_commands::ebi_command_conformance::CONFORMANCE_JSSC;
    execute_javascript_command(command, javascript_inputs, "conformance_jensen_shannon", exporter_file_extension);
}

#[wasm_bindgen]
pub fn conformance_jensen_shannon_sample(javascript_inputs: Vec<JavascriptInput>, exporter_file_extension: &str) {
    ebi_objects::ebi_arithmetic::exact::set_exact_globally(false);
    let command: &&EbiCommand = 
        &&crate::ebi_commands::ebi_command_conformance::CONFORMANCE_JSSC_SAMPLE;
    execute_javascript_command(command, javascript_inputs, "conformance_jensen_shannon_sample", exporter_file_extension);
}

#[wasm_bindgen]
pub fn conformance_markovian(javascript_inputs: Vec<JavascriptInput>, exporter_file_extension: &str) {
    ebi_objects::ebi_arithmetic::exact::set_exact_globally(true);
    let command: &&EbiCommand = 
        &&crate::ebi_commands::ebi_command_conformance::CONFORMANCE_MARKOVIAN;
    execute_javascript_command(command, javascript_inputs, "conformance_markovian", exporter_file_extension);
}

#[wasm_bindgen]
pub fn conformance_unit_earth_movers(javascript_inputs: Vec<JavascriptInput>, exporter_file_extension: &str) {
    ebi_objects::ebi_arithmetic::exact::set_exact_globally(true);
    let command: &&EbiCommand = 
        &&crate::ebi_commands::ebi_command_conformance::CONFORMANCE_UEMSC;
    execute_javascript_command(command, javascript_inputs, "conformance_unit_earth_movers", exporter_file_extension);
}

#[wasm_bindgen]
pub fn conformance_unit_earth_movers_sample(javascript_inputs: Vec<JavascriptInput>, exporter_file_extension: &str) {
    ebi_objects::ebi_arithmetic::exact::set_exact_globally(false);
    let command: &&EbiCommand = 
        &&crate::ebi_commands::ebi_command_conformance::CONFORMANCE_UEMSC_SAMPLE;
    execute_javascript_command(command, javascript_inputs, "conformance_unit_earth_movers_sample", exporter_file_extension);
}

#[wasm_bindgen]
pub fn conformance_non_stochastic_alignments(javascript_inputs: Vec<JavascriptInput>, exporter_file_extension: &str) {
    ebi_objects::ebi_arithmetic::exact::set_exact_globally(true);
    let command: &&EbiCommand = 
        &&crate::ebi_commands::ebi_command_conformance_non_stochastic::EBI_CONFORMANCE_NON_STOCHASTIC_ALIGNMENTS;
    execute_javascript_command(command, javascript_inputs, "conformance_non_stochastic_alignments", exporter_file_extension);
}

#[wasm_bindgen]
pub fn conformance_non_stochastic_escaping_edges_precision(javascript_inputs: Vec<JavascriptInput>, exporter_file_extension: &str) {
    ebi_objects::ebi_arithmetic::exact::set_exact_globally(true);
    let command: &&EbiCommand = 
        &&crate::ebi_commands::ebi_command_conformance_non_stochastic::EBI_CONFORMANCE_NON_STOCHASTIC_ESCAPING_EDGES_PRECISION;
    execute_javascript_command(command, javascript_inputs, "conformance_non_stochastic_escaping_edges_precision", exporter_file_extension);
}

#[wasm_bindgen]
pub fn conformance_non_stochastic_set_alignments(javascript_inputs: Vec<JavascriptInput>, exporter_file_extension: &str) {
    ebi_objects::ebi_arithmetic::exact::set_exact_globally(true);
    let command: &&EbiCommand = 
        &&crate::ebi_commands::ebi_command_conformance_non_stochastic::EBI_CONFORMANCE_NON_STOCHASTIC_SET_ALIGNMENTS;
    execute_javascript_command(command, javascript_inputs, "conformance_non_stochastic_set_alignments", exporter_file_extension);
}

#[wasm_bindgen]
pub fn conformance_non_stochastic_trace_fitness(javascript_inputs: Vec<JavascriptInput>, exporter_file_extension: &str) {
    ebi_objects::ebi_arithmetic::exact::set_exact_globally(true);
    let command: &&EbiCommand = 
        &&crate::ebi_commands::ebi_command_conformance_non_stochastic::EBI_CONFORMANCE_NON_STOCHASTIC_TRACE_FITNESS;
    execute_javascript_command(command, javascript_inputs, "conformance_non_stochastic_trace_fitness", exporter_file_extension);
}

#[wasm_bindgen]
pub fn convert_business_process_model_and_notation(javascript_inputs: Vec<JavascriptInput>, exporter_file_extension: &str) {
    ebi_objects::ebi_arithmetic::exact::set_exact_globally(true);
    let command: &&EbiCommand = 
        &&crate::ebi_commands::ebi_command_convert::EBI_CONVERT_BPMN;
    execute_javascript_command(command, javascript_inputs, "convert_business_process_model_and_notation", exporter_file_extension);
}

#[wasm_bindgen]
pub fn convert_finite_language(javascript_inputs: Vec<JavascriptInput>, exporter_file_extension: &str) {
    ebi_objects::ebi_arithmetic::exact::set_exact_globally(true);
    let command: &&EbiCommand = 
        &&crate::ebi_commands::ebi_command_convert::EBI_CONVERT_LANG;
    execute_javascript_command(command, javascript_inputs, "convert_finite_language", exporter_file_extension);
}

#[wasm_bindgen]
pub fn convert_finite_stochastic_language(javascript_inputs: Vec<JavascriptInput>, exporter_file_extension: &str) {
    ebi_objects::ebi_arithmetic::exact::set_exact_globally(true);
    let command: &&EbiCommand = 
        &&crate::ebi_commands::ebi_command_convert::EBI_CONVERT_SLANG;
    execute_javascript_command(command, javascript_inputs, "convert_finite_stochastic_language", exporter_file_extension);
}

#[wasm_bindgen]
pub fn convert_labelled_petri_net(javascript_inputs: Vec<JavascriptInput>, exporter_file_extension: &str) {
    ebi_objects::ebi_arithmetic::exact::set_exact_globally(true);
    let command: &&EbiCommand = 
        &&crate::ebi_commands::ebi_command_convert::EBI_CONVERT_LPN;
    execute_javascript_command(command, javascript_inputs, "convert_labelled_petri_net", exporter_file_extension);
}

#[wasm_bindgen]
pub fn convert_log(javascript_inputs: Vec<JavascriptInput>, exporter_file_extension: &str) {
    ebi_objects::ebi_arithmetic::exact::set_exact_globally(true);
    let command: &&EbiCommand = 
        &&crate::ebi_commands::ebi_command_convert::EBI_CONVERT_LOG;
    execute_javascript_command(command, javascript_inputs, "convert_log", exporter_file_extension);
}

#[wasm_bindgen]
pub fn convert_stochastic_deterministic_finite_automaton(javascript_inputs: Vec<JavascriptInput>, exporter_file_extension: &str) {
    ebi_objects::ebi_arithmetic::exact::set_exact_globally(true);
    let command: &&EbiCommand = 
        &&crate::ebi_commands::ebi_command_convert::EBI_CONVERT_SDFA;
    execute_javascript_command(command, javascript_inputs, "convert_stochastic_deterministic_finite_automaton", exporter_file_extension);
}

#[wasm_bindgen]
pub fn convert_stochastic_labelled_petri_net(javascript_inputs: Vec<JavascriptInput>, exporter_file_extension: &str) {
    ebi_objects::ebi_arithmetic::exact::set_exact_globally(true);
    let command: &&EbiCommand = 
        &&crate::ebi_commands::ebi_command_convert::EBI_CONVERT_SLPN;
    execute_javascript_command(command, javascript_inputs, "convert_stochastic_labelled_petri_net", exporter_file_extension);
}

#[wasm_bindgen]
pub fn convert_stochastic_nondeterministic_finite_automaton(javascript_inputs: Vec<JavascriptInput>, exporter_file_extension: &str) {
    ebi_objects::ebi_arithmetic::exact::set_exact_globally(true);
    let command: &&EbiCommand = 
        &&crate::ebi_commands::ebi_command_convert::EBI_CONVERT_SNFA;
    execute_javascript_command(command, javascript_inputs, "convert_stochastic_nondeterministic_finite_automaton", exporter_file_extension);
}

#[wasm_bindgen]
pub fn discover_alignments_stochastic_business_process_model_and_notation(javascript_inputs: Vec<JavascriptInput>, exporter_file_extension: &str) {
    ebi_objects::ebi_arithmetic::exact::set_exact_globally(true);
    let command: &&EbiCommand = 
        &&crate::ebi_commands::ebi_command_discover::EBI_DISCOVER_ALIGNMENTS_BPMN;
    execute_javascript_command(command, javascript_inputs, "discover_alignments_stochastic_business_process_model_and_notation", exporter_file_extension);
}

#[wasm_bindgen]
pub fn discover_alignments_stochastic_labelled_petri_nets(javascript_inputs: Vec<JavascriptInput>, exporter_file_extension: &str) {
    ebi_objects::ebi_arithmetic::exact::set_exact_globally(true);
    let command: &&EbiCommand = 
        &&crate::ebi_commands::ebi_command_discover::EBI_DISCOVER_ALIGNMENTS_SLPN;
    execute_javascript_command(command, javascript_inputs, "discover_alignments_stochastic_labelled_petri_nets", exporter_file_extension);
}

#[wasm_bindgen]
pub fn discover_directly_follows_graph(javascript_inputs: Vec<JavascriptInput>, exporter_file_extension: &str) {
    ebi_objects::ebi_arithmetic::exact::set_exact_globally(true);
    let command: &&EbiCommand = 
        &&crate::ebi_commands::ebi_command_discover::EBI_DISCOVER_DIRECTLY_FOLLOWS;
    execute_javascript_command(command, javascript_inputs, "discover_directly_follows_graph", exporter_file_extension);
}

#[wasm_bindgen]
pub fn discover_occurrence_stochastic_business_process_model_and_notation(javascript_inputs: Vec<JavascriptInput>, exporter_file_extension: &str) {
    ebi_objects::ebi_arithmetic::exact::set_exact_globally(true);
    let command: &&EbiCommand = 
        &&crate::ebi_commands::ebi_command_discover::EBI_DISCOVER_OCCURRENCE_SBPMN;
    execute_javascript_command(command, javascript_inputs, "discover_occurrence_stochastic_business_process_model_and_notation", exporter_file_extension);
}

#[wasm_bindgen]
pub fn discover_occurrence_stochastic_labelled_petri_net(javascript_inputs: Vec<JavascriptInput>, exporter_file_extension: &str) {
    ebi_objects::ebi_arithmetic::exact::set_exact_globally(true);
    let command: &&EbiCommand = 
        &&crate::ebi_commands::ebi_command_discover::EBI_DISCOVER_OCCURRENCE_SLPN;
    execute_javascript_command(command, javascript_inputs, "discover_occurrence_stochastic_labelled_petri_net", exporter_file_extension);
}

#[wasm_bindgen]
pub fn discover_occurrence_stochastic_process_tree(javascript_inputs: Vec<JavascriptInput>, exporter_file_extension: &str) {
    ebi_objects::ebi_arithmetic::exact::set_exact_globally(true);
    let command: &&EbiCommand = 
        &&crate::ebi_commands::ebi_command_discover::EBI_DISCOVER_OCCURRENCE_SPTREE;
    execute_javascript_command(command, javascript_inputs, "discover_occurrence_stochastic_process_tree", exporter_file_extension);
}

#[wasm_bindgen]
pub fn discover_random_stochastic_business_process_model_and_notation(javascript_inputs: Vec<JavascriptInput>, exporter_file_extension: &str) {
    ebi_objects::ebi_arithmetic::exact::set_exact_globally(true);
    let command: &&EbiCommand = 
        &&crate::ebi_commands::ebi_command_discover::EBI_DISCOVER_RANDOM_SBPMN;
    execute_javascript_command(command, javascript_inputs, "discover_random_stochastic_business_process_model_and_notation", exporter_file_extension);
}

#[wasm_bindgen]
pub fn discover_random_stochastic_labelled_petri_net(javascript_inputs: Vec<JavascriptInput>, exporter_file_extension: &str) {
    ebi_objects::ebi_arithmetic::exact::set_exact_globally(true);
    let command: &&EbiCommand = 
        &&crate::ebi_commands::ebi_command_discover::EBI_DISCOVER_RANDOM_SLPN;
    execute_javascript_command(command, javascript_inputs, "discover_random_stochastic_labelled_petri_net", exporter_file_extension);
}

#[wasm_bindgen]
pub fn discover_random_stochastic_process_tree(javascript_inputs: Vec<JavascriptInput>, exporter_file_extension: &str) {
    ebi_objects::ebi_arithmetic::exact::set_exact_globally(true);
    let command: &&EbiCommand = 
        &&crate::ebi_commands::ebi_command_discover::EBI_DISCOVER_RANDOM_SPTREE;
    execute_javascript_command(command, javascript_inputs, "discover_random_stochastic_process_tree", exporter_file_extension);
}

#[wasm_bindgen]
pub fn discover_uniform_stochastic_business_process_model_and_notation(javascript_inputs: Vec<JavascriptInput>, exporter_file_extension: &str) {
    ebi_objects::ebi_arithmetic::exact::set_exact_globally(true);
    let command: &&EbiCommand = 
        &&crate::ebi_commands::ebi_command_discover::EBI_DISCOVER_UNIFORM_SBPMN;
    execute_javascript_command(command, javascript_inputs, "discover_uniform_stochastic_business_process_model_and_notation", exporter_file_extension);
}

#[wasm_bindgen]
pub fn discover_uniform_stochastic_labelled_petri_net(javascript_inputs: Vec<JavascriptInput>, exporter_file_extension: &str) {
    ebi_objects::ebi_arithmetic::exact::set_exact_globally(true);
    let command: &&EbiCommand = 
        &&crate::ebi_commands::ebi_command_discover::EBI_DISCOVER_UNIFORM_SLPN;
    execute_javascript_command(command, javascript_inputs, "discover_uniform_stochastic_labelled_petri_net", exporter_file_extension);
}

#[wasm_bindgen]
pub fn discover_uniform_stochastic_process_tree(javascript_inputs: Vec<JavascriptInput>, exporter_file_extension: &str) {
    ebi_objects::ebi_arithmetic::exact::set_exact_globally(true);
    let command: &&EbiCommand = 
        &&crate::ebi_commands::ebi_command_discover::EBI_DISCOVER_UNIFORM_SPTREE;
    execute_javascript_command(command, javascript_inputs, "discover_uniform_stochastic_process_tree", exporter_file_extension);
}

#[wasm_bindgen]
pub fn discover_non_stochastic_flower_deterministic_finite_automaton(javascript_inputs: Vec<JavascriptInput>, exporter_file_extension: &str) {
    ebi_objects::ebi_arithmetic::exact::set_exact_globally(true);
    let command: &&EbiCommand = 
        &&crate::ebi_commands::ebi_command_discover_non_stochastic::EBI_DISCOVER_NON_STOCHASTIC_FLOWER_DFA;
    execute_javascript_command(command, javascript_inputs, "discover_non_stochastic_flower_deterministic_finite_automaton", exporter_file_extension);
}

#[wasm_bindgen]
pub fn discover_non_stochastic_flower_process_tree(javascript_inputs: Vec<JavascriptInput>, exporter_file_extension: &str) {
    ebi_objects::ebi_arithmetic::exact::set_exact_globally(true);
    let command: &&EbiCommand = 
        &&crate::ebi_commands::ebi_command_discover_non_stochastic::EBI_DISCOVER_NON_STOCHASTIC_FLOWER_TREE;
    execute_javascript_command(command, javascript_inputs, "discover_non_stochastic_flower_process_tree", exporter_file_extension);
}

#[wasm_bindgen]
pub fn discover_non_stochastic_prefix_tree_deterministic_finite_automaton(javascript_inputs: Vec<JavascriptInput>, exporter_file_extension: &str) {
    ebi_objects::ebi_arithmetic::exact::set_exact_globally(true);
    let command: &&EbiCommand = 
        &&crate::ebi_commands::ebi_command_discover_non_stochastic::EBI_DISCOVER_NON_STOCHASTIC_TREE_DFA;
    execute_javascript_command(command, javascript_inputs, "discover_non_stochastic_prefix_tree_deterministic_finite_automaton", exporter_file_extension);
}

#[wasm_bindgen]
pub fn discover_non_stochastic_prefix_tree_process_tree(javascript_inputs: Vec<JavascriptInput>, exporter_file_extension: &str) {
    ebi_objects::ebi_arithmetic::exact::set_exact_globally(true);
    let command: &&EbiCommand = 
        &&crate::ebi_commands::ebi_command_discover_non_stochastic::EBI_DISCOVER_NON_STOCHASTIC_TREE_TREE;
    execute_javascript_command(command, javascript_inputs, "discover_non_stochastic_prefix_tree_process_tree", exporter_file_extension);
}

#[wasm_bindgen]
pub fn discover_non_stochastic_trace_model(javascript_inputs: Vec<JavascriptInput>, exporter_file_extension: &str) {
    ebi_objects::ebi_arithmetic::exact::set_exact_globally(true);
    let command: &&EbiCommand = 
        &&crate::ebi_commands::ebi_command_discover_non_stochastic::EBI_DISCOVER_NON_STOCHASTIC_TRACE_MODEL;
    execute_javascript_command(command, javascript_inputs, "discover_non_stochastic_trace_model", exporter_file_extension);
}

#[wasm_bindgen]
pub fn filter_traces_empty(javascript_inputs: Vec<JavascriptInput>, exporter_file_extension: &str) {
    ebi_objects::ebi_arithmetic::exact::set_exact_globally(true);
    let command: &&EbiCommand = 
        &&crate::ebi_commands::ebi_command_filter::EBI_FILTER_TRACES_EMPTY;
    execute_javascript_command(command, javascript_inputs, "filter_traces_empty", exporter_file_extension);
}

#[wasm_bindgen]
pub fn filter_traces_event_activity(javascript_inputs: Vec<JavascriptInput>, exporter_file_extension: &str) {
    ebi_objects::ebi_arithmetic::exact::set_exact_globally(true);
    let command: &&EbiCommand = 
        &&crate::ebi_commands::ebi_command_filter::EBI_FILTER_TRACES_EVENT_ACTIVITY;
    execute_javascript_command(command, javascript_inputs, "filter_traces_event_activity", exporter_file_extension);
}

#[wasm_bindgen]
pub fn filter_traces_length(javascript_inputs: Vec<JavascriptInput>, exporter_file_extension: &str) {
    ebi_objects::ebi_arithmetic::exact::set_exact_globally(true);
    let command: &&EbiCommand = 
        &&crate::ebi_commands::ebi_command_filter::EBI_FILTER_TRACES_LENGTH;
    execute_javascript_command(command, javascript_inputs, "filter_traces_length", exporter_file_extension);
}

#[wasm_bindgen]
pub fn information(javascript_inputs: Vec<JavascriptInput>, exporter_file_extension: &str) {
    ebi_objects::ebi_arithmetic::exact::set_exact_globally(true);
    let command: &&EbiCommand = 
        &&crate::ebi_commands::ebi_command_info::EBI_INFO;
    execute_javascript_command(command, javascript_inputs, "information", exporter_file_extension);
}

#[wasm_bindgen]
pub fn probability_log(javascript_inputs: Vec<JavascriptInput>, exporter_file_extension: &str) {
    ebi_objects::ebi_arithmetic::exact::set_exact_globally(true);
    let command: &&EbiCommand = 
        &&crate::ebi_commands::ebi_command_probability::EBI_PROBABILITY_LOG;
    execute_javascript_command(command, javascript_inputs, "probability_log", exporter_file_extension);
}

#[wasm_bindgen]
pub fn sample_folds(javascript_inputs: Vec<JavascriptInput>, exporter_file_extension: &str) {
    ebi_objects::ebi_arithmetic::exact::set_exact_globally(true);
    let command: &&EbiCommand = 
        &&crate::ebi_commands::ebi_command_sample::EBI_SAMPLE_FOLDS;
    execute_javascript_command(command, javascript_inputs, "sample_folds", exporter_file_extension);
}

#[wasm_bindgen]
pub fn sample_partially_ordered_traces(javascript_inputs: Vec<JavascriptInput>, exporter_file_extension: &str) {
    ebi_objects::ebi_arithmetic::exact::set_exact_globally(true);
    let command: &&EbiCommand = 
        &&crate::ebi_commands::ebi_command_sample::EBI_SAMPLE_PARTIALLY_ORDERED_TRACES;
    execute_javascript_command(command, javascript_inputs, "sample_partially_ordered_traces", exporter_file_extension);
}

#[wasm_bindgen]
pub fn sample_traces(javascript_inputs: Vec<JavascriptInput>, exporter_file_extension: &str) {
    ebi_objects::ebi_arithmetic::exact::set_exact_globally(true);
    let command: &&EbiCommand = 
        &&crate::ebi_commands::ebi_command_sample::EBI_SAMPLE_TRACES;
    execute_javascript_command(command, javascript_inputs, "sample_traces", exporter_file_extension);
}

#[wasm_bindgen]
pub fn test_bootstrap_test(javascript_inputs: Vec<JavascriptInput>, exporter_file_extension: &str) {
    ebi_objects::ebi_arithmetic::exact::set_exact_globally(true);
    let command: &&EbiCommand = 
        &&crate::ebi_commands::ebi_command_test::EBI_BOOTSTRAP_TEST;
    execute_javascript_command(command, javascript_inputs, "test_bootstrap_test", exporter_file_extension);
}

#[wasm_bindgen]
pub fn test_log_categorical_attribute(javascript_inputs: Vec<JavascriptInput>, exporter_file_extension: &str) {
    ebi_objects::ebi_arithmetic::exact::set_exact_globally(true);
    let command: &&EbiCommand = 
        &&crate::ebi_commands::ebi_command_test::EBI_TEST_LOG_ATTRIBUTE;
    execute_javascript_command(command, javascript_inputs, "test_log_categorical_attribute", exporter_file_extension);
}

#[wasm_bindgen]
pub fn visualise_graph(javascript_inputs: Vec<JavascriptInput>, exporter_file_extension: &str) {
    ebi_objects::ebi_arithmetic::exact::set_exact_globally(true);
    let command: &&EbiCommand = 
        &&crate::ebi_commands::ebi_command_visualise::EBI_VISUALISE_GRAPH;
    execute_javascript_command(command, javascript_inputs, "visualise_graph", exporter_file_extension);
}

#[wasm_bindgen]
pub fn visualise_text(javascript_inputs: Vec<JavascriptInput>, exporter_file_extension: &str) {
    ebi_objects::ebi_arithmetic::exact::set_exact_globally(false);
    let command: &&EbiCommand = 
        &&crate::ebi_commands::ebi_command_visualise::EBI_VISUALISE_TEXT;
    execute_javascript_command(command, javascript_inputs, "visualise_text", exporter_file_extension);
}
#[cfg(test)]
mod tests {
	use crate::javascript::javascript_link::JavascriptInput;

	#[test]
	pub fn analyse_all_traces_test() {
        let inputs = vec![
			JavascriptInput::from(std::fs::read_to_string("./testfiles/a-b.csv").unwrap())
			// trait finite stochastic language ./testfiles/a-b.csv

		];
        crate::javascript::javascript_autogen::analyse_all_traces(inputs, ".xes");
    }

	#[test]
	pub fn analyse_completeness_test() {
        let inputs = vec![
			JavascriptInput::from(std::fs::read_to_string("./testfiles/a-b.csv").unwrap())
			// trait event log ./testfiles/a-b.csv

		];
        crate::javascript::javascript_autogen::analyse_completeness(inputs, ".xes");
    }

	#[test]
	pub fn analyse_coverage_test() {
        let inputs = vec![
			JavascriptInput::from(std::fs::read_to_string("./testfiles/a-b.csv").unwrap())
			// trait finite stochastic language ./testfiles/a-b.csv
			,
			JavascriptInput::from("0".to_string())
			// fraction 0

		];
        crate::javascript::javascript_autogen::analyse_coverage(inputs, ".xes");
    }

	#[test]
	pub fn analyse_directly_follows_edge_difference_test() {
        let inputs = vec![
			JavascriptInput::from(std::fs::read_to_string("./testfiles/BPI_Challenge_2013_closed_problems.xes.gz-dfg.dfg").unwrap())
			// object directly follows graph ./testfiles/BPI_Challenge_2013_closed_problems.xes.gz-dfg.dfg
			,
			JavascriptInput::from(std::fs::read_to_string("./testfiles/BPI_Challenge_2013_closed_problems.xes.gz-dfg.dfg").unwrap())
			// object directly follows graph ./testfiles/BPI_Challenge_2013_closed_problems.xes.gz-dfg.dfg

		];
        crate::javascript::javascript_autogen::analyse_directly_follows_edge_difference(inputs, ".xes");
    }

	#[test]
	pub fn analyse_entropy_test() {
        let inputs = vec![
			JavascriptInput::from(std::fs::read_to_string("./testfiles/a-b.csv").unwrap())
			// trait finite stochastic language ./testfiles/a-b.csv

		];
        crate::javascript::javascript_autogen::analyse_entropy(inputs, ".xes");
    }

	#[test]
	pub fn analyse_medoid_test() {
        let inputs = vec![
			JavascriptInput::from(std::fs::read_to_string("./testfiles/a-b.csv").unwrap())
			// trait finite stochastic language ./testfiles/a-b.csv
			,
			JavascriptInput::from("1".to_string())
			// usize 1

		];
        crate::javascript::javascript_autogen::analyse_medoid(inputs, ".xes");
    }

	#[test]
	pub fn analyse_minimum_probability_traces_test() {
        let inputs = vec![
			JavascriptInput::from(std::fs::read_to_string("./testfiles/seq(a-xor(b-c)).sptree").unwrap())
			// trait stochastic deterministic semantics ./testfiles/seq(a-xor(b-c)).sptree
			,
			JavascriptInput::from("0".to_string())
			// fraction 0

		];
        crate::javascript::javascript_autogen::analyse_minimum_probability_traces(inputs, ".xes");
    }

	#[test]
	pub fn analyse_mode_test() {
        let inputs = vec![
			JavascriptInput::from(std::fs::read_to_string("./testfiles/a-b.csv").unwrap())
			// trait finite stochastic language ./testfiles/a-b.csv

		];
        crate::javascript::javascript_autogen::analyse_mode(inputs, ".xes");
    }

	#[test]
	pub fn analyse_most_likely_traces_test() {
        let inputs = vec![
			JavascriptInput::from(std::fs::read_to_string("./testfiles/a-b.csv").unwrap())
			// trait finite stochastic language ./testfiles/a-b.csv
			,
			JavascriptInput::from("1".to_string())
			// usize 1

		];
        crate::javascript::javascript_autogen::analyse_most_likely_traces(inputs, ".xes");
    }

	#[test]
	pub fn analyse_variety_test() {
        let inputs = vec![
			JavascriptInput::from(std::fs::read_to_string("./testfiles/a-b.csv").unwrap())
			// trait finite stochastic language ./testfiles/a-b.csv

		];
        crate::javascript::javascript_autogen::analyse_variety(inputs, ".xes");
    }

	#[test]
	pub fn analyse_non_stochastic_activities_test() {
        let inputs = vec![
			JavascriptInput::from(std::fs::read_to_string("./testfiles/flower.bpmn").unwrap())
			// trait activities ./testfiles/flower.bpmn

		];
        crate::javascript::javascript_autogen::analyse_non_stochastic_activities(inputs, ".xes");
    }

	#[test]
	pub fn analyse_non_stochastic_any_traces_test() {
        let inputs = vec![
			JavascriptInput::from(std::fs::read_to_string("./testfiles/BPI_Challenge_2013_closed_problems.xes.gz-dfg.dfg").unwrap())
			// object directly follows graph ./testfiles/BPI_Challenge_2013_closed_problems.xes.gz-dfg.dfg

		];
        crate::javascript::javascript_autogen::analyse_non_stochastic_any_traces(inputs, ".xes");
    }

	#[test]
	pub fn analyse_non_stochastic_bounded_test() {
        let inputs = vec![
			JavascriptInput::from(std::fs::read_to_string("./testfiles/BPI_Challenge_2013_closed_problems.xes.gz-dfg.dfg").unwrap())
			// object directly follows graph ./testfiles/BPI_Challenge_2013_closed_problems.xes.gz-dfg.dfg

		];
        crate::javascript::javascript_autogen::analyse_non_stochastic_bounded(inputs, ".xes");
    }

	#[test]
	pub fn analyse_non_stochastic_cluster_test() {
        let inputs = vec![
			JavascriptInput::from(std::fs::read_to_string("./testfiles/aa-ab-ba.lang").unwrap())
			// trait finite language ./testfiles/aa-ab-ba.lang
			,
			JavascriptInput::from("1".to_string())
			// usize 1

		];
        crate::javascript::javascript_autogen::analyse_non_stochastic_cluster(inputs, ".xes");
    }

	#[test]
	pub fn analyse_non_stochastic_executions_test() {
        let inputs = vec![
			JavascriptInput::from(std::fs::read_to_string("./testfiles/a-b.csv").unwrap())
			// trait event log with event attributes ./testfiles/a-b.csv
			,
			JavascriptInput::from(std::fs::read_to_string("./testfiles/flower.bpmn").unwrap())
			// trait semantics ./testfiles/flower.bpmn

		];
        crate::javascript::javascript_autogen::analyse_non_stochastic_executions(inputs, ".xes");
    }

	#[test]
	pub fn analyse_non_stochastic_infinitely_many_traces_test() {
        let inputs = vec![
			JavascriptInput::from(std::fs::read_to_string("./testfiles/a-b.csv").unwrap())
			// object event log ./testfiles/a-b.csv

		];
        crate::javascript::javascript_autogen::analyse_non_stochastic_infinitely_many_traces(inputs, ".xes");
    }

	#[test]
	pub fn analyse_non_stochastic_medoid_test() {
        let inputs = vec![
			JavascriptInput::from(std::fs::read_to_string("./testfiles/aa-ab-ba.lang").unwrap())
			// trait finite language ./testfiles/aa-ab-ba.lang
			,
			JavascriptInput::from("1".to_string())
			// usize 1

		];
        crate::javascript::javascript_autogen::analyse_non_stochastic_medoid(inputs, ".xes");
    }

	#[test]
	pub fn analyse_non_stochastic_timestamps_ordered_test() {
        let inputs = vec![
			JavascriptInput::from(std::fs::read_to_string("./testfiles/a-b.csv").unwrap())
			// trait event log with event attributes ./testfiles/a-b.csv

		];
        crate::javascript::javascript_autogen::analyse_non_stochastic_timestamps_ordered(inputs, ".xes");
    }

	#[test]
	pub fn association_all_trace_attributes_test() {
        let inputs = vec![
			JavascriptInput::from(std::fs::read_to_string("./testfiles/a-b-double.xes").unwrap())
			// trait event log with trace attributes ./testfiles/a-b-double.xes
			,
			JavascriptInput::from("10".to_string())
			// usize 10

		];
        crate::javascript::javascript_autogen::association_all_trace_attributes(inputs, ".xes");
    }

	#[test]
	pub fn association_trace_attribute_test() {
        let inputs = vec![
			JavascriptInput::from(std::fs::read_to_string("./testfiles/a-b-double.xes").unwrap())
			// trait event log with trace attributes ./testfiles/a-b-double.xes
			,
			JavascriptInput::from("some string".to_string())
			// string some string
			,
			JavascriptInput::from("10".to_string())
			// usize 10

		];
        crate::javascript::javascript_autogen::association_trace_attribute(inputs, ".xes");
    }

	#[test]
	pub fn conformance_chi_squared_test() {
        let inputs = vec![
			JavascriptInput::from(std::fs::read_to_string("./testfiles/a-b.csv").unwrap())
			// trait finite stochastic language ./testfiles/a-b.csv
			,
			JavascriptInput::from(std::fs::read_to_string("./testfiles/seq(a-xor(b-c)).sptree").unwrap())
			// trait queriable stochastic language ./testfiles/seq(a-xor(b-c)).sptree

		];
        crate::javascript::javascript_autogen::conformance_chi_squared(inputs, ".xes");
    }

	#[test]
	pub fn conformance_chi_squared_sample_test() {
        let inputs = vec![
			JavascriptInput::from(std::fs::read_to_string("./testfiles/a-b.csv").unwrap())
			// trait finite stochastic language ./testfiles/a-b.csv
			,
			JavascriptInput::from(std::fs::read_to_string("./testfiles/a-b.csv").unwrap())
			// trait finite stochastic language ./testfiles/a-b.csv
			,
			JavascriptInput::from("1".to_string())
			// usize 1

		];
        crate::javascript::javascript_autogen::conformance_chi_squared_sample(inputs, ".xes");
    }

	#[test]
	pub fn conformance_earth_movers_test() {
        let inputs = vec![
			JavascriptInput::from(std::fs::read_to_string("./testfiles/a-b.csv").unwrap())
			// trait finite stochastic language ./testfiles/a-b.csv
			,
			JavascriptInput::from(std::fs::read_to_string("./testfiles/a-b.csv").unwrap())
			// trait finite stochastic language ./testfiles/a-b.csv

		];
        crate::javascript::javascript_autogen::conformance_earth_movers(inputs, ".xes");
    }

	#[test]
	pub fn conformance_earth_movers_sample_test() {
        let inputs = vec![
			JavascriptInput::from(std::fs::read_to_string("./testfiles/a-b.csv").unwrap())
			// trait finite stochastic language ./testfiles/a-b.csv
			,
			JavascriptInput::from(std::fs::read_to_string("./testfiles/a-b.csv").unwrap())
			// trait finite stochastic language ./testfiles/a-b.csv
			,
			JavascriptInput::from("1".to_string())
			// usize 1

		];
        crate::javascript::javascript_autogen::conformance_earth_movers_sample(inputs, ".xes");
    }

	#[test]
	pub fn conformance_entropic_relevance_test() {
        let inputs = vec![
			JavascriptInput::from(std::fs::read_to_string("./testfiles/a-b.csv").unwrap())
			// trait finite stochastic language ./testfiles/a-b.csv
			,
			JavascriptInput::from(std::fs::read_to_string("./testfiles/seq(a-xor(b-c)).sptree").unwrap())
			// trait queriable stochastic language ./testfiles/seq(a-xor(b-c)).sptree

		];
        crate::javascript::javascript_autogen::conformance_entropic_relevance(inputs, ".xes");
    }

	#[test]
	#[should_panic]
	pub fn conformance_gain_precision_test() {
        let inputs = vec![
			JavascriptInput::from(std::fs::read_to_string("./testfiles/a-b.csv").unwrap())
			// trait finite stochastic language ./testfiles/a-b.csv
			,
			JavascriptInput::from(std::fs::read_to_string("./testfiles/empty.sdfa").unwrap())
			// object stochastic deterministic finite automaton ./testfiles/empty.sdfa
			,
			JavascriptInput::from("0".to_string())
			// fraction 0

		];
        crate::javascript::javascript_autogen::conformance_gain_precision(inputs, ".xes");
    }

	#[test]
	#[should_panic]
	pub fn conformance_gain_recall_test() {
        let inputs = vec![
			JavascriptInput::from(std::fs::read_to_string("./testfiles/a-b.csv").unwrap())
			// trait finite stochastic language ./testfiles/a-b.csv
			,
			JavascriptInput::from(std::fs::read_to_string("./testfiles/empty.sdfa").unwrap())
			// object stochastic deterministic finite automaton ./testfiles/empty.sdfa
			,
			JavascriptInput::from("0".to_string())
			// fraction 0

		];
        crate::javascript::javascript_autogen::conformance_gain_recall(inputs, ".xes");
    }

	#[test]
	pub fn conformance_hellinger_test() {
        let inputs = vec![
			JavascriptInput::from(std::fs::read_to_string("./testfiles/a-b.csv").unwrap())
			// trait finite stochastic language ./testfiles/a-b.csv
			,
			JavascriptInput::from(std::fs::read_to_string("./testfiles/seq(a-xor(b-c)).sptree").unwrap())
			// trait queriable stochastic language ./testfiles/seq(a-xor(b-c)).sptree

		];
        crate::javascript::javascript_autogen::conformance_hellinger(inputs, ".xes");
    }

	#[test]
	pub fn conformance_hellinger_sample_test() {
        let inputs = vec![
			JavascriptInput::from(std::fs::read_to_string("./testfiles/a-b.csv").unwrap())
			// trait finite stochastic language ./testfiles/a-b.csv
			,
			JavascriptInput::from(std::fs::read_to_string("./testfiles/a-b.csv").unwrap())
			// trait finite stochastic language ./testfiles/a-b.csv
			,
			JavascriptInput::from("1".to_string())
			// usize 1

		];
        crate::javascript::javascript_autogen::conformance_hellinger_sample(inputs, ".xes");
    }

	#[test]
	pub fn conformance_jensen_shannon_test() {
        let inputs = vec![
			JavascriptInput::from(std::fs::read_to_string("./testfiles/a-b.csv").unwrap())
			// trait finite stochastic language ./testfiles/a-b.csv
			,
			JavascriptInput::from(std::fs::read_to_string("./testfiles/a-b.csv").unwrap())
			// trait finite stochastic language ./testfiles/a-b.csv

		];
        crate::javascript::javascript_autogen::conformance_jensen_shannon(inputs, ".xes");
    }

	#[test]
	pub fn conformance_jensen_shannon_sample_test() {
        let inputs = vec![
			JavascriptInput::from(std::fs::read_to_string("./testfiles/a-b.csv").unwrap())
			// trait finite stochastic language ./testfiles/a-b.csv
			,
			JavascriptInput::from(std::fs::read_to_string("./testfiles/a-b.csv").unwrap())
			// trait finite stochastic language ./testfiles/a-b.csv
			,
			JavascriptInput::from("1".to_string())
			// usize 1

		];
        crate::javascript::javascript_autogen::conformance_jensen_shannon_sample(inputs, ".xes");
    }

	#[test]
	pub fn conformance_markovian_test() {
        let inputs = vec![
			JavascriptInput::from(std::fs::read_to_string("./testfiles/a-b.csv").unwrap())
			// trait finite stochastic language ./testfiles/a-b.csv
			,
			JavascriptInput::from(std::fs::read_to_string("./testfiles/a-b.csv").unwrap())
			// trait finite stochastic language ./testfiles/a-b.csv
			,
			JavascriptInput::from("1".to_string())
			// usize 1
			,
			JavascriptInput::from("cssc".to_string())
			// string cssc

		];
        crate::javascript::javascript_autogen::conformance_markovian(inputs, ".xes");
    }

	#[test]
	pub fn conformance_unit_earth_movers_test() {
        let inputs = vec![
			JavascriptInput::from(std::fs::read_to_string("./testfiles/a-b.csv").unwrap())
			// trait finite stochastic language ./testfiles/a-b.csv
			,
			JavascriptInput::from(std::fs::read_to_string("./testfiles/seq(a-xor(b-c)).sptree").unwrap())
			// trait queriable stochastic language ./testfiles/seq(a-xor(b-c)).sptree

		];
        crate::javascript::javascript_autogen::conformance_unit_earth_movers(inputs, ".xes");
    }

	#[test]
	pub fn conformance_unit_earth_movers_sample_test() {
        let inputs = vec![
			JavascriptInput::from(std::fs::read_to_string("./testfiles/a-b.csv").unwrap())
			// trait finite stochastic language ./testfiles/a-b.csv
			,
			JavascriptInput::from(std::fs::read_to_string("./testfiles/a-b.csv").unwrap())
			// trait finite stochastic language ./testfiles/a-b.csv
			,
			JavascriptInput::from("1".to_string())
			// usize 1

		];
        crate::javascript::javascript_autogen::conformance_unit_earth_movers_sample(inputs, ".xes");
    }

	#[test]
	pub fn conformance_non_stochastic_alignments_test() {
        let inputs = vec![
			JavascriptInput::from(std::fs::read_to_string("./testfiles/a-b.csv").unwrap())
			// trait finite stochastic language ./testfiles/a-b.csv
			,
			JavascriptInput::from(std::fs::read_to_string("./testfiles/flower.bpmn").unwrap())
			// trait semantics ./testfiles/flower.bpmn

		];
        crate::javascript::javascript_autogen::conformance_non_stochastic_alignments(inputs, ".xes");
    }

	#[test]
	#[should_panic]
	pub fn conformance_non_stochastic_escaping_edges_precision_test() {
        let inputs = vec![
			JavascriptInput::from(std::fs::read_to_string("./testfiles/aa-ab-ba.sali").unwrap())
			// object stochastic language of alignments ./testfiles/aa-ab-ba.sali
			,
			JavascriptInput::from(std::fs::read_to_string("./testfiles/flower.bpmn").unwrap())
			// trait semantics ./testfiles/flower.bpmn

		];
        crate::javascript::javascript_autogen::conformance_non_stochastic_escaping_edges_precision(inputs, ".xes");
    }

	#[test]
	pub fn conformance_non_stochastic_set_alignments_test() {
        let inputs = vec![
			JavascriptInput::from(std::fs::read_to_string("./testfiles/aa-ab-ba.lang").unwrap())
			// trait finite language ./testfiles/aa-ab-ba.lang
			,
			JavascriptInput::from(std::fs::read_to_string("./testfiles/flower.bpmn").unwrap())
			// trait semantics ./testfiles/flower.bpmn

		];
        crate::javascript::javascript_autogen::conformance_non_stochastic_set_alignments(inputs, ".xes");
    }

	#[test]
	pub fn conformance_non_stochastic_trace_fitness_test() {
        let inputs = vec![
			JavascriptInput::from(std::fs::read_to_string("./testfiles/aa-ab-ba.sali").unwrap())
			// object stochastic language of alignments ./testfiles/aa-ab-ba.sali

		];
        crate::javascript::javascript_autogen::conformance_non_stochastic_trace_fitness(inputs, ".xes");
    }

	#[test]
	pub fn convert_business_process_model_and_notation_test() {
        let inputs = vec![
			JavascriptInput::from(std::fs::read_to_string("./testfiles/flower.bpmn").unwrap())
			// object business process model and notation ./testfiles/flower.bpmn

		];
        crate::javascript::javascript_autogen::convert_business_process_model_and_notation(inputs, ".xes");
    }

	#[test]
	pub fn convert_finite_language_test() {
        let inputs = vec![
			JavascriptInput::from(std::fs::read_to_string("./testfiles/aa-ab-ba.lang").unwrap())
			// object finite language ./testfiles/aa-ab-ba.lang

		];
        crate::javascript::javascript_autogen::convert_finite_language(inputs, ".xes");
    }

	#[test]
	pub fn convert_finite_stochastic_language_test() {
        let inputs = vec![
			JavascriptInput::from(std::fs::read_to_string("./testfiles/a-b.csv").unwrap())
			// object finite stochastic language ./testfiles/a-b.csv

		];
        crate::javascript::javascript_autogen::convert_finite_stochastic_language(inputs, ".xes");
    }

	#[test]
	pub fn convert_labelled_petri_net_test() {
        let inputs = vec![
			JavascriptInput::from(std::fs::read_to_string("./testfiles/seq(a-xor(b-c)).sptree").unwrap())
			// object labelled Petri net ./testfiles/seq(a-xor(b-c)).sptree

		];
        crate::javascript::javascript_autogen::convert_labelled_petri_net(inputs, ".xes");
    }

	#[test]
	pub fn convert_log_test() {
        let inputs = vec![
			JavascriptInput::from(std::fs::read_to_string("./testfiles/a-b.csv").unwrap())
			// object event log ./testfiles/a-b.csv

		];
        crate::javascript::javascript_autogen::convert_log(inputs, ".xes");
    }

	#[test]
	pub fn convert_stochastic_deterministic_finite_automaton_test() {
        let inputs = vec![
			JavascriptInput::from(std::fs::read_to_string("./testfiles/empty.sdfa").unwrap())
			// object stochastic deterministic finite automaton ./testfiles/empty.sdfa

		];
        crate::javascript::javascript_autogen::convert_stochastic_deterministic_finite_automaton(inputs, ".xes");
    }

	#[test]
	pub fn convert_stochastic_labelled_petri_net_test() {
        let inputs = vec![
			JavascriptInput::from(std::fs::read_to_string("./testfiles/simple_markovian_abstraction.slpn").unwrap())
			// object stochastic labelled Petri net ./testfiles/simple_markovian_abstraction.slpn

		];
        crate::javascript::javascript_autogen::convert_stochastic_labelled_petri_net(inputs, ".xes");
    }

	#[test]
	pub fn convert_stochastic_nondeterministic_finite_automaton_test() {
        let inputs = vec![
			JavascriptInput::from(std::fs::read_to_string("./testfiles/seq(a-xor(b-c)).sptree").unwrap())
			// object stochastic non-deterministic finite automaton ./testfiles/seq(a-xor(b-c)).sptree

		];
        crate::javascript::javascript_autogen::convert_stochastic_nondeterministic_finite_automaton(inputs, ".xes");
    }

	#[test]
	pub fn discover_alignments_stochastic_business_process_model_and_notation_test() {
        let inputs = vec![
			JavascriptInput::from(std::fs::read_to_string("./testfiles/a-b.csv").unwrap())
			// trait finite stochastic language ./testfiles/a-b.csv
			,
			JavascriptInput::from(std::fs::read_to_string("./testfiles/flower.bpmn").unwrap())
			// object business process model and notation ./testfiles/flower.bpmn

		];
        crate::javascript::javascript_autogen::discover_alignments_stochastic_business_process_model_and_notation(inputs, ".xes");
    }

	#[test]
	pub fn discover_alignments_stochastic_labelled_petri_nets_test() {
        let inputs = vec![
			JavascriptInput::from(std::fs::read_to_string("./testfiles/a-b.csv").unwrap())
			// trait finite stochastic language ./testfiles/a-b.csv
			,
			JavascriptInput::from(std::fs::read_to_string("./testfiles/seq(a-xor(b-c)).sptree").unwrap())
			// object labelled Petri net ./testfiles/seq(a-xor(b-c)).sptree

		];
        crate::javascript::javascript_autogen::discover_alignments_stochastic_labelled_petri_nets(inputs, ".xes");
    }

	#[test]
	pub fn discover_directly_follows_graph_test() {
        let inputs = vec![
			JavascriptInput::from(std::fs::read_to_string("./testfiles/a-b.csv").unwrap())
			// trait event log ./testfiles/a-b.csv
			,
			JavascriptInput::from("1".to_string())
			// fraction 1

		];
        crate::javascript::javascript_autogen::discover_directly_follows_graph(inputs, ".xes");
    }

	#[test]
	pub fn discover_occurrence_stochastic_business_process_model_and_notation_test() {
        let inputs = vec![
			JavascriptInput::from(std::fs::read_to_string("./testfiles/a-b.csv").unwrap())
			// trait finite stochastic language ./testfiles/a-b.csv
			,
			JavascriptInput::from(std::fs::read_to_string("./testfiles/flower.bpmn").unwrap())
			// object business process model and notation ./testfiles/flower.bpmn

		];
        crate::javascript::javascript_autogen::discover_occurrence_stochastic_business_process_model_and_notation(inputs, ".xes");
    }

	#[test]
	pub fn discover_occurrence_stochastic_labelled_petri_net_test() {
        let inputs = vec![
			JavascriptInput::from(std::fs::read_to_string("./testfiles/a-b.csv").unwrap())
			// trait finite stochastic language ./testfiles/a-b.csv
			,
			JavascriptInput::from(std::fs::read_to_string("./testfiles/seq(a-xor(b-c)).sptree").unwrap())
			// object labelled Petri net ./testfiles/seq(a-xor(b-c)).sptree

		];
        crate::javascript::javascript_autogen::discover_occurrence_stochastic_labelled_petri_net(inputs, ".xes");
    }

	#[test]
	pub fn discover_occurrence_stochastic_process_tree_test() {
        let inputs = vec![
			JavascriptInput::from(std::fs::read_to_string("./testfiles/a-b.csv").unwrap())
			// trait finite stochastic language ./testfiles/a-b.csv
			,
			JavascriptInput::from(std::fs::read_to_string("./testfiles/seq(a-xor(b-c)).sptree").unwrap())
			// object process tree ./testfiles/seq(a-xor(b-c)).sptree

		];
        crate::javascript::javascript_autogen::discover_occurrence_stochastic_process_tree(inputs, ".xes");
    }

	#[test]
	pub fn discover_random_stochastic_business_process_model_and_notation_test() {
        let inputs = vec![
			JavascriptInput::from(std::fs::read_to_string("./testfiles/flower.bpmn").unwrap())
			// object business process model and notation ./testfiles/flower.bpmn

		];
        crate::javascript::javascript_autogen::discover_random_stochastic_business_process_model_and_notation(inputs, ".xes");
    }

	#[test]
	pub fn discover_random_stochastic_labelled_petri_net_test() {
        let inputs = vec![
			JavascriptInput::from(std::fs::read_to_string("./testfiles/seq(a-xor(b-c)).sptree").unwrap())
			// object labelled Petri net ./testfiles/seq(a-xor(b-c)).sptree

		];
        crate::javascript::javascript_autogen::discover_random_stochastic_labelled_petri_net(inputs, ".xes");
    }

	#[test]
	pub fn discover_random_stochastic_process_tree_test() {
        let inputs = vec![
			JavascriptInput::from(std::fs::read_to_string("./testfiles/seq(a-xor(b-c)).sptree").unwrap())
			// object process tree ./testfiles/seq(a-xor(b-c)).sptree

		];
        crate::javascript::javascript_autogen::discover_random_stochastic_process_tree(inputs, ".xes");
    }

	#[test]
	pub fn discover_uniform_stochastic_business_process_model_and_notation_test() {
        let inputs = vec![
			JavascriptInput::from(std::fs::read_to_string("./testfiles/flower.bpmn").unwrap())
			// object business process model and notation ./testfiles/flower.bpmn

		];
        crate::javascript::javascript_autogen::discover_uniform_stochastic_business_process_model_and_notation(inputs, ".xes");
    }

	#[test]
	pub fn discover_uniform_stochastic_labelled_petri_net_test() {
        let inputs = vec![
			JavascriptInput::from(std::fs::read_to_string("./testfiles/seq(a-xor(b-c)).sptree").unwrap())
			// object labelled Petri net ./testfiles/seq(a-xor(b-c)).sptree

		];
        crate::javascript::javascript_autogen::discover_uniform_stochastic_labelled_petri_net(inputs, ".xes");
    }

	#[test]
	pub fn discover_uniform_stochastic_process_tree_test() {
        let inputs = vec![
			JavascriptInput::from(std::fs::read_to_string("./testfiles/seq(a-xor(b-c)).sptree").unwrap())
			// object process tree ./testfiles/seq(a-xor(b-c)).sptree

		];
        crate::javascript::javascript_autogen::discover_uniform_stochastic_process_tree(inputs, ".xes");
    }

	#[test]
	pub fn discover_non_stochastic_flower_deterministic_finite_automaton_test() {
        let inputs = vec![
			JavascriptInput::from(std::fs::read_to_string("./testfiles/aa-ab-ba.lang").unwrap())
			// trait finite language ./testfiles/aa-ab-ba.lang

		];
        crate::javascript::javascript_autogen::discover_non_stochastic_flower_deterministic_finite_automaton(inputs, ".xes");
    }

	#[test]
	pub fn discover_non_stochastic_flower_process_tree_test() {
        let inputs = vec![
			JavascriptInput::from(std::fs::read_to_string("./testfiles/aa-ab-ba.lang").unwrap())
			// trait finite language ./testfiles/aa-ab-ba.lang

		];
        crate::javascript::javascript_autogen::discover_non_stochastic_flower_process_tree(inputs, ".xes");
    }

	#[test]
	pub fn discover_non_stochastic_prefix_tree_deterministic_finite_automaton_test() {
        let inputs = vec![
			JavascriptInput::from(std::fs::read_to_string("./testfiles/aa-ab-ba.lang").unwrap())
			// trait finite language ./testfiles/aa-ab-ba.lang

		];
        crate::javascript::javascript_autogen::discover_non_stochastic_prefix_tree_deterministic_finite_automaton(inputs, ".xes");
    }

	#[test]
	pub fn discover_non_stochastic_prefix_tree_process_tree_test() {
        let inputs = vec![
			JavascriptInput::from(std::fs::read_to_string("./testfiles/aa-ab-ba.lang").unwrap())
			// trait finite language ./testfiles/aa-ab-ba.lang

		];
        crate::javascript::javascript_autogen::discover_non_stochastic_prefix_tree_process_tree(inputs, ".xes");
    }

	#[test]
	pub fn discover_non_stochastic_trace_model_test() {
        let inputs = vec![
			JavascriptInput::from(std::fs::read_to_string("./testfiles/aa-ab-ba.lang").unwrap())
			// trait finite language ./testfiles/aa-ab-ba.lang

		];
        crate::javascript::javascript_autogen::discover_non_stochastic_trace_model(inputs, ".xes");
    }

	#[test]
	pub fn filter_traces_empty_test() {
        let inputs = vec![
			JavascriptInput::from(std::fs::read_to_string("./testfiles/a-b.csv").unwrap())
			// object XES event log ./testfiles/a-b.csv

		];
        crate::javascript::javascript_autogen::filter_traces_empty(inputs, ".xes");
    }

	#[test]
	pub fn filter_traces_event_activity_test() {
        let inputs = vec![
			JavascriptInput::from(std::fs::read_to_string("./testfiles/a-b.csv").unwrap())
			// object XES event log ./testfiles/a-b.csv
			,
			JavascriptInput::from("any".to_string())
			// string any
			,
			JavascriptInput::from("some string".to_string())
			// string some string

		];
        crate::javascript::javascript_autogen::filter_traces_event_activity(inputs, ".xes");
    }

	#[test]
	pub fn filter_traces_length_test() {
        let inputs = vec![
			JavascriptInput::from(std::fs::read_to_string("./testfiles/a-b.csv").unwrap())
			// object XES event log ./testfiles/a-b.csv
			,
			JavascriptInput::from("<".to_string())
			// string <
			,
			JavascriptInput::from("0".to_string())
			// usize 0

		];
        crate::javascript::javascript_autogen::filter_traces_length(inputs, ".xes");
    }

	#[test]
	pub fn information_test() {
        let inputs = vec![
			JavascriptInput::from(std::fs::read_to_string("./testfiles/flower.bpmn").unwrap())
			// object business process model and notation ./testfiles/flower.bpmn

		];
        crate::javascript::javascript_autogen::information(inputs, ".xes");
    }

	#[test]
	pub fn probability_log_test() {
        let inputs = vec![
			JavascriptInput::from(std::fs::read_to_string("./testfiles/seq(a-xor(b-c)).sptree").unwrap())
			// trait queriable stochastic language ./testfiles/seq(a-xor(b-c)).sptree
			,
			JavascriptInput::from(std::fs::read_to_string("./testfiles/aa-ab-ba.lang").unwrap())
			// trait finite language ./testfiles/aa-ab-ba.lang

		];
        crate::javascript::javascript_autogen::probability_log(inputs, ".xes");
    }

	#[test]
	pub fn sample_folds_test() {
        let inputs = vec![
			JavascriptInput::from(std::fs::read_to_string("./testfiles/a-b.csv").unwrap())
			// object event log ./testfiles/a-b.csv
			,
			JavascriptInput::from("1".to_string())
			// usize 1
			,
			JavascriptInput::from("0".to_string())
			// usize 0
			,
			JavascriptInput::from("0".to_string())
			// usize 0

		];
        crate::javascript::javascript_autogen::sample_folds(inputs, ".xes");
    }

	#[test]
	pub fn sample_partially_ordered_traces_test() {
        let inputs = vec![
			JavascriptInput::from(std::fs::read_to_string("./testfiles/flower.sbpmn").unwrap())
			// object stochastic business process model and notation ./testfiles/flower.sbpmn
			,
			JavascriptInput::from("1".to_string())
			// usize 1

		];
        crate::javascript::javascript_autogen::sample_partially_ordered_traces(inputs, ".xes");
    }

	#[test]
	pub fn sample_traces_test() {
        let inputs = vec![
			JavascriptInput::from(std::fs::read_to_string("./testfiles/a-b.csv").unwrap())
			// trait finite stochastic language ./testfiles/a-b.csv
			,
			JavascriptInput::from("1".to_string())
			// usize 1

		];
        crate::javascript::javascript_autogen::sample_traces(inputs, ".xes");
    }

	#[test]
	pub fn test_bootstrap_test_test() {
        let inputs = vec![
			JavascriptInput::from(std::fs::read_to_string("./testfiles/a-b.csv").unwrap())
			// trait finite stochastic language ./testfiles/a-b.csv
			,
			JavascriptInput::from(std::fs::read_to_string("./testfiles/a-b.csv").unwrap())
			// trait finite stochastic language ./testfiles/a-b.csv
			,
			JavascriptInput::from("10".to_string())
			// usize 10
			,
			JavascriptInput::from("0.05".to_string())
			// fraction 0.05

		];
        crate::javascript::javascript_autogen::test_bootstrap_test(inputs, ".xes");
    }

	#[test]
	#[should_panic]
	pub fn test_log_categorical_attribute_test() {
        let inputs = vec![
			JavascriptInput::from(std::fs::read_to_string("./testfiles/a-b-double.xes").unwrap())
			// trait event log with trace attributes ./testfiles/a-b-double.xes
			,
			JavascriptInput::from("some string".to_string())
			// string some string
			,
			JavascriptInput::from("10".to_string())
			// usize 10
			,
			JavascriptInput::from("0.05".to_string())
			// fraction 0.05

		];
        crate::javascript::javascript_autogen::test_log_categorical_attribute(inputs, ".xes");
    }

	#[test]
	pub fn visualise_graph_test() {
        let inputs = vec![
			JavascriptInput::from(std::fs::read_to_string("./testfiles/flower.bpmn").unwrap())
			// trait graphable ./testfiles/flower.bpmn

		];
        crate::javascript::javascript_autogen::visualise_graph(inputs, ".xes");
    }

	#[test]
	pub fn visualise_text_test() {
        let inputs = vec![
			JavascriptInput::from(std::fs::read_to_string("./testfiles/flower.bpmn").unwrap())
			// object business process model and notation ./testfiles/flower.bpmn

		];
        crate::javascript::javascript_autogen::visualise_text(inputs, ".xes");
    }

}
