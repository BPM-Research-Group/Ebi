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
    ebi_objects::ebi_arithmetic::exact::set_exact_globally(false);
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
    ebi_objects::ebi_arithmetic::exact::set_exact_globally(false);
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
