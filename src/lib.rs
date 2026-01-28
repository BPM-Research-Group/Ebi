pub mod ebi_framework {
    pub mod displayable;
    pub mod ebi_command;
    pub mod ebi_file_handler;
    pub mod ebi_importer_parameters;
    pub mod ebi_input;
    pub mod ebi_output;
    pub mod ebi_trait;
    pub mod ebi_trait_object;
    pub mod manual;
    pub mod object_importers;
    pub mod trait_importers;
    pub mod validate;
}
pub mod ebi_commands {
    pub mod ebi_command_analyse;
    pub mod ebi_command_analyse_non_stochastic;
    pub mod ebi_command_association;
    pub mod ebi_command_conformance;
    pub mod ebi_command_conformance_non_stochastic;
    pub mod ebi_command_convert;
    pub mod ebi_command_discover;
    pub mod ebi_command_discover_non_stochastic;
    pub mod ebi_command_filter;
    pub mod ebi_command_info;
    pub mod ebi_command_itself;
    pub mod ebi_command_probability;
    pub mod ebi_command_sample;
    pub mod ebi_command_test;
    pub mod ebi_command_validate;
    pub mod ebi_command_visualise;
}
pub mod ebi_file_handlers {
    pub mod compressed_event_log;
    pub mod deterministic_finite_automaton;
    pub mod directly_follows_graph;
    pub mod directly_follows_model;
    pub mod event_log_csv;
    pub mod event_log_python;
    pub mod event_log_xes;
    pub mod executions;
    pub mod finite_language;
    pub mod finite_stochastic_language;
    pub mod labelled_petri_net;
    pub mod language_of_alignments;
    pub mod lola_net;
    pub mod petri_net_markup_language;
    pub mod portable_document_format;
    pub mod portable_network_graphics;
    pub mod process_tree;
    pub mod process_tree_markup_language;
    pub mod scalable_vector_graphics;
    pub mod stochastic_deterministic_finite_automaton;
    pub mod stochastic_directly_follows_model;
    pub mod stochastic_labelled_petri_net;
    pub mod stochastic_language_of_alignments;
    pub mod stochastic_nondeterministic_finite_automaton;
    pub mod stochastic_process_tree;
}
pub mod ebi_traits {
    pub mod ebi_trait_activities;
    pub mod ebi_trait_event_log;
    pub mod ebi_trait_event_log_trace_attributes;
    pub mod ebi_trait_finite_language;
    pub mod ebi_trait_finite_stochastic_language;
    pub mod ebi_trait_graphable;
    pub mod ebi_trait_iterable_language;
    pub mod ebi_trait_iterable_stochastic_language;
    pub mod ebi_trait_markovian_abstraction;
    pub mod ebi_trait_queriable_stochastic_language;
    pub mod ebi_trait_semantics;
    pub mod ebi_trait_stochastic_deterministic_semantics;
    pub mod ebi_trait_stochastic_partially_ordered_semantics;
    pub mod ebi_trait_stochastic_semantics;
}
pub mod math {
    pub mod average;

    pub mod log_div;
    #[cfg(any(
        all(
            not(feature = "eexactarithmetic"),
            not(feature = "eapproximatearithmetic")
        ),
        all(feature = "eexactarithmetic", feature = "eapproximatearithmetic")
    ))]
    pub mod log_div_enum;
    #[cfg(all(feature = "eexactarithmetic", not(feature = "eapproximatearithmetic")))]
    pub mod log_div_exact;
    #[cfg(all(not(feature = "eexactarithmetic"), feature = "eapproximatearithmetic"))]
    pub mod log_div_f64;

    pub mod fixed_denominator_fraction;
    #[cfg(any(
        all(
            not(feature = "eexactarithmetic"),
            not(feature = "eapproximatearithmetic")
        ),
        all(feature = "eexactarithmetic", feature = "eapproximatearithmetic")
    ))]
    pub mod fixed_denominator_fraction_enum;
    #[cfg(all(feature = "eexactarithmetic", not(feature = "eapproximatearithmetic")))]
    pub mod fixed_denominator_fraction_exact;
    #[cfg(all(not(feature = "eexactarithmetic"), feature = "eapproximatearithmetic"))]
    pub mod fixed_denominator_fraction_f64;

    pub mod correlation;
    pub mod distances;
    pub mod distances_matrix;
    pub mod distances_triangular;
    pub mod levenshtein;
    pub mod markov_model;
    pub mod root;
    pub mod root_log_div;
}

pub mod prom {
    pub mod java_object_handler;
    #[cfg(feature = "java")]
    pub mod prom_link;
    #[cfg(feature = "java")]
    pub mod prom_plugin_generator;
}

pub mod python {
    pub mod python;
    #[cfg(feature = "python")]
    pub mod python_export;
    #[cfg(feature = "python")]
    pub mod python_import;
    #[cfg(feature = "python")]
    pub mod python_link;
    #[cfg(feature = "python")]
    pub mod python_module_autogen;
    #[cfg(feature = "python")]
    pub mod python_module_generator;
}

pub mod semantics {
    pub mod directly_follows_graph_semantics;
    pub mod finite_stochastic_language_semantics;
    pub mod labelled_petri_net_semantics;
    pub mod process_tree_semantics;
    pub mod semantics;
    pub mod stochastic_deterministic_finite_automaton_semantics;
    pub mod stochastic_directly_follows_graph_semantics;
    pub mod stochastic_labelled_petri_net_semantics;
    pub mod stochastic_nondetermininstic_finite_automaton_semantics;
    pub mod stochastic_process_tree_semantics;
}
pub mod stochastic_partially_ordered_semantics {
    pub mod stochastic_partially_ordered_semantics;
    pub mod stochastic_process_tree_stochastic_partially_ordered_semantics;
}
pub mod stochastic_semantics {
    pub mod finite_stochastic_language_semantics;
    pub mod stochastic_deterministic_finite_automaton_semantics;
    pub mod stochastic_directly_follows_model_semantics;
    pub mod stochastic_labelled_petri_net_semantics;
    pub mod stochastic_nondeterministic_finite_automaton_semantics;
    pub mod stochastic_process_tree_semantics;
    pub mod stochastic_semantics;
}
pub mod techniques {
    pub mod align;
    pub mod alignment_stochastic_miner;
    pub mod any_traces;
    pub mod association;
    pub mod bootstrap_test;
    pub mod bounded;
    pub mod chi_square_stochastic_conformance;
    pub mod completeness;
    pub mod deterministic_semantics_for_stochastic_semantics;
    pub mod directly_follows_graph_abstractor;
    pub mod directly_follows_model_miner;
    pub mod earth_movers_stochastic_conformance;
    #[cfg(any(
        all(
            not(feature = "eexactarithmetic"),
            not(feature = "eapproximatearithmetic")
        ),
        all(feature = "eexactarithmetic", feature = "eapproximatearithmetic")
    ))]
    pub mod earth_movers_stochastic_conformance_enum;
    #[cfg(all(feature = "eexactarithmetic", not(feature = "eapproximatearithmetic")))]
    pub mod earth_movers_stochastic_conformance_exact;
    #[cfg(all(not(feature = "eexactarithmetic"), feature = "eapproximatearithmetic"))]
    pub mod earth_movers_stochastic_conformance_f64;
    pub mod edge_difference;
    pub mod entropic_relevance;
    pub mod escaping_edges_precision;
    pub mod executions;
    pub mod explain_trace;
    pub mod filter;
    pub mod fitness;
    pub mod flower_miner;
    pub mod hellinger_stochastic_conformance;
    pub mod infinitely_many_traces;
    pub mod jensen_shannon_stochastic_conformance;
    pub mod livelock;
    pub mod medoid;
    pub mod medoid_non_stochastic;
    pub mod non_decreasing_livelock;
    pub mod occurrences_stochastic_miner;
    pub mod permutation_test;
    pub mod prefix_tree_miner;
    pub mod probability_queries;
    pub mod process_variety;
    pub mod sample;
    pub mod sample_folds;
    pub mod stochastic_markovian_abstraction;
    pub mod stochastic_markovian_abstraction_conformance;
    pub mod tau_removal;
    pub mod trace_probability;
    pub mod uniform_stochastic_miner;
    pub mod unit_earth_movers_stochastic_conformance;
}
pub mod follower_semantics;
pub mod json;
pub mod line_reader;
pub mod multiple_reader;
pub mod tests;
pub mod text;
