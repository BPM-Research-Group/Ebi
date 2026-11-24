#![allow(
    clippy::needless_range_loop,
    clippy::doc_lazy_continuation,
    clippy::doc_overindented_list_items,
    clippy::type_complexity
)]
#![cfg_attr(not(doctest), doc = include_str!("../README.md"))]

//! ## Sister crates
//!
//! Ebi has several sister crates:
//! 1. The crate [Ebi](https://docs.rs/ebi/latest/ebi/) contains the framework, surfaces (CLI and libraries for other languages), traits, documentation and manual.
//! 1. The crate [Ebi_objects](https://docs.rs/ebi_objects/latest/ebi_objects/) contains the logs, models and other object structs, as well as conversions between them.
//! 1. The crate [Ebi_bpmn](https://docs.rs/ebi_bpmn/latest/ebi_bpmn/) takes care of BPMN models.
//! 1. The crate [Ebi_arithmetic](https://docs.rs/ebi_arithmetic/latest/ebi_arithmetic/) contains exact and approximate arithmetic machinery.
//! 1. The crate [Ebi_activity_key](https://docs.rs/ebi_activity_key/latest/ebi_activity_key/) contains a string-deduplication abstraction.
//! 1. The crate [Ebi_optimisation](https://docs.rs/ebi_optimisation/latest/ebi_optimisation/) contains optimisation routines.
//! 1. The crate [Ebi_derive](https://docs.rs/ebi_derive/latest/ebi_derive/) contains derivation macros.

/// A command makes a technique available to the Ebi framework.
/// This module contains the command hierarchy.
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
    pub mod ebi_command_reduce;
    pub mod ebi_command_sample;
    pub mod ebi_command_test;
    pub mod ebi_command_validate;
    pub mod ebi_command_visualise;
}

/// A file handler is an importer and exporter for a particular file format.
/// In Ebi, a file handler defines how a file type is to be handled,
/// as which objects and traits it can be imported,
/// and which objects can be exported to it.
///
/// For documentation on file handlers, please refer to the [manual](https://github.com/BPM-Research-Group/Ebi/releases/latest/download/manual.pdf).
pub mod ebi_file_handlers {
    mod business_process_model_and_notation;
    mod compressed_event_log;
    mod deterministic_finite_automaton;
    mod directly_follows_graph;
    mod directly_follows_model;
    mod event_log_csv;
    mod event_log_ocel;
    mod event_log_python;
    mod event_log_xes;
    mod executions;
    mod finite_language;
    mod finite_stochastic_language;
    mod finite_stochastic_partially_ordered_language;
    mod labelled_petri_net;
    mod language_of_alignments;
    mod lola_net;
    mod partially_ordered_workflow_language;
    mod petri_net_markup_language;
    mod portable_document_format;
    mod portable_network_graphics;
    mod process_tree;
    mod process_tree_markup_language;
    mod scalable_vector_graphics;
    mod stochastic_business_process_model_and_notation;
    mod stochastic_deterministic_finite_automaton;
    mod stochastic_directly_follows_model;
    mod stochastic_labelled_petri_net;
    mod stochastic_language_of_alignments;
    mod stochastic_nondeterministic_finite_automaton;
    mod stochastic_process_tree;

    pub use business_process_model_and_notation::EBI_BUSINESS_PROCESS_MODEL_AND_NOTATION;
    pub use compressed_event_log::EBI_COMPRESSED_EVENT_LOG;
    pub use deterministic_finite_automaton::EBI_DETERMINISTIC_FINITE_AUTOMATON;
    pub use directly_follows_graph::EBI_DIRECTLY_FOLLOWS_GRAPH;
    pub use directly_follows_model::EBI_DIRECTLY_FOLLOWS_MODEL;
    pub use event_log_csv::EBI_EVENT_LOG_CSV;
    pub use event_log_ocel::EBI_EVENT_LOG_OCEL;
    pub use event_log_python::EBI_EVENT_LOG_PYTHON;
    pub use event_log_xes::EBI_EVENT_LOG_XES;
    pub use executions::EBI_EXECUTIONS;
    pub use finite_language::EBI_FINITE_LANGUAGE;
    pub use finite_stochastic_language::EBI_FINITE_STOCHASTIC_LANGUAGE;
    pub use finite_stochastic_partially_ordered_language::EBI_FINITE_STOCHASTIC_PARTIALLY_ORDERED_LANGUAGE;
    pub use labelled_petri_net::EBI_LABELLED_PETRI_NET;
    pub use language_of_alignments::EBI_LANGUAGE_OF_ALIGNMENTS;
    pub use lola_net::EBI_LOLA_NET;
    pub use partially_ordered_workflow_language::EBI_PARTIALLY_ORDERED_WORKFLOW_LANGUAGE;
    pub use petri_net_markup_language::EBI_PETRI_NET_MARKUP_LANGUAGE;
    pub use portable_document_format::EBI_PORTABLE_DOCUMENT_FORMAT;
    pub use portable_network_graphics::EBI_PORTABLE_NETWORK_GRAPHICS;
    pub use process_tree::EBI_PROCESS_TREE;
    pub use process_tree_markup_language::EBI_PROCESS_TREE_MARKUP_LANGUAGE;
    pub use scalable_vector_graphics::EBI_SCALABLE_VECTOR_GRAPHICS;
    pub use stochastic_business_process_model_and_notation::EBI_STOCHASTIC_BUSINESS_PROCESS_MODEL_AND_NOTATION;
    pub use stochastic_deterministic_finite_automaton::EBI_STOCHASTIC_DETERMINISTIC_FINITE_AUTOMATON;
    pub use stochastic_directly_follows_model::EBI_STOCHASTIC_DIRECTLY_FOLLOWS_MODEL;
    pub use stochastic_labelled_petri_net::EBI_STOCHASTIC_LABELLED_PETRI_NET;
    pub use stochastic_language_of_alignments::EBI_STOCHASTIC_LANGUAGE_OF_ALIGNMENTS;
    pub use stochastic_nondeterministic_finite_automaton::EBI_STOCHASTIC_NONDETERMINISTIC_FINITE_AUTOMATON;
    pub use stochastic_process_tree::EBI_STOCHASTIC_PROCESS_TREE;
}

/// The framework module contains the Ebi framework.
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

/// An Ebi trait is a contract or interface on objects.
/// A trait allows to call methods on the object without knowing what object it is, thereby providing an abstraction.
/// For documentation on Ebi traits and mathematical definitions, please refer to the [manual](https://github.com/BPM-Research-Group/Ebi/releases/latest/download/manual.pdf).
pub mod ebi_traits {
    mod ebi_trait_activities;
    mod ebi_trait_event_log;
    mod ebi_trait_event_log_event_attributes;
    mod ebi_trait_event_log_trace_attributes;
    mod ebi_trait_finite_language;
    mod ebi_trait_finite_stochastic_language;
    mod ebi_trait_graphable;
    mod ebi_trait_iterable_language;
    mod ebi_trait_iterable_stochastic_language;
    mod ebi_trait_queriable_stochastic_language;
    mod ebi_trait_semantics;
    mod ebi_trait_stochastic_deterministic_semantics;
    mod ebi_trait_stochastic_partially_ordered_semantics;
    mod ebi_trait_stochastic_semantics;

    pub use ebi_trait_activities::EbiTraitActivities;
    pub use ebi_trait_event_log::EbiTraitEventLog;
    pub use ebi_trait_event_log_event_attributes::EbiTraitEventLogEventAttributes;
    pub use ebi_trait_event_log_trace_attributes::EbiTraitEventLogTraceAttributes;
    pub use ebi_trait_finite_language::EbiTraitFiniteLanguage;
    pub use ebi_trait_finite_stochastic_language::EbiTraitFiniteStochasticLanguage;
    pub use ebi_trait_graphable::EbiTraitGraphable;
    pub use ebi_trait_iterable_language::EbiTraitIterableLanguage;
    pub use ebi_trait_iterable_stochastic_language::EbiTraitIterableStochasticLanguage;
    pub use ebi_trait_queriable_stochastic_language::EbiTraitQueriableStochasticLanguage;
    pub use ebi_trait_semantics::EbiTraitSemantics;
    pub use ebi_trait_stochastic_deterministic_semantics::EbiTraitStochasticDeterministicSemantics;
    pub use ebi_trait_stochastic_partially_ordered_semantics::EbiTraitStochasticPartiallyOrderedSemantics;
    pub use ebi_trait_stochastic_semantics::EbiTraitStochasticSemantics;
}

/// The math module contains helper code for mathematical operations.
pub mod math {
    pub mod average;
    pub mod components;
    pub mod correlation;
    pub mod distances;
    pub mod distances_matrix;
    pub mod distances_triangular;
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
    pub mod levenshtein;
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
    pub mod markov_model;
    pub mod root;
    pub mod root_log_div;
    pub mod sign;
}

pub mod python {
    pub mod python;
    #[cfg(feature = "python")]
    pub mod python_export;
    #[cfg(feature = "python")]
    pub mod python_handle;
    #[cfg(feature = "python")]
    pub mod python_import;
    #[cfg(feature = "python")]
    pub mod python_link;
    // Not built for tests: it exports PyInit__ebi, which would be defined twice in a test binary.
    #[cfg(all(feature = "python", not(test)))]
    pub mod python_module_autogen;
    #[cfg(feature = "surface_generation")]
    pub mod python_module_generator;
}

/// Semantics provide meaning and interpretation to behaviour.
/// This module contains code that defines the behaviour of objects, if that is not included in the Ebi_objects crate.
pub mod semantics {
    pub mod automaton_semantics;
    pub mod business_process_model_and_notation_semantics;
    pub mod finite_stochastic_language_semantics;
    pub mod finite_stochastic_partially_ordered_language_semantics;
    pub mod follower_semantics;
    pub mod labelled_petri_net_semantics;
    pub mod partially_ordered_workflow_language_semantics;
    pub mod process_tree_semantics;
    pub mod semantics_trait;
    pub mod stochastic_business_process_model_and_notation_semantics;
    pub mod stochastic_labelled_petri_net_semantics;
    pub mod stochastic_process_tree_semantics;
    pub mod stochastic_deterministic_semantics {
        pub mod deterministic_semantics_for_stochastic_semantics;
        pub mod stochastic_deterministic_finite_automaton_deterministic;
        pub mod stochastic_deterministic_semantics_trait;
    }
    pub mod stochastic_partially_ordered_semantics {
        pub mod stochastic_partially_ordered_semantics_trait;
        pub mod stochastic_process_tree_stochastic_partially_ordered_semantics;
    }
    pub mod stochastic_semantics {
        pub mod directly_follows_graph_stochastic_semantics;
        pub mod finite_stochastic_language_semantics;
        pub mod stochastic_deterministic_finite_automaton_semantics;
        pub mod stochastic_directly_follows_model_semantics;
        pub mod stochastic_labelled_petri_net_semantics;
        pub mod stochastic_nondeterministic_finite_automaton_semantics;
        pub mod stochastic_process_tree_semantics;
        pub mod stochastic_semantics_trait;
    }
}

/// Ebi can be used through a surface, such as as a Python or Javascript library.
/// In this module, these surfaces are provided.
pub mod surfaces {
    pub mod javascript {
        #[cfg(feature = "javascript")]
        pub mod javascript_autogen;
        #[cfg(feature = "surface_generation")]
        pub mod javascript_generator_html;
        #[cfg(feature = "surface_generation")]
        pub mod javascript_generator_rust;
        #[cfg(feature = "test_generation")]
        pub mod javascript_generator_tests;
        pub mod javascript_link;
        #[cfg(feature = "javascript")]
        pub mod javascript_runner;
    }
    pub mod java {
        #[cfg(feature = "java")]
        pub mod java_autogen;
        #[cfg(feature = "java")]
        pub mod java_export;
        pub mod java_link;
        #[cfg(feature = "surface_generation")]
        pub mod java_module_generator_java;
        #[cfg(feature = "surface_generation")]
        pub mod java_module_generator_rust;
        pub mod java_object_handler;
    }
    pub mod website {
        #[cfg(feature = "surface_generation")]
        pub mod website_generation;
        pub mod website_link;
    }
    pub mod prom {
        pub mod prom_link;
        #[cfg(feature = "surface_generation")]
        pub mod prom_plugin_generator;
    }
}

/// The implementations of algorithms can be found in this module.
pub mod techniques {
    pub mod alergia;
    pub mod align;
    pub mod align_astar;
    pub mod align_process_tree;
    pub mod alignment_stochastic_miner;
    pub mod any_traces;
    pub mod association;
    pub mod bootstrap_test;
    pub mod bounded;
    pub mod chi_square_stochastic_conformance;
    pub mod cibdo_stochastic_miner;
    pub mod cohort_analysis;
    pub mod completeness;
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
    pub mod edge_difference_no_freq;
    pub mod empty_traces;
    pub mod entropic_relevance;
    pub mod entropy;
    pub mod escaping_edges_precision;
    pub mod executions;
    pub mod explain_trace;
    pub mod filter_stochastic_trace;
    pub mod filter_trace;
    pub mod fitness;
    pub mod fitting_traces;
    pub mod flower_miner;
    pub mod gain_precision_recall;
    pub mod hellinger_stochastic_conformance;
    pub mod inductive_miner;
    pub mod infinitely_many_traces;
    pub mod jensen_shannon_stochastic_conformance;
    pub mod livelock;
    pub mod medoid;
    pub mod medoid_non_stochastic;
    pub mod non_decreasing_livelock;
    pub mod occurrences_stochastic_miner;
    pub mod partially_ordered_earth_movers_stochastic_conformance;
    pub mod partially_ordered_trace_distance;
    pub mod permutation_test;
    pub mod permutation_test_log_model;
    pub mod prefix_tree_miner;
    pub mod probability_queries;
    pub mod process_variety;
    pub mod random_stochastic_miner;
    pub mod reachability;
    pub mod reduce;
    pub mod resource_utilisation;
    pub mod sample;
    pub mod sample_folds;
    pub mod split_miner;
    pub mod stochastic_markovian_abstraction;
    pub mod stochastic_markovian_abstraction_conformance;
    pub mod tau_removal;
    pub mod timestamps_ordered;
    pub mod trace_model_miner;
    pub mod trace_probability;
    pub mod uniform_stochastic_miner;
    pub mod unit_earth_movers_stochastic_conformance;
}

/// Ebi generates its some of its own tests, for which generation code is found in this module.
pub mod tests {
    #[cfg(test)]
    pub mod cli_test_support;
    #[cfg(feature = "test_generation")]
    pub mod fallible_test;
    #[cfg(feature = "test_generation")]
    pub mod fallible_test_list;
    #[cfg(feature = "test_generation")]
    pub mod test_input;
    #[cfg(any(test, feature = "test_generation"))]
    pub mod tests;
    #[cfg(test)]
    pub mod tests_autogen;
    #[cfg(feature = "test_generation")]
    pub mod tests_generator;
}

pub(crate) mod multiple_reader;

/// Helper functions for text manipulation.
pub(crate) mod text;
