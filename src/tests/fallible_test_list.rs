use crate::{
    ebi_commands::{
        ebi_command_analyse::{EBI_ANALYSE_MEDOID, EBI_ANALYSE_MINPROB},
        ebi_command_analyse_non_stochastic::{
            EBI_ANALYSE_NON_STOCHASTIC_CLUSTER, EBI_ANALYSE_NON_STOCHASTIC_MEDOID,
            EBI_ANALYSE_NON_STOCHASTIC_TIMESTAMPS_ORDERED,
        },
        ebi_command_association::{EBI_ASSOCIATION_ATTRIBUTE, EBI_ASSOCIATION_ATTRIBUTES},
        ebi_command_conformance::{
            EBI_CONFORMANCE_CHI_SQUARED_SAMPLE, EBI_CONFORMANCE_EARTH_MOVERS,
            EBI_CONFORMANCE_EARTH_MOVERS_SAMPLE, EBI_CONFORMANCE_GAIN_PRECISION,
            EBI_CONFORMANCE_GAIN_RECALL, EBI_CONFORMANCE_HELLINGER_SAMPLE,
        },
        ebi_command_conformance_non_stochastic::{
            EBI_CONFORMANCE_NON_STOCHASTIC_ALIGNMENTS,
            EBI_CONFORMANCE_NON_STOCHASTIC_ESCAPING_EDGES_PRECISION,
            EBI_CONFORMANCE_NON_STOCHASTIC_SET_ALIGNMENTS,
        },
        ebi_command_sample::EBI_SAMPLE_TRACES,
        ebi_command_test::{EBI_TEST_BOOTSTRAP, EBI_TEST_LOG_ATTRIBUTE},
    },
    ebi_framework::ebi_command::EbiCommand,
};

/// these tests are known, accepted and required to fail
pub(crate) const FALLIBLE_TESTS: &'static [(&'static EbiCommand, &'static [&'static str])] = &[
    (
        &EBI_ANALYSE_MEDOID,
        &[
            "trait finite stochastic language#./testfiles/empty.xes",
            "usize 1",
        ],
    ),
    (
        &EBI_ANALYSE_MINPROB,
        &[
            "trait stochastic deterministic semantics#./testfiles/empty.xes",
            "fraction 0",
        ],
    ),
    (
        &EBI_ANALYSE_NON_STOCHASTIC_CLUSTER,
        &["trait finite language#./testfiles/empty.xes", "usize 1"],
    ),
    (
        &EBI_ANALYSE_NON_STOCHASTIC_MEDOID,
        &["trait finite language#./testfiles/empty.xes", "usize 1"],
    ),
    (
        &EBI_ANALYSE_NON_STOCHASTIC_TIMESTAMPS_ORDERED,
        &["trait event log with event attributes#./testfiles/empty.xes"],
    ),
    (
        &EBI_ANALYSE_NON_STOCHASTIC_TIMESTAMPS_ORDERED,
        &["trait event log with event attributes#./testfiles/simple_log_markovian_abstraction.xes"],
    ),
    (
        &EBI_ANALYSE_NON_STOCHASTIC_TIMESTAMPS_ORDERED,
        &["trait event log with event attributes#./testfiles/a-b-double.xes"],
    ),
    (
        &EBI_ANALYSE_NON_STOCHASTIC_TIMESTAMPS_ORDERED,
        &["trait event log with event attributes#./testfiles/a-b.csv"],
    ),
    (
        &EBI_ANALYSE_NON_STOCHASTIC_TIMESTAMPS_ORDERED,
        &["trait event log with event attributes#./testfiles/a-b.xes.gz"],
    ),
    (
        &EBI_ANALYSE_NON_STOCHASTIC_TIMESTAMPS_ORDERED,
        &["trait event log with event attributes#./testfiles/fig_a.xes"],
    ),
    (
        &EBI_ASSOCIATION_ATTRIBUTE,
        &[
            "trait event log with trace attributes#./testfiles/empty.xes",
            "string some string",
            "usize 10",
        ],
    ),
    (
        &EBI_ASSOCIATION_ATTRIBUTE,
        &[
            "trait event log with trace attributes#./testfiles/oc-log.ocel",
            "string some string",
            "usize 10",
        ],
    ),
    (
        &EBI_ASSOCIATION_ATTRIBUTE,
        &[
            "trait event log with trace attributes#./testfiles/simple_log_markovian_abstraction.xes",
            "string some string",
            "usize 10",
        ],
    ),
    (
        &EBI_ASSOCIATION_ATTRIBUTE,
        &[
            "trait event log with trace attributes#./testfiles/a-b-double.xes",
            "string some string",
            "usize 10",
        ],
    ),
    (
        &EBI_ASSOCIATION_ATTRIBUTE,
        &[
            "trait event log with trace attributes#./testfiles/a-b.xes.gz",
            "string some string",
            "usize 10",
        ],
    ),
    (
        &EBI_ASSOCIATION_ATTRIBUTE,
        &[
            "trait event log with trace attributes#./testfiles/fig_a.xes",
            "string some string",
            "usize 10",
        ],
    ),
    (
        &EBI_ASSOCIATION_ATTRIBUTES,
        &[
            "trait event log with trace attributes#./testfiles/a-b-double.xes",
            "usize 10",
        ],
    ),
    (
        &EBI_ASSOCIATION_ATTRIBUTES,
        &[
            "trait event log with trace attributes#./testfiles/a-b.xes.gz",
            "usize 10",
        ],
    ),
    (
        &EBI_CONFORMANCE_GAIN_PRECISION,
        &[
            "trait finite stochastic language#./testfiles/a-b.csv",
            "object stochastic deterministic finite automaton#./testfiles/empty.sdfa",
            "fraction 0",
        ],
    ),
    (
        &EBI_CONFORMANCE_GAIN_RECALL,
        &[
            "trait finite stochastic language#./testfiles/a-b.csv",
            "object stochastic deterministic finite automaton#./testfiles/empty.sdfa",
            "fraction 0",
        ],
    ),
    (
        &EBI_CONFORMANCE_CHI_SQUARED_SAMPLE,
        &[
            "trait finite stochastic language#./testfiles/ba-aa-ab.slang",
            "trait finite stochastic language#./testfiles/empty.xes",
            "usize 1",
        ],
    ),
    (
        &EBI_CONFORMANCE_EARTH_MOVERS,
        &[
            "trait finite stochastic language#./testfiles/ba-aa-ab.slang",
            "trait finite stochastic language#./testfiles/empty.xes",
        ],
    ),
    (
        &EBI_CONFORMANCE_EARTH_MOVERS_SAMPLE,
        &[
            "trait finite stochastic language#./testfiles/ba-aa-ab.slang",
            "trait finite stochastic language#./testfiles/empty.xes",
            "usize 1",
        ],
    ),
    (
        &EBI_CONFORMANCE_HELLINGER_SAMPLE,
        &[
            "trait finite stochastic language#./testfiles/ba-aa-ab.slang",
            "trait finite stochastic language#./testfiles/empty.xes",
            "usize 1",
        ],
    ),
    (
        &EBI_CONFORMANCE_NON_STOCHASTIC_ALIGNMENTS,
        &[
            "trait finite stochastic language#./testfiles/ba-aa-ab.slang",
            "trait semantics#./testfiles/empty.xes",
        ],
    ),
    (
        &EBI_CONFORMANCE_NON_STOCHASTIC_ESCAPING_EDGES_PRECISION,
        &[
            "object stochastic language of alignments#./testfiles/aa-ab-ba.sali",
            "trait semantics#./testfiles/empty.xes",
        ],
    ),
    (
        &EBI_CONFORMANCE_NON_STOCHASTIC_ESCAPING_EDGES_PRECISION,
        &[
            "object stochastic language of alignments#./testfiles/aa-ab-ba.sali",
            "trait semantics#./testfiles/flower.bpmn",
        ],
    ),
    (
        &EBI_CONFORMANCE_NON_STOCHASTIC_ESCAPING_EDGES_PRECISION,
        &[
            "object stochastic language of alignments#./testfiles/aa-ab-ba.sali",
            "trait semantics#./testfiles/seq(a-xor(b-c)).sptree",
        ],
    ),
    (
        &EBI_CONFORMANCE_NON_STOCHASTIC_ESCAPING_EDGES_PRECISION,
        &[
            "object stochastic language of alignments#./testfiles/aa-ab-ba.sali",
            "trait semantics#./testfiles/ba-aa-ab.slang",
        ],
    ),
    (
        &EBI_CONFORMANCE_NON_STOCHASTIC_ESCAPING_EDGES_PRECISION,
        &[
            "object stochastic language of alignments#./testfiles/aa-ab-ba.sali",
            "trait semantics#./testfiles/a-b.slang",
        ],
    ),
    (
        &EBI_CONFORMANCE_NON_STOCHASTIC_SET_ALIGNMENTS,
        &[
            "trait finite language#./testfiles/ba-aa-ab.slang",
            "trait semantics#./testfiles/empty.xes",
        ],
    ),
    (
        &EBI_TEST_BOOTSTRAP,
        &[
            "trait finite stochastic language#./testfiles/ba-aa-ab.slang",
            "trait finite stochastic language#./testfiles/empty.xes",
            "usize 10",
            "fraction 0.05",
        ],
    ),
    (
        &EBI_TEST_LOG_ATTRIBUTE,
        &[
            "trait event log with trace attributes#./testfiles/a-b-double.xes",
            "string some string",
            "usize 10",
            "fraction 0.05",
        ],
    ),
    (
        &EBI_TEST_LOG_ATTRIBUTE,
        &[
            "trait event log with trace attributes#./testfiles/fig_a.xes",
            "string some string",
            "usize 10",
            "fraction 0.05",
        ],
    ),
    (
        &EBI_TEST_LOG_ATTRIBUTE,
        &[
            "trait event log with trace attributes#./testfiles/simple_log_markovian_abstraction.xes",
            "string some string",
            "usize 10",
            "fraction 0.05",
        ],
    ),
    (
        &EBI_TEST_LOG_ATTRIBUTE,
        &[
            "trait event log with trace attributes#./testfiles/empty.xes",
            "string some string",
            "usize 10",
            "fraction 0.05",
        ],
    ),
    (
        &EBI_TEST_LOG_ATTRIBUTE,
        &[
            "trait event log with trace attributes#./testfiles/a-b.xes.gz",
            "string some string",
            "usize 10",
            "fraction 0.05",
        ],
    ),
    (
        &EBI_SAMPLE_TRACES,
        &[
            "trait finite stochastic language#./testfiles/empty.xes",
            "usize 1",
        ],
    ),
];
