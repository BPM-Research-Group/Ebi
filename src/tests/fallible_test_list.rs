use crate::{
    ebi_commands::{
        ebi_command_analyse::{EBI_ANALYSE_MEDOID, EBI_ANALYSE_MINPROB},
        ebi_command_analyse_non_stochastic::{
            EBI_ANALYSE_NON_STOCHASTIC_CLUSTER, EBI_ANALYSE_NON_STOCHASTIC_EXECUTIONS,
            EBI_ANALYSE_NON_STOCHASTIC_MEDOID, EBI_ANALYSE_NON_STOCHASTIC_TIMESTAMPS_ORDERED,
        },
        ebi_command_association::{EBI_ASSOCIATION_ATTRIBUTE, EBI_ASSOCIATION_ATTRIBUTES},
        ebi_command_conformance::{
            EBI_CONFORMANCE_CHI_SQUARED_SAMPLE, EBI_CONFORMANCE_EARTH_MOVERS,
            EBI_CONFORMANCE_EARTH_MOVERS_SAMPLE, EBI_CONFORMANCE_GAIN_PRECISION,
            EBI_CONFORMANCE_GAIN_RECALL, EBI_CONFORMANCE_HELLINGER_SAMPLE,
            EBI_CONFORMANCE_JSSC_SAMPLE, EBI_CONFORMANCE_UEMSC_SAMPLE,
        },
        ebi_command_conformance_non_stochastic::{
            EBI_CONFORMANCE_NON_STOCHASTIC_ALIGNMENTS,
            EBI_CONFORMANCE_NON_STOCHASTIC_ESCAPING_EDGES_PRECISION,
            EBI_CONFORMANCE_NON_STOCHASTIC_SET_ALIGNMENTS,
        },
        ebi_command_discover::{EBI_DISCOVER_ALIGNMENTS_BPMN, EBI_DISCOVER_ALIGNMENTS_SLPN},
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
            "*",
        ],
    ),
    (
        &EBI_ANALYSE_MEDOID,
        &[
            "trait finite stochastic language#./testfiles/empty.slang",
            "usize 1",
        ],
    ),
    (
        &EBI_ANALYSE_MINPROB,
        &[
            "trait stochastic deterministic semantics#./testfiles/empty.xes",
            "*",
        ],
    ),
    (
        &EBI_ANALYSE_MINPROB,
        &[
            "trait stochastic deterministic semantics#./testfiles/empty.sdfa",
            "*",
        ],
    ),
    (
        &EBI_ANALYSE_MINPROB,
        &[
            "trait stochastic deterministic semantics#./testfiles/all_operators.sptree",
            "fraction 0",
        ],
    ),
    (
        &EBI_ANALYSE_MINPROB,
        &[
            "trait stochastic deterministic semantics#./testfiles/fig_c.sdfa",
            "fraction 0",
        ],
    ),
    (
        &EBI_ANALYSE_MINPROB,
        &[
            "trait stochastic deterministic semantics#./testfiles/aa-ab-ba.sdfm",
            "fraction 0",
        ],
    ),
    (
        &EBI_ANALYSE_MINPROB,
        &[
            "trait stochastic deterministic semantics#./testfiles/aa-ab-ba.dfg",
            "fraction 0",
        ],
    ),
    (
        &EBI_ANALYSE_NON_STOCHASTIC_CLUSTER,
        &["trait finite language#./testfiles/empty.xes", "usize 1"],
    ),
    (
        &EBI_ANALYSE_NON_STOCHASTIC_CLUSTER,
        &["trait finite language#./testfiles/empty.slang", "usize 1"],
    ),
    (
        &EBI_ANALYSE_NON_STOCHASTIC_EXECUTIONS,
        &["*", "trait semantics#./testfiles/empty_2.ptree"],
    ),
    (
        &EBI_ANALYSE_NON_STOCHASTIC_EXECUTIONS,
        &["*", "trait semantics#./testfiles/empty.sdfa"],
    ),
    (
        &EBI_ANALYSE_NON_STOCHASTIC_EXECUTIONS,
        &["*", "trait semantics#./testfiles/empty.dfm"],
    ),
    (
        &EBI_ANALYSE_NON_STOCHASTIC_MEDOID,
        &["trait finite language#./testfiles/empty.xes", "usize 1"],
    ),
    (
        &EBI_ANALYSE_NON_STOCHASTIC_MEDOID,
        &["trait finite language#./testfiles/empty.slang", "usize 1"],
    ),
    (
        &EBI_ASSOCIATION_ATTRIBUTE,
        &["*", "string some string", "*"],
    ),
    (
        &EBI_ASSOCIATION_ATTRIBUTES,
        &[
            "trait event log with trace attributes#./testfiles/a-b-double.xes",
            "*",
        ],
    ),
    (
        &EBI_ASSOCIATION_ATTRIBUTES,
        &[
            "trait event log with trace attributes#./testfiles/a-b.xes",
            "*",
        ],
    ),
    (
        &EBI_ASSOCIATION_ATTRIBUTES,
        &[
            "trait event log with trace attributes#./testfiles/a-b.xes.gz",
            "*",
        ],
    ),
    (
        &EBI_ASSOCIATION_ATTRIBUTES,
        &[
            "trait event log with trace attributes#./testfiles/oc-log.ocel",
            "*",
        ],
    ),
    (
        &EBI_CONFORMANCE_CHI_SQUARED_SAMPLE,
        &[
            "*",
            "trait finite stochastic language#./testfiles/empty.xes",
            "usize 1",
        ],
    ),
    (
        &EBI_CONFORMANCE_CHI_SQUARED_SAMPLE,
        &[
            "*",
            "trait finite stochastic language#./testfiles/empty.slang",
            "usize 1",
        ],
    ),
    (
        &EBI_CONFORMANCE_EARTH_MOVERS,
        &[
            "*",
            "trait finite stochastic language#./testfiles/empty.xes",
        ],
    ),
    (
        &EBI_CONFORMANCE_EARTH_MOVERS,
        &[
            "*",
            "trait finite stochastic language#./testfiles/empty.slang",
        ],
    ),
    (
        &EBI_CONFORMANCE_EARTH_MOVERS_SAMPLE,
        &[
            "*",
            "trait finite stochastic language#./testfiles/empty.xes",
            "*",
        ],
    ),
    (
        &EBI_CONFORMANCE_EARTH_MOVERS_SAMPLE,
        &[
            "*",
            "trait finite stochastic language#./testfiles/empty.slang",
            "usize 1",
        ],
    ),
    (
        &EBI_CONFORMANCE_GAIN_PRECISION,
        &[
            "*",
            "object stochastic deterministic finite automaton#./testfiles/empty.sdfa",
            "fraction 0",
        ],
    ),
    (
        &EBI_CONFORMANCE_GAIN_PRECISION,
        &[
            "*",
            "object stochastic deterministic finite automaton#./testfiles/a-b_multiple_separators.csv",
            "fraction 0",
        ],
    ),
    (
        &EBI_CONFORMANCE_GAIN_PRECISION,
        &[
            "*",
            "object stochastic deterministic finite automaton#./testfiles/aa.slang",
            "fraction 0",
        ],
    ),
    (
        &EBI_CONFORMANCE_GAIN_PRECISION,
        &[
            "*",
            "object stochastic deterministic finite automaton#./testfiles/a-b_multiple_separators.csv",
            "fraction 0",
        ],
    ),
    (
        &EBI_CONFORMANCE_GAIN_PRECISION,
        &[
            "*",
            "object stochastic deterministic finite automaton#./testfiles/aa.slang",
            "fraction 0",
        ],
    ),
    (
        &EBI_CONFORMANCE_GAIN_PRECISION,
        &[
            "*",
            "object stochastic deterministic finite automaton#./testfiles/empty.xes",
            "fraction 0",
        ],
    ),
    (
        &EBI_CONFORMANCE_GAIN_PRECISION,
        &[
            "*",
            "object stochastic deterministic finite automaton#./testfiles/a-b.csv",
            "fraction 0",
        ],
    ),
    (
        &EBI_CONFORMANCE_GAIN_PRECISION,
        &[
            "*",
            "object stochastic deterministic finite automaton#./testfiles/bb.slang",
            "fraction 0",
        ],
    ),
    (
        &EBI_CONFORMANCE_GAIN_PRECISION,
        &[
            "*",
            "object stochastic deterministic finite automaton#./testfiles/oc-log.ocel",
            "fraction 0",
        ],
    ),
    (
        &EBI_CONFORMANCE_GAIN_PRECISION,
        &[
            "*",
            "object stochastic deterministic finite automaton#./testfiles/ba.slang",
            "fraction 0",
        ],
    ),
    (
        &EBI_CONFORMANCE_GAIN_RECALL,
        &[
            "trait finite stochastic language#./testfiles/a-b.csv",
            "*",
            "fraction 0",
        ],
    ),
    (
        &EBI_CONFORMANCE_HELLINGER_SAMPLE,
        &[
            "*",
            "trait finite stochastic language#./testfiles/empty.xes",
            "usize 1",
        ],
    ),
    (
        &EBI_CONFORMANCE_HELLINGER_SAMPLE,
        &[
            "*",
            "trait finite stochastic language#./testfiles/empty.slang",
            "usize 1",
        ],
    ),
    (
        &EBI_CONFORMANCE_JSSC_SAMPLE,
        &[
            "*",
            "trait finite stochastic language#./testfiles/empty.xes",
            "usize 1",
        ],
    ),
    (
        &EBI_CONFORMANCE_JSSC_SAMPLE,
        &[
            "*",
            "trait finite stochastic language#./testfiles/empty.slang",
            "usize 1",
        ],
    ),
    (
        &EBI_CONFORMANCE_UEMSC_SAMPLE,
        &[
            "*",
            "trait finite stochastic language#./testfiles/empty.slang",
            "usize 1",
        ],
    ),
    (
        &EBI_CONFORMANCE_UEMSC_SAMPLE,
        &[
            "*",
            "trait finite stochastic language#./testfiles/empty.xes",
            "usize 1",
        ],
    ),
    (
        &EBI_CONFORMANCE_NON_STOCHASTIC_ALIGNMENTS,
        &["*", "trait semantics#./testfiles/empty.xes"],
    ),
    (
        &EBI_CONFORMANCE_NON_STOCHASTIC_ALIGNMENTS,
        &["*", "trait semantics#./testfiles/empty_2.ptree"],
    ),
    (
        &EBI_CONFORMANCE_NON_STOCHASTIC_ALIGNMENTS,
        &["*", "trait semantics#./testfiles/empty.dfm"],
    ),
    (
        &EBI_CONFORMANCE_NON_STOCHASTIC_ALIGNMENTS,
        &["*", "trait semantics#./testfiles/empty.sdfa"],
    ),
    (
        &EBI_CONFORMANCE_NON_STOCHASTIC_ALIGNMENTS,
        &["*", "trait semantics#./testfiles/empty.lang"],
    ),
    (
        &EBI_CONFORMANCE_NON_STOCHASTIC_SET_ALIGNMENTS,
        &["*", "trait semantics#./testfiles/empty.lang"],
    ),
    (
        &EBI_ANALYSE_NON_STOCHASTIC_CLUSTER,
        &["trait finite language#./testfiles/empty.lang", "usize 1"],
    ),
    (
        &EBI_CONFORMANCE_NON_STOCHASTIC_ESCAPING_EDGES_PRECISION,
        &[
            "object stochastic language of alignments#./testfiles/aa-ab-ba.sali",
            "trait semantics#./testfiles/BPI_Challenge_2013_closed_problems.xes.gz-dfg.dfg",
        ],
    ),
    (
        &EBI_CONFORMANCE_NON_STOCHASTIC_ESCAPING_EDGES_PRECISION,
        &[
            "object stochastic language of alignments#./testfiles/aa-ab-ba.sali",
            "trait semantics#./testfiles/bpic12-a.xes.gz-dfg.dfg",
        ],
    ),
    (
        &EBI_CONFORMANCE_NON_STOCHASTIC_ESCAPING_EDGES_PRECISION,
        &["*", "trait semantics#./testfiles/a-b_star_empty.dfm"],
    ),
    (
        &EBI_CONFORMANCE_NON_STOCHASTIC_ESCAPING_EDGES_PRECISION,
        &[
            "object stochastic language of alignments#./testfiles/aa-ab-ba.sali",
            "trait semantics#./testfiles/a-b.xes.gz",
        ],
    ),
    (
        &EBI_CONFORMANCE_NON_STOCHASTIC_ESCAPING_EDGES_PRECISION,
        &[
            "object stochastic language of alignments#./testfiles/aa-ab-ba.sali",
            "trait semantics#./testfiles/a-b.xes",
        ],
    ),
    (
        &EBI_CONFORMANCE_NON_STOCHASTIC_ESCAPING_EDGES_PRECISION,
        &[
            "object stochastic language of alignments#./testfiles/aa-ab-ba.sali",
            "trait semantics#./testfiles/a-b-flower.ptree",
        ],
    ),
    (
        &EBI_CONFORMANCE_NON_STOCHASTIC_ESCAPING_EDGES_PRECISION,
        &[
            "object stochastic language of alignments#./testfiles/aa-ab-ba.sali",
            "trait semantics#./testfiles/a-b-double.xes",
        ],
    ),
    (
        &EBI_CONFORMANCE_NON_STOCHASTIC_ESCAPING_EDGES_PRECISION,
        &[
            "object stochastic language of alignments#./testfiles/aa-ab-ba.sali",
            "trait semantics#./testfiles/aa-ab-ba.dfa",
        ],
    ),
    (
        &EBI_CONFORMANCE_NON_STOCHASTIC_ESCAPING_EDGES_PRECISION,
        &[
            "object stochastic language of alignments#./testfiles/aa-ab-ba.sali",
            "trait semantics#./testfiles/a.pnml",
        ],
    ),
    (
        &EBI_CONFORMANCE_NON_STOCHASTIC_ESCAPING_EDGES_PRECISION,
        &[
            "object stochastic language of alignments#./testfiles/aa-ab-ba.sali",
            "trait semantics#./testfiles/aa-aaa-bb.slpn",
        ],
    ),
    (
        &EBI_CONFORMANCE_NON_STOCHASTIC_ESCAPING_EDGES_PRECISION,
        &[
            "object stochastic language of alignments#./testfiles/aa-ab-ba.sali",
            "trait semantics#./testfiles/a-b_star.dfm",
        ],
    ),
    (
        &EBI_CONFORMANCE_NON_STOCHASTIC_ESCAPING_EDGES_PRECISION,
        &[
            "object stochastic language of alignments#./testfiles/aa-ab-ba.sali",
            "trait semantics#./testfiles/bb.slang",
        ],
    ),
    (
        &EBI_CONFORMANCE_NON_STOCHASTIC_ESCAPING_EDGES_PRECISION,
        &[
            "object stochastic language of alignments#./testfiles/aa-ab-ba.sali",
            "trait semantics#./testfiles/acb-abc-ad-aded-adeded-adededed.slang",
        ],
    ),
    (
        &EBI_CONFORMANCE_NON_STOCHASTIC_ESCAPING_EDGES_PRECISION,
        &["*", "trait semantics#./testfiles/empty.xes"],
    ),
    (
        &EBI_CONFORMANCE_NON_STOCHASTIC_ESCAPING_EDGES_PRECISION,
        &["*", "trait semantics#./testfiles/empty_2.ptree"],
    ),
    (
        &EBI_CONFORMANCE_NON_STOCHASTIC_ESCAPING_EDGES_PRECISION,
        &["*", "trait semantics#./testfiles/empty.lang"],
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
            "trait semantics#./testfiles/aa-ab-ba.ptree",
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
        &EBI_CONFORMANCE_NON_STOCHASTIC_ESCAPING_EDGES_PRECISION,
        &[
            "object stochastic language of alignments#./testfiles/aa-ab-ba.sali",
            "trait semantics#./testfiles/simple_markovian_abstraction.slpn",
        ],
    ),
    (
        &EBI_CONFORMANCE_NON_STOCHASTIC_ESCAPING_EDGES_PRECISION,
        &[
            "object stochastic language of alignments#./testfiles/aa-ab-ba.sali",
            "trait semantics#./testfiles/all_operators.ptree",
        ],
    ),
    (
        &EBI_CONFORMANCE_NON_STOCHASTIC_ESCAPING_EDGES_PRECISION,
        &[
            "object stochastic language of alignments#./testfiles/aa-ab-ba.sali",
            "trait semantics#./testfiles/aa-ab-ba.lang",
        ],
    ),
    (
        &EBI_CONFORMANCE_NON_STOCHASTIC_ESCAPING_EDGES_PRECISION,
        &[
            "object stochastic language of alignments#./testfiles/aa-ab-ba.sali",
            "trait semantics#./testfiles/model.sbpmn.spolang",
        ],
    ),
    (
        &EBI_CONFORMANCE_NON_STOCHASTIC_ESCAPING_EDGES_PRECISION,
        &[
            "object stochastic language of alignments#./testfiles/aa-ab-ba.sali",
            "trait semantics#./testfiles/all_operators.sptree",
        ],
    ),
    (
        &EBI_CONFORMANCE_NON_STOCHASTIC_ESCAPING_EDGES_PRECISION,
        &["*", "trait semantics#./testfiles/empty.lpn"],
    ),
    (
        &EBI_CONFORMANCE_NON_STOCHASTIC_ESCAPING_EDGES_PRECISION,
        &["*", "trait semantics#./testfiles/empty.dfm"],
    ),
    (
        &EBI_CONFORMANCE_NON_STOCHASTIC_ESCAPING_EDGES_PRECISION,
        &[
            "object stochastic language of alignments#./testfiles/aa-ab-ba.sali",
            "trait semantics#./testfiles/a-b_star_empty_trace.dfm",
        ],
    ),
    (
        &EBI_CONFORMANCE_NON_STOCHASTIC_ESCAPING_EDGES_PRECISION,
        &["*", "trait semantics#./testfiles/empty.sdfa"],
    ),
    (
        &EBI_CONFORMANCE_NON_STOCHASTIC_ESCAPING_EDGES_PRECISION,
        &[
            "object stochastic language of alignments#./testfiles/aa-ab-ba.sali",
            "trait semantics#./testfiles/a-aa-bb.slpn",
        ],
    ),
    (
        &EBI_CONFORMANCE_NON_STOCHASTIC_ESCAPING_EDGES_PRECISION,
        &[
            "object stochastic language of alignments#./testfiles/aa-ab-ba.sali",
            "trait semantics#./testfiles/aa-ab-ba.dfg",
        ],
    ),
    (
        &EBI_CONFORMANCE_NON_STOCHASTIC_ESCAPING_EDGES_PRECISION,
        &[
            "object stochastic language of alignments#./testfiles/aa-ab-ba.sali",
            "trait semantics#./testfiles/aa-ab-ba.lpn",
        ],
    ),
    (
        &EBI_CONFORMANCE_NON_STOCHASTIC_ESCAPING_EDGES_PRECISION,
        &[
            "object stochastic language of alignments#./testfiles/aa-ab-ba.sali",
            "trait semantics#./testfiles/aa-ab-ba.ptml",
        ],
    ),
    (
        &EBI_CONFORMANCE_NON_STOCHASTIC_ESCAPING_EDGES_PRECISION,
        &[
            "object stochastic language of alignments#./testfiles/aa-ab-ba.sali",
            "trait semantics#./testfiles/aa-ab-ba.sdfa",
        ],
    ),
    (
        &EBI_ANALYSE_NON_STOCHASTIC_EXECUTIONS,
        &["*", "trait semantics#./testfiles/empty.xes"],
    ),
    (
        &EBI_ANALYSE_NON_STOCHASTIC_EXECUTIONS,
        &["*", "trait semantics#./testfiles/empty.lang"],
    ),
    (
        &EBI_ANALYSE_NON_STOCHASTIC_MEDOID,
        &["trait finite language#./testfiles/empty.lang", "*"],
    ),
    (
        &EBI_CONFORMANCE_NON_STOCHASTIC_SET_ALIGNMENTS,
        &["*", "trait semantics#./testfiles/empty.xes"],
    ),
    (
        &EBI_CONFORMANCE_NON_STOCHASTIC_SET_ALIGNMENTS,
        &["*", "trait semantics#./testfiles/empty_2.ptree"],
    ),
    (
        &EBI_CONFORMANCE_NON_STOCHASTIC_SET_ALIGNMENTS,
        &["*", "trait semantics#./testfiles/empty.dfm"],
    ),
    (
        &EBI_CONFORMANCE_NON_STOCHASTIC_SET_ALIGNMENTS,
        &["*", "trait semantics#./testfiles/empty.sdfa"],
    ),
    (
        &EBI_TEST_BOOTSTRAP,
        &[
            "*",
            "trait finite stochastic language#./testfiles/empty.xes",
            "usize 10",
            "fraction 0.05",
        ],
    ),
    (
        &EBI_TEST_BOOTSTRAP,
        &[
            "*",
            "trait finite stochastic language#./testfiles/empty.slang",
            "usize 10",
            "fraction 0.05",
        ],
    ),
    (
        &EBI_TEST_LOG_ATTRIBUTE,
        &["*", "string some string", "*", "*"],
    ),
    (
        &EBI_SAMPLE_TRACES,
        &[
            "trait finite stochastic language#./testfiles/empty.xes",
            "*",
        ],
    ),
    (
        &EBI_SAMPLE_TRACES,
        &[
            "trait finite stochastic language#./testfiles/empty.slang",
            "*",
        ],
    ),
];
