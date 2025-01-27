#[cfg(test)]
mod tests {
    use std::{
        fs::{self, File},
        io::Cursor,
        ops::Neg,
    };

    use fraction::{GenericFraction, Zero};
    use num_bigint::ToBigUint;

    use crate::{
        ebi_framework::{
            activity_key::HasActivityKey, ebi_file_handler::EBI_FILE_HANDLERS,
            ebi_output::EbiOutput,
        },
        ebi_objects::{
            deterministic_finite_automaton::DeterministicFiniteAutomaton,
            directly_follows_model::DirectlyFollowsModel,
            event_log::EventLog,
            finite_language::FiniteLanguage,
            finite_stochastic_language::FiniteStochasticLanguage,
            labelled_petri_net::{LPNMarking, LabelledPetriNet},
            language_of_alignments::Move,
            process_tree::ProcessTree,
            stochastic_deterministic_finite_automaton::StochasticDeterministicFiniteAutomaton,
            stochastic_labelled_petri_net::StochasticLabelledPetriNet,
        },
        ebi_traits::{
            ebi_trait_event_log::{EbiTraitEventLog, IndexTrace},
            ebi_trait_finite_stochastic_language::EbiTraitFiniteStochasticLanguage,
            ebi_trait_queriable_stochastic_language::EbiTraitQueriableStochasticLanguage,
            ebi_trait_semantics::{Semantics, ToSemantics},
            ebi_trait_stochastic_deterministic_semantics::{
                EbiTraitStochasticDeterministicSemantics, StochasticDeterministicSemantics,
                ToStochasticDeterministicSemantics,
            },
        },
        follower_semantics::FollowerSemantics,
        math::{fraction::Fraction, log_div::LogDiv, matrix::Matrix, root_log_div::RootLogDiv},
        medoid,
        multiple_reader::MultipleReader,
        optimization_algorithms::network_simplex::NetworkSimplex,
        techniques::{
            align::Align, deterministic_semantics_for_stochastic_semantics::PMarking,
            jensen_shannon_stochastic_conformance::JensenShannonStochasticConformance,
            medoid_non_stochastic::MedoidNonStochastic,
            occurrences_stochastic_miner::OccurrencesStochasticMiner,
            probability_queries::ProbabilityQueries, process_variety::ProcessVariety,
            statistical_test::StatisticalTests, uniform_stochastic_miner::UniformStochasticMiner,
            unit_earth_movers_stochastic_conformance::UnitEarthMoversStochasticConformance,
        },
    };

    #[test]
    fn empty_slang() {
        let fin = fs::read_to_string("testfiles/empty.slang").unwrap();
        let mut slang = fin.parse::<FiniteStochasticLanguage>().unwrap();
        slang.normalise();

        assert_eq!(slang.len(), 0);
        assert_eq!(slang.get_probability_sum(), Fraction::zero());
    }

    #[test]
    fn empty_slpn() {
        let fin = fs::read_to_string("testfiles/empty_net.slpn").unwrap();
        let slpn = fin.parse::<StochasticLabelledPetriNet>().unwrap();

        assert_eq!(slpn.get_number_of_places(), 0);
        assert_eq!(slpn.get_number_of_transitions(), 0);
    }

    #[test]
    fn medoid() {
        let fin = fs::read_to_string("testfiles/aa-ab-ba.slang").unwrap();
        let slang = fin.parse::<FiniteStochasticLanguage>().unwrap();
        let fout = fs::read_to_string("testfiles/ba.lang").unwrap();
        let medoid = medoid::medoid(&slang, &1).unwrap();
        assert_eq!(fout, medoid.to_string())
    }

    #[test]
    fn non_stochastic_medoid() {
        let fin = fs::read_to_string("testfiles/aa-ab-ba.slang").unwrap();
        let slang = fin.parse::<FiniteStochasticLanguage>().unwrap();
        let fout = fs::read_to_string("testfiles/aa.lang").unwrap();
        let medoid = slang.medoid(1).unwrap();
        assert_eq!(fout, medoid.to_string())
    }

    #[test]
    fn non_stochastic_clustering() {
        let fin = fs::read_to_string("testfiles/aa-ab-ba.slang").unwrap();
        let slang = fin.parse::<FiniteStochasticLanguage>().unwrap();
        let cluster = slang.k_medoids_clustering(1).unwrap();
        let fout = fs::read_to_string("testfiles/aa.lang").unwrap();
        assert_eq!(fout, cluster.to_string())
    }

    #[test]
    fn lpn_uniform() {
        let fin = fs::read_to_string("testfiles/aa-ab-ba.lpn").unwrap();
        let lpn = Box::new(fin.parse::<LabelledPetriNet>().unwrap());
        let slpn = lpn.mine_uniform_stochastic();
        let fout = fs::read_to_string("testfiles/aa-ab-ba_uni.slpn").unwrap();
        assert_eq!(fout, slpn.to_string())
    }

    #[test]
    fn lpn_occurrence() {
        let fin1 = fs::read_to_string("testfiles/aa-ab-ba.lpn").unwrap();
        let lpn = fin1.parse::<LabelledPetriNet>().unwrap();
        let fin2 = fs::read_to_string("testfiles/aa-ab-ba.slang").unwrap();
        let slang = fin2.parse::<FiniteStochasticLanguage>().unwrap();
        let slpn = lpn.mine_occurrences_stochastic(Box::new(slang));
        let fout = fs::read_to_string("testfiles/aa-ab-ba_occ.slpn").unwrap();
        assert_eq!(fout, slpn.to_string())
    }

    #[test]
    fn mode() {
        let fin = fs::read_to_string("testfiles/aa-ab-ba_uni.slpn").unwrap();
        let slpn = fin.parse::<StochasticLabelledPetriNet>().unwrap();

        let x: Box<
            dyn StochasticDeterministicSemantics<
                DetState = PMarking<LPNMarking>,
                LivState = PMarking<LPNMarking>,
            >,
        > = Box::new(slpn);
        let slang = x.analyse_most_likely_traces(&1).unwrap();

        // let slang = Box::new(slpn).analyse_most_likely_traces(&1).unwrap();
        let fout = fs::read_to_string("testfiles/ba.slang").unwrap();
        assert_eq!(fout, slang.to_string())
    }

    #[test]
    fn fraction_exact() {
        let zero = Fraction::one().one_minus();

        assert!(zero.is_zero());
    }

    #[test]
    fn matrix_vector_multiplication() {
        let m: Matrix = vec![
            vec![6.into(), 2.into(), 4.into()],
            vec![(-1).into(), 4.into(), 3.into()],
            vec![(-2).into(), 9.into(), 3.into()],
        ]
        .into();
        let v: Vec<Fraction> = vec![4.into(), (-2).into(), 1.into()];

        let x = m * v;

        let t = vec![24.into(), (-9).into(), (-23).into()];

        assert_eq!(x, t);
    }

    //test disabled due to inconsistent output order (which is within spec)
    // #[test]
    // fn slang_to_sdfa() {
    //     let fin = fs::read_to_string("testfiles/aa-ab-ba.slang").unwrap();
    //     let slang = fin.parse::<FiniteStochasticLanguage>().unwrap();
    //     let sdfa = slang.to_stochastic_deterministic_finite_automaton();
    //     let fout = fs::read_to_string("testfiles/aa-ab-ba.sdfa").unwrap();
    //     assert_eq!(fout, sdfa.to_string())
    // }

    #[test]
    fn sdfa_minprob_one() {
        let fin = fs::read_to_string("testfiles/aa-ab-ba.sdfa").unwrap();
        let sdfa = fin
            .parse::<StochasticDeterministicFiniteAutomaton>()
            .unwrap();
        let semantics = sdfa.to_stochastic_deterministic_semantics();
        assert_eq!(
            semantics
                .analyse_minimum_probability(&Fraction::one())
                .unwrap()
                .len(),
            0
        );
    }

    #[test]
    fn sdfa_minprob_zero() {
        let fin = fs::read_to_string("testfiles/aa-ab-ba.sdfa").unwrap();
        let sdfa = fin
            .parse::<StochasticDeterministicFiniteAutomaton>()
            .unwrap();
        let semantics = sdfa.to_stochastic_deterministic_semantics();

        let slang2 = semantics
            .analyse_minimum_probability(&Fraction::zero())
            .unwrap();

        let should_string = fs::read_to_string("testfiles/aa-ab-ba.slang").unwrap();
        let should_slang = should_string.parse::<FiniteStochasticLanguage>().unwrap();
        assert_eq!(should_slang, slang2)
    }

    #[test]
    fn sdfa_minprob_zero_loop() {
        let fin = fs::read_to_string("testfiles/a-loop.sdfa").unwrap();
        let sdfa = fin
            .parse::<StochasticDeterministicFiniteAutomaton>()
            .unwrap();
        let semantics = sdfa.to_stochastic_deterministic_semantics();

        assert!(semantics
            .analyse_minimum_probability(&Fraction::zero())
            .is_err());
    }

    #[test]
    fn slpn_minprob_livelock() {
        let fin = fs::read_to_string("testfiles/a-b-c-livelock.slpn").unwrap();
        let slpn = fin.parse::<StochasticLabelledPetriNet>().unwrap();
        let x: Box<
            dyn StochasticDeterministicSemantics<
                DetState = PMarking<LPNMarking>,
                LivState = PMarking<LPNMarking>,
            >,
        > = Box::new(slpn);
        let slang1 = x
            .analyse_minimum_probability(&Fraction::from((1, 5)))
            .unwrap();

        let fin2 = fs::read_to_string("testfiles/a-b-c-livelock.slang").unwrap();
        let slang2 = fin2.parse::<FiniteStochasticLanguage>().unwrap();

        assert_eq!(slang1, slang2);
    }

    #[test]
    fn slang_minprob_one_deterministic() {
        let fin = fs::read_to_string("testfiles/aa-ab-ba.slang").unwrap();
        let slang = fin.parse::<FiniteStochasticLanguage>().unwrap();
        let semantics = slang.to_stochastic_deterministic_semantics();

        assert_eq!(
            semantics
                .analyse_minimum_probability(&Fraction::one())
                .unwrap()
                .len(),
            0
        );
    }

    #[test]
    fn slang_minprob_one() {
        let fin = fs::read_to_string("testfiles/aa-ab-ba.slang").unwrap();
        let slang = fin.parse::<FiniteStochasticLanguage>().unwrap();

        let slang2: &dyn EbiTraitFiniteStochasticLanguage = &slang;
        assert_eq!(
            slang2
                .analyse_minimum_probability(&Fraction::one())
                .unwrap()
                .len(),
            0
        );
    }

    #[test]
    fn slang_minprob_zero() {
        let fin = fs::read_to_string("testfiles/aa-ab-ba.slang").unwrap();
        let slang = fin.parse::<FiniteStochasticLanguage>().unwrap();
        let slang2: &dyn EbiTraitFiniteStochasticLanguage = &slang;

        //should return the same
        let slang3 = slang2
            .analyse_minimum_probability(&Fraction::zero())
            .unwrap();

        assert_eq!(slang, slang3)
    }

    #[test]
    fn slang_minprob_zero_through_sdfa() {
        let fin = fs::read_to_string("testfiles/aa-ab-ba.slang").unwrap();
        let slang = fin.parse::<FiniteStochasticLanguage>().unwrap();
        assert_eq!(slang.len(), 3);
        let mut sdfa = slang.get_stochastic_deterministic_finite_automaton();
        assert_eq!(sdfa.max_state, 5);
        assert_eq!(sdfa.get_number_of_transitions(), 5);

        //initial state
        let state = sdfa.get_deterministic_initial_state().unwrap();
        assert_eq!(state, 0);
        assert_eq!(sdfa.get_deterministic_enabled_activities(&state).len(), 2);

        //take a b
        let b = sdfa.get_activity_key_mut().process_activity("a");
        sdfa.execute_deterministic_activity(&state, b).unwrap();

        let semantics = EbiTraitStochasticDeterministicSemantics::Usize(Box::new(sdfa));

        // let semantics = slang.clone().to_stochastic_deterministic_semantics();

        //should return the same
        let slang2 = semantics
            .analyse_minimum_probability(&Fraction::zero())
            .unwrap();

        assert_eq!(slang, slang2)
    }

    #[test]
    fn cla_test() {
        let fin = fs::read_to_string("testfiles/a-b.xes").unwrap();
        let event_log: Box<dyn EbiTraitEventLog> = Box::new(fin.parse::<EventLog>().unwrap());

        let (_, sustain) = event_log
            .log_categorical_attribute(500, &"attribute".to_string(), &Fraction::from((1, 20)))
            .unwrap();
        assert!(sustain) //The hypothesis should be rejected if we consider the meaning of things, however, as we have only two traces, it will be sustained.
    }

    #[test]
    fn sdfa_invalid() {
        let fin = fs::read_to_string("testfiles/a-b-c-livelock_invalid.sdfa").unwrap();
        let err = fin.parse::<StochasticDeterministicFiniteAutomaton>();
        assert!(err.is_err());
    }

    #[test]
    fn sdfa_trace_prob_livelock() {
        let fin = fs::read_to_string("testfiles/a-b-c-livelock.sdfa").unwrap();
        let mut sdfa = fin
            .parse::<StochasticDeterministicFiniteAutomaton>()
            .unwrap();

        //a
        {
            let strace = vec!["a".to_string()];
            let trace = sdfa.get_activity_key_mut().process_trace(&strace);
            let trace_follower = FollowerSemantics::Trace(&trace);
            let probability = sdfa.get_probability(&trace_follower).unwrap();
            assert_eq!(probability, "2/5".parse::<Fraction>().unwrap());
        }

        //b
        {
            let strace = vec!["b".to_string()];
            let trace = sdfa.get_activity_key_mut().process_trace(&strace);
            let trace_follower = FollowerSemantics::Trace(&trace);
            let probability = sdfa.get_probability(&trace_follower).unwrap();
            assert_eq!(probability, "1/5".parse::<Fraction>().unwrap());
        }

        //c (part of livelock trace)
        {
            let strace = vec!["c".to_string()];
            let trace = sdfa.get_activity_key_mut().process_trace(&strace);
            let trace_follower = FollowerSemantics::Trace(&trace);
            let probability = sdfa.get_probability(&trace_follower).unwrap();
            assert_eq!(probability, "0".parse::<Fraction>().unwrap());
        }

        //d (non-existing label)
        {
            let strace = vec!["d".to_string()];
            let trace = sdfa.get_activity_key_mut().process_trace(&strace);
            let trace_follower = FollowerSemantics::Trace(&trace);
            let probability = sdfa.get_probability(&trace_follower).unwrap();
            assert_eq!(probability, "0".parse::<Fraction>().unwrap());
        }
    }

    #[test]
    fn slpn_trace_prob_livelock() {
        let fin = fs::read_to_string("testfiles/a-b-c-livelock.slpn").unwrap();
        let mut slpn = fin.parse::<StochasticLabelledPetriNet>().unwrap();

        //a
        {
            let strace = vec!["a".to_string()];
            let trace = slpn.get_activity_key_mut().process_trace(&strace);
            let trace_follower = FollowerSemantics::Trace(&trace);
            let probability = slpn.get_probability(&trace_follower).unwrap();
            assert_eq!(probability, "2/5".parse::<Fraction>().unwrap());
        }

        //b
        {
            let strace = vec!["b".to_string()];
            let trace = slpn.get_activity_key_mut().process_trace(&strace);
            let trace_follower = FollowerSemantics::Trace(&trace);
            let probability = slpn.get_probability(&trace_follower).unwrap();
            assert_eq!(probability, "1/5".parse::<Fraction>().unwrap());
        }

        //c (part of livelock trace)
        {
            let strace = vec!["c".to_string()];
            let trace = slpn.get_activity_key_mut().process_trace(&strace);
            let trace_follower = FollowerSemantics::Trace(&trace);
            let probability = slpn.get_probability(&trace_follower).unwrap();
            assert_eq!(probability, "0".parse::<Fraction>().unwrap());
        }

        //d (non-existing label)
        {
            let strace = vec!["d".to_string()];
            let trace = slpn.get_activity_key_mut().process_trace(&strace);
            let trace_follower = FollowerSemantics::Trace(&trace);
            let probability = slpn.get_probability(&trace_follower).unwrap();
            assert_eq!(probability, "0".parse::<Fraction>().unwrap());
        }
    }

    #[test]
    fn slpn_trace_prob_tau_livelock() {
        let fin = fs::read_to_string("testfiles/a-b-tau-livelock.slpn").unwrap();
        let mut slpn = fin.parse::<StochasticLabelledPetriNet>().unwrap();

        //a
        {
            let strace = vec!["a".to_string()];
            let trace = slpn.get_activity_key_mut().process_trace(&strace);
            let trace_follower = FollowerSemantics::Trace(&trace);
            let probability = slpn.get_probability(&trace_follower).unwrap();
            assert_eq!(probability, "2/5".parse::<Fraction>().unwrap());
        }

        //b
        {
            let strace = vec!["b".to_string()];
            let trace = slpn.get_activity_key_mut().process_trace(&strace);
            let trace_follower = FollowerSemantics::Trace(&trace);
            let probability = slpn.get_probability(&trace_follower).unwrap();
            assert_eq!(probability, "1/5".parse::<Fraction>().unwrap());
        }

        //empty trace (part of livelock trace)
        {
            let strace = vec![];
            let trace = slpn.get_activity_key_mut().process_trace(&strace);
            let trace_follower = FollowerSemantics::Trace(&trace);
            let probability = slpn.get_probability(&trace_follower).unwrap();
            assert_eq!(probability, "0".parse::<Fraction>().unwrap());
        }

        //d (non-existing label)
        {
            let strace = vec!["d".to_string()];
            let trace = slpn.get_activity_key_mut().process_trace(&strace);
            let trace_follower = FollowerSemantics::Trace(&trace);
            let probability = slpn.get_probability(&trace_follower).unwrap();
            assert_eq!(probability, "0".parse::<Fraction>().unwrap());
        }
    }

    #[test]
    fn uemsc_activity_key() {
        let fin1 = fs::read_to_string("testfiles/aa-ab-ba.slang").unwrap();
        let slang1 = fin1.parse::<FiniteStochasticLanguage>().unwrap();

        let fin2 = fs::read_to_string("testfiles/ba-aa-ab.slang").unwrap();
        let slang2 = fin2.parse::<FiniteStochasticLanguage>().unwrap();

        let slang1: Box<dyn EbiTraitFiniteStochasticLanguage> = Box::new(slang1);

        let uemsc = slang1
            .unit_earth_movers_stochastic_conformance(Box::new(slang2))
            .unwrap();
        assert_eq!(uemsc, Fraction::one())
    }

    #[test]
    fn jssc_activity_key() {
        let fin1 = fs::read_to_string("testfiles/aa.slang").unwrap();
        let slang1 = fin1.parse::<FiniteStochasticLanguage>().unwrap();

        let fin2 = fs::read_to_string("testfiles/aa.slang").unwrap();
        let slang2 = fin2.parse::<FiniteStochasticLanguage>().unwrap();

        let slang1: Box<dyn EbiTraitFiniteStochasticLanguage> = Box::new(slang1);

        let jssc = slang1.jssc_log2log(Box::new(slang2)).unwrap();

        let right_side = LogDiv::zero();
        assert_eq!(jssc, RootLogDiv::sqrt(right_side).one_minus())
    }

    #[test]
    fn zero_log_div() {
        let mut zero = LogDiv::zero();
        zero /= 2;
        assert_eq!(zero, LogDiv::zero());
    }

    #[test]
    fn align_sdfa_trace() {
        let fin = fs::read_to_string("testfiles/aa-ab-ba.sdfa").unwrap();
        let sdfa = fin
            .parse::<StochasticDeterministicFiniteAutomaton>()
            .unwrap();
        let mut semantics = sdfa.to_semantics();

        let a = semantics.get_activity_key_mut().process_activity("a");
        let b = semantics.get_activity_key_mut().process_activity("b");

        let trace = vec![b, b];

        let (alignment, _) = semantics.align_trace(&trace).unwrap();

        let correct_1 = vec![
            Move::SynchronousMove(b, 1),
            Move::ModelMove(a, 4),
            Move::SilentMove(5),
            Move::LogMove(b),
        ];
        let correct_2 = vec![
            Move::SynchronousMove(b, 1),
            Move::LogMove(b),
            Move::ModelMove(a, 4),
            Move::SilentMove(5),
        ];

        assert!(alignment == correct_1 || alignment == correct_2);
    }

    #[test]
    fn align_sdfa_lang() {
        let fin1 = fs::read_to_string("testfiles/aa-ab-ba.sdfa").unwrap();
        let sdfa = fin1
            .parse::<StochasticDeterministicFiniteAutomaton>()
            .unwrap();
        let mut semantics = sdfa.to_semantics();

        let fin2 = fs::read_to_string("testfiles/bb.lang").unwrap();
        let lang = Box::new(fin2.parse::<FiniteLanguage>().unwrap());

        let a = semantics.get_activity_key_mut().process_activity("a");
        let b = semantics.get_activity_key_mut().process_activity("b");

        let alignment = semantics.align_language(lang).unwrap();

        let correct_1 = vec![
            Move::SynchronousMove(b, 1),
            Move::ModelMove(a, 4),
            Move::SilentMove(5),
            Move::LogMove(b),
        ];
        let correct_2 = vec![
            Move::SynchronousMove(b, 1),
            Move::LogMove(b),
            Move::ModelMove(a, 4),
            Move::SilentMove(5),
        ];

        assert!(alignment.get(0) == Some(&correct_1) || alignment.get(0) == Some(&correct_2));
    }

    #[test]
    fn align_slang_lang() {
        let fin1 = fs::read_to_string("testfiles/aa-ab-ba.slang").unwrap();
        let sdfa = fin1.parse::<FiniteStochasticLanguage>().unwrap();
        let mut semantics = sdfa.to_semantics();

        let fin2 = fs::read_to_string("testfiles/bb.lang").unwrap();
        let lang = Box::new(fin2.parse::<FiniteLanguage>().unwrap());

        let a = semantics.get_activity_key_mut().process_activity("a");
        let b = semantics.get_activity_key_mut().process_activity("b");

        let alignment = semantics.align_language(lang).unwrap();

        let correct_1 = vec![
            Move::ModelMove(a, 1),
            Move::SynchronousMove(b, 2),
            Move::SilentMove(0),
            Move::LogMove(b),
        ]; //other options may be valid, please check semantically when this fails
        let correct_2 = vec![
            Move::SynchronousMove(b, 2),
            Move::ModelMove(a, 1),
            Move::SilentMove(0),
            Move::LogMove(b),
        ]; //other options may be valid, please check semantically when this fails
        assert!(*alignment.get(0).unwrap() == correct_1 || *alignment.get(0).unwrap() == correct_2);
    }

    #[test]
    fn align_lang_lang() {
        let fin1 = fs::read_to_string("testfiles/aa-ab-ba.lang").unwrap();
        let sdfa = fin1.parse::<FiniteLanguage>().unwrap();
        let mut semantics = sdfa.to_semantics();

        let fin2 = fs::read_to_string("testfiles/bb.lang").unwrap();
        let lang = Box::new(fin2.parse::<FiniteLanguage>().unwrap());

        let a = semantics.get_activity_key_mut().process_activity("a");
        let b = semantics.get_activity_key_mut().process_activity("b");

        let alignment = semantics.align_language(lang).unwrap();

        let correct_1 = vec![
            Move::SynchronousMove(b, 1),
            Move::ModelMove(a, 2),
            Move::SilentMove(5),
            Move::LogMove(b),
        ]; //other options may be valid, please check semantically when this fails

        assert_eq!(*alignment.get(0).unwrap(), correct_1);
        // assert!(*alignment.get(0).unwrap() == correct_1 || *alignment.get(0).unwrap() == correct_2);
    }

    #[test]
    fn align_dfa_lang() {
        let fin1 = fs::read_to_string("testfiles/aa-ab-ba.dfa").unwrap();
        let dfa = fin1.parse::<DeterministicFiniteAutomaton>().unwrap();
        let mut semantics = dfa.to_semantics();

        let fin2 = fs::read_to_string("testfiles/bb.lang").unwrap();
        let lang = Box::new(fin2.parse::<FiniteLanguage>().unwrap());

        let a = semantics.get_activity_key_mut().process_activity("a");
        let b = semantics.get_activity_key_mut().process_activity("b");

        let alignment = semantics.align_language(lang).unwrap();

        let correct_1 = vec![
            Move::SynchronousMove(b, 1),
            Move::ModelMove(a, 4),
            Move::SilentMove(5),
            Move::LogMove(b),
        ]; //other options may be valid, please check semantically when this fails
        assert_eq!(*alignment.get(0).unwrap(), correct_1);
    }

    #[test]
    fn probability_sdfa_livelock_zeroweight() {
        let fin1 = fs::read_to_string("testfiles/a-livelock-zeroweight.sdfa").unwrap();
        let mut sdfa = fin1
            .parse::<StochasticDeterministicFiniteAutomaton>()
            .unwrap();

        //a ends in a livelock and has probability 0
        let strace = vec!["a".to_string()];
        let trace = sdfa.get_activity_key_mut().process_trace(&strace);
        let trace_follower = FollowerSemantics::Trace(&trace);
        assert_eq!(
            sdfa.get_probability(&trace_follower).unwrap(),
            Fraction::zero()
        );

        //a, a ends in a livelock and has probability 0
        let strace = vec!["a".to_string(), "a".to_string()];
        let trace = sdfa.get_activity_key_mut().process_trace(&strace);
        let trace_follower = FollowerSemantics::Trace(&trace);
        assert_eq!(
            sdfa.get_probability(&trace_follower).unwrap(),
            Fraction::zero()
        );

        //b has weight 0
        let strace = vec!["b".to_string()];
        let trace = sdfa.get_activity_key_mut().process_trace(&strace);
        let trace_follower = FollowerSemantics::Trace(&trace);
        assert_eq!(
            sdfa.get_probability(&trace_follower).unwrap(),
            Fraction::zero()
        );
    }

    #[test]
    fn probability_slpn_livelock_zeroweight() {
        let fin1 = fs::read_to_string("testfiles/a-livelock-zeroweight.slpn").unwrap();
        let mut slpn = fin1.parse::<StochasticLabelledPetriNet>().unwrap();

        //a ends in a livelock and has probability 0
        let strace = vec!["a".to_string()];
        let trace = slpn.get_activity_key_mut().process_trace(&strace);
        let trace_follower = FollowerSemantics::Trace(&trace);
        assert_eq!(
            slpn.get_probability(&trace_follower).unwrap(),
            Fraction::zero()
        );

        //a, a ends in a livelock and has probability 0
        let strace = vec!["a".to_string(), "a".to_string()];
        let trace = slpn.get_activity_key_mut().process_trace(&strace);
        let trace_follower = FollowerSemantics::Trace(&trace);
        assert_eq!(
            slpn.get_probability(&trace_follower).unwrap(),
            Fraction::zero()
        );

        //b has weight 0
        let strace = vec!["b".to_string()];
        let trace = slpn.get_activity_key_mut().process_trace(&strace);
        let trace_follower = FollowerSemantics::Trace(&trace);
        assert_eq!(
            slpn.get_probability(&trace_follower).unwrap(),
            Fraction::zero()
        );

        //c has weight 0
        let strace = vec!["c".to_string()];
        let trace = slpn.get_activity_key_mut().process_trace(&strace);
        let trace_follower = FollowerSemantics::Trace(&trace);
        assert_eq!(
            slpn.get_probability(&trace_follower).unwrap(),
            Fraction::zero()
        );
    }

    #[test]
    fn fraction_neg() {
        let one = Fraction::one();
        assert!(one.is_positive());
        let one = one.neg();
        assert!(one.is_negative());
    }

    #[test]
    fn jssc() {
        let fin1 = fs::read_to_string("testfiles/aa-ab-ba_ali.slpn").unwrap();
        let slpn = fin1.parse::<StochasticLabelledPetriNet>().unwrap();

        let fin2 = fs::read_to_string("testfiles/a-b.xes").unwrap();
        let log = fin2.parse::<EventLog>().unwrap();

        let slang: Box<dyn EbiTraitFiniteStochasticLanguage> =
            Box::new(log.get_finite_stochastic_language());

        let answer = RootLogDiv::sqrt(LogDiv::Exact(
            GenericFraction::from(4),
            2.to_biguint().unwrap(),
        ))
        .one_minus();

        assert_eq!(slang.jssc_log2model(Box::new(slpn)).unwrap(), answer);
    }

    #[test]
    fn variety() {
        let fin = fs::read_to_string("testfiles/aa-ab-ba.slang").unwrap();
        let slang: Box<dyn EbiTraitFiniteStochasticLanguage> =
            Box::new(fin.parse::<FiniteStochasticLanguage>().unwrap());

        assert_eq!(slang.process_variety(), Fraction::from((2, 5)));
    }

    #[test]
    fn slang_cover_empty() {
        let fin = fs::read_to_string("testfiles/empty.slang").unwrap();
        let slang: Box<dyn EbiTraitFiniteStochasticLanguage> =
            Box::new(fin.parse::<FiniteStochasticLanguage>().unwrap());

        let slang2 = slang
            .analyse_probability_coverage(&Fraction::zero())
            .unwrap();
        assert_eq!(slang2.len(), 0);

        let slang3 = slang.analyse_probability_coverage(&Fraction::one());
        assert!(slang3.is_err());

        assert!(slang
            .analyse_probability_coverage(&Fraction::from((1, 2)))
            .is_err());
    }

    #[test]
    fn slpn_empty() {
        let fin = fs::read_to_string("testfiles/empty_lang_labelled.slpn").unwrap();
        let slpn: Box<
            dyn StochasticDeterministicSemantics<
                DetState = PMarking<LPNMarking>,
                LivState = PMarking<LPNMarking>,
            >,
        > = Box::new(fin.parse::<StochasticLabelledPetriNet>().unwrap());
        let slang2 = slpn
            .analyse_probability_coverage(&Fraction::zero())
            .unwrap();
        assert_eq!(slang2.len(), 0);
    }

    #[test]
    fn slpn_cover_empty_net() {
        let fin = fs::read_to_string("testfiles/empty_net.slpn").unwrap();

        let slpn = Box::new(fin.parse::<StochasticLabelledPetriNet>().unwrap());
        let state = slpn.get_deterministic_initial_state().unwrap();
        assert_eq!(slpn.get_deterministic_enabled_activities(&state).len(), 0);
        assert_eq!(
            slpn.get_deterministic_termination_probability(&state),
            Fraction::one()
        );

        let slpn: Box<
            dyn StochasticDeterministicSemantics<
                DetState = PMarking<LPNMarking>,
                LivState = PMarking<LPNMarking>,
            >,
        > = Box::new(fin.parse::<StochasticLabelledPetriNet>().unwrap());
        let slang2 = slpn
            .analyse_probability_coverage(&Fraction::zero())
            .unwrap();
        assert_eq!(slang2.len(), 0);

        let fin2 = fs::read_to_string("testfiles/empty_trace.slang").unwrap();
        let slang3 = fin2.parse::<FiniteStochasticLanguage>().unwrap();

        assert_eq!(
            slpn.analyse_probability_coverage(&Fraction::from((1, 2)))
                .unwrap(),
            slang3
        );
    }

    #[test]
    fn slpn_empty_lang_labelled_cover() {
        let fin = fs::read_to_string("testfiles/empty_lang_labelled.slpn").unwrap();

        let slpn = Box::new(fin.parse::<StochasticLabelledPetriNet>().unwrap());
        let state = slpn.get_deterministic_initial_state().unwrap();
        assert_eq!(slpn.get_deterministic_enabled_activities(&state).len(), 1);
        assert_eq!(
            slpn.get_deterministic_termination_probability(&state),
            Fraction::zero()
        );

        let slpn: Box<
            dyn StochasticDeterministicSemantics<
                DetState = PMarking<LPNMarking>,
                LivState = PMarking<LPNMarking>,
            >,
        > = slpn;
        let slang2 = slpn
            .analyse_probability_coverage(&Fraction::zero())
            .unwrap();
        assert_eq!(slang2.len(), 0);

        let fin2 = fs::read_to_string("testfiles/empty.slang").unwrap();
        let slang3 = fin2.parse::<FiniteStochasticLanguage>().unwrap();

        assert_eq!(
            slpn.analyse_probability_coverage(&Fraction::from((1, 2)))
                .unwrap(),
            slang3
        );
    }

    #[test]
    fn slpn_empty_lang_silent_cover() {
        let fin = fs::read_to_string("testfiles/empty_lang_silent.slpn").unwrap();

        let slpn = Box::new(fin.parse::<StochasticLabelledPetriNet>().unwrap());
        let state = slpn.get_deterministic_initial_state().unwrap();
        assert_eq!(slpn.get_deterministic_enabled_activities(&state).len(), 0);
        assert_eq!(
            slpn.get_deterministic_termination_probability(&state),
            Fraction::zero()
        );

        let slpn: Box<
            dyn StochasticDeterministicSemantics<
                DetState = PMarking<LPNMarking>,
                LivState = PMarking<LPNMarking>,
            >,
        > = slpn;
        let slang2 = slpn
            .analyse_probability_coverage(&Fraction::zero())
            .unwrap();
        assert_eq!(slang2.len(), 0);

        let fin2 = fs::read_to_string("testfiles/empty.slang").unwrap();
        let slang3 = fin2.parse::<FiniteStochasticLanguage>().unwrap();

        assert_eq!(
            slpn.analyse_probability_coverage(&Fraction::zero())
                .unwrap(),
            slang3
        );
    }

    #[test]
    fn slpn_cover_nothing() {
        let fin = fs::read_to_string("testfiles/aa-ab-ba_ali.slpn").unwrap();
        let slpn: Box<
            dyn StochasticDeterministicSemantics<
                DetState = PMarking<LPNMarking>,
                LivState = PMarking<LPNMarking>,
            >,
        > = Box::new(fin.parse::<StochasticLabelledPetriNet>().unwrap());

        let slang2 = slpn
            .analyse_probability_coverage(&Fraction::zero())
            .unwrap();

        assert_eq!(slang2.len(), 0);
    }

    #[test]
    fn slpn_cover_silent_livelock() {
        let fin = fs::read_to_string("testfiles/empty_lang_multiple_silent.slpn").unwrap();
        let slpn: Box<
            dyn StochasticDeterministicSemantics<
                DetState = PMarking<LPNMarking>,
                LivState = PMarking<LPNMarking>,
            >,
        > = Box::new(fin.parse::<StochasticLabelledPetriNet>().unwrap());

        let slang2 = slpn
            .analyse_probability_coverage(&Fraction::zero())
            .unwrap();

        assert_eq!(slang2.len(), 0);
    }

    #[test]
    fn tree_semantics() {
        let fin = fs::read_to_string("testfiles/aa.ptree").unwrap();
        let tree = fin.parse::<ProcessTree>().unwrap();
        let mut state = tree.get_initial_state();
        println!("{}", state);
        assert_eq!(tree.get_enabled_transitions(&state), vec![0]);

        tree.execute_transition(&mut state, 0).unwrap();

        println!("{}", state);
        assert_eq!(tree.get_enabled_transitions(&state), vec![1]);

        tree.execute_transition(&mut state, 1).unwrap();

        println!("{}", state);
        assert_eq!(tree.get_enabled_transitions(&state), vec![2]);
        assert!(!tree.is_final_state(&state));

        tree.execute_transition(&mut state, 2).unwrap();

        assert_eq!(tree.get_enabled_transitions(&state), Vec::<usize>::new());
        assert!(tree.is_final_state(&state));
    }

    #[test]
    fn tree_semantics_2() {
        let fin = fs::read_to_string("testfiles/aa-ab-ba.ptree").unwrap();
        let tree = fin.parse::<ProcessTree>().unwrap();
        let mut state = tree.get_initial_state();
        println!("{}", state);
        assert_eq!(tree.get_enabled_transitions(&state), vec![0, 2]);
        assert!(!tree.is_final_state(&state));

        tree.execute_transition(&mut state, 2).unwrap();

        println!("{}", state);
        assert_eq!(tree.get_enabled_transitions(&state), vec![3, 4]);
        assert!(!tree.is_final_state(&state));

        tree.execute_transition(&mut state, 3).unwrap();
        println!("{}", state);
        assert!(!tree.is_final_state(&state));

        tree.execute_transition(&mut state, 5).unwrap();

        assert_eq!(tree.get_enabled_transitions(&state), Vec::<usize>::new());
        assert!(tree.is_final_state(&state));
    }

    #[test]
    fn tree_semantics_3() {
        let fin = fs::read_to_string("testfiles/all_operators.ptree").unwrap();
        let tree = fin.parse::<ProcessTree>().unwrap();

        let mut state = tree.get_initial_state();
        println!("{}", state);
        assert_eq!(
            tree.get_enabled_transitions(&state),
            vec![0, 2, 3, 5, 6, 8, 9]
        );
        assert!(!tree.is_final_state(&state));

        tree.execute_transition(&mut state, 2).unwrap();

        println!("{}", state);
        assert_eq!(tree.get_enabled_transitions(&state), vec![3, 5, 6, 8, 9]);
        assert!(!tree.is_final_state(&state));

        tree.execute_transition(&mut state, 3).unwrap();

        println!("{}", state);
        assert_eq!(tree.get_enabled_transitions(&state), vec![4, 5, 6, 8, 9]);
        assert!(!tree.is_final_state(&state));

        tree.execute_transition(&mut state, 6).unwrap();

        println!("{}", state);
        assert_eq!(tree.get_enabled_transitions(&state), vec![4, 7, 8, 9]);
        assert!(!tree.is_final_state(&state));

        tree.execute_transition(&mut state, 8).unwrap();

        println!("{}", state);
        assert_eq!(tree.get_enabled_transitions(&state), vec![4, 7, 9]);
        assert!(!tree.is_final_state(&state));

        tree.execute_transition(&mut state, 7).unwrap();

        println!("{}", state);
        assert_eq!(tree.get_enabled_transitions(&state), vec![4, 5, 9]);
        assert!(!tree.is_final_state(&state));

        tree.execute_transition(&mut state, 5).unwrap();

        println!("{}", state);
        assert_eq!(tree.get_enabled_transitions(&state), vec![4, 9, 10]);
        assert!(!tree.is_final_state(&state));

        tree.execute_transition(&mut state, 4).unwrap();

        println!("{}", state);
        assert_eq!(tree.get_enabled_transitions(&state), vec![3, 9]);
        assert!(!tree.is_final_state(&state));

        tree.execute_transition(&mut state, 3).unwrap();

        println!("{}", state);
        assert_eq!(tree.get_enabled_transitions(&state), vec![4, 9, 10]);
        assert!(!tree.is_final_state(&state));

        tree.execute_transition(&mut state, 10).unwrap();

        assert_eq!(tree.get_enabled_transitions(&state), Vec::<usize>::new());
        assert!(tree.is_final_state(&state));
    }

    #[test]
    fn dfm_semantics() {
        let fin = fs::read_to_string("testfiles/a-b_star.dfm").unwrap();
        let dfm = fin.parse::<DirectlyFollowsModel>().unwrap();

        let mut state = dfm.get_initial_state();

        assert_eq!(dfm.get_enabled_transitions(&state), vec![0]);

        dfm.execute_transition(&mut state, 0).unwrap();

        assert_eq!(dfm.get_enabled_transitions(&state), vec![1]);
        assert!(!dfm.is_final_state(&state));

        dfm.execute_transition(&mut state, 1).unwrap();

        assert_eq!(dfm.get_enabled_transitions(&state), vec![1, 2]);

        dfm.execute_transition(&mut state, 1).unwrap();

        assert_eq!(dfm.get_enabled_transitions(&state), vec![1, 2]);

        dfm.execute_transition(&mut state, 2).unwrap();
        assert!(dfm.is_final_state(&state));
    }

    #[test]
    fn all_importers() {
        let files = fs::read_dir("./testfiles").unwrap();
        for path in files {
            let file = path.unwrap();
            println!("file {:?}", file.file_name());

            let mut reader = MultipleReader::from_file(File::open(file.path()).unwrap());

            //look for file handlers that should accept this file
            for file_handler in EBI_FILE_HANDLERS {
                println!("\tfile handler {}", file_handler);
                if !file.file_name().into_string().unwrap().contains("invalid")
                    && file
                        .file_name()
                        .into_string()
                        .unwrap()
                        .ends_with(&(".".to_string() + file_handler.file_extension))
                {
                    //file handler should be able to accept this file

                    for importer in file_handler.object_importers {
                        println!("\t\timporter {}", importer);
                        assert!((importer.get_importer())(&mut reader.get().unwrap()).is_ok());
                    }
                } else {
                    //file handler should not accept this file

                    for importer in file_handler.object_importers {
                        println!("\t\timporter {} (should fail)", importer);
                        assert!((importer.get_importer())(&mut reader.get().unwrap()).is_err());
                    }
                }
            }
        }
    }

    #[test]
    fn all_trait_importers() {
        let files = fs::read_dir("./testfiles").unwrap();
        for path in files {
            let file = path.unwrap();
            println!("file {:?}", file.file_name());

            let mut reader = MultipleReader::from_file(File::open(file.path()).unwrap());

            //look for file handlers that should accept this file
            for file_handler in EBI_FILE_HANDLERS {
                println!("\tfile handler {}", file_handler);
                if !file.file_name().into_string().unwrap().contains("invalid")
                    && file
                        .file_name()
                        .into_string()
                        .unwrap()
                        .ends_with(&(".".to_string() + file_handler.file_extension))
                {
                    //file handler should be able to accept this file

                    for importer in file_handler.trait_importers {
                        println!("\t\timporter {}", importer);
                        assert!(importer.import(&mut reader.get().unwrap()).is_ok());
                    }
                } else {
                    //file handler should not accept this file

                    for importer in file_handler.trait_importers {
                        println!("\t\timporter {} (should fail)", importer);
                        assert!(importer.import(&mut reader.get().unwrap()).is_err());
                    }
                }
            }
        }
    }

    #[test]
    fn all_exporters() {
        let files = fs::read_dir("./testfiles").unwrap();
        for path in files {
            let file = path.unwrap();
            println!("file {:?}", file.file_name());

            let mut reader = MultipleReader::from_file(File::open(file.path()).unwrap());

            //look for file handlers that should accept this file
            for file_handler in EBI_FILE_HANDLERS {
                if !file.file_name().into_string().unwrap().contains("invalid")
                    && file
                        .file_name()
                        .into_string()
                        .unwrap()
                        .ends_with(&(".".to_string() + file_handler.file_extension))
                {
                    //file handler should be able to accept this file

                    println!("\tfile handler import {}", file_handler);

                    for importer in file_handler.object_importers {
                        for file_handler2 in EBI_FILE_HANDLERS {
                            for exporter in file_handler2.object_exporters {
                                if exporter.get_type() == importer.get_type() {
                                    println!("\t\timporter {}, exporter {}", importer, exporter);

                                    let object =
                                        (importer.get_importer())(&mut reader.get().unwrap())
                                            .unwrap();
                                    let mut c = Cursor::new(Vec::new());
                                    // let mut f = File::open("/dev/null").unwrap();
                                    exporter.export(EbiOutput::Object(object), &mut c).unwrap();
                                }
                            }
                        }
                    }
                }
            }
        }
    }

    // #[test] //disabled until we can handle livelocks
    // fn sample_sdfa_no_language() {
    //     let fin1 = fs::read_to_string("testfiles/a-livelock-zeroweight.sdfa").unwrap();
    //     let sdfa = fin1.parse::<StochasticDeterministicFiniteAutomaton>().unwrap();
    //     let semantics: Box<dyn StochasticSemantics<State = usize>> = Box::new(StochasticDeterministicFiniteAutomatonSemantics::new(Rc::new(sdfa)));

    //     let sample = semantics.sample(1);

    //     //the language of this model is empty, so it should not have any traces in the sample
    //     println!("{:?}", sample);
    //     assert!(sample.is_err())
    // }

    // //#[test] //disabled until we can handle livelocks
    // fn sample_slpn_no_language() {
    //     let fin1 = fs::read_to_string("testfiles/a-livelock-zeroweight.slpn").unwrap();
    //     let slpn = fin1.parse::<StochasticLabelledPetriNet>().unwrap();

    //     let sample = slpn.sample(1);

    //     //the language of this model is empty, so it should not have any traces in the sample
    //     assert!(sample.is_err())
    // }

    #[test]
    fn network_simplex() {
        network_simplex_int();
        network_simplex_bigint();
        network_simplex_float();
    }

    fn network_simplex_int() {
        let supply: Vec<i64> = vec![20, 0, 0, -5, -14];

        let graph_and_costs: Vec<Vec<Option<i64>>> = vec![
            vec![None, Some(4), Some(4), None, None],
            vec![None, None, Some(2), Some(2), Some(6)],
            vec![None, None, None, Some(1), Some(3)],
            vec![None, None, None, None, Some(2)],
            vec![None, None, Some(3), None, None],
        ];
        let mut ns = NetworkSimplex::new(&graph_and_costs, &supply, true, false);
        _ = ns.run(false);
        assert_eq!(ns.get_result().unwrap(), 123);
    }

    fn network_simplex_bigint() {
        use num::BigInt;

        let supply: Vec<BigInt> = vec![20, 0, 0, -5, -14]
            .into_iter()
            .map(|s| BigInt::from(s))
            .collect();

        let graph_and_costs: Vec<Vec<Option<BigInt>>> = vec![
            vec![None, Some(4), Some(4), None, None],
            vec![None, None, Some(2), Some(2), Some(6)],
            vec![None, None, None, Some(1), Some(3)],
            vec![None, None, None, None, Some(2)],
            vec![None, None, Some(3), None, None],
        ]
        .into_iter()
        .map(|row| {
            row.into_iter()
                .map(|x| x.map(|cost| BigInt::from(cost)))
                .collect()
        })
        .collect();

        let mut ns = NetworkSimplex::new(&graph_and_costs, &supply, true, false);
        _ = ns.run(false);
        assert_eq!(ns.get_result().unwrap(), BigInt::from(123));
    }

    fn network_simplex_float() {
        let supply: Vec<f64> = vec![20, 0, 0, -5, -14]
            .into_iter()
            .map(|s| s.into())
            .collect();

        let graph_and_costs: Vec<Vec<Option<f64>>> = vec![
            vec![None, Some(4), Some(4), None, None],
            vec![None, None, Some(2), Some(2), Some(6)],
            vec![None, None, None, Some(1), Some(3)],
            vec![None, None, None, None, Some(2)],
            vec![None, None, Some(3), None, None],
        ]
        .into_iter()
        .map(|row| row.into_iter().map(|x| x.map(|cost| cost.into())).collect())
        .collect();

        let mut ns = NetworkSimplex::new(&graph_and_costs, &supply, true, false);
        _ = ns.run(false);
        assert_eq!(ns.get_result().unwrap(), 123.0);
    }

    // test is working but use of Approx and parallelization causes other tests to fail
    // #[test]
    // fn test_earth_movers_stochastic_conformance() {
    //     Fraction::set_exact_globally(false);
    //     let log_file = fs::File::open("testfiles/aa-ab-ba.slang").unwrap();
    //     let mut log_reader = BufReader::new(log_file);
    //     let log = import::<FiniteStochasticLanguage>(&mut log_reader).unwrap();
    //     let model_file = fs::File::open("testfiles/aa-ab-ba_changed_probs.slang").unwrap();
    //     let mut model_reader = BufReader::new(model_file);
    //     let mut model = import::<FiniteStochasticLanguage>(&mut model_reader).unwrap();

    //     let result = log.earth_movers_stochastic_conformance(model.as_mut());

    //     assert!(
    //         result.is_ok(),
    //         "EMSC calculation failed: {:?}",
    //         result.err()
    //     );
    //     let result_value = result.unwrap();

    //     assert!(
    //         result_value == Fraction::Approx(0.85),
    //         "Unexpected EMSC result: {}",
    //         result_value
    //     );

    //     Fraction::set_exact_globally(true);
    // }
}
