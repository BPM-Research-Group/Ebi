#[cfg(test)]
mod tests {
    use std::{fs, sync::Arc};

    use env_logger::Builder;
    use log::LevelFilter;

    use crate::{ebi_objects::{alignments::Move, event_log::EventLog, finite_language::FiniteLanguage, finite_stochastic_language::FiniteStochasticLanguage, labelled_petri_net::LabelledPetriNet, stochastic_deterministic_finite_automaton::StochasticDeterministicFiniteAutomaton, stochastic_labelled_petri_net::StochasticLabelledPetriNet}, ebi_traits::{ebi_trait_event_log::EbiTraitEventLog, ebi_trait_finite_stochastic_language::EbiTraitFiniteStochasticLanguage, ebi_trait_queriable_stochastic_language::EbiTraitQueriableStochasticLanguage, ebi_trait_semantics::ToSemantics}, follower_semantics::FollowerSemantics, math::fraction::Fraction, medoid, techniques::{align::Align, medoid_non_stochastic::MedoidNonStochastic, occurrences_stochastic_miner::OccurrencesStochasticMiner, probabilistic_queries::FiniteStochasticLanguageAnalyser, statistical_test::StatisticalTests, uniform_stochastic_miner::UniformStochasticMiner, unit_earth_movers_stochastic_conformance::UnitEarthMoversStochasticConformance}};

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
        let semantics = slpn.get_deterministic_stochastic_semantics();
        let slang = semantics.analyse_most_likely_traces(&1).unwrap();
        let fout = fs::read_to_string("testfiles/ba.slang").unwrap();
        assert_eq!(fout, slang.to_string())
    }

    #[test]
    fn fraction_exact() {
        let zero = Fraction::one().one_minus();

        assert!(zero.is_zero());
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
        let sdfa = fin.parse::<StochasticDeterministicFiniteAutomaton>().unwrap();
        let semantics = StochasticDeterministicFiniteAutomaton::get_deterministic_semantics(Arc::new(sdfa)).unwrap();
        assert!(semantics.analyse_minimum_probability(&Fraction::one()).is_err());
    }

    #[test]
    fn sdfa_minprob_zero() {
        let fin = fs::read_to_string("testfiles/aa-ab-ba.sdfa").unwrap();
        let sdfa = fin.parse::<StochasticDeterministicFiniteAutomaton>().unwrap();
        let semantics = StochasticDeterministicFiniteAutomaton::get_deterministic_semantics(Arc::new(sdfa)).unwrap();

        let slang2 = semantics.analyse_minimum_probability(&Fraction::zero()).unwrap();

        let should_string = fs::read_to_string("testfiles/aa-ab-ba.slang").unwrap();
        let should_slang = should_string.parse::<FiniteStochasticLanguage>().unwrap();
        assert_eq!(should_slang, slang2)
    }

    #[test]
    fn sdfa_minprob_zero_loop() {
        let fin = fs::read_to_string("testfiles/a-loop.sdfa").unwrap();
        let sdfa = fin.parse::<StochasticDeterministicFiniteAutomaton>().unwrap();
        let semantics = StochasticDeterministicFiniteAutomaton::get_deterministic_semantics(Arc::new(sdfa)).unwrap();

        assert!(semantics.analyse_minimum_probability(&Fraction::zero()).is_err());
    }

    #[test]
    fn slang_minprob_one_deterministic() {
        let fin = fs::read_to_string("testfiles/aa-ab-ba.slang").unwrap();
        let slang = fin.parse::<FiniteStochasticLanguage>().unwrap();
        let semantics = slang.get_deterministic_stochastic_semantics().unwrap();
        
        //should error
        assert!(semantics.analyse_minimum_probability(&Fraction::one()).is_err());
    }

    #[test]
    fn slang_minprob_one() {
        let fin = fs::read_to_string("testfiles/aa-ab-ba.slang").unwrap();
        let slang = fin.parse::<FiniteStochasticLanguage>().unwrap();
        
        let slang2: &dyn EbiTraitFiniteStochasticLanguage = &slang;
        //should error
        assert!(slang2.analyse_minimum_probability(&Fraction::one()).is_err());
    }

    #[test]
    fn slang_minprob_zero() {
        let fin = fs::read_to_string("testfiles/aa-ab-ba.slang").unwrap();
        let slang = fin.parse::<FiniteStochasticLanguage>().unwrap();
        let semantics = slang.get_deterministic_stochastic_semantics().unwrap();
        
        //should return the same
        let slang2 = semantics.analyse_minimum_probability(&Fraction::zero()).unwrap();

        assert_eq!(slang, slang2)
    }

    #[test]
    fn cla_test() {
        let fin = fs::read_to_string("testfiles/a-b.xes").unwrap();
        let event_log: Box<dyn EbiTraitEventLog> = Box::new(fin.parse::<EventLog>().unwrap());

        let (_, sustain) = event_log.log_categorical_attribute(500, &"attribute".to_string(), &Fraction::from((1, 20))).unwrap();
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
        let mut sdfa = fin.parse::<StochasticDeterministicFiniteAutomaton>().unwrap();

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

        let uemsc = slang1.unit_earth_movers_stochastic_conformance(Box::new(slang2)).unwrap();
        assert_eq!(uemsc, Fraction::one())
    }

    #[test]
    fn align_sdfa_trace() {
        let fin = fs::read_to_string("testfiles/aa-ab-ba.sdfa").unwrap();
        let sdfa = Arc::new(fin.parse::<StochasticDeterministicFiniteAutomaton>().unwrap());
        let mut semantics = StochasticDeterministicFiniteAutomaton::get_semantics(sdfa);

        let a = semantics.get_activity_key_mut().process_activity("a");
        let b = semantics.get_activity_key_mut().process_activity("b");

        let trace = vec![b, b];
        
        let (alignment, _) = semantics.align_trace(&trace).unwrap();

        let correct_1 = vec![Move::SynchronousMove(b, 1), Move::ModelMove(a, 4), Move::SilentMove(5), Move::LogMove(b)];
        let correct_2 = vec![Move::SynchronousMove(b, 1), Move::LogMove(b), Move::ModelMove(a, 4), Move::SilentMove(5)];

        assert!(alignment == correct_1 || alignment == correct_2);
    }

    #[test]
    fn align_sdfa_lang() {
        let fin1 = fs::read_to_string("testfiles/aa-ab-ba.sdfa").unwrap();
        let sdfa = Arc::new(fin1.parse::<StochasticDeterministicFiniteAutomaton>().unwrap());
        let mut semantics = StochasticDeterministicFiniteAutomaton::get_semantics(sdfa);

        let fin2 = fs::read_to_string("testfiles/bb.lang").unwrap();
        let lang = Box::new(fin2.parse::<FiniteLanguage>().unwrap());

        let a = semantics.get_activity_key_mut().process_activity("a");
        let b = semantics.get_activity_key_mut().process_activity("b");
        
        let alignment = semantics.align_language(lang).unwrap();

        let correct_1 = vec![Move::SynchronousMove(b, 1), Move::ModelMove(a, 4), Move::SilentMove(5), Move::LogMove(b)];
        let correct_2 = vec![Move::SynchronousMove(b, 1), Move::LogMove(b), Move::ModelMove(a, 4), Move::SilentMove(5)];

        assert!(alignment.get(0) == Some(&correct_1) || alignment.get(0) == Some(&correct_2));
    }

    #[test]
    fn align_slang_lang() {
        //enable debugging
        Builder::new().filter_level(LevelFilter::Trace).init();

        let fin1 = fs::read_to_string("testfiles/aa-ab-ba.slang").unwrap();
        let sdfa = fin1.parse::<FiniteStochasticLanguage>().unwrap();
        let mut semantics = sdfa.get_semantics();

        let fin2 = fs::read_to_string("testfiles/bb.lang").unwrap();
        let lang = Box::new(fin2.parse::<FiniteLanguage>().unwrap());

        let a = semantics.get_activity_key_mut().process_activity("a");
        let b = semantics.get_activity_key_mut().process_activity("b");
        
        let alignment = semantics.align_language(lang).unwrap();

        let correct_1 = vec![Move::SynchronousMove(b, 1), Move::ModelMove(a, 4), Move::SilentMove(5), Move::LogMove(b)];
        let correct_2 = vec![Move::SynchronousMove(b, 1), Move::LogMove(b), Move::ModelMove(a, 4), Move::SilentMove(5)];

        assert!(alignment.get(0) == Some(&correct_1) || alignment.get(0) == Some(&correct_2));
    }

    #[test]
    fn probability_sdfa_livelock_zeroweight() {
        let fin1 = fs::read_to_string("testfiles/a-livelock-zeroweight.sdfa").unwrap();
        let mut sdfa = fin1.parse::<StochasticDeterministicFiniteAutomaton>().unwrap();

        //a ends in a livelock and has probability 0
        let strace = vec!["a".to_string()];
        let trace = sdfa.get_activity_key_mut().process_trace(&strace);
        let trace_follower = FollowerSemantics::Trace(&trace);
        assert_eq!(sdfa.get_probability(&trace_follower).unwrap(), Fraction::zero());

        //a, a ends in a livelock and has probability 0
        let strace = vec!["a".to_string(), "a".to_string()];
        let trace = sdfa.get_activity_key_mut().process_trace(&strace);
        let trace_follower = FollowerSemantics::Trace(&trace);
        assert_eq!(sdfa.get_probability(&trace_follower).unwrap(), Fraction::zero());

        //b has weight 0
        let strace = vec!["b".to_string()];
        let trace = sdfa.get_activity_key_mut().process_trace(&strace);
        let trace_follower = FollowerSemantics::Trace(&trace);
        assert_eq!(sdfa.get_probability(&trace_follower).unwrap(), Fraction::zero());
    }

    #[test]
    fn probability_slpn_livelock_zeroweight() {
        let fin1 = fs::read_to_string("testfiles/a-livelock-zeroweight.slpn").unwrap();
        let mut slpn = fin1.parse::<StochasticLabelledPetriNet>().unwrap();

        //a ends in a livelock and has probability 0
        let strace = vec!["a".to_string()];
        let trace = slpn.get_activity_key_mut().process_trace(&strace);
        let trace_follower = FollowerSemantics::Trace(&trace);
        assert_eq!(slpn.get_probability(&trace_follower).unwrap(), Fraction::zero());

        //a, a ends in a livelock and has probability 0
        let strace = vec!["a".to_string(), "a".to_string()];
        let trace = slpn.get_activity_key_mut().process_trace(&strace);
        let trace_follower = FollowerSemantics::Trace(&trace);
        assert_eq!(slpn.get_probability(&trace_follower).unwrap(), Fraction::zero());

        //b has weight 0
        let strace = vec!["b".to_string()];
        let trace = slpn.get_activity_key_mut().process_trace(&strace);
        let trace_follower = FollowerSemantics::Trace(&trace);
        assert_eq!(slpn.get_probability(&trace_follower).unwrap(), Fraction::zero());

        //c has weight 0
        let strace = vec!["c".to_string()];
        let trace = slpn.get_activity_key_mut().process_trace(&strace);
        let trace_follower = FollowerSemantics::Trace(&trace);
        assert_eq!(slpn.get_probability(&trace_follower).unwrap(), Fraction::zero());
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
}