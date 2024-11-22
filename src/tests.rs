#[cfg(test)]
mod tests {
    use std::{fs, ops::Neg};

    use fraction::GenericFraction;
    use num_bigint::ToBigUint;

    use crate::{ebi_framework::activity_key::HasActivityKey, ebi_objects::{alignments::Move, deterministic_finite_automaton::DeterministicFiniteAutomaton, event_log::EventLog, finite_language::FiniteLanguage, finite_stochastic_language::FiniteStochasticLanguage, labelled_petri_net::{LPNMarking, LabelledPetriNet}, stochastic_deterministic_finite_automaton::StochasticDeterministicFiniteAutomaton, stochastic_labelled_petri_net::StochasticLabelledPetriNet}, ebi_traits::{ebi_trait_event_log::{EbiTraitEventLog, IndexTrace}, ebi_trait_finite_stochastic_language::EbiTraitFiniteStochasticLanguage, ebi_trait_queriable_stochastic_language::EbiTraitQueriableStochasticLanguage, ebi_trait_semantics::{Semantics, ToSemantics}, ebi_trait_stochastic_deterministic_semantics::{EbiTraitStochasticDeterministicSemantics, StochasticDeterministicSemantics, ToStochasticDeterministicSemantics}}, follower_semantics::FollowerSemantics, math::{fraction::Fraction, log_div::LogDiv, root_log_div::RootLogDiv}, medoid, techniques::{align::Align, deterministic_semantics_for_stochastic_semantics::PMarking, jensen_shannon_stochastic_conformance::JensenShannonStochasticConformance, medoid_non_stochastic::MedoidNonStochastic, occurrences_stochastic_miner::OccurrencesStochasticMiner, probability_queries::ProbabilityQueries, process_variety::ProcessVariety, statistical_test::StatisticalTests, uniform_stochastic_miner::UniformStochasticMiner, unit_earth_movers_stochastic_conformance::UnitEarthMoversStochasticConformance}};

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
        let fin = fs::read_to_string("testfiles/empty.slpn").unwrap();
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
        
        let x : Box<dyn StochasticDeterministicSemantics<DetState = PMarking<LPNMarking>, LivState = PMarking<LPNMarking>>> = Box::new(slpn);
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
        let semantics = sdfa.to_stochastic_deterministic_semantics();
        assert_eq!(semantics.analyse_minimum_probability(&Fraction::one()).unwrap().len(), 0);
    }

    #[test]
    fn sdfa_minprob_zero() {
        let fin = fs::read_to_string("testfiles/aa-ab-ba.sdfa").unwrap();
        let sdfa = fin.parse::<StochasticDeterministicFiniteAutomaton>().unwrap();
        let semantics = sdfa.to_stochastic_deterministic_semantics();

        let slang2 = semantics.analyse_minimum_probability(&Fraction::zero()).unwrap();

        let should_string = fs::read_to_string("testfiles/aa-ab-ba.slang").unwrap();
        let should_slang = should_string.parse::<FiniteStochasticLanguage>().unwrap();
        assert_eq!(should_slang, slang2)
    }

    #[test]
    fn sdfa_minprob_zero_loop() {
        let fin = fs::read_to_string("testfiles/a-loop.sdfa").unwrap();
        let sdfa = fin.parse::<StochasticDeterministicFiniteAutomaton>().unwrap();
        let semantics = sdfa.to_stochastic_deterministic_semantics();

        assert!(semantics.analyse_minimum_probability(&Fraction::zero()).is_err());
    }

    #[test]
    fn slpn_minprob_livelock() {
        let fin = fs::read_to_string("testfiles/a-b-c-livelock.slpn").unwrap();
        let slpn = fin.parse::<StochasticLabelledPetriNet>().unwrap();
        let x : Box<dyn StochasticDeterministicSemantics<DetState = PMarking<LPNMarking>, LivState = PMarking<LPNMarking>>> = Box::new(slpn);
        let slang1 = x.analyse_minimum_probability(&Fraction::from((1,5))).unwrap();

        let fin2 = fs::read_to_string("testfiles/a-b-c-livelock.slang").unwrap();
        let slang2 = fin2.parse::<FiniteStochasticLanguage>().unwrap();

        assert_eq!(slang1, slang2);
    }

    #[test]
    fn slang_minprob_one_deterministic() {
        let fin = fs::read_to_string("testfiles/aa-ab-ba.slang").unwrap();
        let slang = fin.parse::<FiniteStochasticLanguage>().unwrap();
        let semantics = slang.to_stochastic_deterministic_semantics();
        
        assert_eq!(semantics.analyse_minimum_probability(&Fraction::one()).unwrap().len(), 0);
    }

    #[test]
    fn slang_minprob_one() {
        let fin = fs::read_to_string("testfiles/aa-ab-ba.slang").unwrap();
        let slang = fin.parse::<FiniteStochasticLanguage>().unwrap();
        
        let slang2: &dyn EbiTraitFiniteStochasticLanguage = &slang;
        assert_eq!(slang2.analyse_minimum_probability(&Fraction::one()).unwrap().len(), 0);
    }

    #[test]
    fn slang_minprob_zero() {
        let fin = fs::read_to_string("testfiles/aa-ab-ba.slang").unwrap();
        let slang = fin.parse::<FiniteStochasticLanguage>().unwrap();
        let slang2: &dyn EbiTraitFiniteStochasticLanguage = &slang;
        
        //should return the same
        let slang3 = slang2.analyse_minimum_probability(&Fraction::zero()).unwrap();

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
        let sdfa = fin.parse::<StochasticDeterministicFiniteAutomaton>().unwrap();
        let mut semantics = sdfa.to_semantics();

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
        let sdfa = fin1.parse::<StochasticDeterministicFiniteAutomaton>().unwrap();
        let mut semantics = sdfa.to_semantics();

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
        let fin1 = fs::read_to_string("testfiles/aa-ab-ba.slang").unwrap();
        let sdfa = fin1.parse::<FiniteStochasticLanguage>().unwrap();
        let mut semantics = sdfa.to_semantics();

        let fin2 = fs::read_to_string("testfiles/bb.lang").unwrap();
        let lang = Box::new(fin2.parse::<FiniteLanguage>().unwrap());

        let a = semantics.get_activity_key_mut().process_activity("a");
        let b = semantics.get_activity_key_mut().process_activity("b");
        
        let alignment = semantics.align_language(lang).unwrap();

        let correct_1 = vec![Move::ModelMove(a, 1), Move::SynchronousMove(b,2), Move::SilentMove(0), Move::LogMove(b)]; //other options may be valid, please check semantically when this fails
        let correct_2 = vec![Move::SynchronousMove(b,2), Move::ModelMove(a, 1), Move::SilentMove(0), Move::LogMove(b)]; //other options may be valid, please check semantically when this fails
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

        let correct_1 = vec![Move::SynchronousMove(b, 1), Move::ModelMove(a, 2), Move::SilentMove(5), Move::LogMove(b)]; //other options may be valid, please check semantically when this fails

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

        let correct_1 = vec![Move::SynchronousMove(b,1), Move::ModelMove(a, 4), Move::SilentMove(5), Move::LogMove(b)]; //other options may be valid, please check semantically when this fails
        assert_eq!(*alignment.get(0).unwrap(), correct_1);
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

        let slang: Box<dyn EbiTraitFiniteStochasticLanguage> = Box::new(log.get_finite_stochastic_language());

        let answer = RootLogDiv::sqrt(LogDiv::Exact(GenericFraction::from(1), 2.to_biguint().unwrap())).one_minus();
        assert_eq!(slang.jssc_log2model(Box::new(slpn)).unwrap(), answer);
    }

    #[test]
    fn variety() {
        let fin = fs::read_to_string("testfiles/aa-ab-ba.slang").unwrap();
        let slang: Box<dyn EbiTraitFiniteStochasticLanguage> = Box::new(fin.parse::<FiniteStochasticLanguage>().unwrap());

        assert_eq!(slang.process_variety(), Fraction::from((2, 5)));
    }

    #[test]
    fn slang_cover_empty() {
        let fin = fs::read_to_string("testfiles/empty.slang").unwrap();
        let slang: Box<dyn EbiTraitFiniteStochasticLanguage> = Box::new(fin.parse::<FiniteStochasticLanguage>().unwrap());

        let slang2 = slang.analyse_probability_coverage(&Fraction::zero()).unwrap();
        assert_eq!(slang2.len(), 0);

        assert!(slang.analyse_probability_coverage(&Fraction::from((1, 2))).is_err());
    }

    #[test]
    fn slpn_cover_empty() {
        let fin = fs::read_to_string("testfiles/empty.slpn").unwrap();
        
        let slpn = Box::new(fin.parse::<StochasticLabelledPetriNet>().unwrap());
        let state = slpn.get_deterministic_initial_state().unwrap();
        assert_eq!(slpn.get_deterministic_enabled_activities(&state).len(), 0);
        assert_eq!(slpn.get_deterministic_termination_probability(&state), Fraction::one());

        let slpn: Box<dyn StochasticDeterministicSemantics<DetState = PMarking<LPNMarking>, LivState = PMarking<LPNMarking>>> = Box::new(fin.parse::<StochasticLabelledPetriNet>().unwrap());
        let slang2 = slpn.analyse_probability_coverage(&Fraction::zero()).unwrap();
        assert_eq!(slang2.len(), 1);

        let fin2 = fs::read_to_string("testfiles/empty_trace.slang").unwrap();
        let slang3 = fin2.parse::<FiniteStochasticLanguage>().unwrap();

        assert_eq!(slpn.analyse_probability_coverage(&Fraction::from((1, 2))).unwrap(), slang3);
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