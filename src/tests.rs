#[cfg(test)]
mod tests {
    //all tests have been moved to their respective code, as is standard in Rust projects.


    //test disabled due to inconsistent output order (which is within spec)
    // #[test]
    // fn slang_to_sdfa() {
    //     let fin = fs::read_to_string("testfiles/aa-ab-ba.slang").unwrap();
    //     let slang = fin.parse::<FiniteStochasticLanguage>().unwrap();
    //     let sdfa = slang.to_stochastic_deterministic_finite_automaton();
    //     let fout = fs::read_to_string("testfiles/aa-ab-ba.sdfa").unwrap();
    //     assert_eq!(fout, sdfa.to_string())
    // }

    // #[test]
    // fn sdfa_minprob_zero_loop() {
    //     let fin = fs::read_to_string("testfiles/a-loop.sdfa").unwrap();
    //     let sdfa = fin
    //         .parse::<StochasticDeterministicFiniteAutomaton>()
    //         .unwrap();
    //     let semantics = sdfa.to_stochastic_deterministic_semantics();

    //     assert!(semantics
    //         .analyse_minimum_probability(&Fraction::zero())
    //         .is_err());
    // }


    // #[test] //disabled until we can handle livelocks
    // fn sample_sdfa_no_language() {
    //     let fin1 = fs::read_to_string("testfiles/a-livelock-zeroweight.sdfa").unwrap();
    //     let sdfa = fin1.parse::<StochasticDeterministicFiniteAutomaton>().unwrap();

    //     let sample = sdfa.sample(1).unwrap();

    //     //the language of this model is empty, so it should not have any traces in the sample
    //     println!("{:?}", sample);
    //     // assert!(sample.is_err())
    // }

    // //#[test] //disabled until we can handle livelocks
    // fn sample_slpn_no_language() {
    //     let fin1 = fs::read_to_string("testfiles/a-livelock-zeroweight.slpn").unwrap();
    //     let slpn = fin1.parse::<StochasticLabelledPetriNet>().unwrap();

    //     let sample = slpn.sample(1);

    //     //the language of this model is empty, so it should not have any traces in the sample
    //     assert!(sample.is_err())
    // }



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