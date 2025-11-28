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
#[cfg(test)]
use std::fs::{self, DirEntry, File};

#[cfg(test)]
use crate::{
    ebi_framework::{
        ebi_file_handler::{EBI_FILE_HANDLERS, EbiFileHandler},
        ebi_input::{EbiInput, EbiObjectImporter, EbiTraitImporter},
    },
    multiple_reader::MultipleReader,
};

#[cfg(test)]
pub fn get_all_test_files() -> Vec<(
    EbiInput,
    Option<EbiObjectImporter>,
    Option<EbiTraitImporter>,
    String,
)> {
    let mut result = vec![];
    let files = fs::read_dir("./testfiles").unwrap();
    for path in files {
        let file = path.unwrap();

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
                for importer in file_handler.object_importers {
                    if should_file_be_tested(&file, importer, file_handler) {
                        println!(
                            "import {:?}, file handler {}, importer {}",
                            file.file_name(),
                            file_handler,
                            importer
                        );
                        let object = EbiInput::Object(
                            (importer.get_importer())(
                                &mut reader.get().unwrap(),
                                &importer.default_parameter_values(),
                            )
                            .unwrap(),
                            file_handler,
                        );

                        result.push((object, Some(importer.clone()), None, format!("{:?}", file)));
                    }
                }

                for importer in file_handler.trait_importers {
                    let object = EbiInput::Trait(
                        importer
                            .import(
                                &mut reader.get().unwrap(),
                                &importer.default_parameter_values(),
                            )
                            .unwrap(),
                        file_handler,
                    );

                    result.push((object, None, Some(importer.clone()), format!("{:?}", file)));
                }
            } else {
                //file handler should not accept this file

                for importer in file_handler.object_importers {
                    println!(
                        "import {:?}, file handler {}, importer {} (should fail)",
                        file.file_name(),
                        file_handler,
                        importer
                    );
                    assert!(
                        (importer.get_importer())(
                            &mut reader.get().unwrap(),
                            &importer.default_parameter_values()
                        )
                        .is_err()
                    );
                }

                for importer in file_handler.trait_importers {
                    println!(
                        "import {:?}, file handler {}, importer {} (should fail)",
                        file.file_name(),
                        file_handler,
                        importer
                    );
                    assert!(
                        importer
                            .import(
                                &mut reader.get().unwrap(),
                                &importer.default_parameter_values()
                            )
                            .is_err()
                    );
                }
            }
        }
    }
    result
}

#[cfg(test)]
pub fn should_file_be_tested(
    file: &DirEntry,
    importer: &EbiObjectImporter,
    file_handler: &EbiFileHandler,
) -> bool {
    //special case: empty.ptree and empty_2.ptree cannot be imported as an LPN, but it is not invalid

    use crate::ebi_file_handlers::process_tree::EBI_PROCESS_TREE;
    let special = if let EbiObjectImporter::LabelledPetriNet(_, _) = importer {
        true
    } else {
        false
    };
    !(*file_handler == EBI_PROCESS_TREE
        && special
        && (file.file_name().into_string().unwrap() == "empty.ptree"
            || file.file_name().into_string().unwrap() == "empty_2.ptree"))
}

#[cfg(test)]
pub mod tests {

    use crate::{
        ebi_framework::{
            ebi_input::EbiInput, ebi_trait_object::EbiTraitObject,
            trait_importers::{ImportAsSemanticsTrait, ToSemanticsTrait},
        },
        ebi_traits::{
            ebi_trait_event_log::EbiTraitEventLog, ebi_trait_semantics::EbiTraitSemantics,
        },
        multiple_reader::MultipleReader,
        semantics::semantics::Semantics,
    };
    use ebi_objects::{
        DeterministicFiniteAutomaton, EventLog, FiniteLanguage, Importable, Infoable,
        NumberOfTraces, PetriNetMarkupLanguage, ProcessTreeMarkupLanguage,
        StochasticDeterministicFiniteAutomaton,
    };
    use std::fs::{self, File};

    #[test]
    fn all_graphable() {
        for (input, _, _, _) in crate::tests::get_all_test_files() {
            if let EbiInput::Trait(object, _) = input {
                if let EbiTraitObject::Graphable(object) = object {
                    assert!(object.to_dot().is_ok());
                }
            }
        }
    }

    #[test]
    fn objects() {
        for (input, _, _, _) in crate::tests::get_all_test_files() {
            if let EbiInput::Object(object, _) = input {
                object.get_type();
                object.to_string();
            } else if let EbiInput::Trait(object, _) = input {
                object.get_trait();
            }
        }
    }

    #[test]
    fn all_infoable() {
        for (input, _, _, _) in crate::tests::get_all_test_files() {
            if let EbiInput::Object(object, _) = input {
                let mut f = vec![];
                object.info(&mut f).unwrap();
            }
        }
    }

    #[test]
    fn dfa_empty() {
        let fin = fs::read_to_string("testfiles/empty.dfa").unwrap();
        let dfa = fin.parse::<DeterministicFiniteAutomaton>().unwrap();

        if let EbiTraitSemantics::Usize(semantics) = dfa.to_semantics_trait() {
            assert!(semantics.get_initial_state().is_none());
        } else {
            assert!(false);
        }
    }

    #[test]
    fn log_empty() {
        let fin = fs::read_to_string("testfiles/empty.xes").unwrap();
        let log = fin.parse::<EventLog>().unwrap();

        if let EbiTraitSemantics::Usize(semantics) = log.to_semantics_trait() {
            assert!(semantics.get_initial_state().is_none());
        }
    }

    #[test]
    fn lang_empty() {
        let fin = fs::read_to_string("testfiles/empty.lang").unwrap();
        let log = fin.parse::<FiniteLanguage>().unwrap();

        if let EbiTraitSemantics::Usize(semantics) = log.to_semantics_trait() {
            assert!(semantics.get_initial_state().is_none());
        }
    }

    #[test]
    fn pnml_empty() {
        let mut reader = MultipleReader::from_file(File::open("testfiles/empty.pnml").unwrap());
        let semantics = PetriNetMarkupLanguage::import_as_semantics_trait(
            &mut reader.get().unwrap(),
            &PetriNetMarkupLanguage::default_importer_parameter_values(),
        )
        .unwrap();

        if let EbiTraitSemantics::Marking(semantics) = semantics {
            let state = semantics.get_initial_state().unwrap();
            assert_eq!(semantics.get_enabled_transitions(&state).len(), 0);
        } else {
            assert!(false);
        }
    }

    #[test]
    fn nested_ptml() {
        let fin = fs::read_to_string("testfiles/valid nested.ptml").unwrap();
        let ptml = fin.parse::<ProcessTreeMarkupLanguage>().unwrap();

        let sem = ptml.tree;
        let state = sem.get_initial_state().unwrap();
        assert_eq!(sem.get_enabled_transitions(&state).len(), 1);
    }

    #[test]
    fn sdfa_empty() {
        let fin = fs::read_to_string("testfiles/empty.sdfa").unwrap();
        let dfa = fin
            .parse::<StochasticDeterministicFiniteAutomaton>()
            .unwrap();

        if let EbiTraitSemantics::Usize(semantics) = dfa.to_semantics_trait() {
            assert!(semantics.get_initial_state().is_none());
        } else {
            assert!(false);
        }
    }

    #[test]
    fn len_retain() {
        let fin = fs::read_to_string("testfiles/a-b.xes").unwrap();
        let mut log = fin.parse::<EventLog>().unwrap();

        assert_eq!(log.number_of_traces(), 2);

        log.retain_traces(Box::new(|_| false));

        assert_eq!(log.number_of_traces(), 0);
    }
}
