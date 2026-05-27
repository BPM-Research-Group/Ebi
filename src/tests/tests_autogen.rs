// This file has been automatically generated. Manual changes will be overridden.

mod tests{
	use crate::ebi_framework::{
		ebi_command::EbiCommand,
		ebi_input::{self, EbiInput, read_as_object_with_file_handler, TEST_INPUT_TYPE_STRING, TEST_INPUT_TYPE_USIZE, TEST_INPUT_TYPE_FRACTION}
	};
	use crate::multiple_reader::MultipleReader;
	use std::fs::File;


	// ==== group Ebi ====


	// ==== group ana ====


	// ==== command all ====
	#[test]
	pub fn ebi_ana_all_test_0() {
		
		// trait finite stochastic language#./testfiles/ba-aa-ab.slang
		let mut reader = MultipleReader::from_file(File::open("./testfiles/ba-aa-ab.slang").unwrap());
		let (object0, file_handler0) = ebi_input::read_as_trait(&("finite stochastic language".parse().unwrap()), &mut reader, None, 0).unwrap();
		let input0 = EbiInput::Trait(object0, file_handler0);
		let inputs = vec![input0];

		if let EbiCommand::Command{execute, output_type, ..} = crate::ebi_commands::ebi_command_analyse::EBI_ANALYSE_ALL {
			match (execute)(inputs, None) {
				Ok(output) => assert_eq!(&output.get_type(), output_type),
				Err(_) => assert!(false),
			}
		}
	}


	// ==== command comp ====
	#[test]
	pub fn ebi_ana_comp_test_0() {
		
		// trait event log#./testfiles/empty.xes
		let mut reader = MultipleReader::from_file(File::open("./testfiles/empty.xes").unwrap());
		let (object0, file_handler0) = ebi_input::read_as_trait(&("event log".parse().unwrap()), &mut reader, None, 0).unwrap();
		let input0 = EbiInput::Trait(object0, file_handler0);
		let inputs = vec![input0];

		if let EbiCommand::Command{execute, output_type, ..} = crate::ebi_commands::ebi_command_analyse::EBI_ANALYSE_COMPLETENESS {
			match (execute)(inputs, None) {
				Ok(output) => assert_eq!(&output.get_type(), output_type),
				Err(_) => assert!(false),
			}
		}
	}


	// ==== command cov ====
	#[test]
	pub fn ebi_ana_cov_test_0() {
		
		// trait finite stochastic language#./testfiles/ba-aa-ab.slang
		let mut reader = MultipleReader::from_file(File::open("./testfiles/ba-aa-ab.slang").unwrap());
		let (object0, file_handler0) = ebi_input::read_as_trait(&("finite stochastic language".parse().unwrap()), &mut reader, None, 0).unwrap();
		let input0 = EbiInput::Trait(object0, file_handler0);
		// fraction 0
		let input1 = EbiInput::Fraction("0".parse().unwrap(), &TEST_INPUT_TYPE_FRACTION);
		let inputs = vec![input0, input1];

		if let EbiCommand::Command{execute, output_type, ..} = crate::ebi_commands::ebi_command_analyse::EBI_ANALYSE_COVERAGE {
			match (execute)(inputs, None) {
				Ok(output) => assert_eq!(&output.get_type(), output_type),
				Err(_) => assert!(false),
			}
		}
	}


	// ==== command dfgedi ====
	#[test]
	pub fn ebi_ana_dfgedi_test_0() {
		
		// object directly follows graph#./testfiles/bpic12-a.xes.gz-dfg.dfg
		let mut reader = MultipleReader::from_file(File::open("./testfiles/bpic12-a.xes.gz-dfg.dfg").unwrap());
		let object0 = read_as_object_with_file_handler(&"directly follows graph".parse().unwrap(), &mut reader, None, 0, &mut None, "directly follows graph".parse().unwrap()).unwrap();
		let input0 = EbiInput::Object(object0, "directly follows graph".parse().unwrap());
		// object directly follows graph#./testfiles/bpic12-a.xes.gz-dfg.dfg
		let mut reader = MultipleReader::from_file(File::open("./testfiles/bpic12-a.xes.gz-dfg.dfg").unwrap());
		let object1 = read_as_object_with_file_handler(&"directly follows graph".parse().unwrap(), &mut reader, None, 0, &mut None, "directly follows graph".parse().unwrap()).unwrap();
		let input1 = EbiInput::Object(object1, "directly follows graph".parse().unwrap());
		let inputs = vec![input0, input1];

		if let EbiCommand::Command{execute, output_type, ..} = crate::ebi_commands::ebi_command_analyse::EBI_ANALYSE_DIRECTLY_FOLLOWS_EDGE_DIFFERENCE {
			match (execute)(inputs, None) {
				Ok(output) => assert_eq!(&output.get_type(), output_type),
				Err(_) => assert!(false),
			}
		}
	}


	// ==== command en ====
	#[test]
	pub fn ebi_ana_en_test_0() {
		
		// trait finite stochastic language#./testfiles/ba-aa-ab.slang
		let mut reader = MultipleReader::from_file(File::open("./testfiles/ba-aa-ab.slang").unwrap());
		let (object0, file_handler0) = ebi_input::read_as_trait(&("finite stochastic language".parse().unwrap()), &mut reader, None, 0).unwrap();
		let input0 = EbiInput::Trait(object0, file_handler0);
		let inputs = vec![input0];

		if let EbiCommand::Command{execute, output_type, ..} = crate::ebi_commands::ebi_command_analyse::EBI_ANALYSE_ENTROPY {
			match (execute)(inputs, None) {
				Ok(output) => assert_eq!(&output.get_type(), output_type),
				Err(_) => assert!(false),
			}
		}
	}


	// ==== command med ====
	#[test]
	pub fn ebi_ana_med_test_0() {
		
		// trait finite stochastic language#./testfiles/ba-aa-ab.slang
		let mut reader = MultipleReader::from_file(File::open("./testfiles/ba-aa-ab.slang").unwrap());
		let (object0, file_handler0) = ebi_input::read_as_trait(&("finite stochastic language".parse().unwrap()), &mut reader, None, 0).unwrap();
		let input0 = EbiInput::Trait(object0, file_handler0);
		// usize 1
		let input1 = EbiInput::Usize(1, &TEST_INPUT_TYPE_USIZE);
		let inputs = vec![input0, input1];

		if let EbiCommand::Command{execute, output_type, ..} = crate::ebi_commands::ebi_command_analyse::EBI_ANALYSE_MEDOID {
			match (execute)(inputs, None) {
				Ok(output) => assert_eq!(&output.get_type(), output_type),
				Err(_) => assert!(false),
			}
		}
	}


	// ==== command minprob ====
	#[test]
	pub fn ebi_ana_minprob_test_0() {
		
		// trait stochastic deterministic semantics#./testfiles/seq(a-xor(b-c)).sptree
		let mut reader = MultipleReader::from_file(File::open("./testfiles/seq(a-xor(b-c)).sptree").unwrap());
		let (object0, file_handler0) = ebi_input::read_as_trait(&("stochastic deterministic semantics".parse().unwrap()), &mut reader, None, 0).unwrap();
		let input0 = EbiInput::Trait(object0, file_handler0);
		// fraction 0
		let input1 = EbiInput::Fraction("0".parse().unwrap(), &TEST_INPUT_TYPE_FRACTION);
		let inputs = vec![input0, input1];

		if let EbiCommand::Command{execute, output_type, ..} = crate::ebi_commands::ebi_command_analyse::EBI_ANALYSE_MINPROB {
			match (execute)(inputs, None) {
				Ok(output) => assert_eq!(&output.get_type(), output_type),
				Err(_) => assert!(false),
			}
		}
	}


	// ==== command mode ====
	#[test]
	pub fn ebi_ana_mode_test_0() {
		
		// trait finite stochastic language#./testfiles/ba-aa-ab.slang
		let mut reader = MultipleReader::from_file(File::open("./testfiles/ba-aa-ab.slang").unwrap());
		let (object0, file_handler0) = ebi_input::read_as_trait(&("finite stochastic language".parse().unwrap()), &mut reader, None, 0).unwrap();
		let input0 = EbiInput::Trait(object0, file_handler0);
		let inputs = vec![input0];

		if let EbiCommand::Command{execute, output_type, ..} = crate::ebi_commands::ebi_command_analyse::EBI_ANALYSE_MODE {
			match (execute)(inputs, None) {
				Ok(output) => assert_eq!(&output.get_type(), output_type),
				Err(_) => assert!(false),
			}
		}
	}


	// ==== command mostlikely ====
	#[test]
	pub fn ebi_ana_mostlikely_test_0() {
		
		// trait finite stochastic language#./testfiles/ba-aa-ab.slang
		let mut reader = MultipleReader::from_file(File::open("./testfiles/ba-aa-ab.slang").unwrap());
		let (object0, file_handler0) = ebi_input::read_as_trait(&("finite stochastic language".parse().unwrap()), &mut reader, None, 0).unwrap();
		let input0 = EbiInput::Trait(object0, file_handler0);
		// usize 1
		let input1 = EbiInput::Usize(1, &TEST_INPUT_TYPE_USIZE);
		let inputs = vec![input0, input1];

		if let EbiCommand::Command{execute, output_type, ..} = crate::ebi_commands::ebi_command_analyse::EBI_ANALYSE_MOSTLIKELY {
			match (execute)(inputs, None) {
				Ok(output) => assert_eq!(&output.get_type(), output_type),
				Err(_) => assert!(false),
			}
		}
	}


	// ==== command var ====
	#[test]
	pub fn ebi_ana_var_test_0() {
		
		// trait finite stochastic language#./testfiles/ba-aa-ab.slang
		let mut reader = MultipleReader::from_file(File::open("./testfiles/ba-aa-ab.slang").unwrap());
		let (object0, file_handler0) = ebi_input::read_as_trait(&("finite stochastic language".parse().unwrap()), &mut reader, None, 0).unwrap();
		let input0 = EbiInput::Trait(object0, file_handler0);
		let inputs = vec![input0];

		if let EbiCommand::Command{execute, output_type, ..} = crate::ebi_commands::ebi_command_analyse::EBI_ANALYSE_VARIETY {
			match (execute)(inputs, None) {
				Ok(output) => assert_eq!(&output.get_type(), output_type),
				Err(_) => assert!(false),
			}
		}
	}


	// ==== group anans ====


	// ==== command act ====
	#[test]
	pub fn ebi_anans_act_test_0() {
		
		// trait activities#./testfiles/seq(a-xor(b-c)).sptree
		let mut reader = MultipleReader::from_file(File::open("./testfiles/seq(a-xor(b-c)).sptree").unwrap());
		let (object0, file_handler0) = ebi_input::read_as_trait(&("activities".parse().unwrap()), &mut reader, None, 0).unwrap();
		let input0 = EbiInput::Trait(object0, file_handler0);
		let inputs = vec![input0];

		if let EbiCommand::Command{execute, output_type, ..} = crate::ebi_commands::ebi_command_analyse_non_stochastic::EBI_ANALYSE_NON_STOCHASTIC_ACTIVITIES {
			match (execute)(inputs, None) {
				Ok(output) => assert_eq!(&output.get_type(), output_type),
				Err(_) => assert!(false),
			}
		}
	}


	// ==== command bnd ====
	#[test]
	pub fn ebi_anans_bnd_test_0() {
		
		// object directly follows graph#./testfiles/bpic12-a.xes.gz-dfg.dfg
		let mut reader = MultipleReader::from_file(File::open("./testfiles/bpic12-a.xes.gz-dfg.dfg").unwrap());
		let object0 = read_as_object_with_file_handler(&"directly follows graph".parse().unwrap(), &mut reader, None, 0, &mut None, "directly follows graph".parse().unwrap()).unwrap();
		let input0 = EbiInput::Object(object0, "directly follows graph".parse().unwrap());
		let inputs = vec![input0];

		if let EbiCommand::Command{execute, output_type, ..} = crate::ebi_commands::ebi_command_analyse_non_stochastic::EBI_ANALYSE_NON_STOCHASTIC_BOUNDED {
			match (execute)(inputs, None) {
				Ok(output) => assert_eq!(&output.get_type(), output_type),
				Err(_) => assert!(false),
			}
		}
	}


	// ==== command clus ====
	#[test]
	pub fn ebi_anans_clus_test_0() {
		
		// trait finite language#./testfiles/ba-aa-ab.slang
		let mut reader = MultipleReader::from_file(File::open("./testfiles/ba-aa-ab.slang").unwrap());
		let (object0, file_handler0) = ebi_input::read_as_trait(&("finite language".parse().unwrap()), &mut reader, None, 0).unwrap();
		let input0 = EbiInput::Trait(object0, file_handler0);
		// usize 1
		let input1 = EbiInput::Usize(1, &TEST_INPUT_TYPE_USIZE);
		let inputs = vec![input0, input1];

		if let EbiCommand::Command{execute, output_type, ..} = crate::ebi_commands::ebi_command_analyse_non_stochastic::EBI_ANALYSE_NON_STOCHASTIC_CLUSTER {
			match (execute)(inputs, None) {
				Ok(output) => assert_eq!(&output.get_type(), output_type),
				Err(_) => assert!(false),
			}
		}
	}


	// ==== command exe ====
	#[test]
	pub fn ebi_anans_exe_test_0() {
		
		// trait event log with event attributes#./testfiles/empty.xes
		let mut reader = MultipleReader::from_file(File::open("./testfiles/empty.xes").unwrap());
		let (object0, file_handler0) = ebi_input::read_as_trait(&("event log with event attributes".parse().unwrap()), &mut reader, None, 0).unwrap();
		let input0 = EbiInput::Trait(object0, file_handler0);
		// trait semantics#./testfiles/seq(a-xor(b-c)).sptree
		let mut reader = MultipleReader::from_file(File::open("./testfiles/seq(a-xor(b-c)).sptree").unwrap());
		let (object1, file_handler1) = ebi_input::read_as_trait(&("semantics".parse().unwrap()), &mut reader, None, 0).unwrap();
		let input1 = EbiInput::Trait(object1, file_handler1);
		let inputs = vec![input0, input1];

		if let EbiCommand::Command{execute, output_type, ..} = crate::ebi_commands::ebi_command_analyse_non_stochastic::EBI_ANALYSE_NON_STOCHASTIC_EXECUTIONS {
			match (execute)(inputs, None) {
				Ok(output) => assert_eq!(&output.get_type(), output_type),
				Err(_) => assert!(false),
			}
		}
	}


	// ==== command at ====
	#[test]
	pub fn ebi_anans_at_test_0() {
		
		// object directly follows graph#./testfiles/bpic12-a.xes.gz-dfg.dfg
		let mut reader = MultipleReader::from_file(File::open("./testfiles/bpic12-a.xes.gz-dfg.dfg").unwrap());
		let object0 = read_as_object_with_file_handler(&"directly follows graph".parse().unwrap(), &mut reader, None, 0, &mut None, "directly follows graph".parse().unwrap()).unwrap();
		let input0 = EbiInput::Object(object0, "directly follows graph".parse().unwrap());
		let inputs = vec![input0];

		if let EbiCommand::Command{execute, output_type, ..} = crate::ebi_commands::ebi_command_analyse_non_stochastic::EBI_ANALYSE_NON_STOCHASTIC_ANY_TRACES {
			match (execute)(inputs, None) {
				Ok(output) => assert_eq!(&output.get_type(), output_type),
				Err(_) => assert!(false),
			}
		}
	}


	// ==== command inft ====
	#[test]
	pub fn ebi_anans_inft_test_0() {
		
		// object event log#./testfiles/empty.xes
		let mut reader = MultipleReader::from_file(File::open("./testfiles/empty.xes").unwrap());
		let object0 = read_as_object_with_file_handler(&"event log".parse().unwrap(), &mut reader, None, 0, &mut None, "extensible event stream".parse().unwrap()).unwrap();
		let input0 = EbiInput::Object(object0, "extensible event stream".parse().unwrap());
		let inputs = vec![input0];

		if let EbiCommand::Command{execute, output_type, ..} = crate::ebi_commands::ebi_command_analyse_non_stochastic::EBI_ANALYSE_NON_STOCHASTIC_INFINITELY_MANY_TRACES {
			match (execute)(inputs, None) {
				Ok(output) => assert_eq!(&output.get_type(), output_type),
				Err(_) => assert!(false),
			}
		}
	}


	// ==== command med ====
	#[test]
	pub fn ebi_anans_med_test_0() {
		
		// trait finite language#./testfiles/ba-aa-ab.slang
		let mut reader = MultipleReader::from_file(File::open("./testfiles/ba-aa-ab.slang").unwrap());
		let (object0, file_handler0) = ebi_input::read_as_trait(&("finite language".parse().unwrap()), &mut reader, None, 0).unwrap();
		let input0 = EbiInput::Trait(object0, file_handler0);
		// usize 1
		let input1 = EbiInput::Usize(1, &TEST_INPUT_TYPE_USIZE);
		let inputs = vec![input0, input1];

		if let EbiCommand::Command{execute, output_type, ..} = crate::ebi_commands::ebi_command_analyse_non_stochastic::EBI_ANALYSE_NON_STOCHASTIC_MEDOID {
			match (execute)(inputs, None) {
				Ok(output) => assert_eq!(&output.get_type(), output_type),
				Err(_) => assert!(false),
			}
		}
	}


	// ==== command to ====
	#[test]
	pub fn ebi_anans_to_test_0() {
		
		// trait event log with event attributes#./testfiles/empty.xes
		let mut reader = MultipleReader::from_file(File::open("./testfiles/empty.xes").unwrap());
		let (object0, file_handler0) = ebi_input::read_as_trait(&("event log with event attributes".parse().unwrap()), &mut reader, None, 0).unwrap();
		let input0 = EbiInput::Trait(object0, file_handler0);
		let inputs = vec![input0];

		if let EbiCommand::Command{execute, ..} = crate::ebi_commands::ebi_command_analyse_non_stochastic::EBI_ANALYSE_NON_STOCHASTIC_TIMESTAMPS_ORDERED {
			assert!(((execute)(inputs, None)).is_err())
		}
	}


	// ==== group asso ====


	// ==== command att ====
	#[test]
	pub fn ebi_asso_att_test_0() {
		
		// trait event log with trace attributes#./testfiles/empty.xes
		let mut reader = MultipleReader::from_file(File::open("./testfiles/empty.xes").unwrap());
		let (object0, file_handler0) = ebi_input::read_as_trait(&("event log with trace attributes".parse().unwrap()), &mut reader, None, 0).unwrap();
		let input0 = EbiInput::Trait(object0, file_handler0);
		// string some string
		let input1 = EbiInput::String("some string".to_string(), &TEST_INPUT_TYPE_STRING);
		// usize 10
		let input2 = EbiInput::Usize(10, &TEST_INPUT_TYPE_USIZE);
		let inputs = vec![input0, input1, input2];

		if let EbiCommand::Command{execute, ..} = crate::ebi_commands::ebi_command_association::EBI_ASSOCIATION_ATTRIBUTE {
			assert!(((execute)(inputs, None)).is_err())
		}
	}


	// ==== command atts ====
	#[test]
	pub fn ebi_asso_atts_test_0() {
		
		// trait event log with trace attributes#./testfiles/empty.xes
		let mut reader = MultipleReader::from_file(File::open("./testfiles/empty.xes").unwrap());
		let (object0, file_handler0) = ebi_input::read_as_trait(&("event log with trace attributes".parse().unwrap()), &mut reader, None, 0).unwrap();
		let input0 = EbiInput::Trait(object0, file_handler0);
		// usize 10
		let input1 = EbiInput::Usize(10, &TEST_INPUT_TYPE_USIZE);
		let inputs = vec![input0, input1];

		if let EbiCommand::Command{execute, output_type, ..} = crate::ebi_commands::ebi_command_association::EBI_ASSOCIATION_ATTRIBUTES {
			match (execute)(inputs, None) {
				Ok(output) => assert_eq!(&output.get_type(), output_type),
				Err(_) => assert!(false),
			}
		}
	}


	// ==== group conf ====


	// ==== command cssc ====
	#[test]
	pub fn ebi_conf_cssc_test_0() {
		
		// trait finite stochastic language#./testfiles/ba-aa-ab.slang
		let mut reader = MultipleReader::from_file(File::open("./testfiles/ba-aa-ab.slang").unwrap());
		let (object0, file_handler0) = ebi_input::read_as_trait(&("finite stochastic language".parse().unwrap()), &mut reader, None, 0).unwrap();
		let input0 = EbiInput::Trait(object0, file_handler0);
		// trait queriable stochastic language#./testfiles/seq(a-xor(b-c)).sptree
		let mut reader = MultipleReader::from_file(File::open("./testfiles/seq(a-xor(b-c)).sptree").unwrap());
		let (object1, file_handler1) = ebi_input::read_as_trait(&("queriable stochastic language".parse().unwrap()), &mut reader, None, 0).unwrap();
		let input1 = EbiInput::Trait(object1, file_handler1);
		let inputs = vec![input0, input1];

		if let EbiCommand::Command{execute, output_type, ..} = crate::ebi_commands::ebi_command_conformance::EBI_CONFORMANCE_CHI_SQUARED {
			match (execute)(inputs, None) {
				Ok(output) => assert_eq!(&output.get_type(), output_type),
				Err(_) => assert!(false),
			}
		}
	}


	// ==== command cssc-sample ====
	#[test]
	pub fn ebi_conf_cssc_sample_test_0() {
		
		// trait finite stochastic language#./testfiles/ba-aa-ab.slang
		let mut reader = MultipleReader::from_file(File::open("./testfiles/ba-aa-ab.slang").unwrap());
		let (object0, file_handler0) = ebi_input::read_as_trait(&("finite stochastic language".parse().unwrap()), &mut reader, None, 0).unwrap();
		let input0 = EbiInput::Trait(object0, file_handler0);
		// trait finite stochastic language#./testfiles/ba-aa-ab.slang
		let mut reader = MultipleReader::from_file(File::open("./testfiles/ba-aa-ab.slang").unwrap());
		let (object1, file_handler1) = ebi_input::read_as_trait(&("finite stochastic language".parse().unwrap()), &mut reader, None, 0).unwrap();
		let input1 = EbiInput::Trait(object1, file_handler1);
		// usize 1
		let input2 = EbiInput::Usize(1, &TEST_INPUT_TYPE_USIZE);
		let inputs = vec![input0, input1, input2];

		if let EbiCommand::Command{execute, output_type, ..} = crate::ebi_commands::ebi_command_conformance::EBI_CONFORMANCE_CHI_SQUARED_SAMPLE {
			match (execute)(inputs, None) {
				Ok(output) => assert_eq!(&output.get_type(), output_type),
				Err(_) => assert!(false),
			}
		}
	}


	// ==== command emsc ====
	#[test]
	pub fn ebi_conf_emsc_test_0() {
		
		// trait finite stochastic language#./testfiles/ba-aa-ab.slang
		let mut reader = MultipleReader::from_file(File::open("./testfiles/ba-aa-ab.slang").unwrap());
		let (object0, file_handler0) = ebi_input::read_as_trait(&("finite stochastic language".parse().unwrap()), &mut reader, None, 0).unwrap();
		let input0 = EbiInput::Trait(object0, file_handler0);
		// trait finite stochastic language#./testfiles/ba-aa-ab.slang
		let mut reader = MultipleReader::from_file(File::open("./testfiles/ba-aa-ab.slang").unwrap());
		let (object1, file_handler1) = ebi_input::read_as_trait(&("finite stochastic language".parse().unwrap()), &mut reader, None, 0).unwrap();
		let input1 = EbiInput::Trait(object1, file_handler1);
		let inputs = vec![input0, input1];

		if let EbiCommand::Command{execute, output_type, ..} = crate::ebi_commands::ebi_command_conformance::EBI_CONFORMANCE_EARTH_MOVERS {
			match (execute)(inputs, None) {
				Ok(output) => assert_eq!(&output.get_type(), output_type),
				Err(_) => assert!(false),
			}
		}
	}


	// ==== command emsc-sample ====
	#[test]
	pub fn ebi_conf_emsc_sample_test_0() {
		
		// trait finite stochastic language#./testfiles/ba-aa-ab.slang
		let mut reader = MultipleReader::from_file(File::open("./testfiles/ba-aa-ab.slang").unwrap());
		let (object0, file_handler0) = ebi_input::read_as_trait(&("finite stochastic language".parse().unwrap()), &mut reader, None, 0).unwrap();
		let input0 = EbiInput::Trait(object0, file_handler0);
		// trait finite stochastic language#./testfiles/ba-aa-ab.slang
		let mut reader = MultipleReader::from_file(File::open("./testfiles/ba-aa-ab.slang").unwrap());
		let (object1, file_handler1) = ebi_input::read_as_trait(&("finite stochastic language".parse().unwrap()), &mut reader, None, 0).unwrap();
		let input1 = EbiInput::Trait(object1, file_handler1);
		// usize 1
		let input2 = EbiInput::Usize(1, &TEST_INPUT_TYPE_USIZE);
		let inputs = vec![input0, input1, input2];

		if let EbiCommand::Command{execute, output_type, ..} = crate::ebi_commands::ebi_command_conformance::EBI_CONFORMANCE_EARTH_MOVERS_SAMPLE {
			match (execute)(inputs, None) {
				Ok(output) => assert_eq!(&output.get_type(), output_type),
				Err(_) => assert!(false),
			}
		}
	}


	// ==== command er ====
	#[test]
	pub fn ebi_conf_er_test_0() {
		
		// trait finite stochastic language#./testfiles/ba-aa-ab.slang
		let mut reader = MultipleReader::from_file(File::open("./testfiles/ba-aa-ab.slang").unwrap());
		let (object0, file_handler0) = ebi_input::read_as_trait(&("finite stochastic language".parse().unwrap()), &mut reader, None, 0).unwrap();
		let input0 = EbiInput::Trait(object0, file_handler0);
		// trait queriable stochastic language#./testfiles/seq(a-xor(b-c)).sptree
		let mut reader = MultipleReader::from_file(File::open("./testfiles/seq(a-xor(b-c)).sptree").unwrap());
		let (object1, file_handler1) = ebi_input::read_as_trait(&("queriable stochastic language".parse().unwrap()), &mut reader, None, 0).unwrap();
		let input1 = EbiInput::Trait(object1, file_handler1);
		let inputs = vec![input0, input1];

		if let EbiCommand::Command{execute, output_type, ..} = crate::ebi_commands::ebi_command_conformance::EBI_CONFORMANCE_ENTROPIC_RELEVANCE {
			match (execute)(inputs, None) {
				Ok(output) => assert_eq!(&output.get_type(), output_type),
				Err(_) => assert!(false),
			}
		}
	}


	// ==== command gp ====
	#[test]
	pub fn ebi_conf_gp_test_0() {
		if ebi_objects::ebi_arithmetic::is_exact_globally() {
			return;
		}
		// trait finite stochastic language#./testfiles/ba-aa-ab.slang
		let mut reader = MultipleReader::from_file(File::open("./testfiles/ba-aa-ab.slang").unwrap());
		let (object0, file_handler0) = ebi_input::read_as_trait(&("finite stochastic language".parse().unwrap()), &mut reader, None, 0).unwrap();
		let input0 = EbiInput::Trait(object0, file_handler0);
		// object stochastic deterministic finite automaton#./testfiles/ba-aa-ab.slang
		let mut reader = MultipleReader::from_file(File::open("./testfiles/ba-aa-ab.slang").unwrap());
		let object1 = read_as_object_with_file_handler(&"stochastic deterministic finite automaton".parse().unwrap(), &mut reader, None, 0, &mut None, "finite stochastic language".parse().unwrap()).unwrap();
		let input1 = EbiInput::Object(object1, "finite stochastic language".parse().unwrap());
		// fraction 0
		let input2 = EbiInput::Fraction("0".parse().unwrap(), &TEST_INPUT_TYPE_FRACTION);
		let inputs = vec![input0, input1, input2];

		if let EbiCommand::Command{execute, output_type, ..} = crate::ebi_commands::ebi_command_conformance::EBI_CONFORMANCE_GAIN_PRECISION {
			match (execute)(inputs, None) {
				Ok(output) => assert_eq!(&output.get_type(), output_type),
				Err(_) => assert!(false),
			}
		}
	}


	// ==== command gr ====
	#[test]
	pub fn ebi_conf_gr_test_0() {
		if ebi_objects::ebi_arithmetic::is_exact_globally() {
			return;
		}
		// trait finite stochastic language#./testfiles/ba-aa-ab.slang
		let mut reader = MultipleReader::from_file(File::open("./testfiles/ba-aa-ab.slang").unwrap());
		let (object0, file_handler0) = ebi_input::read_as_trait(&("finite stochastic language".parse().unwrap()), &mut reader, None, 0).unwrap();
		let input0 = EbiInput::Trait(object0, file_handler0);
		// object stochastic deterministic finite automaton#./testfiles/ba-aa-ab.slang
		let mut reader = MultipleReader::from_file(File::open("./testfiles/ba-aa-ab.slang").unwrap());
		let object1 = read_as_object_with_file_handler(&"stochastic deterministic finite automaton".parse().unwrap(), &mut reader, None, 0, &mut None, "finite stochastic language".parse().unwrap()).unwrap();
		let input1 = EbiInput::Object(object1, "finite stochastic language".parse().unwrap());
		// fraction 0
		let input2 = EbiInput::Fraction("0".parse().unwrap(), &TEST_INPUT_TYPE_FRACTION);
		let inputs = vec![input0, input1, input2];

		if let EbiCommand::Command{execute, output_type, ..} = crate::ebi_commands::ebi_command_conformance::EBI_CONFORMANCE_GAIN_RECALL {
			match (execute)(inputs, None) {
				Ok(output) => assert_eq!(&output.get_type(), output_type),
				Err(_) => assert!(false),
			}
		}
	}


	// ==== command hsc ====
	#[test]
	pub fn ebi_conf_hsc_test_0() {
		
		// trait finite stochastic language#./testfiles/ba-aa-ab.slang
		let mut reader = MultipleReader::from_file(File::open("./testfiles/ba-aa-ab.slang").unwrap());
		let (object0, file_handler0) = ebi_input::read_as_trait(&("finite stochastic language".parse().unwrap()), &mut reader, None, 0).unwrap();
		let input0 = EbiInput::Trait(object0, file_handler0);
		// trait queriable stochastic language#./testfiles/seq(a-xor(b-c)).sptree
		let mut reader = MultipleReader::from_file(File::open("./testfiles/seq(a-xor(b-c)).sptree").unwrap());
		let (object1, file_handler1) = ebi_input::read_as_trait(&("queriable stochastic language".parse().unwrap()), &mut reader, None, 0).unwrap();
		let input1 = EbiInput::Trait(object1, file_handler1);
		let inputs = vec![input0, input1];

		if let EbiCommand::Command{execute, output_type, ..} = crate::ebi_commands::ebi_command_conformance::EBI_CONFORMANCE_HELLINGER {
			match (execute)(inputs, None) {
				Ok(output) => assert_eq!(&output.get_type(), output_type),
				Err(_) => assert!(false),
			}
		}
	}


	// ==== command hsc-sample ====
	#[test]
	pub fn ebi_conf_hsc_sample_test_0() {
		
		// trait finite stochastic language#./testfiles/ba-aa-ab.slang
		let mut reader = MultipleReader::from_file(File::open("./testfiles/ba-aa-ab.slang").unwrap());
		let (object0, file_handler0) = ebi_input::read_as_trait(&("finite stochastic language".parse().unwrap()), &mut reader, None, 0).unwrap();
		let input0 = EbiInput::Trait(object0, file_handler0);
		// trait finite stochastic language#./testfiles/ba-aa-ab.slang
		let mut reader = MultipleReader::from_file(File::open("./testfiles/ba-aa-ab.slang").unwrap());
		let (object1, file_handler1) = ebi_input::read_as_trait(&("finite stochastic language".parse().unwrap()), &mut reader, None, 0).unwrap();
		let input1 = EbiInput::Trait(object1, file_handler1);
		// usize 1
		let input2 = EbiInput::Usize(1, &TEST_INPUT_TYPE_USIZE);
		let inputs = vec![input0, input1, input2];

		if let EbiCommand::Command{execute, output_type, ..} = crate::ebi_commands::ebi_command_conformance::EBI_CONFORMANCE_HELLINGER_SAMPLE {
			match (execute)(inputs, None) {
				Ok(output) => assert_eq!(&output.get_type(), output_type),
				Err(_) => assert!(false),
			}
		}
	}


	// ==== command jssc ====
	#[test]
	pub fn ebi_conf_jssc_test_0() {
		if ebi_objects::ebi_arithmetic::is_exact_globally() {
			return;
		}
		// trait finite stochastic language#./testfiles/ba-aa-ab.slang
		let mut reader = MultipleReader::from_file(File::open("./testfiles/ba-aa-ab.slang").unwrap());
		let (object0, file_handler0) = ebi_input::read_as_trait(&("finite stochastic language".parse().unwrap()), &mut reader, None, 0).unwrap();
		let input0 = EbiInput::Trait(object0, file_handler0);
		// trait finite stochastic language#./testfiles/ba-aa-ab.slang
		let mut reader = MultipleReader::from_file(File::open("./testfiles/ba-aa-ab.slang").unwrap());
		let (object1, file_handler1) = ebi_input::read_as_trait(&("finite stochastic language".parse().unwrap()), &mut reader, None, 0).unwrap();
		let input1 = EbiInput::Trait(object1, file_handler1);
		let inputs = vec![input0, input1];

		if let EbiCommand::Command{execute, output_type, ..} = crate::ebi_commands::ebi_command_conformance::EBI_CONFORMANCE_JSSC {
			match (execute)(inputs, None) {
				Ok(output) => assert_eq!(&output.get_type(), output_type),
				Err(_) => assert!(false),
			}
		}
	}


	// ==== command jssc-sample ====
	#[test]
	pub fn ebi_conf_jssc_sample_test_0() {
		if ebi_objects::ebi_arithmetic::is_exact_globally() {
			return;
		}
		// trait finite stochastic language#./testfiles/ba-aa-ab.slang
		let mut reader = MultipleReader::from_file(File::open("./testfiles/ba-aa-ab.slang").unwrap());
		let (object0, file_handler0) = ebi_input::read_as_trait(&("finite stochastic language".parse().unwrap()), &mut reader, None, 0).unwrap();
		let input0 = EbiInput::Trait(object0, file_handler0);
		// trait finite stochastic language#./testfiles/ba-aa-ab.slang
		let mut reader = MultipleReader::from_file(File::open("./testfiles/ba-aa-ab.slang").unwrap());
		let (object1, file_handler1) = ebi_input::read_as_trait(&("finite stochastic language".parse().unwrap()), &mut reader, None, 0).unwrap();
		let input1 = EbiInput::Trait(object1, file_handler1);
		// usize 1
		let input2 = EbiInput::Usize(1, &TEST_INPUT_TYPE_USIZE);
		let inputs = vec![input0, input1, input2];

		if let EbiCommand::Command{execute, output_type, ..} = crate::ebi_commands::ebi_command_conformance::EBI_CONFORMANCE_JSSC_SAMPLE {
			match (execute)(inputs, None) {
				Ok(output) => assert_eq!(&output.get_type(), output_type),
				Err(_) => assert!(false),
			}
		}
	}


	// ==== command ma ====
	#[test]
	pub fn ebi_conf_ma_test_0() {
		
		// trait finite stochastic language#./testfiles/ba-aa-ab.slang
		let mut reader = MultipleReader::from_file(File::open("./testfiles/ba-aa-ab.slang").unwrap());
		let (object0, file_handler0) = ebi_input::read_as_trait(&("finite stochastic language".parse().unwrap()), &mut reader, None, 0).unwrap();
		let input0 = EbiInput::Trait(object0, file_handler0);
		// trait finite stochastic language#./testfiles/ba-aa-ab.slang
		let mut reader = MultipleReader::from_file(File::open("./testfiles/ba-aa-ab.slang").unwrap());
		let (object1, file_handler1) = ebi_input::read_as_trait(&("finite stochastic language".parse().unwrap()), &mut reader, None, 0).unwrap();
		let input1 = EbiInput::Trait(object1, file_handler1);
		// usize 1
		let input2 = EbiInput::Usize(1, &TEST_INPUT_TYPE_USIZE);
		// string cssc
		let input3 = EbiInput::String("cssc".to_string(), &TEST_INPUT_TYPE_STRING);
		let inputs = vec![input0, input1, input2, input3];

		if let EbiCommand::Command{execute, output_type, ..} = crate::ebi_commands::ebi_command_conformance::EBI_CONFORMANCE_MARKOVIAN {
			match (execute)(inputs, None) {
				Ok(output) => assert_eq!(&output.get_type(), output_type),
				Err(_) => assert!(false),
			}
		}
	}


	// ==== command uemsc ====
	#[test]
	pub fn ebi_conf_uemsc_test_0() {
		
		// trait finite stochastic language#./testfiles/ba-aa-ab.slang
		let mut reader = MultipleReader::from_file(File::open("./testfiles/ba-aa-ab.slang").unwrap());
		let (object0, file_handler0) = ebi_input::read_as_trait(&("finite stochastic language".parse().unwrap()), &mut reader, None, 0).unwrap();
		let input0 = EbiInput::Trait(object0, file_handler0);
		// trait queriable stochastic language#./testfiles/seq(a-xor(b-c)).sptree
		let mut reader = MultipleReader::from_file(File::open("./testfiles/seq(a-xor(b-c)).sptree").unwrap());
		let (object1, file_handler1) = ebi_input::read_as_trait(&("queriable stochastic language".parse().unwrap()), &mut reader, None, 0).unwrap();
		let input1 = EbiInput::Trait(object1, file_handler1);
		let inputs = vec![input0, input1];

		if let EbiCommand::Command{execute, output_type, ..} = crate::ebi_commands::ebi_command_conformance::EBI_CONFORMANCE_UEMSC {
			match (execute)(inputs, None) {
				Ok(output) => assert_eq!(&output.get_type(), output_type),
				Err(_) => assert!(false),
			}
		}
	}


	// ==== command uemsc-sample ====
	#[test]
	pub fn ebi_conf_uemsc_sample_test_0() {
		if ebi_objects::ebi_arithmetic::is_exact_globally() {
			return;
		}
		// trait finite stochastic language#./testfiles/ba-aa-ab.slang
		let mut reader = MultipleReader::from_file(File::open("./testfiles/ba-aa-ab.slang").unwrap());
		let (object0, file_handler0) = ebi_input::read_as_trait(&("finite stochastic language".parse().unwrap()), &mut reader, None, 0).unwrap();
		let input0 = EbiInput::Trait(object0, file_handler0);
		// trait finite stochastic language#./testfiles/ba-aa-ab.slang
		let mut reader = MultipleReader::from_file(File::open("./testfiles/ba-aa-ab.slang").unwrap());
		let (object1, file_handler1) = ebi_input::read_as_trait(&("finite stochastic language".parse().unwrap()), &mut reader, None, 0).unwrap();
		let input1 = EbiInput::Trait(object1, file_handler1);
		// usize 1
		let input2 = EbiInput::Usize(1, &TEST_INPUT_TYPE_USIZE);
		let inputs = vec![input0, input1, input2];

		if let EbiCommand::Command{execute, output_type, ..} = crate::ebi_commands::ebi_command_conformance::EBI_CONFORMANCE_UEMSC_SAMPLE {
			match (execute)(inputs, None) {
				Ok(output) => assert_eq!(&output.get_type(), output_type),
				Err(_) => assert!(false),
			}
		}
	}


	// ==== group confns ====


	// ==== command ali ====
	#[test]
	pub fn ebi_confns_ali_test_0() {
		
		// trait finite stochastic language#./testfiles/ba-aa-ab.slang
		let mut reader = MultipleReader::from_file(File::open("./testfiles/ba-aa-ab.slang").unwrap());
		let (object0, file_handler0) = ebi_input::read_as_trait(&("finite stochastic language".parse().unwrap()), &mut reader, None, 0).unwrap();
		let input0 = EbiInput::Trait(object0, file_handler0);
		// trait semantics#./testfiles/seq(a-xor(b-c)).sptree
		let mut reader = MultipleReader::from_file(File::open("./testfiles/seq(a-xor(b-c)).sptree").unwrap());
		let (object1, file_handler1) = ebi_input::read_as_trait(&("semantics".parse().unwrap()), &mut reader, None, 0).unwrap();
		let input1 = EbiInput::Trait(object1, file_handler1);
		let inputs = vec![input0, input1];

		if let EbiCommand::Command{execute, output_type, ..} = crate::ebi_commands::ebi_command_conformance_non_stochastic::EBI_CONFORMANCE_NON_STOCHASTIC_ALIGNMENTS {
			match (execute)(inputs, None) {
				Ok(output) => assert_eq!(&output.get_type(), output_type),
				Err(_) => assert!(false),
			}
		}
	}


	// ==== command eep ====
	#[test]
	pub fn ebi_confns_eep_test_0() {
		
		// object stochastic language of alignments#./testfiles/aa-ab-ba.sali
		let mut reader = MultipleReader::from_file(File::open("./testfiles/aa-ab-ba.sali").unwrap());
		let object0 = read_as_object_with_file_handler(&"stochastic language of alignments".parse().unwrap(), &mut reader, None, 0, &mut None, "stochastic language of alignments".parse().unwrap()).unwrap();
		let input0 = EbiInput::Object(object0, "stochastic language of alignments".parse().unwrap());
		// trait semantics#./testfiles/seq(a-xor(b-c)).sptree
		let mut reader = MultipleReader::from_file(File::open("./testfiles/seq(a-xor(b-c)).sptree").unwrap());
		let (object1, file_handler1) = ebi_input::read_as_trait(&("semantics".parse().unwrap()), &mut reader, None, 0).unwrap();
		let input1 = EbiInput::Trait(object1, file_handler1);
		let inputs = vec![input0, input1];

		if let EbiCommand::Command{execute, ..} = crate::ebi_commands::ebi_command_conformance_non_stochastic::EBI_CONFORMANCE_NON_STOCHASTIC_ESCAPING_EDGES_PRECISION {
			assert!(((execute)(inputs, None)).is_err())
		}
	}


	// ==== command setali ====
	#[test]
	pub fn ebi_confns_setali_test_0() {
		
		// trait finite language#./testfiles/ba-aa-ab.slang
		let mut reader = MultipleReader::from_file(File::open("./testfiles/ba-aa-ab.slang").unwrap());
		let (object0, file_handler0) = ebi_input::read_as_trait(&("finite language".parse().unwrap()), &mut reader, None, 0).unwrap();
		let input0 = EbiInput::Trait(object0, file_handler0);
		// trait semantics#./testfiles/seq(a-xor(b-c)).sptree
		let mut reader = MultipleReader::from_file(File::open("./testfiles/seq(a-xor(b-c)).sptree").unwrap());
		let (object1, file_handler1) = ebi_input::read_as_trait(&("semantics".parse().unwrap()), &mut reader, None, 0).unwrap();
		let input1 = EbiInput::Trait(object1, file_handler1);
		let inputs = vec![input0, input1];

		if let EbiCommand::Command{execute, output_type, ..} = crate::ebi_commands::ebi_command_conformance_non_stochastic::EBI_CONFORMANCE_NON_STOCHASTIC_SET_ALIGNMENTS {
			match (execute)(inputs, None) {
				Ok(output) => assert_eq!(&output.get_type(), output_type),
				Err(_) => assert!(false),
			}
		}
	}


	// ==== command tfit ====
	#[test]
	pub fn ebi_confns_tfit_test_0() {
		
		// object stochastic language of alignments#./testfiles/aa-ab-ba.sali
		let mut reader = MultipleReader::from_file(File::open("./testfiles/aa-ab-ba.sali").unwrap());
		let object0 = read_as_object_with_file_handler(&"stochastic language of alignments".parse().unwrap(), &mut reader, None, 0, &mut None, "stochastic language of alignments".parse().unwrap()).unwrap();
		let input0 = EbiInput::Object(object0, "stochastic language of alignments".parse().unwrap());
		let inputs = vec![input0];

		if let EbiCommand::Command{execute, output_type, ..} = crate::ebi_commands::ebi_command_conformance_non_stochastic::EBI_CONFORMANCE_NON_STOCHASTIC_TRACE_FITNESS {
			match (execute)(inputs, None) {
				Ok(output) => assert_eq!(&output.get_type(), output_type),
				Err(_) => assert!(false),
			}
		}
	}


	// ==== group conv ====


	// ==== command bpmn ====
	#[test]
	pub fn ebi_conv_bpmn_test_0() {
		
		// object business process model and notation#./testfiles/seq(a-xor(b-c)).sptree
		let mut reader = MultipleReader::from_file(File::open("./testfiles/seq(a-xor(b-c)).sptree").unwrap());
		let object0 = read_as_object_with_file_handler(&"business process model and notation".parse().unwrap(), &mut reader, None, 0, &mut None, "stochastic process tree".parse().unwrap()).unwrap();
		let input0 = EbiInput::Object(object0, "stochastic process tree".parse().unwrap());
		let inputs = vec![input0];

		if let EbiCommand::Command{execute, output_type, ..} = crate::ebi_commands::ebi_command_convert::EBI_CONVERT_BPMN {
			match (execute)(inputs, None) {
				Ok(output) => assert_eq!(&output.get_type(), output_type),
				Err(_) => assert!(false),
			}
		}
	}


	// ==== command lang ====
	#[test]
	pub fn ebi_conv_lang_test_0() {
		
		// object finite language#./testfiles/empty.xes
		let mut reader = MultipleReader::from_file(File::open("./testfiles/empty.xes").unwrap());
		let object0 = read_as_object_with_file_handler(&"finite language".parse().unwrap(), &mut reader, None, 0, &mut None, "extensible event stream".parse().unwrap()).unwrap();
		let input0 = EbiInput::Object(object0, "extensible event stream".parse().unwrap());
		let inputs = vec![input0];

		if let EbiCommand::Command{execute, output_type, ..} = crate::ebi_commands::ebi_command_convert::EBI_CONVERT_LANG {
			match (execute)(inputs, None) {
				Ok(output) => assert_eq!(&output.get_type(), output_type),
				Err(_) => assert!(false),
			}
		}
	}


	// ==== command log ====
	#[test]
	pub fn ebi_conv_log_test_0() {
		
		// object event log#./testfiles/empty.xes
		let mut reader = MultipleReader::from_file(File::open("./testfiles/empty.xes").unwrap());
		let object0 = read_as_object_with_file_handler(&"event log".parse().unwrap(), &mut reader, None, 0, &mut None, "extensible event stream".parse().unwrap()).unwrap();
		let input0 = EbiInput::Object(object0, "extensible event stream".parse().unwrap());
		let inputs = vec![input0];

		if let EbiCommand::Command{execute, output_type, ..} = crate::ebi_commands::ebi_command_convert::EBI_CONVERT_LOG {
			match (execute)(inputs, None) {
				Ok(output) => assert_eq!(&output.get_type(), output_type),
				Err(_) => assert!(false),
			}
		}
	}


	// ==== command lpn ====
	#[test]
	pub fn ebi_conv_lpn_test_0() {
		
		// object labelled Petri net#./testfiles/seq(a-xor(b-c)).sptree
		let mut reader = MultipleReader::from_file(File::open("./testfiles/seq(a-xor(b-c)).sptree").unwrap());
		let object0 = read_as_object_with_file_handler(&"labelled Petri net".parse().unwrap(), &mut reader, None, 0, &mut None, "stochastic process tree".parse().unwrap()).unwrap();
		let input0 = EbiInput::Object(object0, "stochastic process tree".parse().unwrap());
		let inputs = vec![input0];

		if let EbiCommand::Command{execute, output_type, ..} = crate::ebi_commands::ebi_command_convert::EBI_CONVERT_LPN {
			match (execute)(inputs, None) {
				Ok(output) => assert_eq!(&output.get_type(), output_type),
				Err(_) => assert!(false),
			}
		}
	}


	// ==== command slang ====
	#[test]
	pub fn ebi_conv_slang_test_0() {
		
		// object finite stochastic language#./testfiles/ba-aa-ab.slang
		let mut reader = MultipleReader::from_file(File::open("./testfiles/ba-aa-ab.slang").unwrap());
		let object0 = read_as_object_with_file_handler(&"finite stochastic language".parse().unwrap(), &mut reader, None, 0, &mut None, "finite stochastic language".parse().unwrap()).unwrap();
		let input0 = EbiInput::Object(object0, "finite stochastic language".parse().unwrap());
		let inputs = vec![input0];

		if let EbiCommand::Command{execute, output_type, ..} = crate::ebi_commands::ebi_command_convert::EBI_CONVERT_SLANG {
			match (execute)(inputs, None) {
				Ok(output) => assert_eq!(&output.get_type(), output_type),
				Err(_) => assert!(false),
			}
		}
	}


	// ==== command sdfa ====
	#[test]
	pub fn ebi_conv_sdfa_test_0() {
		
		// object stochastic deterministic finite automaton#./testfiles/ba-aa-ab.slang
		let mut reader = MultipleReader::from_file(File::open("./testfiles/ba-aa-ab.slang").unwrap());
		let object0 = read_as_object_with_file_handler(&"stochastic deterministic finite automaton".parse().unwrap(), &mut reader, None, 0, &mut None, "finite stochastic language".parse().unwrap()).unwrap();
		let input0 = EbiInput::Object(object0, "finite stochastic language".parse().unwrap());
		let inputs = vec![input0];

		if let EbiCommand::Command{execute, output_type, ..} = crate::ebi_commands::ebi_command_convert::EBI_CONVERT_SDFA {
			match (execute)(inputs, None) {
				Ok(output) => assert_eq!(&output.get_type(), output_type),
				Err(_) => assert!(false),
			}
		}
	}


	// ==== command slpn ====
	#[test]
	pub fn ebi_conv_slpn_test_0() {
		
		// object stochastic labelled Petri net#./testfiles/bpic12-a.xes.gz-dfg.dfg
		let mut reader = MultipleReader::from_file(File::open("./testfiles/bpic12-a.xes.gz-dfg.dfg").unwrap());
		let object0 = read_as_object_with_file_handler(&"stochastic labelled Petri net".parse().unwrap(), &mut reader, None, 0, &mut None, "directly follows graph".parse().unwrap()).unwrap();
		let input0 = EbiInput::Object(object0, "directly follows graph".parse().unwrap());
		let inputs = vec![input0];

		if let EbiCommand::Command{execute, output_type, ..} = crate::ebi_commands::ebi_command_convert::EBI_CONVERT_SLPN {
			match (execute)(inputs, None) {
				Ok(output) => assert_eq!(&output.get_type(), output_type),
				Err(_) => assert!(false),
			}
		}
	}


	// ==== command snfa ====
	#[test]
	pub fn ebi_conv_snfa_test_0() {
		
		// object stochastic non-deterministic finite automaton#./testfiles/seq(a-xor(b-c)).sptree
		let mut reader = MultipleReader::from_file(File::open("./testfiles/seq(a-xor(b-c)).sptree").unwrap());
		let object0 = read_as_object_with_file_handler(&"stochastic non-deterministic finite automaton".parse().unwrap(), &mut reader, None, 0, &mut None, "stochastic process tree".parse().unwrap()).unwrap();
		let input0 = EbiInput::Object(object0, "stochastic process tree".parse().unwrap());
		let inputs = vec![input0];

		if let EbiCommand::Command{execute, output_type, ..} = crate::ebi_commands::ebi_command_convert::EBI_CONVERT_SNFA {
			match (execute)(inputs, None) {
				Ok(output) => assert_eq!(&output.get_type(), output_type),
				Err(_) => assert!(false),
			}
		}
	}


	// ==== group disc ====


	// ==== group ali ====


	// ==== command sbpmn ====
	#[test]
	pub fn ebi_disc_ali_sbpmn_test_0() {
		
		// trait finite stochastic language#./testfiles/ba-aa-ab.slang
		let mut reader = MultipleReader::from_file(File::open("./testfiles/ba-aa-ab.slang").unwrap());
		let (object0, file_handler0) = ebi_input::read_as_trait(&("finite stochastic language".parse().unwrap()), &mut reader, None, 0).unwrap();
		let input0 = EbiInput::Trait(object0, file_handler0);
		// object business process model and notation#./testfiles/seq(a-xor(b-c)).sptree
		let mut reader = MultipleReader::from_file(File::open("./testfiles/seq(a-xor(b-c)).sptree").unwrap());
		let object1 = read_as_object_with_file_handler(&"business process model and notation".parse().unwrap(), &mut reader, None, 0, &mut None, "stochastic process tree".parse().unwrap()).unwrap();
		let input1 = EbiInput::Object(object1, "stochastic process tree".parse().unwrap());
		let inputs = vec![input0, input1];

		if let EbiCommand::Command{execute, output_type, ..} = crate::ebi_commands::ebi_command_discover::EBI_DISCOVER_ALIGNMENTS_BPMN {
			match (execute)(inputs, None) {
				Ok(output) => assert_eq!(&output.get_type(), output_type),
				Err(_) => assert!(false),
			}
		}
	}


	// ==== command slpn ====
	#[test]
	pub fn ebi_disc_ali_slpn_test_0() {
		
		// trait finite stochastic language#./testfiles/ba-aa-ab.slang
		let mut reader = MultipleReader::from_file(File::open("./testfiles/ba-aa-ab.slang").unwrap());
		let (object0, file_handler0) = ebi_input::read_as_trait(&("finite stochastic language".parse().unwrap()), &mut reader, None, 0).unwrap();
		let input0 = EbiInput::Trait(object0, file_handler0);
		// object labelled Petri net#./testfiles/seq(a-xor(b-c)).sptree
		let mut reader = MultipleReader::from_file(File::open("./testfiles/seq(a-xor(b-c)).sptree").unwrap());
		let object1 = read_as_object_with_file_handler(&"labelled Petri net".parse().unwrap(), &mut reader, None, 0, &mut None, "stochastic process tree".parse().unwrap()).unwrap();
		let input1 = EbiInput::Object(object1, "stochastic process tree".parse().unwrap());
		let inputs = vec![input0, input1];

		if let EbiCommand::Command{execute, output_type, ..} = crate::ebi_commands::ebi_command_discover::EBI_DISCOVER_ALIGNMENTS_SLPN {
			match (execute)(inputs, None) {
				Ok(output) => assert_eq!(&output.get_type(), output_type),
				Err(_) => assert!(false),
			}
		}
	}


	// ==== command dfg ====
	#[test]
	pub fn ebi_disc_dfg_test_0() {
		
		// trait event log#./testfiles/empty.xes
		let mut reader = MultipleReader::from_file(File::open("./testfiles/empty.xes").unwrap());
		let (object0, file_handler0) = ebi_input::read_as_trait(&("event log".parse().unwrap()), &mut reader, None, 0).unwrap();
		let input0 = EbiInput::Trait(object0, file_handler0);
		// fraction 1
		let input1 = EbiInput::Fraction("1".parse().unwrap(), &TEST_INPUT_TYPE_FRACTION);
		let inputs = vec![input0, input1];

		if let EbiCommand::Command{execute, output_type, ..} = crate::ebi_commands::ebi_command_discover::EBI_DISCOVER_DIRECTLY_FOLLOWS {
			match (execute)(inputs, None) {
				Ok(output) => assert_eq!(&output.get_type(), output_type),
				Err(_) => assert!(false),
			}
		}
	}


	// ==== group occ ====


	// ==== command sbpmn ====
	#[test]
	pub fn ebi_disc_occ_sbpmn_test_0() {
		
		// trait finite stochastic language#./testfiles/ba-aa-ab.slang
		let mut reader = MultipleReader::from_file(File::open("./testfiles/ba-aa-ab.slang").unwrap());
		let (object0, file_handler0) = ebi_input::read_as_trait(&("finite stochastic language".parse().unwrap()), &mut reader, None, 0).unwrap();
		let input0 = EbiInput::Trait(object0, file_handler0);
		// object business process model and notation#./testfiles/seq(a-xor(b-c)).sptree
		let mut reader = MultipleReader::from_file(File::open("./testfiles/seq(a-xor(b-c)).sptree").unwrap());
		let object1 = read_as_object_with_file_handler(&"business process model and notation".parse().unwrap(), &mut reader, None, 0, &mut None, "stochastic process tree".parse().unwrap()).unwrap();
		let input1 = EbiInput::Object(object1, "stochastic process tree".parse().unwrap());
		let inputs = vec![input0, input1];

		if let EbiCommand::Command{execute, output_type, ..} = crate::ebi_commands::ebi_command_discover::EBI_DISCOVER_OCCURRENCE_SBPMN {
			match (execute)(inputs, None) {
				Ok(output) => assert_eq!(&output.get_type(), output_type),
				Err(_) => assert!(false),
			}
		}
	}


	// ==== command slpn ====
	#[test]
	pub fn ebi_disc_occ_slpn_test_0() {
		
		// trait finite stochastic language#./testfiles/ba-aa-ab.slang
		let mut reader = MultipleReader::from_file(File::open("./testfiles/ba-aa-ab.slang").unwrap());
		let (object0, file_handler0) = ebi_input::read_as_trait(&("finite stochastic language".parse().unwrap()), &mut reader, None, 0).unwrap();
		let input0 = EbiInput::Trait(object0, file_handler0);
		// object labelled Petri net#./testfiles/seq(a-xor(b-c)).sptree
		let mut reader = MultipleReader::from_file(File::open("./testfiles/seq(a-xor(b-c)).sptree").unwrap());
		let object1 = read_as_object_with_file_handler(&"labelled Petri net".parse().unwrap(), &mut reader, None, 0, &mut None, "stochastic process tree".parse().unwrap()).unwrap();
		let input1 = EbiInput::Object(object1, "stochastic process tree".parse().unwrap());
		let inputs = vec![input0, input1];

		if let EbiCommand::Command{execute, output_type, ..} = crate::ebi_commands::ebi_command_discover::EBI_DISCOVER_OCCURRENCE_SLPN {
			match (execute)(inputs, None) {
				Ok(output) => assert_eq!(&output.get_type(), output_type),
				Err(_) => assert!(false),
			}
		}
	}


	// ==== command sptree ====
	#[test]
	pub fn ebi_disc_occ_sptree_test_0() {
		
		// trait finite stochastic language#./testfiles/ba-aa-ab.slang
		let mut reader = MultipleReader::from_file(File::open("./testfiles/ba-aa-ab.slang").unwrap());
		let (object0, file_handler0) = ebi_input::read_as_trait(&("finite stochastic language".parse().unwrap()), &mut reader, None, 0).unwrap();
		let input0 = EbiInput::Trait(object0, file_handler0);
		// object process tree#./testfiles/seq(a-xor(b-c)).sptree
		let mut reader = MultipleReader::from_file(File::open("./testfiles/seq(a-xor(b-c)).sptree").unwrap());
		let object1 = read_as_object_with_file_handler(&"process tree".parse().unwrap(), &mut reader, None, 0, &mut None, "stochastic process tree".parse().unwrap()).unwrap();
		let input1 = EbiInput::Object(object1, "stochastic process tree".parse().unwrap());
		let inputs = vec![input0, input1];

		if let EbiCommand::Command{execute, output_type, ..} = crate::ebi_commands::ebi_command_discover::EBI_DISCOVER_OCCURRENCE_SPTREE {
			match (execute)(inputs, None) {
				Ok(output) => assert_eq!(&output.get_type(), output_type),
				Err(_) => assert!(false),
			}
		}
	}


	// ==== group rnd ====


	// ==== command sbpmn ====
	#[test]
	pub fn ebi_disc_rnd_sbpmn_test_0() {
		
		// object business process model and notation#./testfiles/seq(a-xor(b-c)).sptree
		let mut reader = MultipleReader::from_file(File::open("./testfiles/seq(a-xor(b-c)).sptree").unwrap());
		let object0 = read_as_object_with_file_handler(&"business process model and notation".parse().unwrap(), &mut reader, None, 0, &mut None, "stochastic process tree".parse().unwrap()).unwrap();
		let input0 = EbiInput::Object(object0, "stochastic process tree".parse().unwrap());
		let inputs = vec![input0];

		if let EbiCommand::Command{execute, output_type, ..} = crate::ebi_commands::ebi_command_discover::EBI_DISCOVER_RANDOM_SBPMN {
			match (execute)(inputs, None) {
				Ok(output) => assert_eq!(&output.get_type(), output_type),
				Err(_) => assert!(false),
			}
		}
	}


	// ==== command slpn ====
	#[test]
	pub fn ebi_disc_rnd_slpn_test_0() {
		
		// object labelled Petri net#./testfiles/seq(a-xor(b-c)).sptree
		let mut reader = MultipleReader::from_file(File::open("./testfiles/seq(a-xor(b-c)).sptree").unwrap());
		let object0 = read_as_object_with_file_handler(&"labelled Petri net".parse().unwrap(), &mut reader, None, 0, &mut None, "stochastic process tree".parse().unwrap()).unwrap();
		let input0 = EbiInput::Object(object0, "stochastic process tree".parse().unwrap());
		let inputs = vec![input0];

		if let EbiCommand::Command{execute, output_type, ..} = crate::ebi_commands::ebi_command_discover::EBI_DISCOVER_RANDOM_SLPN {
			match (execute)(inputs, None) {
				Ok(output) => assert_eq!(&output.get_type(), output_type),
				Err(_) => assert!(false),
			}
		}
	}


	// ==== command sptree ====
	#[test]
	pub fn ebi_disc_rnd_sptree_test_0() {
		
		// object process tree#./testfiles/seq(a-xor(b-c)).sptree
		let mut reader = MultipleReader::from_file(File::open("./testfiles/seq(a-xor(b-c)).sptree").unwrap());
		let object0 = read_as_object_with_file_handler(&"process tree".parse().unwrap(), &mut reader, None, 0, &mut None, "stochastic process tree".parse().unwrap()).unwrap();
		let input0 = EbiInput::Object(object0, "stochastic process tree".parse().unwrap());
		let inputs = vec![input0];

		if let EbiCommand::Command{execute, output_type, ..} = crate::ebi_commands::ebi_command_discover::EBI_DISCOVER_RANDOM_SPTREE {
			match (execute)(inputs, None) {
				Ok(output) => assert_eq!(&output.get_type(), output_type),
				Err(_) => assert!(false),
			}
		}
	}


	// ==== group uni ====


	// ==== command sbpmn ====
	#[test]
	pub fn ebi_disc_uni_sbpmn_test_0() {
		
		// object business process model and notation#./testfiles/seq(a-xor(b-c)).sptree
		let mut reader = MultipleReader::from_file(File::open("./testfiles/seq(a-xor(b-c)).sptree").unwrap());
		let object0 = read_as_object_with_file_handler(&"business process model and notation".parse().unwrap(), &mut reader, None, 0, &mut None, "stochastic process tree".parse().unwrap()).unwrap();
		let input0 = EbiInput::Object(object0, "stochastic process tree".parse().unwrap());
		let inputs = vec![input0];

		if let EbiCommand::Command{execute, output_type, ..} = crate::ebi_commands::ebi_command_discover::EBI_DISCOVER_UNIFORM_SBPMN {
			match (execute)(inputs, None) {
				Ok(output) => assert_eq!(&output.get_type(), output_type),
				Err(_) => assert!(false),
			}
		}
	}


	// ==== command slpn ====
	#[test]
	pub fn ebi_disc_uni_slpn_test_0() {
		
		// object labelled Petri net#./testfiles/seq(a-xor(b-c)).sptree
		let mut reader = MultipleReader::from_file(File::open("./testfiles/seq(a-xor(b-c)).sptree").unwrap());
		let object0 = read_as_object_with_file_handler(&"labelled Petri net".parse().unwrap(), &mut reader, None, 0, &mut None, "stochastic process tree".parse().unwrap()).unwrap();
		let input0 = EbiInput::Object(object0, "stochastic process tree".parse().unwrap());
		let inputs = vec![input0];

		if let EbiCommand::Command{execute, output_type, ..} = crate::ebi_commands::ebi_command_discover::EBI_DISCOVER_UNIFORM_SLPN {
			match (execute)(inputs, None) {
				Ok(output) => assert_eq!(&output.get_type(), output_type),
				Err(_) => assert!(false),
			}
		}
	}


	// ==== command sptree ====
	#[test]
	pub fn ebi_disc_uni_sptree_test_0() {
		
		// object process tree#./testfiles/seq(a-xor(b-c)).sptree
		let mut reader = MultipleReader::from_file(File::open("./testfiles/seq(a-xor(b-c)).sptree").unwrap());
		let object0 = read_as_object_with_file_handler(&"process tree".parse().unwrap(), &mut reader, None, 0, &mut None, "stochastic process tree".parse().unwrap()).unwrap();
		let input0 = EbiInput::Object(object0, "stochastic process tree".parse().unwrap());
		let inputs = vec![input0];

		if let EbiCommand::Command{execute, output_type, ..} = crate::ebi_commands::ebi_command_discover::EBI_DISCOVER_UNIFORM_SPTREE {
			match (execute)(inputs, None) {
				Ok(output) => assert_eq!(&output.get_type(), output_type),
				Err(_) => assert!(false),
			}
		}
	}


	// ==== group dins ====


	// ==== group flw ====


	// ==== command dfa ====
	#[test]
	pub fn ebi_dins_flw_dfa_test_0() {
		
		// trait finite language#./testfiles/ba-aa-ab.slang
		let mut reader = MultipleReader::from_file(File::open("./testfiles/ba-aa-ab.slang").unwrap());
		let (object0, file_handler0) = ebi_input::read_as_trait(&("finite language".parse().unwrap()), &mut reader, None, 0).unwrap();
		let input0 = EbiInput::Trait(object0, file_handler0);
		let inputs = vec![input0];

		if let EbiCommand::Command{execute, output_type, ..} = crate::ebi_commands::ebi_command_discover_non_stochastic::EBI_DISCOVER_NON_STOCHASTIC_FLOWER_DFA {
			match (execute)(inputs, None) {
				Ok(output) => assert_eq!(&output.get_type(), output_type),
				Err(_) => assert!(false),
			}
		}
	}


	// ==== command ptree ====
	#[test]
	pub fn ebi_dins_flw_ptree_test_0() {
		
		// trait finite language#./testfiles/ba-aa-ab.slang
		let mut reader = MultipleReader::from_file(File::open("./testfiles/ba-aa-ab.slang").unwrap());
		let (object0, file_handler0) = ebi_input::read_as_trait(&("finite language".parse().unwrap()), &mut reader, None, 0).unwrap();
		let input0 = EbiInput::Trait(object0, file_handler0);
		let inputs = vec![input0];

		if let EbiCommand::Command{execute, output_type, ..} = crate::ebi_commands::ebi_command_discover_non_stochastic::EBI_DISCOVER_NON_STOCHASTIC_FLOWER_TREE {
			match (execute)(inputs, None) {
				Ok(output) => assert_eq!(&output.get_type(), output_type),
				Err(_) => assert!(false),
			}
		}
	}


	// ==== group pfxt ====


	// ==== command dfa ====
	#[test]
	pub fn ebi_dins_pfxt_dfa_test_0() {
		
		// trait finite language#./testfiles/ba-aa-ab.slang
		let mut reader = MultipleReader::from_file(File::open("./testfiles/ba-aa-ab.slang").unwrap());
		let (object0, file_handler0) = ebi_input::read_as_trait(&("finite language".parse().unwrap()), &mut reader, None, 0).unwrap();
		let input0 = EbiInput::Trait(object0, file_handler0);
		let inputs = vec![input0];

		if let EbiCommand::Command{execute, output_type, ..} = crate::ebi_commands::ebi_command_discover_non_stochastic::EBI_DISCOVER_NON_STOCHASTIC_TREE_DFA {
			match (execute)(inputs, None) {
				Ok(output) => assert_eq!(&output.get_type(), output_type),
				Err(_) => assert!(false),
			}
		}
	}


	// ==== command ptree ====
	#[test]
	pub fn ebi_dins_pfxt_ptree_test_0() {
		
		// trait finite language#./testfiles/ba-aa-ab.slang
		let mut reader = MultipleReader::from_file(File::open("./testfiles/ba-aa-ab.slang").unwrap());
		let (object0, file_handler0) = ebi_input::read_as_trait(&("finite language".parse().unwrap()), &mut reader, None, 0).unwrap();
		let input0 = EbiInput::Trait(object0, file_handler0);
		let inputs = vec![input0];

		if let EbiCommand::Command{execute, output_type, ..} = crate::ebi_commands::ebi_command_discover_non_stochastic::EBI_DISCOVER_NON_STOCHASTIC_TREE_TREE {
			match (execute)(inputs, None) {
				Ok(output) => assert_eq!(&output.get_type(), output_type),
				Err(_) => assert!(false),
			}
		}
	}


	// ==== command tm ====
	#[test]
	pub fn ebi_dins_tm_test_0() {
		
		// trait finite language#./testfiles/ba-aa-ab.slang
		let mut reader = MultipleReader::from_file(File::open("./testfiles/ba-aa-ab.slang").unwrap());
		let (object0, file_handler0) = ebi_input::read_as_trait(&("finite language".parse().unwrap()), &mut reader, None, 0).unwrap();
		let input0 = EbiInput::Trait(object0, file_handler0);
		let inputs = vec![input0];

		if let EbiCommand::Command{execute, output_type, ..} = crate::ebi_commands::ebi_command_discover_non_stochastic::EBI_DISCOVER_NON_STOCHASTIC_TRACE_MODEL {
			match (execute)(inputs, None) {
				Ok(output) => assert_eq!(&output.get_type(), output_type),
				Err(_) => assert!(false),
			}
		}
	}


	// ==== group fil ====


	// ==== group tr ====


	// ==== command empty ====
	#[test]
	pub fn ebi_fil_tr_empty_test_0() {
		
		// object XES event log#./testfiles/empty.xes
		let mut reader = MultipleReader::from_file(File::open("./testfiles/empty.xes").unwrap());
		let object0 = read_as_object_with_file_handler(&"XES event log".parse().unwrap(), &mut reader, None, 0, &mut None, "extensible event stream".parse().unwrap()).unwrap();
		let input0 = EbiInput::Object(object0, "extensible event stream".parse().unwrap());
		let inputs = vec![input0];

		if let EbiCommand::Command{execute, output_type, ..} = crate::ebi_commands::ebi_command_filter::EBI_FILTER_TRACES_EMPTY {
			match (execute)(inputs, None) {
				Ok(output) => assert_eq!(&output.get_type(), output_type),
				Err(_) => assert!(false),
			}
		}
	}


	// ==== command len ====
	#[test]
	pub fn ebi_fil_tr_len_test_0() {
		
		// object XES event log#./testfiles/empty.xes
		let mut reader = MultipleReader::from_file(File::open("./testfiles/empty.xes").unwrap());
		let object0 = read_as_object_with_file_handler(&"XES event log".parse().unwrap(), &mut reader, None, 0, &mut None, "extensible event stream".parse().unwrap()).unwrap();
		let input0 = EbiInput::Object(object0, "extensible event stream".parse().unwrap());
		// string <
		let input1 = EbiInput::String("<".to_string(), &TEST_INPUT_TYPE_STRING);
		// usize 0
		let input2 = EbiInput::Usize(0, &TEST_INPUT_TYPE_USIZE);
		let inputs = vec![input0, input1, input2];

		if let EbiCommand::Command{execute, output_type, ..} = crate::ebi_commands::ebi_command_filter::EBI_FILTER_TRACES_LENGTH {
			match (execute)(inputs, None) {
				Ok(output) => assert_eq!(&output.get_type(), output_type),
				Err(_) => assert!(false),
			}
		}
	}


	// ==== group event ====


	// ==== command act ====
	#[test]
	pub fn ebi_fil_tr_event_act_test_0() {
		
		// object XES event log#./testfiles/empty.xes
		let mut reader = MultipleReader::from_file(File::open("./testfiles/empty.xes").unwrap());
		let object0 = read_as_object_with_file_handler(&"XES event log".parse().unwrap(), &mut reader, None, 0, &mut None, "extensible event stream".parse().unwrap()).unwrap();
		let input0 = EbiInput::Object(object0, "extensible event stream".parse().unwrap());
		// string any
		let input1 = EbiInput::String("any".to_string(), &TEST_INPUT_TYPE_STRING);
		// string some string
		let input2 = EbiInput::String("some string".to_string(), &TEST_INPUT_TYPE_STRING);
		let inputs = vec![input0, input1, input2];

		if let EbiCommand::Command{execute, output_type, ..} = crate::ebi_commands::ebi_command_filter::EBI_FILTER_TRACES_EVENT_ACTIVITY {
			match (execute)(inputs, None) {
				Ok(output) => assert_eq!(&output.get_type(), output_type),
				Err(_) => assert!(false),
			}
		}
	}


	// ==== group it ====


	// ==== group docs ====










	// ==== command info ====
	#[test]
	pub fn ebi_info_test_0() {
		
		// object stochastic process tree#./testfiles/seq(a-xor(b-c)).sptree
		let mut reader = MultipleReader::from_file(File::open("./testfiles/seq(a-xor(b-c)).sptree").unwrap());
		let object0 = read_as_object_with_file_handler(&"stochastic process tree".parse().unwrap(), &mut reader, None, 0, &mut None, "stochastic process tree".parse().unwrap()).unwrap();
		let input0 = EbiInput::Object(object0, "stochastic process tree".parse().unwrap());
		let inputs = vec![input0];

		if let EbiCommand::Command{execute, output_type, ..} = crate::ebi_commands::ebi_command_info::EBI_INFO {
			match (execute)(inputs, None) {
				Ok(output) => assert_eq!(&output.get_type(), output_type),
				Err(_) => assert!(false),
			}
		}
	}


	// ==== group prob ====


	// ==== command log ====
	#[test]
	pub fn ebi_prob_log_test_0() {
		
		// trait queriable stochastic language#./testfiles/seq(a-xor(b-c)).sptree
		let mut reader = MultipleReader::from_file(File::open("./testfiles/seq(a-xor(b-c)).sptree").unwrap());
		let (object0, file_handler0) = ebi_input::read_as_trait(&("queriable stochastic language".parse().unwrap()), &mut reader, None, 0).unwrap();
		let input0 = EbiInput::Trait(object0, file_handler0);
		// trait finite language#./testfiles/ba-aa-ab.slang
		let mut reader = MultipleReader::from_file(File::open("./testfiles/ba-aa-ab.slang").unwrap());
		let (object1, file_handler1) = ebi_input::read_as_trait(&("finite language".parse().unwrap()), &mut reader, None, 0).unwrap();
		let input1 = EbiInput::Trait(object1, file_handler1);
		let inputs = vec![input0, input1];

		if let EbiCommand::Command{execute, output_type, ..} = crate::ebi_commands::ebi_command_probability::EBI_PROBABILITY_LOG {
			match (execute)(inputs, None) {
				Ok(output) => assert_eq!(&output.get_type(), output_type),
				Err(_) => assert!(false),
			}
		}
	}




	// ==== group sam ====


	// ==== command folds ====
	#[test]
	pub fn ebi_sam_folds_test_0() {
		
		// object event log#./testfiles/empty.xes
		let mut reader = MultipleReader::from_file(File::open("./testfiles/empty.xes").unwrap());
		let object0 = read_as_object_with_file_handler(&"event log".parse().unwrap(), &mut reader, None, 0, &mut None, "extensible event stream".parse().unwrap()).unwrap();
		let input0 = EbiInput::Object(object0, "extensible event stream".parse().unwrap());
		// usize 1
		let input1 = EbiInput::Usize(1, &TEST_INPUT_TYPE_USIZE);
		// usize 0
		let input2 = EbiInput::Usize(0, &TEST_INPUT_TYPE_USIZE);
		// usize 0
		let input3 = EbiInput::Usize(0, &TEST_INPUT_TYPE_USIZE);
		let inputs = vec![input0, input1, input2, input3];

		if let EbiCommand::Command{execute, output_type, ..} = crate::ebi_commands::ebi_command_sample::EBI_SAMPLE_FOLDS {
			match (execute)(inputs, None) {
				Ok(output) => assert_eq!(&output.get_type(), output_type),
				Err(_) => assert!(false),
			}
		}
	}


	// ==== command potr ====
	#[test]
	pub fn ebi_sam_potr_test_0() {
		
		// object stochastic business process model and notation#./testfiles/model.sbpmn
		let mut reader = MultipleReader::from_file(File::open("./testfiles/model.sbpmn").unwrap());
		let object0 = read_as_object_with_file_handler(&"stochastic business process model and notation".parse().unwrap(), &mut reader, None, 0, &mut None, "stochastic business process model and notation".parse().unwrap()).unwrap();
		let input0 = EbiInput::Object(object0, "stochastic business process model and notation".parse().unwrap());
		// usize 1
		let input1 = EbiInput::Usize(1, &TEST_INPUT_TYPE_USIZE);
		let inputs = vec![input0, input1];

		if let EbiCommand::Command{execute, output_type, ..} = crate::ebi_commands::ebi_command_sample::EBI_SAMPLE_PARTIALLY_ORDERED_TRACES {
			match (execute)(inputs, None) {
				Ok(output) => assert_eq!(&output.get_type(), output_type),
				Err(_) => assert!(false),
			}
		}
	}


	// ==== command tra ====
	#[test]
	pub fn ebi_sam_tra_test_0() {
		
		// trait finite stochastic language#./testfiles/ba-aa-ab.slang
		let mut reader = MultipleReader::from_file(File::open("./testfiles/ba-aa-ab.slang").unwrap());
		let (object0, file_handler0) = ebi_input::read_as_trait(&("finite stochastic language".parse().unwrap()), &mut reader, None, 0).unwrap();
		let input0 = EbiInput::Trait(object0, file_handler0);
		// usize 1
		let input1 = EbiInput::Usize(1, &TEST_INPUT_TYPE_USIZE);
		let inputs = vec![input0, input1];

		if let EbiCommand::Command{execute, output_type, ..} = crate::ebi_commands::ebi_command_sample::EBI_SAMPLE_TRACES {
			match (execute)(inputs, None) {
				Ok(output) => assert_eq!(&output.get_type(), output_type),
				Err(_) => assert!(false),
			}
		}
	}


	// ==== group tst ====


	// ==== command btst ====
	#[test]
	pub fn ebi_tst_btst_test_0() {
		
		// trait finite stochastic language#./testfiles/ba-aa-ab.slang
		let mut reader = MultipleReader::from_file(File::open("./testfiles/ba-aa-ab.slang").unwrap());
		let (object0, file_handler0) = ebi_input::read_as_trait(&("finite stochastic language".parse().unwrap()), &mut reader, None, 0).unwrap();
		let input0 = EbiInput::Trait(object0, file_handler0);
		// trait finite stochastic language#./testfiles/ba-aa-ab.slang
		let mut reader = MultipleReader::from_file(File::open("./testfiles/ba-aa-ab.slang").unwrap());
		let (object1, file_handler1) = ebi_input::read_as_trait(&("finite stochastic language".parse().unwrap()), &mut reader, None, 0).unwrap();
		let input1 = EbiInput::Trait(object1, file_handler1);
		// usize 10
		let input2 = EbiInput::Usize(10, &TEST_INPUT_TYPE_USIZE);
		// fraction 0.05
		let input3 = EbiInput::Fraction("0.05".parse().unwrap(), &TEST_INPUT_TYPE_FRACTION);
		let inputs = vec![input0, input1, input2, input3];

		if let EbiCommand::Command{execute, output_type, ..} = crate::ebi_commands::ebi_command_test::EBI_TEST_BOOTSTRAP {
			match (execute)(inputs, None) {
				Ok(output) => assert_eq!(&output.get_type(), output_type),
				Err(_) => assert!(false),
			}
		}
	}


	// ==== command lcat ====
	#[test]
	pub fn ebi_tst_lcat_test_0() {
		
		// trait event log with trace attributes#./testfiles/empty.xes
		let mut reader = MultipleReader::from_file(File::open("./testfiles/empty.xes").unwrap());
		let (object0, file_handler0) = ebi_input::read_as_trait(&("event log with trace attributes".parse().unwrap()), &mut reader, None, 0).unwrap();
		let input0 = EbiInput::Trait(object0, file_handler0);
		// string some string
		let input1 = EbiInput::String("some string".to_string(), &TEST_INPUT_TYPE_STRING);
		// usize 10
		let input2 = EbiInput::Usize(10, &TEST_INPUT_TYPE_USIZE);
		// fraction 0.05
		let input3 = EbiInput::Fraction("0.05".parse().unwrap(), &TEST_INPUT_TYPE_FRACTION);
		let inputs = vec![input0, input1, input2, input3];

		if let EbiCommand::Command{execute, ..} = crate::ebi_commands::ebi_command_test::EBI_TEST_LOG_ATTRIBUTE {
			assert!(((execute)(inputs, None)).is_err())
		}
	}



	// ==== group vis ====


	// ==== command graph ====
	#[test]
	pub fn ebi_vis_graph_test_0() {
		
		// trait graphable#./testfiles/seq(a-xor(b-c)).sptree
		let mut reader = MultipleReader::from_file(File::open("./testfiles/seq(a-xor(b-c)).sptree").unwrap());
		let (object0, file_handler0) = ebi_input::read_as_trait(&("graphable".parse().unwrap()), &mut reader, None, 0).unwrap();
		let input0 = EbiInput::Trait(object0, file_handler0);
		let inputs = vec![input0];

		if let EbiCommand::Command{execute, output_type, ..} = crate::ebi_commands::ebi_command_visualise::EBI_VISUALISE_GRAPH {
			match (execute)(inputs, None) {
				Ok(output) => assert_eq!(&output.get_type(), output_type),
				Err(_) => assert!(false),
			}
		}
	}


	// ==== command txt ====
	#[test]
	pub fn ebi_vis_txt_test_0() {
		if ebi_objects::ebi_arithmetic::is_exact_globally() {
			return;
		}
		// object stochastic process tree#./testfiles/seq(a-xor(b-c)).sptree
		let mut reader = MultipleReader::from_file(File::open("./testfiles/seq(a-xor(b-c)).sptree").unwrap());
		let object0 = read_as_object_with_file_handler(&"stochastic process tree".parse().unwrap(), &mut reader, None, 0, &mut None, "stochastic process tree".parse().unwrap()).unwrap();
		let input0 = EbiInput::Object(object0, "stochastic process tree".parse().unwrap());
		let inputs = vec![input0];

		if let EbiCommand::Command{execute, output_type, ..} = crate::ebi_commands::ebi_command_visualise::EBI_VISUALISE_TEXT {
			match (execute)(inputs, None) {
				Ok(output) => assert_eq!(&output.get_type(), output_type),
				Err(_) => assert!(false),
			}
		}
	}
}
