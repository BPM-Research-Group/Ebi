// This file has been automatically generated. Manual changes will be overridden.

mod tests{
	use crate::ebi_framework::{
		ebi_command::EbiCommand,
		ebi_input::{self, EbiInput, read_as_object_with_file_handler, TEST_INPUT_TYPE_STRING, TEST_INPUT_TYPE_USIZE, TEST_INPUT_TYPE_FRACTION}
	};
	use ebi_objects::ebi_activity_key::TestActivityKey;
	use crate::multiple_reader::MultipleReader;
	use std::fs::File;


	// ==== group Ebi ====


	// ==== group ana ====


	// ==== command all ====
	#[test]
	pub fn ebi_ana_all_test_0() {
		// to indicate that this test is expected to fail, add the following to src/tests/fallible_test_list.rs:
		/* 
		(
			&EBI_ANALYSE_ALL,
			&[
				"trait finite stochastic language#./testfiles/ba-aa-ab.slang"			
			]
		),
		*/
		
		let mut reader = MultipleReader::from_file(File::open("./testfiles/ba-aa-ab.slang").unwrap());
		let (object0, file_handler0) = ebi_input::read_as_trait(&("finite stochastic language".parse().unwrap()), &mut reader, None, 0).unwrap();
		let input0 = EbiInput::Trait(object0, file_handler0);
		let inputs = vec![input0];

		if let EbiCommand::Command{execute, output_type, ..} = crate::ebi_commands::ebi_command_analyse::EBI_ANALYSE_ALL {
			match (execute)(inputs, None) {
				Ok(output) => {
					output.test_activity_key();
					assert_eq!(&output.get_type(), output_type);
				}
				Err(e) => Err(e).unwrap(),
			}
		}
	}
	#[test]
	pub fn ebi_ana_all_test_1() {
		// to indicate that this test is expected to fail, add the following to src/tests/fallible_test_list.rs:
		/* 
		(
			&EBI_ANALYSE_ALL,
			&[
				"trait finite stochastic language#./testfiles/a-b.slang"			
			]
		),
		*/
		
		let mut reader = MultipleReader::from_file(File::open("./testfiles/a-b.slang").unwrap());
		let (object0, file_handler0) = ebi_input::read_as_trait(&("finite stochastic language".parse().unwrap()), &mut reader, None, 0).unwrap();
		let input0 = EbiInput::Trait(object0, file_handler0);
		let inputs = vec![input0];

		if let EbiCommand::Command{execute, output_type, ..} = crate::ebi_commands::ebi_command_analyse::EBI_ANALYSE_ALL {
			match (execute)(inputs, None) {
				Ok(output) => {
					output.test_activity_key();
					assert_eq!(&output.get_type(), output_type);
				}
				Err(e) => Err(e).unwrap(),
			}
		}
	}
	#[test]
	pub fn ebi_ana_all_test_2() {
		// to indicate that this test is expected to fail, add the following to src/tests/fallible_test_list.rs:
		/* 
		(
			&EBI_ANALYSE_ALL,
			&[
				"trait finite stochastic language#./testfiles/empty.xes"			
			]
		),
		*/
		
		let mut reader = MultipleReader::from_file(File::open("./testfiles/empty.xes").unwrap());
		let (object0, file_handler0) = ebi_input::read_as_trait(&("finite stochastic language".parse().unwrap()), &mut reader, None, 0).unwrap();
		let input0 = EbiInput::Trait(object0, file_handler0);
		let inputs = vec![input0];

		if let EbiCommand::Command{execute, output_type, ..} = crate::ebi_commands::ebi_command_analyse::EBI_ANALYSE_ALL {
			match (execute)(inputs, None) {
				Ok(output) => {
					output.test_activity_key();
					assert_eq!(&output.get_type(), output_type);
				}
				Err(e) => Err(e).unwrap(),
			}
		}
	}
	#[test]
	pub fn ebi_ana_all_test_3() {
		// to indicate that this test is expected to fail, add the following to src/tests/fallible_test_list.rs:
		/* 
		(
			&EBI_ANALYSE_ALL,
			&[
				"trait finite stochastic language#./testfiles/acb-abc-ad-aded-adeded-adededed.slang"			
			]
		),
		*/
		
		let mut reader = MultipleReader::from_file(File::open("./testfiles/acb-abc-ad-aded-adeded-adededed.slang").unwrap());
		let (object0, file_handler0) = ebi_input::read_as_trait(&("finite stochastic language".parse().unwrap()), &mut reader, None, 0).unwrap();
		let input0 = EbiInput::Trait(object0, file_handler0);
		let inputs = vec![input0];

		if let EbiCommand::Command{execute, output_type, ..} = crate::ebi_commands::ebi_command_analyse::EBI_ANALYSE_ALL {
			match (execute)(inputs, None) {
				Ok(output) => {
					output.test_activity_key();
					assert_eq!(&output.get_type(), output_type);
				}
				Err(e) => Err(e).unwrap(),
			}
		}
	}


	// ==== command comp ====
	#[test]
	pub fn ebi_ana_comp_test_0() {
		// to indicate that this test is expected to fail, add the following to src/tests/fallible_test_list.rs:
		/* 
		(
			&EBI_ANALYSE_COMPLETENESS,
			&[
				"trait event log#./testfiles/empty.xes"			
			]
		),
		*/
		
		let mut reader = MultipleReader::from_file(File::open("./testfiles/empty.xes").unwrap());
		let (object0, file_handler0) = ebi_input::read_as_trait(&("event log".parse().unwrap()), &mut reader, None, 0).unwrap();
		let input0 = EbiInput::Trait(object0, file_handler0);
		let inputs = vec![input0];

		if let EbiCommand::Command{execute, output_type, ..} = crate::ebi_commands::ebi_command_analyse::EBI_ANALYSE_COMPLETENESS {
			match (execute)(inputs, None) {
				Ok(output) => {
					output.test_activity_key();
					assert_eq!(&output.get_type(), output_type);
				}
				Err(e) => Err(e).unwrap(),
			}
		}
	}
	#[test]
	pub fn ebi_ana_comp_test_1() {
		// to indicate that this test is expected to fail, add the following to src/tests/fallible_test_list.rs:
		/* 
		(
			&EBI_ANALYSE_COMPLETENESS,
			&[
				"trait event log#./testfiles/simple_log_markovian_abstraction.xes"			
			]
		),
		*/
		
		let mut reader = MultipleReader::from_file(File::open("./testfiles/simple_log_markovian_abstraction.xes").unwrap());
		let (object0, file_handler0) = ebi_input::read_as_trait(&("event log".parse().unwrap()), &mut reader, None, 0).unwrap();
		let input0 = EbiInput::Trait(object0, file_handler0);
		let inputs = vec![input0];

		if let EbiCommand::Command{execute, output_type, ..} = crate::ebi_commands::ebi_command_analyse::EBI_ANALYSE_COMPLETENESS {
			match (execute)(inputs, None) {
				Ok(output) => {
					output.test_activity_key();
					assert_eq!(&output.get_type(), output_type);
				}
				Err(e) => Err(e).unwrap(),
			}
		}
	}
	#[test]
	pub fn ebi_ana_comp_test_2() {
		// to indicate that this test is expected to fail, add the following to src/tests/fallible_test_list.rs:
		/* 
		(
			&EBI_ANALYSE_COMPLETENESS,
			&[
				"trait event log#./testfiles/a-b-double.xes"			
			]
		),
		*/
		
		let mut reader = MultipleReader::from_file(File::open("./testfiles/a-b-double.xes").unwrap());
		let (object0, file_handler0) = ebi_input::read_as_trait(&("event log".parse().unwrap()), &mut reader, None, 0).unwrap();
		let input0 = EbiInput::Trait(object0, file_handler0);
		let inputs = vec![input0];

		if let EbiCommand::Command{execute, output_type, ..} = crate::ebi_commands::ebi_command_analyse::EBI_ANALYSE_COMPLETENESS {
			match (execute)(inputs, None) {
				Ok(output) => {
					output.test_activity_key();
					assert_eq!(&output.get_type(), output_type);
				}
				Err(e) => Err(e).unwrap(),
			}
		}
	}
	#[test]
	pub fn ebi_ana_comp_test_3() {
		// to indicate that this test is expected to fail, add the following to src/tests/fallible_test_list.rs:
		/* 
		(
			&EBI_ANALYSE_COMPLETENESS,
			&[
				"trait event log#./testfiles/a-b.xes.gz"			
			]
		),
		*/
		
		let mut reader = MultipleReader::from_file(File::open("./testfiles/a-b.xes.gz").unwrap());
		let (object0, file_handler0) = ebi_input::read_as_trait(&("event log".parse().unwrap()), &mut reader, None, 0).unwrap();
		let input0 = EbiInput::Trait(object0, file_handler0);
		let inputs = vec![input0];

		if let EbiCommand::Command{execute, output_type, ..} = crate::ebi_commands::ebi_command_analyse::EBI_ANALYSE_COMPLETENESS {
			match (execute)(inputs, None) {
				Ok(output) => {
					output.test_activity_key();
					assert_eq!(&output.get_type(), output_type);
				}
				Err(e) => Err(e).unwrap(),
			}
		}
	}


	// ==== command cov ====
	#[test]
	pub fn ebi_ana_cov_test_0() {
		// to indicate that this test is expected to fail, add the following to src/tests/fallible_test_list.rs:
		/* 
		(
			&EBI_ANALYSE_COVERAGE,
			&[
				"trait finite stochastic language#./testfiles/ba-aa-ab.slang",
				"fraction 0"			
			]
		),
		*/
		
		let mut reader = MultipleReader::from_file(File::open("./testfiles/ba-aa-ab.slang").unwrap());
		let (object0, file_handler0) = ebi_input::read_as_trait(&("finite stochastic language".parse().unwrap()), &mut reader, None, 0).unwrap();
		let input0 = EbiInput::Trait(object0, file_handler0);
		let input1 = EbiInput::Fraction("0".parse().unwrap(), &TEST_INPUT_TYPE_FRACTION);
		let inputs = vec![input0, input1];

		if let EbiCommand::Command{execute, output_type, ..} = crate::ebi_commands::ebi_command_analyse::EBI_ANALYSE_COVERAGE {
			match (execute)(inputs, None) {
				Ok(output) => {
					output.test_activity_key();
					assert_eq!(&output.get_type(), output_type);
				}
				Err(e) => Err(e).unwrap(),
			}
		}
	}
	#[test]
	pub fn ebi_ana_cov_test_1() {
		// to indicate that this test is expected to fail, add the following to src/tests/fallible_test_list.rs:
		/* 
		(
			&EBI_ANALYSE_COVERAGE,
			&[
				"trait finite stochastic language#./testfiles/a-b.slang",
				"fraction 0"			
			]
		),
		*/
		
		let mut reader = MultipleReader::from_file(File::open("./testfiles/a-b.slang").unwrap());
		let (object0, file_handler0) = ebi_input::read_as_trait(&("finite stochastic language".parse().unwrap()), &mut reader, None, 0).unwrap();
		let input0 = EbiInput::Trait(object0, file_handler0);
		let input1 = EbiInput::Fraction("0".parse().unwrap(), &TEST_INPUT_TYPE_FRACTION);
		let inputs = vec![input0, input1];

		if let EbiCommand::Command{execute, output_type, ..} = crate::ebi_commands::ebi_command_analyse::EBI_ANALYSE_COVERAGE {
			match (execute)(inputs, None) {
				Ok(output) => {
					output.test_activity_key();
					assert_eq!(&output.get_type(), output_type);
				}
				Err(e) => Err(e).unwrap(),
			}
		}
	}
	#[test]
	pub fn ebi_ana_cov_test_2() {
		// to indicate that this test is expected to fail, add the following to src/tests/fallible_test_list.rs:
		/* 
		(
			&EBI_ANALYSE_COVERAGE,
			&[
				"trait finite stochastic language#./testfiles/empty.xes",
				"fraction 0"			
			]
		),
		*/
		
		let mut reader = MultipleReader::from_file(File::open("./testfiles/empty.xes").unwrap());
		let (object0, file_handler0) = ebi_input::read_as_trait(&("finite stochastic language".parse().unwrap()), &mut reader, None, 0).unwrap();
		let input0 = EbiInput::Trait(object0, file_handler0);
		let input1 = EbiInput::Fraction("0".parse().unwrap(), &TEST_INPUT_TYPE_FRACTION);
		let inputs = vec![input0, input1];

		if let EbiCommand::Command{execute, output_type, ..} = crate::ebi_commands::ebi_command_analyse::EBI_ANALYSE_COVERAGE {
			match (execute)(inputs, None) {
				Ok(output) => {
					output.test_activity_key();
					assert_eq!(&output.get_type(), output_type);
				}
				Err(e) => Err(e).unwrap(),
			}
		}
	}
	#[test]
	pub fn ebi_ana_cov_test_3() {
		// to indicate that this test is expected to fail, add the following to src/tests/fallible_test_list.rs:
		/* 
		(
			&EBI_ANALYSE_COVERAGE,
			&[
				"trait finite stochastic language#./testfiles/acb-abc-ad-aded-adeded-adededed.slang",
				"fraction 0"			
			]
		),
		*/
		
		let mut reader = MultipleReader::from_file(File::open("./testfiles/acb-abc-ad-aded-adeded-adededed.slang").unwrap());
		let (object0, file_handler0) = ebi_input::read_as_trait(&("finite stochastic language".parse().unwrap()), &mut reader, None, 0).unwrap();
		let input0 = EbiInput::Trait(object0, file_handler0);
		let input1 = EbiInput::Fraction("0".parse().unwrap(), &TEST_INPUT_TYPE_FRACTION);
		let inputs = vec![input0, input1];

		if let EbiCommand::Command{execute, output_type, ..} = crate::ebi_commands::ebi_command_analyse::EBI_ANALYSE_COVERAGE {
			match (execute)(inputs, None) {
				Ok(output) => {
					output.test_activity_key();
					assert_eq!(&output.get_type(), output_type);
				}
				Err(e) => Err(e).unwrap(),
			}
		}
	}


	// ==== command dfgedi ====
	#[test]
	pub fn ebi_ana_dfgedi_test_0() {
		// to indicate that this test is expected to fail, add the following to src/tests/fallible_test_list.rs:
		/* 
		(
			&EBI_ANALYSE_DIRECTLY_FOLLOWS_EDGE_DIFFERENCE,
			&[
				"object directly follows graph#./testfiles/bpic12-a.xes.gz-dfg.dfg",
				"object directly follows graph#./testfiles/bpic12-a.xes.gz-dfg.dfg"			
			]
		),
		*/
		
		let mut reader = MultipleReader::from_file(File::open("./testfiles/bpic12-a.xes.gz-dfg.dfg").unwrap());
		let object0 = read_as_object_with_file_handler(&"directly follows graph".parse().unwrap(), &mut reader, None, 0, &mut None, "directly follows graph".parse().unwrap()).unwrap();
		let input0 = EbiInput::Object(object0, "directly follows graph".parse().unwrap());
		let mut reader = MultipleReader::from_file(File::open("./testfiles/bpic12-a.xes.gz-dfg.dfg").unwrap());
		let object1 = read_as_object_with_file_handler(&"directly follows graph".parse().unwrap(), &mut reader, None, 0, &mut None, "directly follows graph".parse().unwrap()).unwrap();
		let input1 = EbiInput::Object(object1, "directly follows graph".parse().unwrap());
		let inputs = vec![input0, input1];

		if let EbiCommand::Command{execute, output_type, ..} = crate::ebi_commands::ebi_command_analyse::EBI_ANALYSE_DIRECTLY_FOLLOWS_EDGE_DIFFERENCE {
			match (execute)(inputs, None) {
				Ok(output) => {
					output.test_activity_key();
					assert_eq!(&output.get_type(), output_type);
				}
				Err(e) => Err(e).unwrap(),
			}
		}
	}
	#[test]
	pub fn ebi_ana_dfgedi_test_1() {
		// to indicate that this test is expected to fail, add the following to src/tests/fallible_test_list.rs:
		/* 
		(
			&EBI_ANALYSE_DIRECTLY_FOLLOWS_EDGE_DIFFERENCE,
			&[
				"object directly follows graph#./testfiles/bpic12-a.xes.gz-dfg.dfg",
				"object directly follows graph#./testfiles/BPI_Challenge_2013_closed_problems.xes.gz-dfg.dfg"			
			]
		),
		*/
		
		let mut reader = MultipleReader::from_file(File::open("./testfiles/bpic12-a.xes.gz-dfg.dfg").unwrap());
		let object0 = read_as_object_with_file_handler(&"directly follows graph".parse().unwrap(), &mut reader, None, 0, &mut None, "directly follows graph".parse().unwrap()).unwrap();
		let input0 = EbiInput::Object(object0, "directly follows graph".parse().unwrap());
		let mut reader = MultipleReader::from_file(File::open("./testfiles/BPI_Challenge_2013_closed_problems.xes.gz-dfg.dfg").unwrap());
		let object1 = read_as_object_with_file_handler(&"directly follows graph".parse().unwrap(), &mut reader, None, 0, &mut None, "directly follows graph".parse().unwrap()).unwrap();
		let input1 = EbiInput::Object(object1, "directly follows graph".parse().unwrap());
		let inputs = vec![input0, input1];

		if let EbiCommand::Command{execute, output_type, ..} = crate::ebi_commands::ebi_command_analyse::EBI_ANALYSE_DIRECTLY_FOLLOWS_EDGE_DIFFERENCE {
			match (execute)(inputs, None) {
				Ok(output) => {
					output.test_activity_key();
					assert_eq!(&output.get_type(), output_type);
				}
				Err(e) => Err(e).unwrap(),
			}
		}
	}
	#[test]
	pub fn ebi_ana_dfgedi_test_2() {
		// to indicate that this test is expected to fail, add the following to src/tests/fallible_test_list.rs:
		/* 
		(
			&EBI_ANALYSE_DIRECTLY_FOLLOWS_EDGE_DIFFERENCE,
			&[
				"object directly follows graph#./testfiles/bpic12-a.xes.gz-dfg.dfg",
				"object directly follows graph#./testfiles/aa-ab-ba.dfg"			
			]
		),
		*/
		
		let mut reader = MultipleReader::from_file(File::open("./testfiles/bpic12-a.xes.gz-dfg.dfg").unwrap());
		let object0 = read_as_object_with_file_handler(&"directly follows graph".parse().unwrap(), &mut reader, None, 0, &mut None, "directly follows graph".parse().unwrap()).unwrap();
		let input0 = EbiInput::Object(object0, "directly follows graph".parse().unwrap());
		let mut reader = MultipleReader::from_file(File::open("./testfiles/aa-ab-ba.dfg").unwrap());
		let object1 = read_as_object_with_file_handler(&"directly follows graph".parse().unwrap(), &mut reader, None, 0, &mut None, "directly follows graph".parse().unwrap()).unwrap();
		let input1 = EbiInput::Object(object1, "directly follows graph".parse().unwrap());
		let inputs = vec![input0, input1];

		if let EbiCommand::Command{execute, output_type, ..} = crate::ebi_commands::ebi_command_analyse::EBI_ANALYSE_DIRECTLY_FOLLOWS_EDGE_DIFFERENCE {
			match (execute)(inputs, None) {
				Ok(output) => {
					output.test_activity_key();
					assert_eq!(&output.get_type(), output_type);
				}
				Err(e) => Err(e).unwrap(),
			}
		}
	}
	#[test]
	pub fn ebi_ana_dfgedi_test_3() {
		// to indicate that this test is expected to fail, add the following to src/tests/fallible_test_list.rs:
		/* 
		(
			&EBI_ANALYSE_DIRECTLY_FOLLOWS_EDGE_DIFFERENCE,
			&[
				"object directly follows graph#./testfiles/BPI_Challenge_2013_closed_problems.xes.gz-dfg.dfg",
				"object directly follows graph#./testfiles/bpic12-a.xes.gz-dfg.dfg"			
			]
		),
		*/
		
		let mut reader = MultipleReader::from_file(File::open("./testfiles/BPI_Challenge_2013_closed_problems.xes.gz-dfg.dfg").unwrap());
		let object0 = read_as_object_with_file_handler(&"directly follows graph".parse().unwrap(), &mut reader, None, 0, &mut None, "directly follows graph".parse().unwrap()).unwrap();
		let input0 = EbiInput::Object(object0, "directly follows graph".parse().unwrap());
		let mut reader = MultipleReader::from_file(File::open("./testfiles/bpic12-a.xes.gz-dfg.dfg").unwrap());
		let object1 = read_as_object_with_file_handler(&"directly follows graph".parse().unwrap(), &mut reader, None, 0, &mut None, "directly follows graph".parse().unwrap()).unwrap();
		let input1 = EbiInput::Object(object1, "directly follows graph".parse().unwrap());
		let inputs = vec![input0, input1];

		if let EbiCommand::Command{execute, output_type, ..} = crate::ebi_commands::ebi_command_analyse::EBI_ANALYSE_DIRECTLY_FOLLOWS_EDGE_DIFFERENCE {
			match (execute)(inputs, None) {
				Ok(output) => {
					output.test_activity_key();
					assert_eq!(&output.get_type(), output_type);
				}
				Err(e) => Err(e).unwrap(),
			}
		}
	}


	// ==== command en ====
	#[test]
	pub fn ebi_ana_en_test_0() {
		// to indicate that this test is expected to fail, add the following to src/tests/fallible_test_list.rs:
		/* 
		(
			&EBI_ANALYSE_ENTROPY,
			&[
				"trait finite stochastic language#./testfiles/ba-aa-ab.slang"			
			]
		),
		*/
		
		let mut reader = MultipleReader::from_file(File::open("./testfiles/ba-aa-ab.slang").unwrap());
		let (object0, file_handler0) = ebi_input::read_as_trait(&("finite stochastic language".parse().unwrap()), &mut reader, None, 0).unwrap();
		let input0 = EbiInput::Trait(object0, file_handler0);
		let inputs = vec![input0];

		if let EbiCommand::Command{execute, output_type, ..} = crate::ebi_commands::ebi_command_analyse::EBI_ANALYSE_ENTROPY {
			match (execute)(inputs, None) {
				Ok(output) => {
					output.test_activity_key();
					assert_eq!(&output.get_type(), output_type);
				}
				Err(e) => Err(e).unwrap(),
			}
		}
	}
	#[test]
	pub fn ebi_ana_en_test_1() {
		// to indicate that this test is expected to fail, add the following to src/tests/fallible_test_list.rs:
		/* 
		(
			&EBI_ANALYSE_ENTROPY,
			&[
				"trait finite stochastic language#./testfiles/a-b.slang"			
			]
		),
		*/
		
		let mut reader = MultipleReader::from_file(File::open("./testfiles/a-b.slang").unwrap());
		let (object0, file_handler0) = ebi_input::read_as_trait(&("finite stochastic language".parse().unwrap()), &mut reader, None, 0).unwrap();
		let input0 = EbiInput::Trait(object0, file_handler0);
		let inputs = vec![input0];

		if let EbiCommand::Command{execute, output_type, ..} = crate::ebi_commands::ebi_command_analyse::EBI_ANALYSE_ENTROPY {
			match (execute)(inputs, None) {
				Ok(output) => {
					output.test_activity_key();
					assert_eq!(&output.get_type(), output_type);
				}
				Err(e) => Err(e).unwrap(),
			}
		}
	}
	#[test]
	pub fn ebi_ana_en_test_2() {
		// to indicate that this test is expected to fail, add the following to src/tests/fallible_test_list.rs:
		/* 
		(
			&EBI_ANALYSE_ENTROPY,
			&[
				"trait finite stochastic language#./testfiles/empty.xes"			
			]
		),
		*/
		
		let mut reader = MultipleReader::from_file(File::open("./testfiles/empty.xes").unwrap());
		let (object0, file_handler0) = ebi_input::read_as_trait(&("finite stochastic language".parse().unwrap()), &mut reader, None, 0).unwrap();
		let input0 = EbiInput::Trait(object0, file_handler0);
		let inputs = vec![input0];

		if let EbiCommand::Command{execute, output_type, ..} = crate::ebi_commands::ebi_command_analyse::EBI_ANALYSE_ENTROPY {
			match (execute)(inputs, None) {
				Ok(output) => {
					output.test_activity_key();
					assert_eq!(&output.get_type(), output_type);
				}
				Err(e) => Err(e).unwrap(),
			}
		}
	}
	#[test]
	pub fn ebi_ana_en_test_3() {
		// to indicate that this test is expected to fail, add the following to src/tests/fallible_test_list.rs:
		/* 
		(
			&EBI_ANALYSE_ENTROPY,
			&[
				"trait finite stochastic language#./testfiles/acb-abc-ad-aded-adeded-adededed.slang"			
			]
		),
		*/
		
		let mut reader = MultipleReader::from_file(File::open("./testfiles/acb-abc-ad-aded-adeded-adededed.slang").unwrap());
		let (object0, file_handler0) = ebi_input::read_as_trait(&("finite stochastic language".parse().unwrap()), &mut reader, None, 0).unwrap();
		let input0 = EbiInput::Trait(object0, file_handler0);
		let inputs = vec![input0];

		if let EbiCommand::Command{execute, output_type, ..} = crate::ebi_commands::ebi_command_analyse::EBI_ANALYSE_ENTROPY {
			match (execute)(inputs, None) {
				Ok(output) => {
					output.test_activity_key();
					assert_eq!(&output.get_type(), output_type);
				}
				Err(e) => Err(e).unwrap(),
			}
		}
	}


	// ==== command med ====
	#[test]
	pub fn ebi_ana_med_test_0() {
		// to indicate that this test is expected to fail, add the following to src/tests/fallible_test_list.rs:
		/* 
		(
			&EBI_ANALYSE_MEDOID,
			&[
				"trait finite stochastic language#./testfiles/ba-aa-ab.slang",
				"usize 1"			
			]
		),
		*/
		
		let mut reader = MultipleReader::from_file(File::open("./testfiles/ba-aa-ab.slang").unwrap());
		let (object0, file_handler0) = ebi_input::read_as_trait(&("finite stochastic language".parse().unwrap()), &mut reader, None, 0).unwrap();
		let input0 = EbiInput::Trait(object0, file_handler0);
		let input1 = EbiInput::Usize(1, &TEST_INPUT_TYPE_USIZE);
		let inputs = vec![input0, input1];

		if let EbiCommand::Command{execute, output_type, ..} = crate::ebi_commands::ebi_command_analyse::EBI_ANALYSE_MEDOID {
			match (execute)(inputs, None) {
				Ok(output) => {
					output.test_activity_key();
					assert_eq!(&output.get_type(), output_type);
				}
				Err(e) => Err(e).unwrap(),
			}
		}
	}
	#[test]
	pub fn ebi_ana_med_test_1() {
		// to indicate that this test is expected to fail, add the following to src/tests/fallible_test_list.rs:
		/* 
		(
			&EBI_ANALYSE_MEDOID,
			&[
				"trait finite stochastic language#./testfiles/a-b.slang",
				"usize 1"			
			]
		),
		*/
		
		let mut reader = MultipleReader::from_file(File::open("./testfiles/a-b.slang").unwrap());
		let (object0, file_handler0) = ebi_input::read_as_trait(&("finite stochastic language".parse().unwrap()), &mut reader, None, 0).unwrap();
		let input0 = EbiInput::Trait(object0, file_handler0);
		let input1 = EbiInput::Usize(1, &TEST_INPUT_TYPE_USIZE);
		let inputs = vec![input0, input1];

		if let EbiCommand::Command{execute, output_type, ..} = crate::ebi_commands::ebi_command_analyse::EBI_ANALYSE_MEDOID {
			match (execute)(inputs, None) {
				Ok(output) => {
					output.test_activity_key();
					assert_eq!(&output.get_type(), output_type);
				}
				Err(e) => Err(e).unwrap(),
			}
		}
	}
	#[test]
	pub fn ebi_ana_med_test_2() {
		// this test has been indicated as to be expected to fail
		
		let mut reader = MultipleReader::from_file(File::open("./testfiles/empty.xes").unwrap());
		let (object0, file_handler0) = ebi_input::read_as_trait(&("finite stochastic language".parse().unwrap()), &mut reader, None, 0).unwrap();
		let input0 = EbiInput::Trait(object0, file_handler0);
		let input1 = EbiInput::Usize(1, &TEST_INPUT_TYPE_USIZE);
		let inputs = vec![input0, input1];

		if let EbiCommand::Command{execute, ..} = crate::ebi_commands::ebi_command_analyse::EBI_ANALYSE_MEDOID {
			assert!(((execute)(inputs, None)).is_err())
		}
	}
	#[test]
	pub fn ebi_ana_med_test_3() {
		// to indicate that this test is expected to fail, add the following to src/tests/fallible_test_list.rs:
		/* 
		(
			&EBI_ANALYSE_MEDOID,
			&[
				"trait finite stochastic language#./testfiles/acb-abc-ad-aded-adeded-adededed.slang",
				"usize 1"			
			]
		),
		*/
		
		let mut reader = MultipleReader::from_file(File::open("./testfiles/acb-abc-ad-aded-adeded-adededed.slang").unwrap());
		let (object0, file_handler0) = ebi_input::read_as_trait(&("finite stochastic language".parse().unwrap()), &mut reader, None, 0).unwrap();
		let input0 = EbiInput::Trait(object0, file_handler0);
		let input1 = EbiInput::Usize(1, &TEST_INPUT_TYPE_USIZE);
		let inputs = vec![input0, input1];

		if let EbiCommand::Command{execute, output_type, ..} = crate::ebi_commands::ebi_command_analyse::EBI_ANALYSE_MEDOID {
			match (execute)(inputs, None) {
				Ok(output) => {
					output.test_activity_key();
					assert_eq!(&output.get_type(), output_type);
				}
				Err(e) => Err(e).unwrap(),
			}
		}
	}


	// ==== command minprob ====
	#[test]
	pub fn ebi_ana_minprob_test_0() {
		// to indicate that this test is expected to fail, add the following to src/tests/fallible_test_list.rs:
		/* 
		(
			&EBI_ANALYSE_MINPROB,
			&[
				"trait stochastic deterministic semantics#./testfiles/seq(a-xor(b-c)).sptree",
				"fraction 0"			
			]
		),
		*/
		
		let mut reader = MultipleReader::from_file(File::open("./testfiles/seq(a-xor(b-c)).sptree").unwrap());
		let (object0, file_handler0) = ebi_input::read_as_trait(&("stochastic deterministic semantics".parse().unwrap()), &mut reader, None, 0).unwrap();
		let input0 = EbiInput::Trait(object0, file_handler0);
		let input1 = EbiInput::Fraction("0".parse().unwrap(), &TEST_INPUT_TYPE_FRACTION);
		let inputs = vec![input0, input1];

		if let EbiCommand::Command{execute, output_type, ..} = crate::ebi_commands::ebi_command_analyse::EBI_ANALYSE_MINPROB {
			match (execute)(inputs, None) {
				Ok(output) => {
					output.test_activity_key();
					assert_eq!(&output.get_type(), output_type);
				}
				Err(e) => Err(e).unwrap(),
			}
		}
	}
	#[test]
	pub fn ebi_ana_minprob_test_1() {
		// to indicate that this test is expected to fail, add the following to src/tests/fallible_test_list.rs:
		/* 
		(
			&EBI_ANALYSE_MINPROB,
			&[
				"trait stochastic deterministic semantics#./testfiles/ba-aa-ab.slang",
				"fraction 0"			
			]
		),
		*/
		
		let mut reader = MultipleReader::from_file(File::open("./testfiles/ba-aa-ab.slang").unwrap());
		let (object0, file_handler0) = ebi_input::read_as_trait(&("stochastic deterministic semantics".parse().unwrap()), &mut reader, None, 0).unwrap();
		let input0 = EbiInput::Trait(object0, file_handler0);
		let input1 = EbiInput::Fraction("0".parse().unwrap(), &TEST_INPUT_TYPE_FRACTION);
		let inputs = vec![input0, input1];

		if let EbiCommand::Command{execute, output_type, ..} = crate::ebi_commands::ebi_command_analyse::EBI_ANALYSE_MINPROB {
			match (execute)(inputs, None) {
				Ok(output) => {
					output.test_activity_key();
					assert_eq!(&output.get_type(), output_type);
				}
				Err(e) => Err(e).unwrap(),
			}
		}
	}
	#[test]
	pub fn ebi_ana_minprob_test_2() {
		// to indicate that this test is expected to fail, add the following to src/tests/fallible_test_list.rs:
		/* 
		(
			&EBI_ANALYSE_MINPROB,
			&[
				"trait stochastic deterministic semantics#./testfiles/a-b.slang",
				"fraction 0"			
			]
		),
		*/
		
		let mut reader = MultipleReader::from_file(File::open("./testfiles/a-b.slang").unwrap());
		let (object0, file_handler0) = ebi_input::read_as_trait(&("stochastic deterministic semantics".parse().unwrap()), &mut reader, None, 0).unwrap();
		let input0 = EbiInput::Trait(object0, file_handler0);
		let input1 = EbiInput::Fraction("0".parse().unwrap(), &TEST_INPUT_TYPE_FRACTION);
		let inputs = vec![input0, input1];

		if let EbiCommand::Command{execute, output_type, ..} = crate::ebi_commands::ebi_command_analyse::EBI_ANALYSE_MINPROB {
			match (execute)(inputs, None) {
				Ok(output) => {
					output.test_activity_key();
					assert_eq!(&output.get_type(), output_type);
				}
				Err(e) => Err(e).unwrap(),
			}
		}
	}
	#[test]
	pub fn ebi_ana_minprob_test_3() {
		// this test has been indicated as to be expected to fail
		
		let mut reader = MultipleReader::from_file(File::open("./testfiles/empty.xes").unwrap());
		let (object0, file_handler0) = ebi_input::read_as_trait(&("stochastic deterministic semantics".parse().unwrap()), &mut reader, None, 0).unwrap();
		let input0 = EbiInput::Trait(object0, file_handler0);
		let input1 = EbiInput::Fraction("0".parse().unwrap(), &TEST_INPUT_TYPE_FRACTION);
		let inputs = vec![input0, input1];

		if let EbiCommand::Command{execute, ..} = crate::ebi_commands::ebi_command_analyse::EBI_ANALYSE_MINPROB {
			assert!(((execute)(inputs, None)).is_err())
		}
	}


	// ==== command mode ====
	#[test]
	pub fn ebi_ana_mode_test_0() {
		// to indicate that this test is expected to fail, add the following to src/tests/fallible_test_list.rs:
		/* 
		(
			&EBI_ANALYSE_MODE,
			&[
				"trait finite stochastic language#./testfiles/ba-aa-ab.slang"			
			]
		),
		*/
		
		let mut reader = MultipleReader::from_file(File::open("./testfiles/ba-aa-ab.slang").unwrap());
		let (object0, file_handler0) = ebi_input::read_as_trait(&("finite stochastic language".parse().unwrap()), &mut reader, None, 0).unwrap();
		let input0 = EbiInput::Trait(object0, file_handler0);
		let inputs = vec![input0];

		if let EbiCommand::Command{execute, output_type, ..} = crate::ebi_commands::ebi_command_analyse::EBI_ANALYSE_MODE {
			match (execute)(inputs, None) {
				Ok(output) => {
					output.test_activity_key();
					assert_eq!(&output.get_type(), output_type);
				}
				Err(e) => Err(e).unwrap(),
			}
		}
	}
	#[test]
	pub fn ebi_ana_mode_test_1() {
		// to indicate that this test is expected to fail, add the following to src/tests/fallible_test_list.rs:
		/* 
		(
			&EBI_ANALYSE_MODE,
			&[
				"trait finite stochastic language#./testfiles/a-b.slang"			
			]
		),
		*/
		
		let mut reader = MultipleReader::from_file(File::open("./testfiles/a-b.slang").unwrap());
		let (object0, file_handler0) = ebi_input::read_as_trait(&("finite stochastic language".parse().unwrap()), &mut reader, None, 0).unwrap();
		let input0 = EbiInput::Trait(object0, file_handler0);
		let inputs = vec![input0];

		if let EbiCommand::Command{execute, output_type, ..} = crate::ebi_commands::ebi_command_analyse::EBI_ANALYSE_MODE {
			match (execute)(inputs, None) {
				Ok(output) => {
					output.test_activity_key();
					assert_eq!(&output.get_type(), output_type);
				}
				Err(e) => Err(e).unwrap(),
			}
		}
	}
	#[test]
	pub fn ebi_ana_mode_test_2() {
		// to indicate that this test is expected to fail, add the following to src/tests/fallible_test_list.rs:
		/* 
		(
			&EBI_ANALYSE_MODE,
			&[
				"trait finite stochastic language#./testfiles/empty.xes"			
			]
		),
		*/
		
		let mut reader = MultipleReader::from_file(File::open("./testfiles/empty.xes").unwrap());
		let (object0, file_handler0) = ebi_input::read_as_trait(&("finite stochastic language".parse().unwrap()), &mut reader, None, 0).unwrap();
		let input0 = EbiInput::Trait(object0, file_handler0);
		let inputs = vec![input0];

		if let EbiCommand::Command{execute, output_type, ..} = crate::ebi_commands::ebi_command_analyse::EBI_ANALYSE_MODE {
			match (execute)(inputs, None) {
				Ok(output) => {
					output.test_activity_key();
					assert_eq!(&output.get_type(), output_type);
				}
				Err(e) => Err(e).unwrap(),
			}
		}
	}
	#[test]
	pub fn ebi_ana_mode_test_3() {
		// to indicate that this test is expected to fail, add the following to src/tests/fallible_test_list.rs:
		/* 
		(
			&EBI_ANALYSE_MODE,
			&[
				"trait finite stochastic language#./testfiles/acb-abc-ad-aded-adeded-adededed.slang"			
			]
		),
		*/
		
		let mut reader = MultipleReader::from_file(File::open("./testfiles/acb-abc-ad-aded-adeded-adededed.slang").unwrap());
		let (object0, file_handler0) = ebi_input::read_as_trait(&("finite stochastic language".parse().unwrap()), &mut reader, None, 0).unwrap();
		let input0 = EbiInput::Trait(object0, file_handler0);
		let inputs = vec![input0];

		if let EbiCommand::Command{execute, output_type, ..} = crate::ebi_commands::ebi_command_analyse::EBI_ANALYSE_MODE {
			match (execute)(inputs, None) {
				Ok(output) => {
					output.test_activity_key();
					assert_eq!(&output.get_type(), output_type);
				}
				Err(e) => Err(e).unwrap(),
			}
		}
	}


	// ==== command mostlikely ====
	#[test]
	pub fn ebi_ana_mostlikely_test_0() {
		// to indicate that this test is expected to fail, add the following to src/tests/fallible_test_list.rs:
		/* 
		(
			&EBI_ANALYSE_MOSTLIKELY,
			&[
				"trait finite stochastic language#./testfiles/ba-aa-ab.slang",
				"usize 1"			
			]
		),
		*/
		
		let mut reader = MultipleReader::from_file(File::open("./testfiles/ba-aa-ab.slang").unwrap());
		let (object0, file_handler0) = ebi_input::read_as_trait(&("finite stochastic language".parse().unwrap()), &mut reader, None, 0).unwrap();
		let input0 = EbiInput::Trait(object0, file_handler0);
		let input1 = EbiInput::Usize(1, &TEST_INPUT_TYPE_USIZE);
		let inputs = vec![input0, input1];

		if let EbiCommand::Command{execute, output_type, ..} = crate::ebi_commands::ebi_command_analyse::EBI_ANALYSE_MOSTLIKELY {
			match (execute)(inputs, None) {
				Ok(output) => {
					output.test_activity_key();
					assert_eq!(&output.get_type(), output_type);
				}
				Err(e) => Err(e).unwrap(),
			}
		}
	}
	#[test]
	pub fn ebi_ana_mostlikely_test_1() {
		// to indicate that this test is expected to fail, add the following to src/tests/fallible_test_list.rs:
		/* 
		(
			&EBI_ANALYSE_MOSTLIKELY,
			&[
				"trait finite stochastic language#./testfiles/a-b.slang",
				"usize 1"			
			]
		),
		*/
		
		let mut reader = MultipleReader::from_file(File::open("./testfiles/a-b.slang").unwrap());
		let (object0, file_handler0) = ebi_input::read_as_trait(&("finite stochastic language".parse().unwrap()), &mut reader, None, 0).unwrap();
		let input0 = EbiInput::Trait(object0, file_handler0);
		let input1 = EbiInput::Usize(1, &TEST_INPUT_TYPE_USIZE);
		let inputs = vec![input0, input1];

		if let EbiCommand::Command{execute, output_type, ..} = crate::ebi_commands::ebi_command_analyse::EBI_ANALYSE_MOSTLIKELY {
			match (execute)(inputs, None) {
				Ok(output) => {
					output.test_activity_key();
					assert_eq!(&output.get_type(), output_type);
				}
				Err(e) => Err(e).unwrap(),
			}
		}
	}
	#[test]
	pub fn ebi_ana_mostlikely_test_2() {
		// to indicate that this test is expected to fail, add the following to src/tests/fallible_test_list.rs:
		/* 
		(
			&EBI_ANALYSE_MOSTLIKELY,
			&[
				"trait finite stochastic language#./testfiles/empty.xes",
				"usize 1"			
			]
		),
		*/
		
		let mut reader = MultipleReader::from_file(File::open("./testfiles/empty.xes").unwrap());
		let (object0, file_handler0) = ebi_input::read_as_trait(&("finite stochastic language".parse().unwrap()), &mut reader, None, 0).unwrap();
		let input0 = EbiInput::Trait(object0, file_handler0);
		let input1 = EbiInput::Usize(1, &TEST_INPUT_TYPE_USIZE);
		let inputs = vec![input0, input1];

		if let EbiCommand::Command{execute, output_type, ..} = crate::ebi_commands::ebi_command_analyse::EBI_ANALYSE_MOSTLIKELY {
			match (execute)(inputs, None) {
				Ok(output) => {
					output.test_activity_key();
					assert_eq!(&output.get_type(), output_type);
				}
				Err(e) => Err(e).unwrap(),
			}
		}
	}
	#[test]
	pub fn ebi_ana_mostlikely_test_3() {
		// to indicate that this test is expected to fail, add the following to src/tests/fallible_test_list.rs:
		/* 
		(
			&EBI_ANALYSE_MOSTLIKELY,
			&[
				"trait finite stochastic language#./testfiles/acb-abc-ad-aded-adeded-adededed.slang",
				"usize 1"			
			]
		),
		*/
		
		let mut reader = MultipleReader::from_file(File::open("./testfiles/acb-abc-ad-aded-adeded-adededed.slang").unwrap());
		let (object0, file_handler0) = ebi_input::read_as_trait(&("finite stochastic language".parse().unwrap()), &mut reader, None, 0).unwrap();
		let input0 = EbiInput::Trait(object0, file_handler0);
		let input1 = EbiInput::Usize(1, &TEST_INPUT_TYPE_USIZE);
		let inputs = vec![input0, input1];

		if let EbiCommand::Command{execute, output_type, ..} = crate::ebi_commands::ebi_command_analyse::EBI_ANALYSE_MOSTLIKELY {
			match (execute)(inputs, None) {
				Ok(output) => {
					output.test_activity_key();
					assert_eq!(&output.get_type(), output_type);
				}
				Err(e) => Err(e).unwrap(),
			}
		}
	}


	// ==== command var ====
	#[test]
	pub fn ebi_ana_var_test_0() {
		// to indicate that this test is expected to fail, add the following to src/tests/fallible_test_list.rs:
		/* 
		(
			&EBI_ANALYSE_VARIETY,
			&[
				"trait finite stochastic language#./testfiles/ba-aa-ab.slang"			
			]
		),
		*/
		
		let mut reader = MultipleReader::from_file(File::open("./testfiles/ba-aa-ab.slang").unwrap());
		let (object0, file_handler0) = ebi_input::read_as_trait(&("finite stochastic language".parse().unwrap()), &mut reader, None, 0).unwrap();
		let input0 = EbiInput::Trait(object0, file_handler0);
		let inputs = vec![input0];

		if let EbiCommand::Command{execute, output_type, ..} = crate::ebi_commands::ebi_command_analyse::EBI_ANALYSE_VARIETY {
			match (execute)(inputs, None) {
				Ok(output) => {
					output.test_activity_key();
					assert_eq!(&output.get_type(), output_type);
				}
				Err(e) => Err(e).unwrap(),
			}
		}
	}
	#[test]
	pub fn ebi_ana_var_test_1() {
		// to indicate that this test is expected to fail, add the following to src/tests/fallible_test_list.rs:
		/* 
		(
			&EBI_ANALYSE_VARIETY,
			&[
				"trait finite stochastic language#./testfiles/a-b.slang"			
			]
		),
		*/
		
		let mut reader = MultipleReader::from_file(File::open("./testfiles/a-b.slang").unwrap());
		let (object0, file_handler0) = ebi_input::read_as_trait(&("finite stochastic language".parse().unwrap()), &mut reader, None, 0).unwrap();
		let input0 = EbiInput::Trait(object0, file_handler0);
		let inputs = vec![input0];

		if let EbiCommand::Command{execute, output_type, ..} = crate::ebi_commands::ebi_command_analyse::EBI_ANALYSE_VARIETY {
			match (execute)(inputs, None) {
				Ok(output) => {
					output.test_activity_key();
					assert_eq!(&output.get_type(), output_type);
				}
				Err(e) => Err(e).unwrap(),
			}
		}
	}
	#[test]
	pub fn ebi_ana_var_test_2() {
		// to indicate that this test is expected to fail, add the following to src/tests/fallible_test_list.rs:
		/* 
		(
			&EBI_ANALYSE_VARIETY,
			&[
				"trait finite stochastic language#./testfiles/empty.xes"			
			]
		),
		*/
		
		let mut reader = MultipleReader::from_file(File::open("./testfiles/empty.xes").unwrap());
		let (object0, file_handler0) = ebi_input::read_as_trait(&("finite stochastic language".parse().unwrap()), &mut reader, None, 0).unwrap();
		let input0 = EbiInput::Trait(object0, file_handler0);
		let inputs = vec![input0];

		if let EbiCommand::Command{execute, output_type, ..} = crate::ebi_commands::ebi_command_analyse::EBI_ANALYSE_VARIETY {
			match (execute)(inputs, None) {
				Ok(output) => {
					output.test_activity_key();
					assert_eq!(&output.get_type(), output_type);
				}
				Err(e) => Err(e).unwrap(),
			}
		}
	}
	#[test]
	pub fn ebi_ana_var_test_3() {
		// to indicate that this test is expected to fail, add the following to src/tests/fallible_test_list.rs:
		/* 
		(
			&EBI_ANALYSE_VARIETY,
			&[
				"trait finite stochastic language#./testfiles/acb-abc-ad-aded-adeded-adededed.slang"			
			]
		),
		*/
		
		let mut reader = MultipleReader::from_file(File::open("./testfiles/acb-abc-ad-aded-adeded-adededed.slang").unwrap());
		let (object0, file_handler0) = ebi_input::read_as_trait(&("finite stochastic language".parse().unwrap()), &mut reader, None, 0).unwrap();
		let input0 = EbiInput::Trait(object0, file_handler0);
		let inputs = vec![input0];

		if let EbiCommand::Command{execute, output_type, ..} = crate::ebi_commands::ebi_command_analyse::EBI_ANALYSE_VARIETY {
			match (execute)(inputs, None) {
				Ok(output) => {
					output.test_activity_key();
					assert_eq!(&output.get_type(), output_type);
				}
				Err(e) => Err(e).unwrap(),
			}
		}
	}


	// ==== group anans ====


	// ==== command act ====
	#[test]
	pub fn ebi_anans_act_test_0() {
		// to indicate that this test is expected to fail, add the following to src/tests/fallible_test_list.rs:
		/* 
		(
			&EBI_ANALYSE_NON_STOCHASTIC_ACTIVITIES,
			&[
				"trait activities#./testfiles/seq(a-xor(b-c)).sptree"			
			]
		),
		*/
		
		let mut reader = MultipleReader::from_file(File::open("./testfiles/seq(a-xor(b-c)).sptree").unwrap());
		let (object0, file_handler0) = ebi_input::read_as_trait(&("activities".parse().unwrap()), &mut reader, None, 0).unwrap();
		let input0 = EbiInput::Trait(object0, file_handler0);
		let inputs = vec![input0];

		if let EbiCommand::Command{execute, output_type, ..} = crate::ebi_commands::ebi_command_analyse_non_stochastic::EBI_ANALYSE_NON_STOCHASTIC_ACTIVITIES {
			match (execute)(inputs, None) {
				Ok(output) => {
					output.test_activity_key();
					assert_eq!(&output.get_type(), output_type);
				}
				Err(e) => Err(e).unwrap(),
			}
		}
	}
	#[test]
	pub fn ebi_anans_act_test_1() {
		// to indicate that this test is expected to fail, add the following to src/tests/fallible_test_list.rs:
		/* 
		(
			&EBI_ANALYSE_NON_STOCHASTIC_ACTIVITIES,
			&[
				"trait activities#./testfiles/ba-aa-ab.slang"			
			]
		),
		*/
		
		let mut reader = MultipleReader::from_file(File::open("./testfiles/ba-aa-ab.slang").unwrap());
		let (object0, file_handler0) = ebi_input::read_as_trait(&("activities".parse().unwrap()), &mut reader, None, 0).unwrap();
		let input0 = EbiInput::Trait(object0, file_handler0);
		let inputs = vec![input0];

		if let EbiCommand::Command{execute, output_type, ..} = crate::ebi_commands::ebi_command_analyse_non_stochastic::EBI_ANALYSE_NON_STOCHASTIC_ACTIVITIES {
			match (execute)(inputs, None) {
				Ok(output) => {
					output.test_activity_key();
					assert_eq!(&output.get_type(), output_type);
				}
				Err(e) => Err(e).unwrap(),
			}
		}
	}
	#[test]
	pub fn ebi_anans_act_test_2() {
		// to indicate that this test is expected to fail, add the following to src/tests/fallible_test_list.rs:
		/* 
		(
			&EBI_ANALYSE_NON_STOCHASTIC_ACTIVITIES,
			&[
				"trait activities#./testfiles/model.bpmn"			
			]
		),
		*/
		
		let mut reader = MultipleReader::from_file(File::open("./testfiles/model.bpmn").unwrap());
		let (object0, file_handler0) = ebi_input::read_as_trait(&("activities".parse().unwrap()), &mut reader, None, 0).unwrap();
		let input0 = EbiInput::Trait(object0, file_handler0);
		let inputs = vec![input0];

		if let EbiCommand::Command{execute, output_type, ..} = crate::ebi_commands::ebi_command_analyse_non_stochastic::EBI_ANALYSE_NON_STOCHASTIC_ACTIVITIES {
			match (execute)(inputs, None) {
				Ok(output) => {
					output.test_activity_key();
					assert_eq!(&output.get_type(), output_type);
				}
				Err(e) => Err(e).unwrap(),
			}
		}
	}
	#[test]
	pub fn ebi_anans_act_test_3() {
		// to indicate that this test is expected to fail, add the following to src/tests/fallible_test_list.rs:
		/* 
		(
			&EBI_ANALYSE_NON_STOCHASTIC_ACTIVITIES,
			&[
				"trait activities#./testfiles/a-b.slang"			
			]
		),
		*/
		
		let mut reader = MultipleReader::from_file(File::open("./testfiles/a-b.slang").unwrap());
		let (object0, file_handler0) = ebi_input::read_as_trait(&("activities".parse().unwrap()), &mut reader, None, 0).unwrap();
		let input0 = EbiInput::Trait(object0, file_handler0);
		let inputs = vec![input0];

		if let EbiCommand::Command{execute, output_type, ..} = crate::ebi_commands::ebi_command_analyse_non_stochastic::EBI_ANALYSE_NON_STOCHASTIC_ACTIVITIES {
			match (execute)(inputs, None) {
				Ok(output) => {
					output.test_activity_key();
					assert_eq!(&output.get_type(), output_type);
				}
				Err(e) => Err(e).unwrap(),
			}
		}
	}


	// ==== command bnd ====
	#[test]
	pub fn ebi_anans_bnd_test_0() {
		// to indicate that this test is expected to fail, add the following to src/tests/fallible_test_list.rs:
		/* 
		(
			&EBI_ANALYSE_NON_STOCHASTIC_BOUNDED,
			&[
				"object directly follows graph#./testfiles/bpic12-a.xes.gz-dfg.dfg"			
			]
		),
		*/
		
		let mut reader = MultipleReader::from_file(File::open("./testfiles/bpic12-a.xes.gz-dfg.dfg").unwrap());
		let object0 = read_as_object_with_file_handler(&"directly follows graph".parse().unwrap(), &mut reader, None, 0, &mut None, "directly follows graph".parse().unwrap()).unwrap();
		let input0 = EbiInput::Object(object0, "directly follows graph".parse().unwrap());
		let inputs = vec![input0];

		if let EbiCommand::Command{execute, output_type, ..} = crate::ebi_commands::ebi_command_analyse_non_stochastic::EBI_ANALYSE_NON_STOCHASTIC_BOUNDED {
			match (execute)(inputs, None) {
				Ok(output) => {
					output.test_activity_key();
					assert_eq!(&output.get_type(), output_type);
				}
				Err(e) => Err(e).unwrap(),
			}
		}
	}
	#[test]
	pub fn ebi_anans_bnd_test_1() {
		// to indicate that this test is expected to fail, add the following to src/tests/fallible_test_list.rs:
		/* 
		(
			&EBI_ANALYSE_NON_STOCHASTIC_BOUNDED,
			&[
				"object directly follows graph#./testfiles/BPI_Challenge_2013_closed_problems.xes.gz-dfg.dfg"			
			]
		),
		*/
		
		let mut reader = MultipleReader::from_file(File::open("./testfiles/BPI_Challenge_2013_closed_problems.xes.gz-dfg.dfg").unwrap());
		let object0 = read_as_object_with_file_handler(&"directly follows graph".parse().unwrap(), &mut reader, None, 0, &mut None, "directly follows graph".parse().unwrap()).unwrap();
		let input0 = EbiInput::Object(object0, "directly follows graph".parse().unwrap());
		let inputs = vec![input0];

		if let EbiCommand::Command{execute, output_type, ..} = crate::ebi_commands::ebi_command_analyse_non_stochastic::EBI_ANALYSE_NON_STOCHASTIC_BOUNDED {
			match (execute)(inputs, None) {
				Ok(output) => {
					output.test_activity_key();
					assert_eq!(&output.get_type(), output_type);
				}
				Err(e) => Err(e).unwrap(),
			}
		}
	}
	#[test]
	pub fn ebi_anans_bnd_test_2() {
		// to indicate that this test is expected to fail, add the following to src/tests/fallible_test_list.rs:
		/* 
		(
			&EBI_ANALYSE_NON_STOCHASTIC_BOUNDED,
			&[
				"object directly follows graph#./testfiles/aa-ab-ba.dfg"			
			]
		),
		*/
		
		let mut reader = MultipleReader::from_file(File::open("./testfiles/aa-ab-ba.dfg").unwrap());
		let object0 = read_as_object_with_file_handler(&"directly follows graph".parse().unwrap(), &mut reader, None, 0, &mut None, "directly follows graph".parse().unwrap()).unwrap();
		let input0 = EbiInput::Object(object0, "directly follows graph".parse().unwrap());
		let inputs = vec![input0];

		if let EbiCommand::Command{execute, output_type, ..} = crate::ebi_commands::ebi_command_analyse_non_stochastic::EBI_ANALYSE_NON_STOCHASTIC_BOUNDED {
			match (execute)(inputs, None) {
				Ok(output) => {
					output.test_activity_key();
					assert_eq!(&output.get_type(), output_type);
				}
				Err(e) => Err(e).unwrap(),
			}
		}
	}
	#[test]
	pub fn ebi_anans_bnd_test_3() {
		// to indicate that this test is expected to fail, add the following to src/tests/fallible_test_list.rs:
		/* 
		(
			&EBI_ANALYSE_NON_STOCHASTIC_BOUNDED,
			&[
				"object stochastic process tree#./testfiles/seq(a-xor(b-c)).sptree"			
			]
		),
		*/
		
		let mut reader = MultipleReader::from_file(File::open("./testfiles/seq(a-xor(b-c)).sptree").unwrap());
		let object0 = read_as_object_with_file_handler(&"stochastic process tree".parse().unwrap(), &mut reader, None, 0, &mut None, "stochastic process tree".parse().unwrap()).unwrap();
		let input0 = EbiInput::Object(object0, "stochastic process tree".parse().unwrap());
		let inputs = vec![input0];

		if let EbiCommand::Command{execute, output_type, ..} = crate::ebi_commands::ebi_command_analyse_non_stochastic::EBI_ANALYSE_NON_STOCHASTIC_BOUNDED {
			match (execute)(inputs, None) {
				Ok(output) => {
					output.test_activity_key();
					assert_eq!(&output.get_type(), output_type);
				}
				Err(e) => Err(e).unwrap(),
			}
		}
	}


	// ==== command clus ====
	#[test]
	pub fn ebi_anans_clus_test_0() {
		// to indicate that this test is expected to fail, add the following to src/tests/fallible_test_list.rs:
		/* 
		(
			&EBI_ANALYSE_NON_STOCHASTIC_CLUSTER,
			&[
				"trait finite language#./testfiles/ba-aa-ab.slang",
				"usize 1"			
			]
		),
		*/
		
		let mut reader = MultipleReader::from_file(File::open("./testfiles/ba-aa-ab.slang").unwrap());
		let (object0, file_handler0) = ebi_input::read_as_trait(&("finite language".parse().unwrap()), &mut reader, None, 0).unwrap();
		let input0 = EbiInput::Trait(object0, file_handler0);
		let input1 = EbiInput::Usize(1, &TEST_INPUT_TYPE_USIZE);
		let inputs = vec![input0, input1];

		if let EbiCommand::Command{execute, output_type, ..} = crate::ebi_commands::ebi_command_analyse_non_stochastic::EBI_ANALYSE_NON_STOCHASTIC_CLUSTER {
			match (execute)(inputs, None) {
				Ok(output) => {
					output.test_activity_key();
					assert_eq!(&output.get_type(), output_type);
				}
				Err(e) => Err(e).unwrap(),
			}
		}
	}
	#[test]
	pub fn ebi_anans_clus_test_1() {
		// to indicate that this test is expected to fail, add the following to src/tests/fallible_test_list.rs:
		/* 
		(
			&EBI_ANALYSE_NON_STOCHASTIC_CLUSTER,
			&[
				"trait finite language#./testfiles/a-b.slang",
				"usize 1"			
			]
		),
		*/
		
		let mut reader = MultipleReader::from_file(File::open("./testfiles/a-b.slang").unwrap());
		let (object0, file_handler0) = ebi_input::read_as_trait(&("finite language".parse().unwrap()), &mut reader, None, 0).unwrap();
		let input0 = EbiInput::Trait(object0, file_handler0);
		let input1 = EbiInput::Usize(1, &TEST_INPUT_TYPE_USIZE);
		let inputs = vec![input0, input1];

		if let EbiCommand::Command{execute, output_type, ..} = crate::ebi_commands::ebi_command_analyse_non_stochastic::EBI_ANALYSE_NON_STOCHASTIC_CLUSTER {
			match (execute)(inputs, None) {
				Ok(output) => {
					output.test_activity_key();
					assert_eq!(&output.get_type(), output_type);
				}
				Err(e) => Err(e).unwrap(),
			}
		}
	}
	#[test]
	pub fn ebi_anans_clus_test_2() {
		// this test has been indicated as to be expected to fail
		
		let mut reader = MultipleReader::from_file(File::open("./testfiles/empty.xes").unwrap());
		let (object0, file_handler0) = ebi_input::read_as_trait(&("finite language".parse().unwrap()), &mut reader, None, 0).unwrap();
		let input0 = EbiInput::Trait(object0, file_handler0);
		let input1 = EbiInput::Usize(1, &TEST_INPUT_TYPE_USIZE);
		let inputs = vec![input0, input1];

		if let EbiCommand::Command{execute, ..} = crate::ebi_commands::ebi_command_analyse_non_stochastic::EBI_ANALYSE_NON_STOCHASTIC_CLUSTER {
			assert!(((execute)(inputs, None)).is_err())
		}
	}
	#[test]
	pub fn ebi_anans_clus_test_3() {
		// to indicate that this test is expected to fail, add the following to src/tests/fallible_test_list.rs:
		/* 
		(
			&EBI_ANALYSE_NON_STOCHASTIC_CLUSTER,
			&[
				"trait finite language#./testfiles/acb-abc-ad-aded-adeded-adededed.slang",
				"usize 1"			
			]
		),
		*/
		
		let mut reader = MultipleReader::from_file(File::open("./testfiles/acb-abc-ad-aded-adeded-adededed.slang").unwrap());
		let (object0, file_handler0) = ebi_input::read_as_trait(&("finite language".parse().unwrap()), &mut reader, None, 0).unwrap();
		let input0 = EbiInput::Trait(object0, file_handler0);
		let input1 = EbiInput::Usize(1, &TEST_INPUT_TYPE_USIZE);
		let inputs = vec![input0, input1];

		if let EbiCommand::Command{execute, output_type, ..} = crate::ebi_commands::ebi_command_analyse_non_stochastic::EBI_ANALYSE_NON_STOCHASTIC_CLUSTER {
			match (execute)(inputs, None) {
				Ok(output) => {
					output.test_activity_key();
					assert_eq!(&output.get_type(), output_type);
				}
				Err(e) => Err(e).unwrap(),
			}
		}
	}


	// ==== command exe ====
	#[test]
	pub fn ebi_anans_exe_test_0() {
		// to indicate that this test is expected to fail, add the following to src/tests/fallible_test_list.rs:
		/* 
		(
			&EBI_ANALYSE_NON_STOCHASTIC_EXECUTIONS,
			&[
				"trait event log with event attributes#./testfiles/empty.xes",
				"trait semantics#./testfiles/seq(a-xor(b-c)).sptree"			
			]
		),
		*/
		
		let mut reader = MultipleReader::from_file(File::open("./testfiles/empty.xes").unwrap());
		let (object0, file_handler0) = ebi_input::read_as_trait(&("event log with event attributes".parse().unwrap()), &mut reader, None, 0).unwrap();
		let input0 = EbiInput::Trait(object0, file_handler0);
		let mut reader = MultipleReader::from_file(File::open("./testfiles/seq(a-xor(b-c)).sptree").unwrap());
		let (object1, file_handler1) = ebi_input::read_as_trait(&("semantics".parse().unwrap()), &mut reader, None, 0).unwrap();
		let input1 = EbiInput::Trait(object1, file_handler1);
		let inputs = vec![input0, input1];

		if let EbiCommand::Command{execute, output_type, ..} = crate::ebi_commands::ebi_command_analyse_non_stochastic::EBI_ANALYSE_NON_STOCHASTIC_EXECUTIONS {
			match (execute)(inputs, None) {
				Ok(output) => {
					output.test_activity_key();
					assert_eq!(&output.get_type(), output_type);
				}
				Err(e) => Err(e).unwrap(),
			}
		}
	}
	#[test]
	pub fn ebi_anans_exe_test_1() {
		// to indicate that this test is expected to fail, add the following to src/tests/fallible_test_list.rs:
		/* 
		(
			&EBI_ANALYSE_NON_STOCHASTIC_EXECUTIONS,
			&[
				"trait event log with event attributes#./testfiles/empty.xes",
				"trait semantics#./testfiles/ba-aa-ab.slang"			
			]
		),
		*/
		
		let mut reader = MultipleReader::from_file(File::open("./testfiles/empty.xes").unwrap());
		let (object0, file_handler0) = ebi_input::read_as_trait(&("event log with event attributes".parse().unwrap()), &mut reader, None, 0).unwrap();
		let input0 = EbiInput::Trait(object0, file_handler0);
		let mut reader = MultipleReader::from_file(File::open("./testfiles/ba-aa-ab.slang").unwrap());
		let (object1, file_handler1) = ebi_input::read_as_trait(&("semantics".parse().unwrap()), &mut reader, None, 0).unwrap();
		let input1 = EbiInput::Trait(object1, file_handler1);
		let inputs = vec![input0, input1];

		if let EbiCommand::Command{execute, output_type, ..} = crate::ebi_commands::ebi_command_analyse_non_stochastic::EBI_ANALYSE_NON_STOCHASTIC_EXECUTIONS {
			match (execute)(inputs, None) {
				Ok(output) => {
					output.test_activity_key();
					assert_eq!(&output.get_type(), output_type);
				}
				Err(e) => Err(e).unwrap(),
			}
		}
	}
	#[test]
	pub fn ebi_anans_exe_test_2() {
		// to indicate that this test is expected to fail, add the following to src/tests/fallible_test_list.rs:
		/* 
		(
			&EBI_ANALYSE_NON_STOCHASTIC_EXECUTIONS,
			&[
				"trait event log with event attributes#./testfiles/empty.xes",
				"trait semantics#./testfiles/model.bpmn"			
			]
		),
		*/
		
		let mut reader = MultipleReader::from_file(File::open("./testfiles/empty.xes").unwrap());
		let (object0, file_handler0) = ebi_input::read_as_trait(&("event log with event attributes".parse().unwrap()), &mut reader, None, 0).unwrap();
		let input0 = EbiInput::Trait(object0, file_handler0);
		let mut reader = MultipleReader::from_file(File::open("./testfiles/model.bpmn").unwrap());
		let (object1, file_handler1) = ebi_input::read_as_trait(&("semantics".parse().unwrap()), &mut reader, None, 0).unwrap();
		let input1 = EbiInput::Trait(object1, file_handler1);
		let inputs = vec![input0, input1];

		if let EbiCommand::Command{execute, output_type, ..} = crate::ebi_commands::ebi_command_analyse_non_stochastic::EBI_ANALYSE_NON_STOCHASTIC_EXECUTIONS {
			match (execute)(inputs, None) {
				Ok(output) => {
					output.test_activity_key();
					assert_eq!(&output.get_type(), output_type);
				}
				Err(e) => Err(e).unwrap(),
			}
		}
	}
	#[test]
	pub fn ebi_anans_exe_test_3() {
		// to indicate that this test is expected to fail, add the following to src/tests/fallible_test_list.rs:
		/* 
		(
			&EBI_ANALYSE_NON_STOCHASTIC_EXECUTIONS,
			&[
				"trait event log with event attributes#./testfiles/empty.xes",
				"trait semantics#./testfiles/a-b.slang"			
			]
		),
		*/
		
		let mut reader = MultipleReader::from_file(File::open("./testfiles/empty.xes").unwrap());
		let (object0, file_handler0) = ebi_input::read_as_trait(&("event log with event attributes".parse().unwrap()), &mut reader, None, 0).unwrap();
		let input0 = EbiInput::Trait(object0, file_handler0);
		let mut reader = MultipleReader::from_file(File::open("./testfiles/a-b.slang").unwrap());
		let (object1, file_handler1) = ebi_input::read_as_trait(&("semantics".parse().unwrap()), &mut reader, None, 0).unwrap();
		let input1 = EbiInput::Trait(object1, file_handler1);
		let inputs = vec![input0, input1];

		if let EbiCommand::Command{execute, output_type, ..} = crate::ebi_commands::ebi_command_analyse_non_stochastic::EBI_ANALYSE_NON_STOCHASTIC_EXECUTIONS {
			match (execute)(inputs, None) {
				Ok(output) => {
					output.test_activity_key();
					assert_eq!(&output.get_type(), output_type);
				}
				Err(e) => Err(e).unwrap(),
			}
		}
	}


	// ==== command at ====
	#[test]
	pub fn ebi_anans_at_test_0() {
		// to indicate that this test is expected to fail, add the following to src/tests/fallible_test_list.rs:
		/* 
		(
			&EBI_ANALYSE_NON_STOCHASTIC_ANY_TRACES,
			&[
				"object directly follows graph#./testfiles/bpic12-a.xes.gz-dfg.dfg"			
			]
		),
		*/
		
		let mut reader = MultipleReader::from_file(File::open("./testfiles/bpic12-a.xes.gz-dfg.dfg").unwrap());
		let object0 = read_as_object_with_file_handler(&"directly follows graph".parse().unwrap(), &mut reader, None, 0, &mut None, "directly follows graph".parse().unwrap()).unwrap();
		let input0 = EbiInput::Object(object0, "directly follows graph".parse().unwrap());
		let inputs = vec![input0];

		if let EbiCommand::Command{execute, output_type, ..} = crate::ebi_commands::ebi_command_analyse_non_stochastic::EBI_ANALYSE_NON_STOCHASTIC_ANY_TRACES {
			match (execute)(inputs, None) {
				Ok(output) => {
					output.test_activity_key();
					assert_eq!(&output.get_type(), output_type);
				}
				Err(e) => Err(e).unwrap(),
			}
		}
	}
	#[test]
	pub fn ebi_anans_at_test_1() {
		// to indicate that this test is expected to fail, add the following to src/tests/fallible_test_list.rs:
		/* 
		(
			&EBI_ANALYSE_NON_STOCHASTIC_ANY_TRACES,
			&[
				"object directly follows graph#./testfiles/BPI_Challenge_2013_closed_problems.xes.gz-dfg.dfg"			
			]
		),
		*/
		
		let mut reader = MultipleReader::from_file(File::open("./testfiles/BPI_Challenge_2013_closed_problems.xes.gz-dfg.dfg").unwrap());
		let object0 = read_as_object_with_file_handler(&"directly follows graph".parse().unwrap(), &mut reader, None, 0, &mut None, "directly follows graph".parse().unwrap()).unwrap();
		let input0 = EbiInput::Object(object0, "directly follows graph".parse().unwrap());
		let inputs = vec![input0];

		if let EbiCommand::Command{execute, output_type, ..} = crate::ebi_commands::ebi_command_analyse_non_stochastic::EBI_ANALYSE_NON_STOCHASTIC_ANY_TRACES {
			match (execute)(inputs, None) {
				Ok(output) => {
					output.test_activity_key();
					assert_eq!(&output.get_type(), output_type);
				}
				Err(e) => Err(e).unwrap(),
			}
		}
	}
	#[test]
	pub fn ebi_anans_at_test_2() {
		// to indicate that this test is expected to fail, add the following to src/tests/fallible_test_list.rs:
		/* 
		(
			&EBI_ANALYSE_NON_STOCHASTIC_ANY_TRACES,
			&[
				"object directly follows graph#./testfiles/aa-ab-ba.dfg"			
			]
		),
		*/
		
		let mut reader = MultipleReader::from_file(File::open("./testfiles/aa-ab-ba.dfg").unwrap());
		let object0 = read_as_object_with_file_handler(&"directly follows graph".parse().unwrap(), &mut reader, None, 0, &mut None, "directly follows graph".parse().unwrap()).unwrap();
		let input0 = EbiInput::Object(object0, "directly follows graph".parse().unwrap());
		let inputs = vec![input0];

		if let EbiCommand::Command{execute, output_type, ..} = crate::ebi_commands::ebi_command_analyse_non_stochastic::EBI_ANALYSE_NON_STOCHASTIC_ANY_TRACES {
			match (execute)(inputs, None) {
				Ok(output) => {
					output.test_activity_key();
					assert_eq!(&output.get_type(), output_type);
				}
				Err(e) => Err(e).unwrap(),
			}
		}
	}
	#[test]
	pub fn ebi_anans_at_test_3() {
		// to indicate that this test is expected to fail, add the following to src/tests/fallible_test_list.rs:
		/* 
		(
			&EBI_ANALYSE_NON_STOCHASTIC_ANY_TRACES,
			&[
				"object stochastic process tree#./testfiles/seq(a-xor(b-c)).sptree"			
			]
		),
		*/
		
		let mut reader = MultipleReader::from_file(File::open("./testfiles/seq(a-xor(b-c)).sptree").unwrap());
		let object0 = read_as_object_with_file_handler(&"stochastic process tree".parse().unwrap(), &mut reader, None, 0, &mut None, "stochastic process tree".parse().unwrap()).unwrap();
		let input0 = EbiInput::Object(object0, "stochastic process tree".parse().unwrap());
		let inputs = vec![input0];

		if let EbiCommand::Command{execute, output_type, ..} = crate::ebi_commands::ebi_command_analyse_non_stochastic::EBI_ANALYSE_NON_STOCHASTIC_ANY_TRACES {
			match (execute)(inputs, None) {
				Ok(output) => {
					output.test_activity_key();
					assert_eq!(&output.get_type(), output_type);
				}
				Err(e) => Err(e).unwrap(),
			}
		}
	}


	// ==== command inft ====
	#[test]
	pub fn ebi_anans_inft_test_0() {
		// to indicate that this test is expected to fail, add the following to src/tests/fallible_test_list.rs:
		/* 
		(
			&EBI_ANALYSE_NON_STOCHASTIC_INFINITELY_MANY_TRACES,
			&[
				"object event log#./testfiles/empty.xes"			
			]
		),
		*/
		
		let mut reader = MultipleReader::from_file(File::open("./testfiles/empty.xes").unwrap());
		let object0 = read_as_object_with_file_handler(&"event log".parse().unwrap(), &mut reader, None, 0, &mut None, "extensible event stream".parse().unwrap()).unwrap();
		let input0 = EbiInput::Object(object0, "extensible event stream".parse().unwrap());
		let inputs = vec![input0];

		if let EbiCommand::Command{execute, output_type, ..} = crate::ebi_commands::ebi_command_analyse_non_stochastic::EBI_ANALYSE_NON_STOCHASTIC_INFINITELY_MANY_TRACES {
			match (execute)(inputs, None) {
				Ok(output) => {
					output.test_activity_key();
					assert_eq!(&output.get_type(), output_type);
				}
				Err(e) => Err(e).unwrap(),
			}
		}
	}
	#[test]
	pub fn ebi_anans_inft_test_1() {
		// to indicate that this test is expected to fail, add the following to src/tests/fallible_test_list.rs:
		/* 
		(
			&EBI_ANALYSE_NON_STOCHASTIC_INFINITELY_MANY_TRACES,
			&[
				"object event log#./testfiles/simple_log_markovian_abstraction.xes"			
			]
		),
		*/
		
		let mut reader = MultipleReader::from_file(File::open("./testfiles/simple_log_markovian_abstraction.xes").unwrap());
		let object0 = read_as_object_with_file_handler(&"event log".parse().unwrap(), &mut reader, None, 0, &mut None, "extensible event stream".parse().unwrap()).unwrap();
		let input0 = EbiInput::Object(object0, "extensible event stream".parse().unwrap());
		let inputs = vec![input0];

		if let EbiCommand::Command{execute, output_type, ..} = crate::ebi_commands::ebi_command_analyse_non_stochastic::EBI_ANALYSE_NON_STOCHASTIC_INFINITELY_MANY_TRACES {
			match (execute)(inputs, None) {
				Ok(output) => {
					output.test_activity_key();
					assert_eq!(&output.get_type(), output_type);
				}
				Err(e) => Err(e).unwrap(),
			}
		}
	}
	#[test]
	pub fn ebi_anans_inft_test_2() {
		// to indicate that this test is expected to fail, add the following to src/tests/fallible_test_list.rs:
		/* 
		(
			&EBI_ANALYSE_NON_STOCHASTIC_INFINITELY_MANY_TRACES,
			&[
				"object event log#./testfiles/a-b-double.xes"			
			]
		),
		*/
		
		let mut reader = MultipleReader::from_file(File::open("./testfiles/a-b-double.xes").unwrap());
		let object0 = read_as_object_with_file_handler(&"event log".parse().unwrap(), &mut reader, None, 0, &mut None, "extensible event stream".parse().unwrap()).unwrap();
		let input0 = EbiInput::Object(object0, "extensible event stream".parse().unwrap());
		let inputs = vec![input0];

		if let EbiCommand::Command{execute, output_type, ..} = crate::ebi_commands::ebi_command_analyse_non_stochastic::EBI_ANALYSE_NON_STOCHASTIC_INFINITELY_MANY_TRACES {
			match (execute)(inputs, None) {
				Ok(output) => {
					output.test_activity_key();
					assert_eq!(&output.get_type(), output_type);
				}
				Err(e) => Err(e).unwrap(),
			}
		}
	}
	#[test]
	pub fn ebi_anans_inft_test_3() {
		// to indicate that this test is expected to fail, add the following to src/tests/fallible_test_list.rs:
		/* 
		(
			&EBI_ANALYSE_NON_STOCHASTIC_INFINITELY_MANY_TRACES,
			&[
				"object event log#./testfiles/a-b.xes.gz"			
			]
		),
		*/
		
		let mut reader = MultipleReader::from_file(File::open("./testfiles/a-b.xes.gz").unwrap());
		let object0 = read_as_object_with_file_handler(&"event log".parse().unwrap(), &mut reader, None, 0, &mut None, "compressed event log".parse().unwrap()).unwrap();
		let input0 = EbiInput::Object(object0, "compressed event log".parse().unwrap());
		let inputs = vec![input0];

		if let EbiCommand::Command{execute, output_type, ..} = crate::ebi_commands::ebi_command_analyse_non_stochastic::EBI_ANALYSE_NON_STOCHASTIC_INFINITELY_MANY_TRACES {
			match (execute)(inputs, None) {
				Ok(output) => {
					output.test_activity_key();
					assert_eq!(&output.get_type(), output_type);
				}
				Err(e) => Err(e).unwrap(),
			}
		}
	}


	// ==== command med ====
	#[test]
	pub fn ebi_anans_med_test_0() {
		// to indicate that this test is expected to fail, add the following to src/tests/fallible_test_list.rs:
		/* 
		(
			&EBI_ANALYSE_NON_STOCHASTIC_MEDOID,
			&[
				"trait finite language#./testfiles/ba-aa-ab.slang",
				"usize 1"			
			]
		),
		*/
		
		let mut reader = MultipleReader::from_file(File::open("./testfiles/ba-aa-ab.slang").unwrap());
		let (object0, file_handler0) = ebi_input::read_as_trait(&("finite language".parse().unwrap()), &mut reader, None, 0).unwrap();
		let input0 = EbiInput::Trait(object0, file_handler0);
		let input1 = EbiInput::Usize(1, &TEST_INPUT_TYPE_USIZE);
		let inputs = vec![input0, input1];

		if let EbiCommand::Command{execute, output_type, ..} = crate::ebi_commands::ebi_command_analyse_non_stochastic::EBI_ANALYSE_NON_STOCHASTIC_MEDOID {
			match (execute)(inputs, None) {
				Ok(output) => {
					output.test_activity_key();
					assert_eq!(&output.get_type(), output_type);
				}
				Err(e) => Err(e).unwrap(),
			}
		}
	}
	#[test]
	pub fn ebi_anans_med_test_1() {
		// to indicate that this test is expected to fail, add the following to src/tests/fallible_test_list.rs:
		/* 
		(
			&EBI_ANALYSE_NON_STOCHASTIC_MEDOID,
			&[
				"trait finite language#./testfiles/a-b.slang",
				"usize 1"			
			]
		),
		*/
		
		let mut reader = MultipleReader::from_file(File::open("./testfiles/a-b.slang").unwrap());
		let (object0, file_handler0) = ebi_input::read_as_trait(&("finite language".parse().unwrap()), &mut reader, None, 0).unwrap();
		let input0 = EbiInput::Trait(object0, file_handler0);
		let input1 = EbiInput::Usize(1, &TEST_INPUT_TYPE_USIZE);
		let inputs = vec![input0, input1];

		if let EbiCommand::Command{execute, output_type, ..} = crate::ebi_commands::ebi_command_analyse_non_stochastic::EBI_ANALYSE_NON_STOCHASTIC_MEDOID {
			match (execute)(inputs, None) {
				Ok(output) => {
					output.test_activity_key();
					assert_eq!(&output.get_type(), output_type);
				}
				Err(e) => Err(e).unwrap(),
			}
		}
	}
	#[test]
	pub fn ebi_anans_med_test_2() {
		// this test has been indicated as to be expected to fail
		
		let mut reader = MultipleReader::from_file(File::open("./testfiles/empty.xes").unwrap());
		let (object0, file_handler0) = ebi_input::read_as_trait(&("finite language".parse().unwrap()), &mut reader, None, 0).unwrap();
		let input0 = EbiInput::Trait(object0, file_handler0);
		let input1 = EbiInput::Usize(1, &TEST_INPUT_TYPE_USIZE);
		let inputs = vec![input0, input1];

		if let EbiCommand::Command{execute, ..} = crate::ebi_commands::ebi_command_analyse_non_stochastic::EBI_ANALYSE_NON_STOCHASTIC_MEDOID {
			assert!(((execute)(inputs, None)).is_err())
		}
	}
	#[test]
	pub fn ebi_anans_med_test_3() {
		// to indicate that this test is expected to fail, add the following to src/tests/fallible_test_list.rs:
		/* 
		(
			&EBI_ANALYSE_NON_STOCHASTIC_MEDOID,
			&[
				"trait finite language#./testfiles/acb-abc-ad-aded-adeded-adededed.slang",
				"usize 1"			
			]
		),
		*/
		
		let mut reader = MultipleReader::from_file(File::open("./testfiles/acb-abc-ad-aded-adeded-adededed.slang").unwrap());
		let (object0, file_handler0) = ebi_input::read_as_trait(&("finite language".parse().unwrap()), &mut reader, None, 0).unwrap();
		let input0 = EbiInput::Trait(object0, file_handler0);
		let input1 = EbiInput::Usize(1, &TEST_INPUT_TYPE_USIZE);
		let inputs = vec![input0, input1];

		if let EbiCommand::Command{execute, output_type, ..} = crate::ebi_commands::ebi_command_analyse_non_stochastic::EBI_ANALYSE_NON_STOCHASTIC_MEDOID {
			match (execute)(inputs, None) {
				Ok(output) => {
					output.test_activity_key();
					assert_eq!(&output.get_type(), output_type);
				}
				Err(e) => Err(e).unwrap(),
			}
		}
	}


	// ==== command to ====
	#[test]
	pub fn ebi_anans_to_test_0() {
		// this test has been indicated as to be expected to fail
		
		let mut reader = MultipleReader::from_file(File::open("./testfiles/empty.xes").unwrap());
		let (object0, file_handler0) = ebi_input::read_as_trait(&("event log with event attributes".parse().unwrap()), &mut reader, None, 0).unwrap();
		let input0 = EbiInput::Trait(object0, file_handler0);
		let inputs = vec![input0];

		if let EbiCommand::Command{execute, ..} = crate::ebi_commands::ebi_command_analyse_non_stochastic::EBI_ANALYSE_NON_STOCHASTIC_TIMESTAMPS_ORDERED {
			assert!(((execute)(inputs, None)).is_err())
		}
	}
	#[test]
	pub fn ebi_anans_to_test_1() {
		// this test has been indicated as to be expected to fail
		
		let mut reader = MultipleReader::from_file(File::open("./testfiles/simple_log_markovian_abstraction.xes").unwrap());
		let (object0, file_handler0) = ebi_input::read_as_trait(&("event log with event attributes".parse().unwrap()), &mut reader, None, 0).unwrap();
		let input0 = EbiInput::Trait(object0, file_handler0);
		let inputs = vec![input0];

		if let EbiCommand::Command{execute, ..} = crate::ebi_commands::ebi_command_analyse_non_stochastic::EBI_ANALYSE_NON_STOCHASTIC_TIMESTAMPS_ORDERED {
			assert!(((execute)(inputs, None)).is_err())
		}
	}
	#[test]
	pub fn ebi_anans_to_test_2() {
		// this test has been indicated as to be expected to fail
		
		let mut reader = MultipleReader::from_file(File::open("./testfiles/a-b-double.xes").unwrap());
		let (object0, file_handler0) = ebi_input::read_as_trait(&("event log with event attributes".parse().unwrap()), &mut reader, None, 0).unwrap();
		let input0 = EbiInput::Trait(object0, file_handler0);
		let inputs = vec![input0];

		if let EbiCommand::Command{execute, ..} = crate::ebi_commands::ebi_command_analyse_non_stochastic::EBI_ANALYSE_NON_STOCHASTIC_TIMESTAMPS_ORDERED {
			assert!(((execute)(inputs, None)).is_err())
		}
	}
	#[test]
	pub fn ebi_anans_to_test_3() {
		// this test has been indicated as to be expected to fail
		
		let mut reader = MultipleReader::from_file(File::open("./testfiles/a-b.xes.gz").unwrap());
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
		// this test has been indicated as to be expected to fail
		
		let mut reader = MultipleReader::from_file(File::open("./testfiles/empty.xes").unwrap());
		let (object0, file_handler0) = ebi_input::read_as_trait(&("event log with trace attributes".parse().unwrap()), &mut reader, None, 0).unwrap();
		let input0 = EbiInput::Trait(object0, file_handler0);
		let input1 = EbiInput::String("some string".to_string(), &TEST_INPUT_TYPE_STRING);
		let input2 = EbiInput::Usize(10, &TEST_INPUT_TYPE_USIZE);
		let inputs = vec![input0, input1, input2];

		if let EbiCommand::Command{execute, ..} = crate::ebi_commands::ebi_command_association::EBI_ASSOCIATION_ATTRIBUTE {
			assert!(((execute)(inputs, None)).is_err())
		}
	}
	#[test]
	pub fn ebi_asso_att_test_1() {
		// this test has been indicated as to be expected to fail
		
		let mut reader = MultipleReader::from_file(File::open("./testfiles/simple_log_markovian_abstraction.xes").unwrap());
		let (object0, file_handler0) = ebi_input::read_as_trait(&("event log with trace attributes".parse().unwrap()), &mut reader, None, 0).unwrap();
		let input0 = EbiInput::Trait(object0, file_handler0);
		let input1 = EbiInput::String("some string".to_string(), &TEST_INPUT_TYPE_STRING);
		let input2 = EbiInput::Usize(10, &TEST_INPUT_TYPE_USIZE);
		let inputs = vec![input0, input1, input2];

		if let EbiCommand::Command{execute, ..} = crate::ebi_commands::ebi_command_association::EBI_ASSOCIATION_ATTRIBUTE {
			assert!(((execute)(inputs, None)).is_err())
		}
	}
	#[test]
	pub fn ebi_asso_att_test_2() {
		// this test has been indicated as to be expected to fail
		
		let mut reader = MultipleReader::from_file(File::open("./testfiles/a-b-double.xes").unwrap());
		let (object0, file_handler0) = ebi_input::read_as_trait(&("event log with trace attributes".parse().unwrap()), &mut reader, None, 0).unwrap();
		let input0 = EbiInput::Trait(object0, file_handler0);
		let input1 = EbiInput::String("some string".to_string(), &TEST_INPUT_TYPE_STRING);
		let input2 = EbiInput::Usize(10, &TEST_INPUT_TYPE_USIZE);
		let inputs = vec![input0, input1, input2];

		if let EbiCommand::Command{execute, ..} = crate::ebi_commands::ebi_command_association::EBI_ASSOCIATION_ATTRIBUTE {
			assert!(((execute)(inputs, None)).is_err())
		}
	}
	#[test]
	pub fn ebi_asso_att_test_3() {
		// this test has been indicated as to be expected to fail
		
		let mut reader = MultipleReader::from_file(File::open("./testfiles/a-b.xes.gz").unwrap());
		let (object0, file_handler0) = ebi_input::read_as_trait(&("event log with trace attributes".parse().unwrap()), &mut reader, None, 0).unwrap();
		let input0 = EbiInput::Trait(object0, file_handler0);
		let input1 = EbiInput::String("some string".to_string(), &TEST_INPUT_TYPE_STRING);
		let input2 = EbiInput::Usize(10, &TEST_INPUT_TYPE_USIZE);
		let inputs = vec![input0, input1, input2];

		if let EbiCommand::Command{execute, ..} = crate::ebi_commands::ebi_command_association::EBI_ASSOCIATION_ATTRIBUTE {
			assert!(((execute)(inputs, None)).is_err())
		}
	}


	// ==== command atts ====
	#[test]
	pub fn ebi_asso_atts_test_0() {
		// to indicate that this test is expected to fail, add the following to src/tests/fallible_test_list.rs:
		/* 
		(
			&EBI_ASSOCIATION_ATTRIBUTES,
			&[
				"trait event log with trace attributes#./testfiles/empty.xes",
				"usize 10"			
			]
		),
		*/
		
		let mut reader = MultipleReader::from_file(File::open("./testfiles/empty.xes").unwrap());
		let (object0, file_handler0) = ebi_input::read_as_trait(&("event log with trace attributes".parse().unwrap()), &mut reader, None, 0).unwrap();
		let input0 = EbiInput::Trait(object0, file_handler0);
		let input1 = EbiInput::Usize(10, &TEST_INPUT_TYPE_USIZE);
		let inputs = vec![input0, input1];

		if let EbiCommand::Command{execute, output_type, ..} = crate::ebi_commands::ebi_command_association::EBI_ASSOCIATION_ATTRIBUTES {
			match (execute)(inputs, None) {
				Ok(output) => {
					output.test_activity_key();
					assert_eq!(&output.get_type(), output_type);
				}
				Err(e) => Err(e).unwrap(),
			}
		}
	}
	#[test]
	pub fn ebi_asso_atts_test_1() {
		// to indicate that this test is expected to fail, add the following to src/tests/fallible_test_list.rs:
		/* 
		(
			&EBI_ASSOCIATION_ATTRIBUTES,
			&[
				"trait event log with trace attributes#./testfiles/simple_log_markovian_abstraction.xes",
				"usize 10"			
			]
		),
		*/
		
		let mut reader = MultipleReader::from_file(File::open("./testfiles/simple_log_markovian_abstraction.xes").unwrap());
		let (object0, file_handler0) = ebi_input::read_as_trait(&("event log with trace attributes".parse().unwrap()), &mut reader, None, 0).unwrap();
		let input0 = EbiInput::Trait(object0, file_handler0);
		let input1 = EbiInput::Usize(10, &TEST_INPUT_TYPE_USIZE);
		let inputs = vec![input0, input1];

		if let EbiCommand::Command{execute, output_type, ..} = crate::ebi_commands::ebi_command_association::EBI_ASSOCIATION_ATTRIBUTES {
			match (execute)(inputs, None) {
				Ok(output) => {
					output.test_activity_key();
					assert_eq!(&output.get_type(), output_type);
				}
				Err(e) => Err(e).unwrap(),
			}
		}
	}
	#[test]
	pub fn ebi_asso_atts_test_2() {
		// this test has been indicated as to be expected to fail
		
		let mut reader = MultipleReader::from_file(File::open("./testfiles/a-b-double.xes").unwrap());
		let (object0, file_handler0) = ebi_input::read_as_trait(&("event log with trace attributes".parse().unwrap()), &mut reader, None, 0).unwrap();
		let input0 = EbiInput::Trait(object0, file_handler0);
		let input1 = EbiInput::Usize(10, &TEST_INPUT_TYPE_USIZE);
		let inputs = vec![input0, input1];

		if let EbiCommand::Command{execute, ..} = crate::ebi_commands::ebi_command_association::EBI_ASSOCIATION_ATTRIBUTES {
			assert!(((execute)(inputs, None)).is_err())
		}
	}
	#[test]
	pub fn ebi_asso_atts_test_3() {
		// this test has been indicated as to be expected to fail
		
		let mut reader = MultipleReader::from_file(File::open("./testfiles/a-b.xes.gz").unwrap());
		let (object0, file_handler0) = ebi_input::read_as_trait(&("event log with trace attributes".parse().unwrap()), &mut reader, None, 0).unwrap();
		let input0 = EbiInput::Trait(object0, file_handler0);
		let input1 = EbiInput::Usize(10, &TEST_INPUT_TYPE_USIZE);
		let inputs = vec![input0, input1];

		if let EbiCommand::Command{execute, ..} = crate::ebi_commands::ebi_command_association::EBI_ASSOCIATION_ATTRIBUTES {
			assert!(((execute)(inputs, None)).is_err())
		}
	}


	// ==== group conf ====


	// ==== command cssc ====
	#[test]
	pub fn ebi_conf_cssc_test_0() {
		// to indicate that this test is expected to fail, add the following to src/tests/fallible_test_list.rs:
		/* 
		(
			&EBI_CONFORMANCE_CHI_SQUARED,
			&[
				"trait finite stochastic language#./testfiles/ba-aa-ab.slang",
				"trait queriable stochastic language#./testfiles/seq(a-xor(b-c)).sptree"			
			]
		),
		*/
		
		let mut reader = MultipleReader::from_file(File::open("./testfiles/ba-aa-ab.slang").unwrap());
		let (object0, file_handler0) = ebi_input::read_as_trait(&("finite stochastic language".parse().unwrap()), &mut reader, None, 0).unwrap();
		let input0 = EbiInput::Trait(object0, file_handler0);
		let mut reader = MultipleReader::from_file(File::open("./testfiles/seq(a-xor(b-c)).sptree").unwrap());
		let (object1, file_handler1) = ebi_input::read_as_trait(&("queriable stochastic language".parse().unwrap()), &mut reader, None, 0).unwrap();
		let input1 = EbiInput::Trait(object1, file_handler1);
		let inputs = vec![input0, input1];

		if let EbiCommand::Command{execute, output_type, ..} = crate::ebi_commands::ebi_command_conformance::EBI_CONFORMANCE_CHI_SQUARED {
			match (execute)(inputs, None) {
				Ok(output) => {
					output.test_activity_key();
					assert_eq!(&output.get_type(), output_type);
				}
				Err(e) => Err(e).unwrap(),
			}
		}
	}
	#[test]
	pub fn ebi_conf_cssc_test_1() {
		// to indicate that this test is expected to fail, add the following to src/tests/fallible_test_list.rs:
		/* 
		(
			&EBI_CONFORMANCE_CHI_SQUARED,
			&[
				"trait finite stochastic language#./testfiles/ba-aa-ab.slang",
				"trait queriable stochastic language#./testfiles/ba-aa-ab.slang"			
			]
		),
		*/
		
		let mut reader = MultipleReader::from_file(File::open("./testfiles/ba-aa-ab.slang").unwrap());
		let (object0, file_handler0) = ebi_input::read_as_trait(&("finite stochastic language".parse().unwrap()), &mut reader, None, 0).unwrap();
		let input0 = EbiInput::Trait(object0, file_handler0);
		let mut reader = MultipleReader::from_file(File::open("./testfiles/ba-aa-ab.slang").unwrap());
		let (object1, file_handler1) = ebi_input::read_as_trait(&("queriable stochastic language".parse().unwrap()), &mut reader, None, 0).unwrap();
		let input1 = EbiInput::Trait(object1, file_handler1);
		let inputs = vec![input0, input1];

		if let EbiCommand::Command{execute, output_type, ..} = crate::ebi_commands::ebi_command_conformance::EBI_CONFORMANCE_CHI_SQUARED {
			match (execute)(inputs, None) {
				Ok(output) => {
					output.test_activity_key();
					assert_eq!(&output.get_type(), output_type);
				}
				Err(e) => Err(e).unwrap(),
			}
		}
	}
	#[test]
	pub fn ebi_conf_cssc_test_2() {
		// to indicate that this test is expected to fail, add the following to src/tests/fallible_test_list.rs:
		/* 
		(
			&EBI_CONFORMANCE_CHI_SQUARED,
			&[
				"trait finite stochastic language#./testfiles/ba-aa-ab.slang",
				"trait queriable stochastic language#./testfiles/a-b.slang"			
			]
		),
		*/
		
		let mut reader = MultipleReader::from_file(File::open("./testfiles/ba-aa-ab.slang").unwrap());
		let (object0, file_handler0) = ebi_input::read_as_trait(&("finite stochastic language".parse().unwrap()), &mut reader, None, 0).unwrap();
		let input0 = EbiInput::Trait(object0, file_handler0);
		let mut reader = MultipleReader::from_file(File::open("./testfiles/a-b.slang").unwrap());
		let (object1, file_handler1) = ebi_input::read_as_trait(&("queriable stochastic language".parse().unwrap()), &mut reader, None, 0).unwrap();
		let input1 = EbiInput::Trait(object1, file_handler1);
		let inputs = vec![input0, input1];

		if let EbiCommand::Command{execute, output_type, ..} = crate::ebi_commands::ebi_command_conformance::EBI_CONFORMANCE_CHI_SQUARED {
			match (execute)(inputs, None) {
				Ok(output) => {
					output.test_activity_key();
					assert_eq!(&output.get_type(), output_type);
				}
				Err(e) => Err(e).unwrap(),
			}
		}
	}
	#[test]
	pub fn ebi_conf_cssc_test_3() {
		// to indicate that this test is expected to fail, add the following to src/tests/fallible_test_list.rs:
		/* 
		(
			&EBI_CONFORMANCE_CHI_SQUARED,
			&[
				"trait finite stochastic language#./testfiles/ba-aa-ab.slang",
				"trait queriable stochastic language#./testfiles/empty.xes"			
			]
		),
		*/
		
		let mut reader = MultipleReader::from_file(File::open("./testfiles/ba-aa-ab.slang").unwrap());
		let (object0, file_handler0) = ebi_input::read_as_trait(&("finite stochastic language".parse().unwrap()), &mut reader, None, 0).unwrap();
		let input0 = EbiInput::Trait(object0, file_handler0);
		let mut reader = MultipleReader::from_file(File::open("./testfiles/empty.xes").unwrap());
		let (object1, file_handler1) = ebi_input::read_as_trait(&("queriable stochastic language".parse().unwrap()), &mut reader, None, 0).unwrap();
		let input1 = EbiInput::Trait(object1, file_handler1);
		let inputs = vec![input0, input1];

		if let EbiCommand::Command{execute, output_type, ..} = crate::ebi_commands::ebi_command_conformance::EBI_CONFORMANCE_CHI_SQUARED {
			match (execute)(inputs, None) {
				Ok(output) => {
					output.test_activity_key();
					assert_eq!(&output.get_type(), output_type);
				}
				Err(e) => Err(e).unwrap(),
			}
		}
	}


	// ==== command cssc-sample ====
	#[test]
	pub fn ebi_conf_cssc_sample_test_0() {
		// to indicate that this test is expected to fail, add the following to src/tests/fallible_test_list.rs:
		/* 
		(
			&EBI_CONFORMANCE_CHI_SQUARED_SAMPLE,
			&[
				"trait finite stochastic language#./testfiles/ba-aa-ab.slang",
				"trait finite stochastic language#./testfiles/ba-aa-ab.slang",
				"usize 1"			
			]
		),
		*/
		
		let mut reader = MultipleReader::from_file(File::open("./testfiles/ba-aa-ab.slang").unwrap());
		let (object0, file_handler0) = ebi_input::read_as_trait(&("finite stochastic language".parse().unwrap()), &mut reader, None, 0).unwrap();
		let input0 = EbiInput::Trait(object0, file_handler0);
		let mut reader = MultipleReader::from_file(File::open("./testfiles/ba-aa-ab.slang").unwrap());
		let (object1, file_handler1) = ebi_input::read_as_trait(&("finite stochastic language".parse().unwrap()), &mut reader, None, 0).unwrap();
		let input1 = EbiInput::Trait(object1, file_handler1);
		let input2 = EbiInput::Usize(1, &TEST_INPUT_TYPE_USIZE);
		let inputs = vec![input0, input1, input2];

		if let EbiCommand::Command{execute, output_type, ..} = crate::ebi_commands::ebi_command_conformance::EBI_CONFORMANCE_CHI_SQUARED_SAMPLE {
			match (execute)(inputs, None) {
				Ok(output) => {
					output.test_activity_key();
					assert_eq!(&output.get_type(), output_type);
				}
				Err(e) => Err(e).unwrap(),
			}
		}
	}
	#[test]
	pub fn ebi_conf_cssc_sample_test_1() {
		// to indicate that this test is expected to fail, add the following to src/tests/fallible_test_list.rs:
		/* 
		(
			&EBI_CONFORMANCE_CHI_SQUARED_SAMPLE,
			&[
				"trait finite stochastic language#./testfiles/ba-aa-ab.slang",
				"trait finite stochastic language#./testfiles/a-b.slang",
				"usize 1"			
			]
		),
		*/
		
		let mut reader = MultipleReader::from_file(File::open("./testfiles/ba-aa-ab.slang").unwrap());
		let (object0, file_handler0) = ebi_input::read_as_trait(&("finite stochastic language".parse().unwrap()), &mut reader, None, 0).unwrap();
		let input0 = EbiInput::Trait(object0, file_handler0);
		let mut reader = MultipleReader::from_file(File::open("./testfiles/a-b.slang").unwrap());
		let (object1, file_handler1) = ebi_input::read_as_trait(&("finite stochastic language".parse().unwrap()), &mut reader, None, 0).unwrap();
		let input1 = EbiInput::Trait(object1, file_handler1);
		let input2 = EbiInput::Usize(1, &TEST_INPUT_TYPE_USIZE);
		let inputs = vec![input0, input1, input2];

		if let EbiCommand::Command{execute, output_type, ..} = crate::ebi_commands::ebi_command_conformance::EBI_CONFORMANCE_CHI_SQUARED_SAMPLE {
			match (execute)(inputs, None) {
				Ok(output) => {
					output.test_activity_key();
					assert_eq!(&output.get_type(), output_type);
				}
				Err(e) => Err(e).unwrap(),
			}
		}
	}
	#[test]
	pub fn ebi_conf_cssc_sample_test_2() {
		// this test has been indicated as to be expected to fail
		
		let mut reader = MultipleReader::from_file(File::open("./testfiles/ba-aa-ab.slang").unwrap());
		let (object0, file_handler0) = ebi_input::read_as_trait(&("finite stochastic language".parse().unwrap()), &mut reader, None, 0).unwrap();
		let input0 = EbiInput::Trait(object0, file_handler0);
		let mut reader = MultipleReader::from_file(File::open("./testfiles/empty.xes").unwrap());
		let (object1, file_handler1) = ebi_input::read_as_trait(&("finite stochastic language".parse().unwrap()), &mut reader, None, 0).unwrap();
		let input1 = EbiInput::Trait(object1, file_handler1);
		let input2 = EbiInput::Usize(1, &TEST_INPUT_TYPE_USIZE);
		let inputs = vec![input0, input1, input2];

		if let EbiCommand::Command{execute, ..} = crate::ebi_commands::ebi_command_conformance::EBI_CONFORMANCE_CHI_SQUARED_SAMPLE {
			assert!(((execute)(inputs, None)).is_err())
		}
	}
	#[test]
	pub fn ebi_conf_cssc_sample_test_3() {
		// to indicate that this test is expected to fail, add the following to src/tests/fallible_test_list.rs:
		/* 
		(
			&EBI_CONFORMANCE_CHI_SQUARED_SAMPLE,
			&[
				"trait finite stochastic language#./testfiles/ba-aa-ab.slang",
				"trait finite stochastic language#./testfiles/acb-abc-ad-aded-adeded-adededed.slang",
				"usize 1"			
			]
		),
		*/
		
		let mut reader = MultipleReader::from_file(File::open("./testfiles/ba-aa-ab.slang").unwrap());
		let (object0, file_handler0) = ebi_input::read_as_trait(&("finite stochastic language".parse().unwrap()), &mut reader, None, 0).unwrap();
		let input0 = EbiInput::Trait(object0, file_handler0);
		let mut reader = MultipleReader::from_file(File::open("./testfiles/acb-abc-ad-aded-adeded-adededed.slang").unwrap());
		let (object1, file_handler1) = ebi_input::read_as_trait(&("finite stochastic language".parse().unwrap()), &mut reader, None, 0).unwrap();
		let input1 = EbiInput::Trait(object1, file_handler1);
		let input2 = EbiInput::Usize(1, &TEST_INPUT_TYPE_USIZE);
		let inputs = vec![input0, input1, input2];

		if let EbiCommand::Command{execute, output_type, ..} = crate::ebi_commands::ebi_command_conformance::EBI_CONFORMANCE_CHI_SQUARED_SAMPLE {
			match (execute)(inputs, None) {
				Ok(output) => {
					output.test_activity_key();
					assert_eq!(&output.get_type(), output_type);
				}
				Err(e) => Err(e).unwrap(),
			}
		}
	}


	// ==== command emsc ====
	#[test]
	pub fn ebi_conf_emsc_test_0() {
		// to indicate that this test is expected to fail, add the following to src/tests/fallible_test_list.rs:
		/* 
		(
			&EBI_CONFORMANCE_EARTH_MOVERS,
			&[
				"trait finite stochastic language#./testfiles/ba-aa-ab.slang",
				"trait finite stochastic language#./testfiles/ba-aa-ab.slang"			
			]
		),
		*/
		
		let mut reader = MultipleReader::from_file(File::open("./testfiles/ba-aa-ab.slang").unwrap());
		let (object0, file_handler0) = ebi_input::read_as_trait(&("finite stochastic language".parse().unwrap()), &mut reader, None, 0).unwrap();
		let input0 = EbiInput::Trait(object0, file_handler0);
		let mut reader = MultipleReader::from_file(File::open("./testfiles/ba-aa-ab.slang").unwrap());
		let (object1, file_handler1) = ebi_input::read_as_trait(&("finite stochastic language".parse().unwrap()), &mut reader, None, 0).unwrap();
		let input1 = EbiInput::Trait(object1, file_handler1);
		let inputs = vec![input0, input1];

		if let EbiCommand::Command{execute, output_type, ..} = crate::ebi_commands::ebi_command_conformance::EBI_CONFORMANCE_EARTH_MOVERS {
			match (execute)(inputs, None) {
				Ok(output) => {
					output.test_activity_key();
					assert_eq!(&output.get_type(), output_type);
				}
				Err(e) => Err(e).unwrap(),
			}
		}
	}
	#[test]
	pub fn ebi_conf_emsc_test_1() {
		// to indicate that this test is expected to fail, add the following to src/tests/fallible_test_list.rs:
		/* 
		(
			&EBI_CONFORMANCE_EARTH_MOVERS,
			&[
				"trait finite stochastic language#./testfiles/ba-aa-ab.slang",
				"trait finite stochastic language#./testfiles/a-b.slang"			
			]
		),
		*/
		
		let mut reader = MultipleReader::from_file(File::open("./testfiles/ba-aa-ab.slang").unwrap());
		let (object0, file_handler0) = ebi_input::read_as_trait(&("finite stochastic language".parse().unwrap()), &mut reader, None, 0).unwrap();
		let input0 = EbiInput::Trait(object0, file_handler0);
		let mut reader = MultipleReader::from_file(File::open("./testfiles/a-b.slang").unwrap());
		let (object1, file_handler1) = ebi_input::read_as_trait(&("finite stochastic language".parse().unwrap()), &mut reader, None, 0).unwrap();
		let input1 = EbiInput::Trait(object1, file_handler1);
		let inputs = vec![input0, input1];

		if let EbiCommand::Command{execute, output_type, ..} = crate::ebi_commands::ebi_command_conformance::EBI_CONFORMANCE_EARTH_MOVERS {
			match (execute)(inputs, None) {
				Ok(output) => {
					output.test_activity_key();
					assert_eq!(&output.get_type(), output_type);
				}
				Err(e) => Err(e).unwrap(),
			}
		}
	}
	#[test]
	pub fn ebi_conf_emsc_test_2() {
		// this test has been indicated as to be expected to fail
		
		let mut reader = MultipleReader::from_file(File::open("./testfiles/ba-aa-ab.slang").unwrap());
		let (object0, file_handler0) = ebi_input::read_as_trait(&("finite stochastic language".parse().unwrap()), &mut reader, None, 0).unwrap();
		let input0 = EbiInput::Trait(object0, file_handler0);
		let mut reader = MultipleReader::from_file(File::open("./testfiles/empty.xes").unwrap());
		let (object1, file_handler1) = ebi_input::read_as_trait(&("finite stochastic language".parse().unwrap()), &mut reader, None, 0).unwrap();
		let input1 = EbiInput::Trait(object1, file_handler1);
		let inputs = vec![input0, input1];

		if let EbiCommand::Command{execute, ..} = crate::ebi_commands::ebi_command_conformance::EBI_CONFORMANCE_EARTH_MOVERS {
			assert!(((execute)(inputs, None)).is_err())
		}
	}
	#[test]
	pub fn ebi_conf_emsc_test_3() {
		// to indicate that this test is expected to fail, add the following to src/tests/fallible_test_list.rs:
		/* 
		(
			&EBI_CONFORMANCE_EARTH_MOVERS,
			&[
				"trait finite stochastic language#./testfiles/ba-aa-ab.slang",
				"trait finite stochastic language#./testfiles/acb-abc-ad-aded-adeded-adededed.slang"			
			]
		),
		*/
		
		let mut reader = MultipleReader::from_file(File::open("./testfiles/ba-aa-ab.slang").unwrap());
		let (object0, file_handler0) = ebi_input::read_as_trait(&("finite stochastic language".parse().unwrap()), &mut reader, None, 0).unwrap();
		let input0 = EbiInput::Trait(object0, file_handler0);
		let mut reader = MultipleReader::from_file(File::open("./testfiles/acb-abc-ad-aded-adeded-adededed.slang").unwrap());
		let (object1, file_handler1) = ebi_input::read_as_trait(&("finite stochastic language".parse().unwrap()), &mut reader, None, 0).unwrap();
		let input1 = EbiInput::Trait(object1, file_handler1);
		let inputs = vec![input0, input1];

		if let EbiCommand::Command{execute, output_type, ..} = crate::ebi_commands::ebi_command_conformance::EBI_CONFORMANCE_EARTH_MOVERS {
			match (execute)(inputs, None) {
				Ok(output) => {
					output.test_activity_key();
					assert_eq!(&output.get_type(), output_type);
				}
				Err(e) => Err(e).unwrap(),
			}
		}
	}


	// ==== command emsc-sample ====
	#[test]
	pub fn ebi_conf_emsc_sample_test_0() {
		// to indicate that this test is expected to fail, add the following to src/tests/fallible_test_list.rs:
		/* 
		(
			&EBI_CONFORMANCE_EARTH_MOVERS_SAMPLE,
			&[
				"trait finite stochastic language#./testfiles/ba-aa-ab.slang",
				"trait finite stochastic language#./testfiles/ba-aa-ab.slang",
				"usize 1"			
			]
		),
		*/
		
		let mut reader = MultipleReader::from_file(File::open("./testfiles/ba-aa-ab.slang").unwrap());
		let (object0, file_handler0) = ebi_input::read_as_trait(&("finite stochastic language".parse().unwrap()), &mut reader, None, 0).unwrap();
		let input0 = EbiInput::Trait(object0, file_handler0);
		let mut reader = MultipleReader::from_file(File::open("./testfiles/ba-aa-ab.slang").unwrap());
		let (object1, file_handler1) = ebi_input::read_as_trait(&("finite stochastic language".parse().unwrap()), &mut reader, None, 0).unwrap();
		let input1 = EbiInput::Trait(object1, file_handler1);
		let input2 = EbiInput::Usize(1, &TEST_INPUT_TYPE_USIZE);
		let inputs = vec![input0, input1, input2];

		if let EbiCommand::Command{execute, output_type, ..} = crate::ebi_commands::ebi_command_conformance::EBI_CONFORMANCE_EARTH_MOVERS_SAMPLE {
			match (execute)(inputs, None) {
				Ok(output) => {
					output.test_activity_key();
					assert_eq!(&output.get_type(), output_type);
				}
				Err(e) => Err(e).unwrap(),
			}
		}
	}
	#[test]
	pub fn ebi_conf_emsc_sample_test_1() {
		// to indicate that this test is expected to fail, add the following to src/tests/fallible_test_list.rs:
		/* 
		(
			&EBI_CONFORMANCE_EARTH_MOVERS_SAMPLE,
			&[
				"trait finite stochastic language#./testfiles/ba-aa-ab.slang",
				"trait finite stochastic language#./testfiles/a-b.slang",
				"usize 1"			
			]
		),
		*/
		
		let mut reader = MultipleReader::from_file(File::open("./testfiles/ba-aa-ab.slang").unwrap());
		let (object0, file_handler0) = ebi_input::read_as_trait(&("finite stochastic language".parse().unwrap()), &mut reader, None, 0).unwrap();
		let input0 = EbiInput::Trait(object0, file_handler0);
		let mut reader = MultipleReader::from_file(File::open("./testfiles/a-b.slang").unwrap());
		let (object1, file_handler1) = ebi_input::read_as_trait(&("finite stochastic language".parse().unwrap()), &mut reader, None, 0).unwrap();
		let input1 = EbiInput::Trait(object1, file_handler1);
		let input2 = EbiInput::Usize(1, &TEST_INPUT_TYPE_USIZE);
		let inputs = vec![input0, input1, input2];

		if let EbiCommand::Command{execute, output_type, ..} = crate::ebi_commands::ebi_command_conformance::EBI_CONFORMANCE_EARTH_MOVERS_SAMPLE {
			match (execute)(inputs, None) {
				Ok(output) => {
					output.test_activity_key();
					assert_eq!(&output.get_type(), output_type);
				}
				Err(e) => Err(e).unwrap(),
			}
		}
	}
	#[test]
	pub fn ebi_conf_emsc_sample_test_2() {
		// this test has been indicated as to be expected to fail
		
		let mut reader = MultipleReader::from_file(File::open("./testfiles/ba-aa-ab.slang").unwrap());
		let (object0, file_handler0) = ebi_input::read_as_trait(&("finite stochastic language".parse().unwrap()), &mut reader, None, 0).unwrap();
		let input0 = EbiInput::Trait(object0, file_handler0);
		let mut reader = MultipleReader::from_file(File::open("./testfiles/empty.xes").unwrap());
		let (object1, file_handler1) = ebi_input::read_as_trait(&("finite stochastic language".parse().unwrap()), &mut reader, None, 0).unwrap();
		let input1 = EbiInput::Trait(object1, file_handler1);
		let input2 = EbiInput::Usize(1, &TEST_INPUT_TYPE_USIZE);
		let inputs = vec![input0, input1, input2];

		if let EbiCommand::Command{execute, ..} = crate::ebi_commands::ebi_command_conformance::EBI_CONFORMANCE_EARTH_MOVERS_SAMPLE {
			assert!(((execute)(inputs, None)).is_err())
		}
	}
	#[test]
	pub fn ebi_conf_emsc_sample_test_3() {
		// to indicate that this test is expected to fail, add the following to src/tests/fallible_test_list.rs:
		/* 
		(
			&EBI_CONFORMANCE_EARTH_MOVERS_SAMPLE,
			&[
				"trait finite stochastic language#./testfiles/ba-aa-ab.slang",
				"trait finite stochastic language#./testfiles/acb-abc-ad-aded-adeded-adededed.slang",
				"usize 1"			
			]
		),
		*/
		
		let mut reader = MultipleReader::from_file(File::open("./testfiles/ba-aa-ab.slang").unwrap());
		let (object0, file_handler0) = ebi_input::read_as_trait(&("finite stochastic language".parse().unwrap()), &mut reader, None, 0).unwrap();
		let input0 = EbiInput::Trait(object0, file_handler0);
		let mut reader = MultipleReader::from_file(File::open("./testfiles/acb-abc-ad-aded-adeded-adededed.slang").unwrap());
		let (object1, file_handler1) = ebi_input::read_as_trait(&("finite stochastic language".parse().unwrap()), &mut reader, None, 0).unwrap();
		let input1 = EbiInput::Trait(object1, file_handler1);
		let input2 = EbiInput::Usize(1, &TEST_INPUT_TYPE_USIZE);
		let inputs = vec![input0, input1, input2];

		if let EbiCommand::Command{execute, output_type, ..} = crate::ebi_commands::ebi_command_conformance::EBI_CONFORMANCE_EARTH_MOVERS_SAMPLE {
			match (execute)(inputs, None) {
				Ok(output) => {
					output.test_activity_key();
					assert_eq!(&output.get_type(), output_type);
				}
				Err(e) => Err(e).unwrap(),
			}
		}
	}


	// ==== command er ====
	#[test]
	pub fn ebi_conf_er_test_0() {
		// to indicate that this test is expected to fail, add the following to src/tests/fallible_test_list.rs:
		/* 
		(
			&EBI_CONFORMANCE_ENTROPIC_RELEVANCE,
			&[
				"trait finite stochastic language#./testfiles/ba-aa-ab.slang",
				"trait queriable stochastic language#./testfiles/seq(a-xor(b-c)).sptree"			
			]
		),
		*/
		
		let mut reader = MultipleReader::from_file(File::open("./testfiles/ba-aa-ab.slang").unwrap());
		let (object0, file_handler0) = ebi_input::read_as_trait(&("finite stochastic language".parse().unwrap()), &mut reader, None, 0).unwrap();
		let input0 = EbiInput::Trait(object0, file_handler0);
		let mut reader = MultipleReader::from_file(File::open("./testfiles/seq(a-xor(b-c)).sptree").unwrap());
		let (object1, file_handler1) = ebi_input::read_as_trait(&("queriable stochastic language".parse().unwrap()), &mut reader, None, 0).unwrap();
		let input1 = EbiInput::Trait(object1, file_handler1);
		let inputs = vec![input0, input1];

		if let EbiCommand::Command{execute, output_type, ..} = crate::ebi_commands::ebi_command_conformance::EBI_CONFORMANCE_ENTROPIC_RELEVANCE {
			match (execute)(inputs, None) {
				Ok(output) => {
					output.test_activity_key();
					assert_eq!(&output.get_type(), output_type);
				}
				Err(e) => Err(e).unwrap(),
			}
		}
	}
	#[test]
	pub fn ebi_conf_er_test_1() {
		// to indicate that this test is expected to fail, add the following to src/tests/fallible_test_list.rs:
		/* 
		(
			&EBI_CONFORMANCE_ENTROPIC_RELEVANCE,
			&[
				"trait finite stochastic language#./testfiles/ba-aa-ab.slang",
				"trait queriable stochastic language#./testfiles/ba-aa-ab.slang"			
			]
		),
		*/
		
		let mut reader = MultipleReader::from_file(File::open("./testfiles/ba-aa-ab.slang").unwrap());
		let (object0, file_handler0) = ebi_input::read_as_trait(&("finite stochastic language".parse().unwrap()), &mut reader, None, 0).unwrap();
		let input0 = EbiInput::Trait(object0, file_handler0);
		let mut reader = MultipleReader::from_file(File::open("./testfiles/ba-aa-ab.slang").unwrap());
		let (object1, file_handler1) = ebi_input::read_as_trait(&("queriable stochastic language".parse().unwrap()), &mut reader, None, 0).unwrap();
		let input1 = EbiInput::Trait(object1, file_handler1);
		let inputs = vec![input0, input1];

		if let EbiCommand::Command{execute, output_type, ..} = crate::ebi_commands::ebi_command_conformance::EBI_CONFORMANCE_ENTROPIC_RELEVANCE {
			match (execute)(inputs, None) {
				Ok(output) => {
					output.test_activity_key();
					assert_eq!(&output.get_type(), output_type);
				}
				Err(e) => Err(e).unwrap(),
			}
		}
	}
	#[test]
	pub fn ebi_conf_er_test_2() {
		// to indicate that this test is expected to fail, add the following to src/tests/fallible_test_list.rs:
		/* 
		(
			&EBI_CONFORMANCE_ENTROPIC_RELEVANCE,
			&[
				"trait finite stochastic language#./testfiles/ba-aa-ab.slang",
				"trait queriable stochastic language#./testfiles/a-b.slang"			
			]
		),
		*/
		
		let mut reader = MultipleReader::from_file(File::open("./testfiles/ba-aa-ab.slang").unwrap());
		let (object0, file_handler0) = ebi_input::read_as_trait(&("finite stochastic language".parse().unwrap()), &mut reader, None, 0).unwrap();
		let input0 = EbiInput::Trait(object0, file_handler0);
		let mut reader = MultipleReader::from_file(File::open("./testfiles/a-b.slang").unwrap());
		let (object1, file_handler1) = ebi_input::read_as_trait(&("queriable stochastic language".parse().unwrap()), &mut reader, None, 0).unwrap();
		let input1 = EbiInput::Trait(object1, file_handler1);
		let inputs = vec![input0, input1];

		if let EbiCommand::Command{execute, output_type, ..} = crate::ebi_commands::ebi_command_conformance::EBI_CONFORMANCE_ENTROPIC_RELEVANCE {
			match (execute)(inputs, None) {
				Ok(output) => {
					output.test_activity_key();
					assert_eq!(&output.get_type(), output_type);
				}
				Err(e) => Err(e).unwrap(),
			}
		}
	}
	#[test]
	pub fn ebi_conf_er_test_3() {
		// to indicate that this test is expected to fail, add the following to src/tests/fallible_test_list.rs:
		/* 
		(
			&EBI_CONFORMANCE_ENTROPIC_RELEVANCE,
			&[
				"trait finite stochastic language#./testfiles/ba-aa-ab.slang",
				"trait queriable stochastic language#./testfiles/empty.xes"			
			]
		),
		*/
		
		let mut reader = MultipleReader::from_file(File::open("./testfiles/ba-aa-ab.slang").unwrap());
		let (object0, file_handler0) = ebi_input::read_as_trait(&("finite stochastic language".parse().unwrap()), &mut reader, None, 0).unwrap();
		let input0 = EbiInput::Trait(object0, file_handler0);
		let mut reader = MultipleReader::from_file(File::open("./testfiles/empty.xes").unwrap());
		let (object1, file_handler1) = ebi_input::read_as_trait(&("queriable stochastic language".parse().unwrap()), &mut reader, None, 0).unwrap();
		let input1 = EbiInput::Trait(object1, file_handler1);
		let inputs = vec![input0, input1];

		if let EbiCommand::Command{execute, output_type, ..} = crate::ebi_commands::ebi_command_conformance::EBI_CONFORMANCE_ENTROPIC_RELEVANCE {
			match (execute)(inputs, None) {
				Ok(output) => {
					output.test_activity_key();
					assert_eq!(&output.get_type(), output_type);
				}
				Err(e) => Err(e).unwrap(),
			}
		}
	}


	// ==== command gp ====
	#[test]
	pub fn ebi_conf_gp_test_0() {
		// to indicate that this test is expected to fail, add the following to src/tests/fallible_test_list.rs:
		/* 
		(
			&EBI_CONFORMANCE_GAIN_PRECISION,
			&[
				"trait finite stochastic language#./testfiles/ba-aa-ab.slang",
				"object stochastic deterministic finite automaton#./testfiles/ba-aa-ab.slang",
				"fraction 0"			
			]
		),
		*/
		if ebi_objects::ebi_arithmetic::is_exact_globally() {
			return;
		}
		let mut reader = MultipleReader::from_file(File::open("./testfiles/ba-aa-ab.slang").unwrap());
		let (object0, file_handler0) = ebi_input::read_as_trait(&("finite stochastic language".parse().unwrap()), &mut reader, None, 0).unwrap();
		let input0 = EbiInput::Trait(object0, file_handler0);
		let mut reader = MultipleReader::from_file(File::open("./testfiles/ba-aa-ab.slang").unwrap());
		let object1 = read_as_object_with_file_handler(&"stochastic deterministic finite automaton".parse().unwrap(), &mut reader, None, 0, &mut None, "finite stochastic language".parse().unwrap()).unwrap();
		let input1 = EbiInput::Object(object1, "finite stochastic language".parse().unwrap());
		let input2 = EbiInput::Fraction("0".parse().unwrap(), &TEST_INPUT_TYPE_FRACTION);
		let inputs = vec![input0, input1, input2];

		if let EbiCommand::Command{execute, output_type, ..} = crate::ebi_commands::ebi_command_conformance::EBI_CONFORMANCE_GAIN_PRECISION {
			match (execute)(inputs, None) {
				Ok(output) => {
					output.test_activity_key();
					assert_eq!(&output.get_type(), output_type);
				}
				Err(e) => Err(e).unwrap(),
			}
		}
	}
	#[test]
	pub fn ebi_conf_gp_test_1() {
		// to indicate that this test is expected to fail, add the following to src/tests/fallible_test_list.rs:
		/* 
		(
			&EBI_CONFORMANCE_GAIN_PRECISION,
			&[
				"trait finite stochastic language#./testfiles/ba-aa-ab.slang",
				"object stochastic deterministic finite automaton#./testfiles/a-b.slang",
				"fraction 0"			
			]
		),
		*/
		if ebi_objects::ebi_arithmetic::is_exact_globally() {
			return;
		}
		let mut reader = MultipleReader::from_file(File::open("./testfiles/ba-aa-ab.slang").unwrap());
		let (object0, file_handler0) = ebi_input::read_as_trait(&("finite stochastic language".parse().unwrap()), &mut reader, None, 0).unwrap();
		let input0 = EbiInput::Trait(object0, file_handler0);
		let mut reader = MultipleReader::from_file(File::open("./testfiles/a-b.slang").unwrap());
		let object1 = read_as_object_with_file_handler(&"stochastic deterministic finite automaton".parse().unwrap(), &mut reader, None, 0, &mut None, "finite stochastic language".parse().unwrap()).unwrap();
		let input1 = EbiInput::Object(object1, "finite stochastic language".parse().unwrap());
		let input2 = EbiInput::Fraction("0".parse().unwrap(), &TEST_INPUT_TYPE_FRACTION);
		let inputs = vec![input0, input1, input2];

		if let EbiCommand::Command{execute, output_type, ..} = crate::ebi_commands::ebi_command_conformance::EBI_CONFORMANCE_GAIN_PRECISION {
			match (execute)(inputs, None) {
				Ok(output) => {
					output.test_activity_key();
					assert_eq!(&output.get_type(), output_type);
				}
				Err(e) => Err(e).unwrap(),
			}
		}
	}
	#[test]
	pub fn ebi_conf_gp_test_2() {
		// to indicate that this test is expected to fail, add the following to src/tests/fallible_test_list.rs:
		/* 
		(
			&EBI_CONFORMANCE_GAIN_PRECISION,
			&[
				"trait finite stochastic language#./testfiles/ba-aa-ab.slang",
				"object stochastic deterministic finite automaton#./testfiles/empty.xes",
				"fraction 0"			
			]
		),
		*/
		if ebi_objects::ebi_arithmetic::is_exact_globally() {
			return;
		}
		let mut reader = MultipleReader::from_file(File::open("./testfiles/ba-aa-ab.slang").unwrap());
		let (object0, file_handler0) = ebi_input::read_as_trait(&("finite stochastic language".parse().unwrap()), &mut reader, None, 0).unwrap();
		let input0 = EbiInput::Trait(object0, file_handler0);
		let mut reader = MultipleReader::from_file(File::open("./testfiles/empty.xes").unwrap());
		let object1 = read_as_object_with_file_handler(&"stochastic deterministic finite automaton".parse().unwrap(), &mut reader, None, 0, &mut None, "extensible event stream".parse().unwrap()).unwrap();
		let input1 = EbiInput::Object(object1, "extensible event stream".parse().unwrap());
		let input2 = EbiInput::Fraction("0".parse().unwrap(), &TEST_INPUT_TYPE_FRACTION);
		let inputs = vec![input0, input1, input2];

		if let EbiCommand::Command{execute, output_type, ..} = crate::ebi_commands::ebi_command_conformance::EBI_CONFORMANCE_GAIN_PRECISION {
			match (execute)(inputs, None) {
				Ok(output) => {
					output.test_activity_key();
					assert_eq!(&output.get_type(), output_type);
				}
				Err(e) => Err(e).unwrap(),
			}
		}
	}
	#[test]
	pub fn ebi_conf_gp_test_3() {
		// to indicate that this test is expected to fail, add the following to src/tests/fallible_test_list.rs:
		/* 
		(
			&EBI_CONFORMANCE_GAIN_PRECISION,
			&[
				"trait finite stochastic language#./testfiles/ba-aa-ab.slang",
				"object stochastic deterministic finite automaton#./testfiles/acb-abc-ad-aded-adeded-adededed.slang",
				"fraction 0"			
			]
		),
		*/
		if ebi_objects::ebi_arithmetic::is_exact_globally() {
			return;
		}
		let mut reader = MultipleReader::from_file(File::open("./testfiles/ba-aa-ab.slang").unwrap());
		let (object0, file_handler0) = ebi_input::read_as_trait(&("finite stochastic language".parse().unwrap()), &mut reader, None, 0).unwrap();
		let input0 = EbiInput::Trait(object0, file_handler0);
		let mut reader = MultipleReader::from_file(File::open("./testfiles/acb-abc-ad-aded-adeded-adededed.slang").unwrap());
		let object1 = read_as_object_with_file_handler(&"stochastic deterministic finite automaton".parse().unwrap(), &mut reader, None, 0, &mut None, "finite stochastic language".parse().unwrap()).unwrap();
		let input1 = EbiInput::Object(object1, "finite stochastic language".parse().unwrap());
		let input2 = EbiInput::Fraction("0".parse().unwrap(), &TEST_INPUT_TYPE_FRACTION);
		let inputs = vec![input0, input1, input2];

		if let EbiCommand::Command{execute, output_type, ..} = crate::ebi_commands::ebi_command_conformance::EBI_CONFORMANCE_GAIN_PRECISION {
			match (execute)(inputs, None) {
				Ok(output) => {
					output.test_activity_key();
					assert_eq!(&output.get_type(), output_type);
				}
				Err(e) => Err(e).unwrap(),
			}
		}
	}


	// ==== command gr ====
	#[test]
	pub fn ebi_conf_gr_test_0() {
		// to indicate that this test is expected to fail, add the following to src/tests/fallible_test_list.rs:
		/* 
		(
			&EBI_CONFORMANCE_GAIN_RECALL,
			&[
				"trait finite stochastic language#./testfiles/ba-aa-ab.slang",
				"object stochastic deterministic finite automaton#./testfiles/ba-aa-ab.slang",
				"fraction 0"			
			]
		),
		*/
		if ebi_objects::ebi_arithmetic::is_exact_globally() {
			return;
		}
		let mut reader = MultipleReader::from_file(File::open("./testfiles/ba-aa-ab.slang").unwrap());
		let (object0, file_handler0) = ebi_input::read_as_trait(&("finite stochastic language".parse().unwrap()), &mut reader, None, 0).unwrap();
		let input0 = EbiInput::Trait(object0, file_handler0);
		let mut reader = MultipleReader::from_file(File::open("./testfiles/ba-aa-ab.slang").unwrap());
		let object1 = read_as_object_with_file_handler(&"stochastic deterministic finite automaton".parse().unwrap(), &mut reader, None, 0, &mut None, "finite stochastic language".parse().unwrap()).unwrap();
		let input1 = EbiInput::Object(object1, "finite stochastic language".parse().unwrap());
		let input2 = EbiInput::Fraction("0".parse().unwrap(), &TEST_INPUT_TYPE_FRACTION);
		let inputs = vec![input0, input1, input2];

		if let EbiCommand::Command{execute, output_type, ..} = crate::ebi_commands::ebi_command_conformance::EBI_CONFORMANCE_GAIN_RECALL {
			match (execute)(inputs, None) {
				Ok(output) => {
					output.test_activity_key();
					assert_eq!(&output.get_type(), output_type);
				}
				Err(e) => Err(e).unwrap(),
			}
		}
	}
	#[test]
	pub fn ebi_conf_gr_test_1() {
		// to indicate that this test is expected to fail, add the following to src/tests/fallible_test_list.rs:
		/* 
		(
			&EBI_CONFORMANCE_GAIN_RECALL,
			&[
				"trait finite stochastic language#./testfiles/ba-aa-ab.slang",
				"object stochastic deterministic finite automaton#./testfiles/a-b.slang",
				"fraction 0"			
			]
		),
		*/
		if ebi_objects::ebi_arithmetic::is_exact_globally() {
			return;
		}
		let mut reader = MultipleReader::from_file(File::open("./testfiles/ba-aa-ab.slang").unwrap());
		let (object0, file_handler0) = ebi_input::read_as_trait(&("finite stochastic language".parse().unwrap()), &mut reader, None, 0).unwrap();
		let input0 = EbiInput::Trait(object0, file_handler0);
		let mut reader = MultipleReader::from_file(File::open("./testfiles/a-b.slang").unwrap());
		let object1 = read_as_object_with_file_handler(&"stochastic deterministic finite automaton".parse().unwrap(), &mut reader, None, 0, &mut None, "finite stochastic language".parse().unwrap()).unwrap();
		let input1 = EbiInput::Object(object1, "finite stochastic language".parse().unwrap());
		let input2 = EbiInput::Fraction("0".parse().unwrap(), &TEST_INPUT_TYPE_FRACTION);
		let inputs = vec![input0, input1, input2];

		if let EbiCommand::Command{execute, output_type, ..} = crate::ebi_commands::ebi_command_conformance::EBI_CONFORMANCE_GAIN_RECALL {
			match (execute)(inputs, None) {
				Ok(output) => {
					output.test_activity_key();
					assert_eq!(&output.get_type(), output_type);
				}
				Err(e) => Err(e).unwrap(),
			}
		}
	}
	#[test]
	pub fn ebi_conf_gr_test_2() {
		// to indicate that this test is expected to fail, add the following to src/tests/fallible_test_list.rs:
		/* 
		(
			&EBI_CONFORMANCE_GAIN_RECALL,
			&[
				"trait finite stochastic language#./testfiles/ba-aa-ab.slang",
				"object stochastic deterministic finite automaton#./testfiles/empty.xes",
				"fraction 0"			
			]
		),
		*/
		if ebi_objects::ebi_arithmetic::is_exact_globally() {
			return;
		}
		let mut reader = MultipleReader::from_file(File::open("./testfiles/ba-aa-ab.slang").unwrap());
		let (object0, file_handler0) = ebi_input::read_as_trait(&("finite stochastic language".parse().unwrap()), &mut reader, None, 0).unwrap();
		let input0 = EbiInput::Trait(object0, file_handler0);
		let mut reader = MultipleReader::from_file(File::open("./testfiles/empty.xes").unwrap());
		let object1 = read_as_object_with_file_handler(&"stochastic deterministic finite automaton".parse().unwrap(), &mut reader, None, 0, &mut None, "extensible event stream".parse().unwrap()).unwrap();
		let input1 = EbiInput::Object(object1, "extensible event stream".parse().unwrap());
		let input2 = EbiInput::Fraction("0".parse().unwrap(), &TEST_INPUT_TYPE_FRACTION);
		let inputs = vec![input0, input1, input2];

		if let EbiCommand::Command{execute, output_type, ..} = crate::ebi_commands::ebi_command_conformance::EBI_CONFORMANCE_GAIN_RECALL {
			match (execute)(inputs, None) {
				Ok(output) => {
					output.test_activity_key();
					assert_eq!(&output.get_type(), output_type);
				}
				Err(e) => Err(e).unwrap(),
			}
		}
	}
	#[test]
	pub fn ebi_conf_gr_test_3() {
		// to indicate that this test is expected to fail, add the following to src/tests/fallible_test_list.rs:
		/* 
		(
			&EBI_CONFORMANCE_GAIN_RECALL,
			&[
				"trait finite stochastic language#./testfiles/ba-aa-ab.slang",
				"object stochastic deterministic finite automaton#./testfiles/acb-abc-ad-aded-adeded-adededed.slang",
				"fraction 0"			
			]
		),
		*/
		if ebi_objects::ebi_arithmetic::is_exact_globally() {
			return;
		}
		let mut reader = MultipleReader::from_file(File::open("./testfiles/ba-aa-ab.slang").unwrap());
		let (object0, file_handler0) = ebi_input::read_as_trait(&("finite stochastic language".parse().unwrap()), &mut reader, None, 0).unwrap();
		let input0 = EbiInput::Trait(object0, file_handler0);
		let mut reader = MultipleReader::from_file(File::open("./testfiles/acb-abc-ad-aded-adeded-adededed.slang").unwrap());
		let object1 = read_as_object_with_file_handler(&"stochastic deterministic finite automaton".parse().unwrap(), &mut reader, None, 0, &mut None, "finite stochastic language".parse().unwrap()).unwrap();
		let input1 = EbiInput::Object(object1, "finite stochastic language".parse().unwrap());
		let input2 = EbiInput::Fraction("0".parse().unwrap(), &TEST_INPUT_TYPE_FRACTION);
		let inputs = vec![input0, input1, input2];

		if let EbiCommand::Command{execute, output_type, ..} = crate::ebi_commands::ebi_command_conformance::EBI_CONFORMANCE_GAIN_RECALL {
			match (execute)(inputs, None) {
				Ok(output) => {
					output.test_activity_key();
					assert_eq!(&output.get_type(), output_type);
				}
				Err(e) => Err(e).unwrap(),
			}
		}
	}


	// ==== command hsc ====
	#[test]
	pub fn ebi_conf_hsc_test_0() {
		// to indicate that this test is expected to fail, add the following to src/tests/fallible_test_list.rs:
		/* 
		(
			&EBI_CONFORMANCE_HELLINGER,
			&[
				"trait finite stochastic language#./testfiles/ba-aa-ab.slang",
				"trait queriable stochastic language#./testfiles/seq(a-xor(b-c)).sptree"			
			]
		),
		*/
		
		let mut reader = MultipleReader::from_file(File::open("./testfiles/ba-aa-ab.slang").unwrap());
		let (object0, file_handler0) = ebi_input::read_as_trait(&("finite stochastic language".parse().unwrap()), &mut reader, None, 0).unwrap();
		let input0 = EbiInput::Trait(object0, file_handler0);
		let mut reader = MultipleReader::from_file(File::open("./testfiles/seq(a-xor(b-c)).sptree").unwrap());
		let (object1, file_handler1) = ebi_input::read_as_trait(&("queriable stochastic language".parse().unwrap()), &mut reader, None, 0).unwrap();
		let input1 = EbiInput::Trait(object1, file_handler1);
		let inputs = vec![input0, input1];

		if let EbiCommand::Command{execute, output_type, ..} = crate::ebi_commands::ebi_command_conformance::EBI_CONFORMANCE_HELLINGER {
			match (execute)(inputs, None) {
				Ok(output) => {
					output.test_activity_key();
					assert_eq!(&output.get_type(), output_type);
				}
				Err(e) => Err(e).unwrap(),
			}
		}
	}
	#[test]
	pub fn ebi_conf_hsc_test_1() {
		// to indicate that this test is expected to fail, add the following to src/tests/fallible_test_list.rs:
		/* 
		(
			&EBI_CONFORMANCE_HELLINGER,
			&[
				"trait finite stochastic language#./testfiles/ba-aa-ab.slang",
				"trait queriable stochastic language#./testfiles/ba-aa-ab.slang"			
			]
		),
		*/
		
		let mut reader = MultipleReader::from_file(File::open("./testfiles/ba-aa-ab.slang").unwrap());
		let (object0, file_handler0) = ebi_input::read_as_trait(&("finite stochastic language".parse().unwrap()), &mut reader, None, 0).unwrap();
		let input0 = EbiInput::Trait(object0, file_handler0);
		let mut reader = MultipleReader::from_file(File::open("./testfiles/ba-aa-ab.slang").unwrap());
		let (object1, file_handler1) = ebi_input::read_as_trait(&("queriable stochastic language".parse().unwrap()), &mut reader, None, 0).unwrap();
		let input1 = EbiInput::Trait(object1, file_handler1);
		let inputs = vec![input0, input1];

		if let EbiCommand::Command{execute, output_type, ..} = crate::ebi_commands::ebi_command_conformance::EBI_CONFORMANCE_HELLINGER {
			match (execute)(inputs, None) {
				Ok(output) => {
					output.test_activity_key();
					assert_eq!(&output.get_type(), output_type);
				}
				Err(e) => Err(e).unwrap(),
			}
		}
	}
	#[test]
	pub fn ebi_conf_hsc_test_2() {
		// to indicate that this test is expected to fail, add the following to src/tests/fallible_test_list.rs:
		/* 
		(
			&EBI_CONFORMANCE_HELLINGER,
			&[
				"trait finite stochastic language#./testfiles/ba-aa-ab.slang",
				"trait queriable stochastic language#./testfiles/a-b.slang"			
			]
		),
		*/
		
		let mut reader = MultipleReader::from_file(File::open("./testfiles/ba-aa-ab.slang").unwrap());
		let (object0, file_handler0) = ebi_input::read_as_trait(&("finite stochastic language".parse().unwrap()), &mut reader, None, 0).unwrap();
		let input0 = EbiInput::Trait(object0, file_handler0);
		let mut reader = MultipleReader::from_file(File::open("./testfiles/a-b.slang").unwrap());
		let (object1, file_handler1) = ebi_input::read_as_trait(&("queriable stochastic language".parse().unwrap()), &mut reader, None, 0).unwrap();
		let input1 = EbiInput::Trait(object1, file_handler1);
		let inputs = vec![input0, input1];

		if let EbiCommand::Command{execute, output_type, ..} = crate::ebi_commands::ebi_command_conformance::EBI_CONFORMANCE_HELLINGER {
			match (execute)(inputs, None) {
				Ok(output) => {
					output.test_activity_key();
					assert_eq!(&output.get_type(), output_type);
				}
				Err(e) => Err(e).unwrap(),
			}
		}
	}
	#[test]
	pub fn ebi_conf_hsc_test_3() {
		// to indicate that this test is expected to fail, add the following to src/tests/fallible_test_list.rs:
		/* 
		(
			&EBI_CONFORMANCE_HELLINGER,
			&[
				"trait finite stochastic language#./testfiles/ba-aa-ab.slang",
				"trait queriable stochastic language#./testfiles/empty.xes"			
			]
		),
		*/
		
		let mut reader = MultipleReader::from_file(File::open("./testfiles/ba-aa-ab.slang").unwrap());
		let (object0, file_handler0) = ebi_input::read_as_trait(&("finite stochastic language".parse().unwrap()), &mut reader, None, 0).unwrap();
		let input0 = EbiInput::Trait(object0, file_handler0);
		let mut reader = MultipleReader::from_file(File::open("./testfiles/empty.xes").unwrap());
		let (object1, file_handler1) = ebi_input::read_as_trait(&("queriable stochastic language".parse().unwrap()), &mut reader, None, 0).unwrap();
		let input1 = EbiInput::Trait(object1, file_handler1);
		let inputs = vec![input0, input1];

		if let EbiCommand::Command{execute, output_type, ..} = crate::ebi_commands::ebi_command_conformance::EBI_CONFORMANCE_HELLINGER {
			match (execute)(inputs, None) {
				Ok(output) => {
					output.test_activity_key();
					assert_eq!(&output.get_type(), output_type);
				}
				Err(e) => Err(e).unwrap(),
			}
		}
	}


	// ==== command hsc-sample ====
	#[test]
	pub fn ebi_conf_hsc_sample_test_0() {
		// to indicate that this test is expected to fail, add the following to src/tests/fallible_test_list.rs:
		/* 
		(
			&EBI_CONFORMANCE_HELLINGER_SAMPLE,
			&[
				"trait finite stochastic language#./testfiles/ba-aa-ab.slang",
				"trait finite stochastic language#./testfiles/ba-aa-ab.slang",
				"usize 1"			
			]
		),
		*/
		
		let mut reader = MultipleReader::from_file(File::open("./testfiles/ba-aa-ab.slang").unwrap());
		let (object0, file_handler0) = ebi_input::read_as_trait(&("finite stochastic language".parse().unwrap()), &mut reader, None, 0).unwrap();
		let input0 = EbiInput::Trait(object0, file_handler0);
		let mut reader = MultipleReader::from_file(File::open("./testfiles/ba-aa-ab.slang").unwrap());
		let (object1, file_handler1) = ebi_input::read_as_trait(&("finite stochastic language".parse().unwrap()), &mut reader, None, 0).unwrap();
		let input1 = EbiInput::Trait(object1, file_handler1);
		let input2 = EbiInput::Usize(1, &TEST_INPUT_TYPE_USIZE);
		let inputs = vec![input0, input1, input2];

		if let EbiCommand::Command{execute, output_type, ..} = crate::ebi_commands::ebi_command_conformance::EBI_CONFORMANCE_HELLINGER_SAMPLE {
			match (execute)(inputs, None) {
				Ok(output) => {
					output.test_activity_key();
					assert_eq!(&output.get_type(), output_type);
				}
				Err(e) => Err(e).unwrap(),
			}
		}
	}
	#[test]
	pub fn ebi_conf_hsc_sample_test_1() {
		// to indicate that this test is expected to fail, add the following to src/tests/fallible_test_list.rs:
		/* 
		(
			&EBI_CONFORMANCE_HELLINGER_SAMPLE,
			&[
				"trait finite stochastic language#./testfiles/ba-aa-ab.slang",
				"trait finite stochastic language#./testfiles/a-b.slang",
				"usize 1"			
			]
		),
		*/
		
		let mut reader = MultipleReader::from_file(File::open("./testfiles/ba-aa-ab.slang").unwrap());
		let (object0, file_handler0) = ebi_input::read_as_trait(&("finite stochastic language".parse().unwrap()), &mut reader, None, 0).unwrap();
		let input0 = EbiInput::Trait(object0, file_handler0);
		let mut reader = MultipleReader::from_file(File::open("./testfiles/a-b.slang").unwrap());
		let (object1, file_handler1) = ebi_input::read_as_trait(&("finite stochastic language".parse().unwrap()), &mut reader, None, 0).unwrap();
		let input1 = EbiInput::Trait(object1, file_handler1);
		let input2 = EbiInput::Usize(1, &TEST_INPUT_TYPE_USIZE);
		let inputs = vec![input0, input1, input2];

		if let EbiCommand::Command{execute, output_type, ..} = crate::ebi_commands::ebi_command_conformance::EBI_CONFORMANCE_HELLINGER_SAMPLE {
			match (execute)(inputs, None) {
				Ok(output) => {
					output.test_activity_key();
					assert_eq!(&output.get_type(), output_type);
				}
				Err(e) => Err(e).unwrap(),
			}
		}
	}
	#[test]
	pub fn ebi_conf_hsc_sample_test_2() {
		// this test has been indicated as to be expected to fail
		
		let mut reader = MultipleReader::from_file(File::open("./testfiles/ba-aa-ab.slang").unwrap());
		let (object0, file_handler0) = ebi_input::read_as_trait(&("finite stochastic language".parse().unwrap()), &mut reader, None, 0).unwrap();
		let input0 = EbiInput::Trait(object0, file_handler0);
		let mut reader = MultipleReader::from_file(File::open("./testfiles/empty.xes").unwrap());
		let (object1, file_handler1) = ebi_input::read_as_trait(&("finite stochastic language".parse().unwrap()), &mut reader, None, 0).unwrap();
		let input1 = EbiInput::Trait(object1, file_handler1);
		let input2 = EbiInput::Usize(1, &TEST_INPUT_TYPE_USIZE);
		let inputs = vec![input0, input1, input2];

		if let EbiCommand::Command{execute, ..} = crate::ebi_commands::ebi_command_conformance::EBI_CONFORMANCE_HELLINGER_SAMPLE {
			assert!(((execute)(inputs, None)).is_err())
		}
	}
	#[test]
	pub fn ebi_conf_hsc_sample_test_3() {
		// to indicate that this test is expected to fail, add the following to src/tests/fallible_test_list.rs:
		/* 
		(
			&EBI_CONFORMANCE_HELLINGER_SAMPLE,
			&[
				"trait finite stochastic language#./testfiles/ba-aa-ab.slang",
				"trait finite stochastic language#./testfiles/acb-abc-ad-aded-adeded-adededed.slang",
				"usize 1"			
			]
		),
		*/
		
		let mut reader = MultipleReader::from_file(File::open("./testfiles/ba-aa-ab.slang").unwrap());
		let (object0, file_handler0) = ebi_input::read_as_trait(&("finite stochastic language".parse().unwrap()), &mut reader, None, 0).unwrap();
		let input0 = EbiInput::Trait(object0, file_handler0);
		let mut reader = MultipleReader::from_file(File::open("./testfiles/acb-abc-ad-aded-adeded-adededed.slang").unwrap());
		let (object1, file_handler1) = ebi_input::read_as_trait(&("finite stochastic language".parse().unwrap()), &mut reader, None, 0).unwrap();
		let input1 = EbiInput::Trait(object1, file_handler1);
		let input2 = EbiInput::Usize(1, &TEST_INPUT_TYPE_USIZE);
		let inputs = vec![input0, input1, input2];

		if let EbiCommand::Command{execute, output_type, ..} = crate::ebi_commands::ebi_command_conformance::EBI_CONFORMANCE_HELLINGER_SAMPLE {
			match (execute)(inputs, None) {
				Ok(output) => {
					output.test_activity_key();
					assert_eq!(&output.get_type(), output_type);
				}
				Err(e) => Err(e).unwrap(),
			}
		}
	}


	// ==== command jssc ====
	#[test]
	pub fn ebi_conf_jssc_test_0() {
		// to indicate that this test is expected to fail, add the following to src/tests/fallible_test_list.rs:
		/* 
		(
			&EBI_CONFORMANCE_JSSC,
			&[
				"trait finite stochastic language#./testfiles/ba-aa-ab.slang",
				"trait finite stochastic language#./testfiles/ba-aa-ab.slang"			
			]
		),
		*/
		if ebi_objects::ebi_arithmetic::is_exact_globally() {
			return;
		}
		let mut reader = MultipleReader::from_file(File::open("./testfiles/ba-aa-ab.slang").unwrap());
		let (object0, file_handler0) = ebi_input::read_as_trait(&("finite stochastic language".parse().unwrap()), &mut reader, None, 0).unwrap();
		let input0 = EbiInput::Trait(object0, file_handler0);
		let mut reader = MultipleReader::from_file(File::open("./testfiles/ba-aa-ab.slang").unwrap());
		let (object1, file_handler1) = ebi_input::read_as_trait(&("finite stochastic language".parse().unwrap()), &mut reader, None, 0).unwrap();
		let input1 = EbiInput::Trait(object1, file_handler1);
		let inputs = vec![input0, input1];

		if let EbiCommand::Command{execute, output_type, ..} = crate::ebi_commands::ebi_command_conformance::EBI_CONFORMANCE_JSSC {
			match (execute)(inputs, None) {
				Ok(output) => {
					output.test_activity_key();
					assert_eq!(&output.get_type(), output_type);
				}
				Err(e) => Err(e).unwrap(),
			}
		}
	}
	#[test]
	pub fn ebi_conf_jssc_test_1() {
		// to indicate that this test is expected to fail, add the following to src/tests/fallible_test_list.rs:
		/* 
		(
			&EBI_CONFORMANCE_JSSC,
			&[
				"trait finite stochastic language#./testfiles/ba-aa-ab.slang",
				"trait finite stochastic language#./testfiles/a-b.slang"			
			]
		),
		*/
		if ebi_objects::ebi_arithmetic::is_exact_globally() {
			return;
		}
		let mut reader = MultipleReader::from_file(File::open("./testfiles/ba-aa-ab.slang").unwrap());
		let (object0, file_handler0) = ebi_input::read_as_trait(&("finite stochastic language".parse().unwrap()), &mut reader, None, 0).unwrap();
		let input0 = EbiInput::Trait(object0, file_handler0);
		let mut reader = MultipleReader::from_file(File::open("./testfiles/a-b.slang").unwrap());
		let (object1, file_handler1) = ebi_input::read_as_trait(&("finite stochastic language".parse().unwrap()), &mut reader, None, 0).unwrap();
		let input1 = EbiInput::Trait(object1, file_handler1);
		let inputs = vec![input0, input1];

		if let EbiCommand::Command{execute, output_type, ..} = crate::ebi_commands::ebi_command_conformance::EBI_CONFORMANCE_JSSC {
			match (execute)(inputs, None) {
				Ok(output) => {
					output.test_activity_key();
					assert_eq!(&output.get_type(), output_type);
				}
				Err(e) => Err(e).unwrap(),
			}
		}
	}
	#[test]
	pub fn ebi_conf_jssc_test_2() {
		// to indicate that this test is expected to fail, add the following to src/tests/fallible_test_list.rs:
		/* 
		(
			&EBI_CONFORMANCE_JSSC,
			&[
				"trait finite stochastic language#./testfiles/ba-aa-ab.slang",
				"trait finite stochastic language#./testfiles/empty.xes"			
			]
		),
		*/
		if ebi_objects::ebi_arithmetic::is_exact_globally() {
			return;
		}
		let mut reader = MultipleReader::from_file(File::open("./testfiles/ba-aa-ab.slang").unwrap());
		let (object0, file_handler0) = ebi_input::read_as_trait(&("finite stochastic language".parse().unwrap()), &mut reader, None, 0).unwrap();
		let input0 = EbiInput::Trait(object0, file_handler0);
		let mut reader = MultipleReader::from_file(File::open("./testfiles/empty.xes").unwrap());
		let (object1, file_handler1) = ebi_input::read_as_trait(&("finite stochastic language".parse().unwrap()), &mut reader, None, 0).unwrap();
		let input1 = EbiInput::Trait(object1, file_handler1);
		let inputs = vec![input0, input1];

		if let EbiCommand::Command{execute, output_type, ..} = crate::ebi_commands::ebi_command_conformance::EBI_CONFORMANCE_JSSC {
			match (execute)(inputs, None) {
				Ok(output) => {
					output.test_activity_key();
					assert_eq!(&output.get_type(), output_type);
				}
				Err(e) => Err(e).unwrap(),
			}
		}
	}
	#[test]
	pub fn ebi_conf_jssc_test_3() {
		// to indicate that this test is expected to fail, add the following to src/tests/fallible_test_list.rs:
		/* 
		(
			&EBI_CONFORMANCE_JSSC,
			&[
				"trait finite stochastic language#./testfiles/ba-aa-ab.slang",
				"trait finite stochastic language#./testfiles/acb-abc-ad-aded-adeded-adededed.slang"			
			]
		),
		*/
		if ebi_objects::ebi_arithmetic::is_exact_globally() {
			return;
		}
		let mut reader = MultipleReader::from_file(File::open("./testfiles/ba-aa-ab.slang").unwrap());
		let (object0, file_handler0) = ebi_input::read_as_trait(&("finite stochastic language".parse().unwrap()), &mut reader, None, 0).unwrap();
		let input0 = EbiInput::Trait(object0, file_handler0);
		let mut reader = MultipleReader::from_file(File::open("./testfiles/acb-abc-ad-aded-adeded-adededed.slang").unwrap());
		let (object1, file_handler1) = ebi_input::read_as_trait(&("finite stochastic language".parse().unwrap()), &mut reader, None, 0).unwrap();
		let input1 = EbiInput::Trait(object1, file_handler1);
		let inputs = vec![input0, input1];

		if let EbiCommand::Command{execute, output_type, ..} = crate::ebi_commands::ebi_command_conformance::EBI_CONFORMANCE_JSSC {
			match (execute)(inputs, None) {
				Ok(output) => {
					output.test_activity_key();
					assert_eq!(&output.get_type(), output_type);
				}
				Err(e) => Err(e).unwrap(),
			}
		}
	}


	// ==== command jssc-sample ====
	#[test]
	pub fn ebi_conf_jssc_sample_test_0() {
		// to indicate that this test is expected to fail, add the following to src/tests/fallible_test_list.rs:
		/* 
		(
			&EBI_CONFORMANCE_JSSC_SAMPLE,
			&[
				"trait finite stochastic language#./testfiles/ba-aa-ab.slang",
				"trait finite stochastic language#./testfiles/ba-aa-ab.slang",
				"usize 1"			
			]
		),
		*/
		if ebi_objects::ebi_arithmetic::is_exact_globally() {
			return;
		}
		let mut reader = MultipleReader::from_file(File::open("./testfiles/ba-aa-ab.slang").unwrap());
		let (object0, file_handler0) = ebi_input::read_as_trait(&("finite stochastic language".parse().unwrap()), &mut reader, None, 0).unwrap();
		let input0 = EbiInput::Trait(object0, file_handler0);
		let mut reader = MultipleReader::from_file(File::open("./testfiles/ba-aa-ab.slang").unwrap());
		let (object1, file_handler1) = ebi_input::read_as_trait(&("finite stochastic language".parse().unwrap()), &mut reader, None, 0).unwrap();
		let input1 = EbiInput::Trait(object1, file_handler1);
		let input2 = EbiInput::Usize(1, &TEST_INPUT_TYPE_USIZE);
		let inputs = vec![input0, input1, input2];

		if let EbiCommand::Command{execute, output_type, ..} = crate::ebi_commands::ebi_command_conformance::EBI_CONFORMANCE_JSSC_SAMPLE {
			match (execute)(inputs, None) {
				Ok(output) => {
					output.test_activity_key();
					assert_eq!(&output.get_type(), output_type);
				}
				Err(e) => Err(e).unwrap(),
			}
		}
	}
	#[test]
	pub fn ebi_conf_jssc_sample_test_1() {
		// to indicate that this test is expected to fail, add the following to src/tests/fallible_test_list.rs:
		/* 
		(
			&EBI_CONFORMANCE_JSSC_SAMPLE,
			&[
				"trait finite stochastic language#./testfiles/ba-aa-ab.slang",
				"trait finite stochastic language#./testfiles/a-b.slang",
				"usize 1"			
			]
		),
		*/
		if ebi_objects::ebi_arithmetic::is_exact_globally() {
			return;
		}
		let mut reader = MultipleReader::from_file(File::open("./testfiles/ba-aa-ab.slang").unwrap());
		let (object0, file_handler0) = ebi_input::read_as_trait(&("finite stochastic language".parse().unwrap()), &mut reader, None, 0).unwrap();
		let input0 = EbiInput::Trait(object0, file_handler0);
		let mut reader = MultipleReader::from_file(File::open("./testfiles/a-b.slang").unwrap());
		let (object1, file_handler1) = ebi_input::read_as_trait(&("finite stochastic language".parse().unwrap()), &mut reader, None, 0).unwrap();
		let input1 = EbiInput::Trait(object1, file_handler1);
		let input2 = EbiInput::Usize(1, &TEST_INPUT_TYPE_USIZE);
		let inputs = vec![input0, input1, input2];

		if let EbiCommand::Command{execute, output_type, ..} = crate::ebi_commands::ebi_command_conformance::EBI_CONFORMANCE_JSSC_SAMPLE {
			match (execute)(inputs, None) {
				Ok(output) => {
					output.test_activity_key();
					assert_eq!(&output.get_type(), output_type);
				}
				Err(e) => Err(e).unwrap(),
			}
		}
	}
	#[test]
	pub fn ebi_conf_jssc_sample_test_2() {
		// to indicate that this test is expected to fail, add the following to src/tests/fallible_test_list.rs:
		/* 
		(
			&EBI_CONFORMANCE_JSSC_SAMPLE,
			&[
				"trait finite stochastic language#./testfiles/ba-aa-ab.slang",
				"trait finite stochastic language#./testfiles/empty.xes",
				"usize 1"			
			]
		),
		*/
		if ebi_objects::ebi_arithmetic::is_exact_globally() {
			return;
		}
		let mut reader = MultipleReader::from_file(File::open("./testfiles/ba-aa-ab.slang").unwrap());
		let (object0, file_handler0) = ebi_input::read_as_trait(&("finite stochastic language".parse().unwrap()), &mut reader, None, 0).unwrap();
		let input0 = EbiInput::Trait(object0, file_handler0);
		let mut reader = MultipleReader::from_file(File::open("./testfiles/empty.xes").unwrap());
		let (object1, file_handler1) = ebi_input::read_as_trait(&("finite stochastic language".parse().unwrap()), &mut reader, None, 0).unwrap();
		let input1 = EbiInput::Trait(object1, file_handler1);
		let input2 = EbiInput::Usize(1, &TEST_INPUT_TYPE_USIZE);
		let inputs = vec![input0, input1, input2];

		if let EbiCommand::Command{execute, output_type, ..} = crate::ebi_commands::ebi_command_conformance::EBI_CONFORMANCE_JSSC_SAMPLE {
			match (execute)(inputs, None) {
				Ok(output) => {
					output.test_activity_key();
					assert_eq!(&output.get_type(), output_type);
				}
				Err(e) => Err(e).unwrap(),
			}
		}
	}
	#[test]
	pub fn ebi_conf_jssc_sample_test_3() {
		// to indicate that this test is expected to fail, add the following to src/tests/fallible_test_list.rs:
		/* 
		(
			&EBI_CONFORMANCE_JSSC_SAMPLE,
			&[
				"trait finite stochastic language#./testfiles/ba-aa-ab.slang",
				"trait finite stochastic language#./testfiles/acb-abc-ad-aded-adeded-adededed.slang",
				"usize 1"			
			]
		),
		*/
		if ebi_objects::ebi_arithmetic::is_exact_globally() {
			return;
		}
		let mut reader = MultipleReader::from_file(File::open("./testfiles/ba-aa-ab.slang").unwrap());
		let (object0, file_handler0) = ebi_input::read_as_trait(&("finite stochastic language".parse().unwrap()), &mut reader, None, 0).unwrap();
		let input0 = EbiInput::Trait(object0, file_handler0);
		let mut reader = MultipleReader::from_file(File::open("./testfiles/acb-abc-ad-aded-adeded-adededed.slang").unwrap());
		let (object1, file_handler1) = ebi_input::read_as_trait(&("finite stochastic language".parse().unwrap()), &mut reader, None, 0).unwrap();
		let input1 = EbiInput::Trait(object1, file_handler1);
		let input2 = EbiInput::Usize(1, &TEST_INPUT_TYPE_USIZE);
		let inputs = vec![input0, input1, input2];

		if let EbiCommand::Command{execute, output_type, ..} = crate::ebi_commands::ebi_command_conformance::EBI_CONFORMANCE_JSSC_SAMPLE {
			match (execute)(inputs, None) {
				Ok(output) => {
					output.test_activity_key();
					assert_eq!(&output.get_type(), output_type);
				}
				Err(e) => Err(e).unwrap(),
			}
		}
	}


	// ==== command ma ====
	#[test]
	pub fn ebi_conf_ma_test_0() {
		// to indicate that this test is expected to fail, add the following to src/tests/fallible_test_list.rs:
		/* 
		(
			&EBI_CONFORMANCE_MARKOVIAN,
			&[
				"trait finite stochastic language#./testfiles/ba-aa-ab.slang",
				"trait finite stochastic language#./testfiles/ba-aa-ab.slang",
				"usize 1",
				"string cssc"			
			]
		),
		*/
		
		let mut reader = MultipleReader::from_file(File::open("./testfiles/ba-aa-ab.slang").unwrap());
		let (object0, file_handler0) = ebi_input::read_as_trait(&("finite stochastic language".parse().unwrap()), &mut reader, None, 0).unwrap();
		let input0 = EbiInput::Trait(object0, file_handler0);
		let mut reader = MultipleReader::from_file(File::open("./testfiles/ba-aa-ab.slang").unwrap());
		let (object1, file_handler1) = ebi_input::read_as_trait(&("finite stochastic language".parse().unwrap()), &mut reader, None, 0).unwrap();
		let input1 = EbiInput::Trait(object1, file_handler1);
		let input2 = EbiInput::Usize(1, &TEST_INPUT_TYPE_USIZE);
		let input3 = EbiInput::String("cssc".to_string(), &TEST_INPUT_TYPE_STRING);
		let inputs = vec![input0, input1, input2, input3];

		if let EbiCommand::Command{execute, output_type, ..} = crate::ebi_commands::ebi_command_conformance::EBI_CONFORMANCE_MARKOVIAN {
			match (execute)(inputs, None) {
				Ok(output) => {
					output.test_activity_key();
					assert_eq!(&output.get_type(), output_type);
				}
				Err(e) => Err(e).unwrap(),
			}
		}
	}
	#[test]
	pub fn ebi_conf_ma_test_1() {
		// to indicate that this test is expected to fail, add the following to src/tests/fallible_test_list.rs:
		/* 
		(
			&EBI_CONFORMANCE_MARKOVIAN,
			&[
				"trait finite stochastic language#./testfiles/ba-aa-ab.slang",
				"trait finite stochastic language#./testfiles/a-b.slang",
				"usize 1",
				"string cssc"			
			]
		),
		*/
		
		let mut reader = MultipleReader::from_file(File::open("./testfiles/ba-aa-ab.slang").unwrap());
		let (object0, file_handler0) = ebi_input::read_as_trait(&("finite stochastic language".parse().unwrap()), &mut reader, None, 0).unwrap();
		let input0 = EbiInput::Trait(object0, file_handler0);
		let mut reader = MultipleReader::from_file(File::open("./testfiles/a-b.slang").unwrap());
		let (object1, file_handler1) = ebi_input::read_as_trait(&("finite stochastic language".parse().unwrap()), &mut reader, None, 0).unwrap();
		let input1 = EbiInput::Trait(object1, file_handler1);
		let input2 = EbiInput::Usize(1, &TEST_INPUT_TYPE_USIZE);
		let input3 = EbiInput::String("cssc".to_string(), &TEST_INPUT_TYPE_STRING);
		let inputs = vec![input0, input1, input2, input3];

		if let EbiCommand::Command{execute, output_type, ..} = crate::ebi_commands::ebi_command_conformance::EBI_CONFORMANCE_MARKOVIAN {
			match (execute)(inputs, None) {
				Ok(output) => {
					output.test_activity_key();
					assert_eq!(&output.get_type(), output_type);
				}
				Err(e) => Err(e).unwrap(),
			}
		}
	}
	#[test]
	pub fn ebi_conf_ma_test_2() {
		// to indicate that this test is expected to fail, add the following to src/tests/fallible_test_list.rs:
		/* 
		(
			&EBI_CONFORMANCE_MARKOVIAN,
			&[
				"trait finite stochastic language#./testfiles/ba-aa-ab.slang",
				"trait finite stochastic language#./testfiles/empty.xes",
				"usize 1",
				"string cssc"			
			]
		),
		*/
		
		let mut reader = MultipleReader::from_file(File::open("./testfiles/ba-aa-ab.slang").unwrap());
		let (object0, file_handler0) = ebi_input::read_as_trait(&("finite stochastic language".parse().unwrap()), &mut reader, None, 0).unwrap();
		let input0 = EbiInput::Trait(object0, file_handler0);
		let mut reader = MultipleReader::from_file(File::open("./testfiles/empty.xes").unwrap());
		let (object1, file_handler1) = ebi_input::read_as_trait(&("finite stochastic language".parse().unwrap()), &mut reader, None, 0).unwrap();
		let input1 = EbiInput::Trait(object1, file_handler1);
		let input2 = EbiInput::Usize(1, &TEST_INPUT_TYPE_USIZE);
		let input3 = EbiInput::String("cssc".to_string(), &TEST_INPUT_TYPE_STRING);
		let inputs = vec![input0, input1, input2, input3];

		if let EbiCommand::Command{execute, output_type, ..} = crate::ebi_commands::ebi_command_conformance::EBI_CONFORMANCE_MARKOVIAN {
			match (execute)(inputs, None) {
				Ok(output) => {
					output.test_activity_key();
					assert_eq!(&output.get_type(), output_type);
				}
				Err(e) => Err(e).unwrap(),
			}
		}
	}
	#[test]
	pub fn ebi_conf_ma_test_3() {
		// to indicate that this test is expected to fail, add the following to src/tests/fallible_test_list.rs:
		/* 
		(
			&EBI_CONFORMANCE_MARKOVIAN,
			&[
				"trait finite stochastic language#./testfiles/ba-aa-ab.slang",
				"trait finite stochastic language#./testfiles/acb-abc-ad-aded-adeded-adededed.slang",
				"usize 1",
				"string cssc"			
			]
		),
		*/
		
		let mut reader = MultipleReader::from_file(File::open("./testfiles/ba-aa-ab.slang").unwrap());
		let (object0, file_handler0) = ebi_input::read_as_trait(&("finite stochastic language".parse().unwrap()), &mut reader, None, 0).unwrap();
		let input0 = EbiInput::Trait(object0, file_handler0);
		let mut reader = MultipleReader::from_file(File::open("./testfiles/acb-abc-ad-aded-adeded-adededed.slang").unwrap());
		let (object1, file_handler1) = ebi_input::read_as_trait(&("finite stochastic language".parse().unwrap()), &mut reader, None, 0).unwrap();
		let input1 = EbiInput::Trait(object1, file_handler1);
		let input2 = EbiInput::Usize(1, &TEST_INPUT_TYPE_USIZE);
		let input3 = EbiInput::String("cssc".to_string(), &TEST_INPUT_TYPE_STRING);
		let inputs = vec![input0, input1, input2, input3];

		if let EbiCommand::Command{execute, output_type, ..} = crate::ebi_commands::ebi_command_conformance::EBI_CONFORMANCE_MARKOVIAN {
			match (execute)(inputs, None) {
				Ok(output) => {
					output.test_activity_key();
					assert_eq!(&output.get_type(), output_type);
				}
				Err(e) => Err(e).unwrap(),
			}
		}
	}


	// ==== command uemsc ====
	#[test]
	pub fn ebi_conf_uemsc_test_0() {
		// to indicate that this test is expected to fail, add the following to src/tests/fallible_test_list.rs:
		/* 
		(
			&EBI_CONFORMANCE_UEMSC,
			&[
				"trait finite stochastic language#./testfiles/ba-aa-ab.slang",
				"trait queriable stochastic language#./testfiles/seq(a-xor(b-c)).sptree"			
			]
		),
		*/
		
		let mut reader = MultipleReader::from_file(File::open("./testfiles/ba-aa-ab.slang").unwrap());
		let (object0, file_handler0) = ebi_input::read_as_trait(&("finite stochastic language".parse().unwrap()), &mut reader, None, 0).unwrap();
		let input0 = EbiInput::Trait(object0, file_handler0);
		let mut reader = MultipleReader::from_file(File::open("./testfiles/seq(a-xor(b-c)).sptree").unwrap());
		let (object1, file_handler1) = ebi_input::read_as_trait(&("queriable stochastic language".parse().unwrap()), &mut reader, None, 0).unwrap();
		let input1 = EbiInput::Trait(object1, file_handler1);
		let inputs = vec![input0, input1];

		if let EbiCommand::Command{execute, output_type, ..} = crate::ebi_commands::ebi_command_conformance::EBI_CONFORMANCE_UEMSC {
			match (execute)(inputs, None) {
				Ok(output) => {
					output.test_activity_key();
					assert_eq!(&output.get_type(), output_type);
				}
				Err(e) => Err(e).unwrap(),
			}
		}
	}
	#[test]
	pub fn ebi_conf_uemsc_test_1() {
		// to indicate that this test is expected to fail, add the following to src/tests/fallible_test_list.rs:
		/* 
		(
			&EBI_CONFORMANCE_UEMSC,
			&[
				"trait finite stochastic language#./testfiles/ba-aa-ab.slang",
				"trait queriable stochastic language#./testfiles/ba-aa-ab.slang"			
			]
		),
		*/
		
		let mut reader = MultipleReader::from_file(File::open("./testfiles/ba-aa-ab.slang").unwrap());
		let (object0, file_handler0) = ebi_input::read_as_trait(&("finite stochastic language".parse().unwrap()), &mut reader, None, 0).unwrap();
		let input0 = EbiInput::Trait(object0, file_handler0);
		let mut reader = MultipleReader::from_file(File::open("./testfiles/ba-aa-ab.slang").unwrap());
		let (object1, file_handler1) = ebi_input::read_as_trait(&("queriable stochastic language".parse().unwrap()), &mut reader, None, 0).unwrap();
		let input1 = EbiInput::Trait(object1, file_handler1);
		let inputs = vec![input0, input1];

		if let EbiCommand::Command{execute, output_type, ..} = crate::ebi_commands::ebi_command_conformance::EBI_CONFORMANCE_UEMSC {
			match (execute)(inputs, None) {
				Ok(output) => {
					output.test_activity_key();
					assert_eq!(&output.get_type(), output_type);
				}
				Err(e) => Err(e).unwrap(),
			}
		}
	}
	#[test]
	pub fn ebi_conf_uemsc_test_2() {
		// to indicate that this test is expected to fail, add the following to src/tests/fallible_test_list.rs:
		/* 
		(
			&EBI_CONFORMANCE_UEMSC,
			&[
				"trait finite stochastic language#./testfiles/ba-aa-ab.slang",
				"trait queriable stochastic language#./testfiles/a-b.slang"			
			]
		),
		*/
		
		let mut reader = MultipleReader::from_file(File::open("./testfiles/ba-aa-ab.slang").unwrap());
		let (object0, file_handler0) = ebi_input::read_as_trait(&("finite stochastic language".parse().unwrap()), &mut reader, None, 0).unwrap();
		let input0 = EbiInput::Trait(object0, file_handler0);
		let mut reader = MultipleReader::from_file(File::open("./testfiles/a-b.slang").unwrap());
		let (object1, file_handler1) = ebi_input::read_as_trait(&("queriable stochastic language".parse().unwrap()), &mut reader, None, 0).unwrap();
		let input1 = EbiInput::Trait(object1, file_handler1);
		let inputs = vec![input0, input1];

		if let EbiCommand::Command{execute, output_type, ..} = crate::ebi_commands::ebi_command_conformance::EBI_CONFORMANCE_UEMSC {
			match (execute)(inputs, None) {
				Ok(output) => {
					output.test_activity_key();
					assert_eq!(&output.get_type(), output_type);
				}
				Err(e) => Err(e).unwrap(),
			}
		}
	}
	#[test]
	pub fn ebi_conf_uemsc_test_3() {
		// to indicate that this test is expected to fail, add the following to src/tests/fallible_test_list.rs:
		/* 
		(
			&EBI_CONFORMANCE_UEMSC,
			&[
				"trait finite stochastic language#./testfiles/ba-aa-ab.slang",
				"trait queriable stochastic language#./testfiles/empty.xes"			
			]
		),
		*/
		
		let mut reader = MultipleReader::from_file(File::open("./testfiles/ba-aa-ab.slang").unwrap());
		let (object0, file_handler0) = ebi_input::read_as_trait(&("finite stochastic language".parse().unwrap()), &mut reader, None, 0).unwrap();
		let input0 = EbiInput::Trait(object0, file_handler0);
		let mut reader = MultipleReader::from_file(File::open("./testfiles/empty.xes").unwrap());
		let (object1, file_handler1) = ebi_input::read_as_trait(&("queriable stochastic language".parse().unwrap()), &mut reader, None, 0).unwrap();
		let input1 = EbiInput::Trait(object1, file_handler1);
		let inputs = vec![input0, input1];

		if let EbiCommand::Command{execute, output_type, ..} = crate::ebi_commands::ebi_command_conformance::EBI_CONFORMANCE_UEMSC {
			match (execute)(inputs, None) {
				Ok(output) => {
					output.test_activity_key();
					assert_eq!(&output.get_type(), output_type);
				}
				Err(e) => Err(e).unwrap(),
			}
		}
	}


	// ==== command uemsc-sample ====
	#[test]
	pub fn ebi_conf_uemsc_sample_test_0() {
		// to indicate that this test is expected to fail, add the following to src/tests/fallible_test_list.rs:
		/* 
		(
			&EBI_CONFORMANCE_UEMSC_SAMPLE,
			&[
				"trait finite stochastic language#./testfiles/ba-aa-ab.slang",
				"trait finite stochastic language#./testfiles/ba-aa-ab.slang",
				"usize 1"			
			]
		),
		*/
		if ebi_objects::ebi_arithmetic::is_exact_globally() {
			return;
		}
		let mut reader = MultipleReader::from_file(File::open("./testfiles/ba-aa-ab.slang").unwrap());
		let (object0, file_handler0) = ebi_input::read_as_trait(&("finite stochastic language".parse().unwrap()), &mut reader, None, 0).unwrap();
		let input0 = EbiInput::Trait(object0, file_handler0);
		let mut reader = MultipleReader::from_file(File::open("./testfiles/ba-aa-ab.slang").unwrap());
		let (object1, file_handler1) = ebi_input::read_as_trait(&("finite stochastic language".parse().unwrap()), &mut reader, None, 0).unwrap();
		let input1 = EbiInput::Trait(object1, file_handler1);
		let input2 = EbiInput::Usize(1, &TEST_INPUT_TYPE_USIZE);
		let inputs = vec![input0, input1, input2];

		if let EbiCommand::Command{execute, output_type, ..} = crate::ebi_commands::ebi_command_conformance::EBI_CONFORMANCE_UEMSC_SAMPLE {
			match (execute)(inputs, None) {
				Ok(output) => {
					output.test_activity_key();
					assert_eq!(&output.get_type(), output_type);
				}
				Err(e) => Err(e).unwrap(),
			}
		}
	}
	#[test]
	pub fn ebi_conf_uemsc_sample_test_1() {
		// to indicate that this test is expected to fail, add the following to src/tests/fallible_test_list.rs:
		/* 
		(
			&EBI_CONFORMANCE_UEMSC_SAMPLE,
			&[
				"trait finite stochastic language#./testfiles/ba-aa-ab.slang",
				"trait finite stochastic language#./testfiles/a-b.slang",
				"usize 1"			
			]
		),
		*/
		if ebi_objects::ebi_arithmetic::is_exact_globally() {
			return;
		}
		let mut reader = MultipleReader::from_file(File::open("./testfiles/ba-aa-ab.slang").unwrap());
		let (object0, file_handler0) = ebi_input::read_as_trait(&("finite stochastic language".parse().unwrap()), &mut reader, None, 0).unwrap();
		let input0 = EbiInput::Trait(object0, file_handler0);
		let mut reader = MultipleReader::from_file(File::open("./testfiles/a-b.slang").unwrap());
		let (object1, file_handler1) = ebi_input::read_as_trait(&("finite stochastic language".parse().unwrap()), &mut reader, None, 0).unwrap();
		let input1 = EbiInput::Trait(object1, file_handler1);
		let input2 = EbiInput::Usize(1, &TEST_INPUT_TYPE_USIZE);
		let inputs = vec![input0, input1, input2];

		if let EbiCommand::Command{execute, output_type, ..} = crate::ebi_commands::ebi_command_conformance::EBI_CONFORMANCE_UEMSC_SAMPLE {
			match (execute)(inputs, None) {
				Ok(output) => {
					output.test_activity_key();
					assert_eq!(&output.get_type(), output_type);
				}
				Err(e) => Err(e).unwrap(),
			}
		}
	}
	#[test]
	pub fn ebi_conf_uemsc_sample_test_2() {
		// to indicate that this test is expected to fail, add the following to src/tests/fallible_test_list.rs:
		/* 
		(
			&EBI_CONFORMANCE_UEMSC_SAMPLE,
			&[
				"trait finite stochastic language#./testfiles/ba-aa-ab.slang",
				"trait finite stochastic language#./testfiles/empty.xes",
				"usize 1"			
			]
		),
		*/
		if ebi_objects::ebi_arithmetic::is_exact_globally() {
			return;
		}
		let mut reader = MultipleReader::from_file(File::open("./testfiles/ba-aa-ab.slang").unwrap());
		let (object0, file_handler0) = ebi_input::read_as_trait(&("finite stochastic language".parse().unwrap()), &mut reader, None, 0).unwrap();
		let input0 = EbiInput::Trait(object0, file_handler0);
		let mut reader = MultipleReader::from_file(File::open("./testfiles/empty.xes").unwrap());
		let (object1, file_handler1) = ebi_input::read_as_trait(&("finite stochastic language".parse().unwrap()), &mut reader, None, 0).unwrap();
		let input1 = EbiInput::Trait(object1, file_handler1);
		let input2 = EbiInput::Usize(1, &TEST_INPUT_TYPE_USIZE);
		let inputs = vec![input0, input1, input2];

		if let EbiCommand::Command{execute, output_type, ..} = crate::ebi_commands::ebi_command_conformance::EBI_CONFORMANCE_UEMSC_SAMPLE {
			match (execute)(inputs, None) {
				Ok(output) => {
					output.test_activity_key();
					assert_eq!(&output.get_type(), output_type);
				}
				Err(e) => Err(e).unwrap(),
			}
		}
	}
	#[test]
	pub fn ebi_conf_uemsc_sample_test_3() {
		// to indicate that this test is expected to fail, add the following to src/tests/fallible_test_list.rs:
		/* 
		(
			&EBI_CONFORMANCE_UEMSC_SAMPLE,
			&[
				"trait finite stochastic language#./testfiles/ba-aa-ab.slang",
				"trait finite stochastic language#./testfiles/acb-abc-ad-aded-adeded-adededed.slang",
				"usize 1"			
			]
		),
		*/
		if ebi_objects::ebi_arithmetic::is_exact_globally() {
			return;
		}
		let mut reader = MultipleReader::from_file(File::open("./testfiles/ba-aa-ab.slang").unwrap());
		let (object0, file_handler0) = ebi_input::read_as_trait(&("finite stochastic language".parse().unwrap()), &mut reader, None, 0).unwrap();
		let input0 = EbiInput::Trait(object0, file_handler0);
		let mut reader = MultipleReader::from_file(File::open("./testfiles/acb-abc-ad-aded-adeded-adededed.slang").unwrap());
		let (object1, file_handler1) = ebi_input::read_as_trait(&("finite stochastic language".parse().unwrap()), &mut reader, None, 0).unwrap();
		let input1 = EbiInput::Trait(object1, file_handler1);
		let input2 = EbiInput::Usize(1, &TEST_INPUT_TYPE_USIZE);
		let inputs = vec![input0, input1, input2];

		if let EbiCommand::Command{execute, output_type, ..} = crate::ebi_commands::ebi_command_conformance::EBI_CONFORMANCE_UEMSC_SAMPLE {
			match (execute)(inputs, None) {
				Ok(output) => {
					output.test_activity_key();
					assert_eq!(&output.get_type(), output_type);
				}
				Err(e) => Err(e).unwrap(),
			}
		}
	}


	// ==== group confns ====


	// ==== command ali ====
	#[test]
	pub fn ebi_confns_ali_test_0() {
		// to indicate that this test is expected to fail, add the following to src/tests/fallible_test_list.rs:
		/* 
		(
			&EBI_CONFORMANCE_NON_STOCHASTIC_ALIGNMENTS,
			&[
				"trait finite stochastic language#./testfiles/ba-aa-ab.slang",
				"trait semantics#./testfiles/seq(a-xor(b-c)).sptree"			
			]
		),
		*/
		
		let mut reader = MultipleReader::from_file(File::open("./testfiles/ba-aa-ab.slang").unwrap());
		let (object0, file_handler0) = ebi_input::read_as_trait(&("finite stochastic language".parse().unwrap()), &mut reader, None, 0).unwrap();
		let input0 = EbiInput::Trait(object0, file_handler0);
		let mut reader = MultipleReader::from_file(File::open("./testfiles/seq(a-xor(b-c)).sptree").unwrap());
		let (object1, file_handler1) = ebi_input::read_as_trait(&("semantics".parse().unwrap()), &mut reader, None, 0).unwrap();
		let input1 = EbiInput::Trait(object1, file_handler1);
		let inputs = vec![input0, input1];

		if let EbiCommand::Command{execute, output_type, ..} = crate::ebi_commands::ebi_command_conformance_non_stochastic::EBI_CONFORMANCE_NON_STOCHASTIC_ALIGNMENTS {
			match (execute)(inputs, None) {
				Ok(output) => {
					output.test_activity_key();
					assert_eq!(&output.get_type(), output_type);
				}
				Err(e) => Err(e).unwrap(),
			}
		}
	}
	#[test]
	pub fn ebi_confns_ali_test_1() {
		// to indicate that this test is expected to fail, add the following to src/tests/fallible_test_list.rs:
		/* 
		(
			&EBI_CONFORMANCE_NON_STOCHASTIC_ALIGNMENTS,
			&[
				"trait finite stochastic language#./testfiles/ba-aa-ab.slang",
				"trait semantics#./testfiles/ba-aa-ab.slang"			
			]
		),
		*/
		
		let mut reader = MultipleReader::from_file(File::open("./testfiles/ba-aa-ab.slang").unwrap());
		let (object0, file_handler0) = ebi_input::read_as_trait(&("finite stochastic language".parse().unwrap()), &mut reader, None, 0).unwrap();
		let input0 = EbiInput::Trait(object0, file_handler0);
		let mut reader = MultipleReader::from_file(File::open("./testfiles/ba-aa-ab.slang").unwrap());
		let (object1, file_handler1) = ebi_input::read_as_trait(&("semantics".parse().unwrap()), &mut reader, None, 0).unwrap();
		let input1 = EbiInput::Trait(object1, file_handler1);
		let inputs = vec![input0, input1];

		if let EbiCommand::Command{execute, output_type, ..} = crate::ebi_commands::ebi_command_conformance_non_stochastic::EBI_CONFORMANCE_NON_STOCHASTIC_ALIGNMENTS {
			match (execute)(inputs, None) {
				Ok(output) => {
					output.test_activity_key();
					assert_eq!(&output.get_type(), output_type);
				}
				Err(e) => Err(e).unwrap(),
			}
		}
	}
	#[test]
	pub fn ebi_confns_ali_test_2() {
		// to indicate that this test is expected to fail, add the following to src/tests/fallible_test_list.rs:
		/* 
		(
			&EBI_CONFORMANCE_NON_STOCHASTIC_ALIGNMENTS,
			&[
				"trait finite stochastic language#./testfiles/ba-aa-ab.slang",
				"trait semantics#./testfiles/model.bpmn"			
			]
		),
		*/
		
		let mut reader = MultipleReader::from_file(File::open("./testfiles/ba-aa-ab.slang").unwrap());
		let (object0, file_handler0) = ebi_input::read_as_trait(&("finite stochastic language".parse().unwrap()), &mut reader, None, 0).unwrap();
		let input0 = EbiInput::Trait(object0, file_handler0);
		let mut reader = MultipleReader::from_file(File::open("./testfiles/model.bpmn").unwrap());
		let (object1, file_handler1) = ebi_input::read_as_trait(&("semantics".parse().unwrap()), &mut reader, None, 0).unwrap();
		let input1 = EbiInput::Trait(object1, file_handler1);
		let inputs = vec![input0, input1];

		if let EbiCommand::Command{execute, output_type, ..} = crate::ebi_commands::ebi_command_conformance_non_stochastic::EBI_CONFORMANCE_NON_STOCHASTIC_ALIGNMENTS {
			match (execute)(inputs, None) {
				Ok(output) => {
					output.test_activity_key();
					assert_eq!(&output.get_type(), output_type);
				}
				Err(e) => Err(e).unwrap(),
			}
		}
	}
	#[test]
	pub fn ebi_confns_ali_test_3() {
		// to indicate that this test is expected to fail, add the following to src/tests/fallible_test_list.rs:
		/* 
		(
			&EBI_CONFORMANCE_NON_STOCHASTIC_ALIGNMENTS,
			&[
				"trait finite stochastic language#./testfiles/ba-aa-ab.slang",
				"trait semantics#./testfiles/a-b.slang"			
			]
		),
		*/
		
		let mut reader = MultipleReader::from_file(File::open("./testfiles/ba-aa-ab.slang").unwrap());
		let (object0, file_handler0) = ebi_input::read_as_trait(&("finite stochastic language".parse().unwrap()), &mut reader, None, 0).unwrap();
		let input0 = EbiInput::Trait(object0, file_handler0);
		let mut reader = MultipleReader::from_file(File::open("./testfiles/a-b.slang").unwrap());
		let (object1, file_handler1) = ebi_input::read_as_trait(&("semantics".parse().unwrap()), &mut reader, None, 0).unwrap();
		let input1 = EbiInput::Trait(object1, file_handler1);
		let inputs = vec![input0, input1];

		if let EbiCommand::Command{execute, output_type, ..} = crate::ebi_commands::ebi_command_conformance_non_stochastic::EBI_CONFORMANCE_NON_STOCHASTIC_ALIGNMENTS {
			match (execute)(inputs, None) {
				Ok(output) => {
					output.test_activity_key();
					assert_eq!(&output.get_type(), output_type);
				}
				Err(e) => Err(e).unwrap(),
			}
		}
	}


	// ==== command eep ====
	#[test]
	pub fn ebi_confns_eep_test_0() {
		// this test has been indicated as to be expected to fail
		
		let mut reader = MultipleReader::from_file(File::open("./testfiles/aa-ab-ba.sali").unwrap());
		let object0 = read_as_object_with_file_handler(&"stochastic language of alignments".parse().unwrap(), &mut reader, None, 0, &mut None, "stochastic language of alignments".parse().unwrap()).unwrap();
		let input0 = EbiInput::Object(object0, "stochastic language of alignments".parse().unwrap());
		let mut reader = MultipleReader::from_file(File::open("./testfiles/seq(a-xor(b-c)).sptree").unwrap());
		let (object1, file_handler1) = ebi_input::read_as_trait(&("semantics".parse().unwrap()), &mut reader, None, 0).unwrap();
		let input1 = EbiInput::Trait(object1, file_handler1);
		let inputs = vec![input0, input1];

		if let EbiCommand::Command{execute, ..} = crate::ebi_commands::ebi_command_conformance_non_stochastic::EBI_CONFORMANCE_NON_STOCHASTIC_ESCAPING_EDGES_PRECISION {
			assert!(((execute)(inputs, None)).is_err())
		}
	}
	#[test]
	pub fn ebi_confns_eep_test_1() {
		// this test has been indicated as to be expected to fail
		
		let mut reader = MultipleReader::from_file(File::open("./testfiles/aa-ab-ba.sali").unwrap());
		let object0 = read_as_object_with_file_handler(&"stochastic language of alignments".parse().unwrap(), &mut reader, None, 0, &mut None, "stochastic language of alignments".parse().unwrap()).unwrap();
		let input0 = EbiInput::Object(object0, "stochastic language of alignments".parse().unwrap());
		let mut reader = MultipleReader::from_file(File::open("./testfiles/ba-aa-ab.slang").unwrap());
		let (object1, file_handler1) = ebi_input::read_as_trait(&("semantics".parse().unwrap()), &mut reader, None, 0).unwrap();
		let input1 = EbiInput::Trait(object1, file_handler1);
		let inputs = vec![input0, input1];

		if let EbiCommand::Command{execute, ..} = crate::ebi_commands::ebi_command_conformance_non_stochastic::EBI_CONFORMANCE_NON_STOCHASTIC_ESCAPING_EDGES_PRECISION {
			assert!(((execute)(inputs, None)).is_err())
		}
	}
	#[test]
	pub fn ebi_confns_eep_test_2() {
		// to indicate that this test is expected to fail, add the following to src/tests/fallible_test_list.rs:
		/* 
		(
			&EBI_CONFORMANCE_NON_STOCHASTIC_ESCAPING_EDGES_PRECISION,
			&[
				"object stochastic language of alignments#./testfiles/aa-ab-ba.sali",
				"trait semantics#./testfiles/model.bpmn"			
			]
		),
		*/
		
		let mut reader = MultipleReader::from_file(File::open("./testfiles/aa-ab-ba.sali").unwrap());
		let object0 = read_as_object_with_file_handler(&"stochastic language of alignments".parse().unwrap(), &mut reader, None, 0, &mut None, "stochastic language of alignments".parse().unwrap()).unwrap();
		let input0 = EbiInput::Object(object0, "stochastic language of alignments".parse().unwrap());
		let mut reader = MultipleReader::from_file(File::open("./testfiles/model.bpmn").unwrap());
		let (object1, file_handler1) = ebi_input::read_as_trait(&("semantics".parse().unwrap()), &mut reader, None, 0).unwrap();
		let input1 = EbiInput::Trait(object1, file_handler1);
		let inputs = vec![input0, input1];

		if let EbiCommand::Command{execute, output_type, ..} = crate::ebi_commands::ebi_command_conformance_non_stochastic::EBI_CONFORMANCE_NON_STOCHASTIC_ESCAPING_EDGES_PRECISION {
			match (execute)(inputs, None) {
				Ok(output) => {
					output.test_activity_key();
					assert_eq!(&output.get_type(), output_type);
				}
				Err(e) => Err(e).unwrap(),
			}
		}
	}
	#[test]
	pub fn ebi_confns_eep_test_3() {
		// this test has been indicated as to be expected to fail
		
		let mut reader = MultipleReader::from_file(File::open("./testfiles/aa-ab-ba.sali").unwrap());
		let object0 = read_as_object_with_file_handler(&"stochastic language of alignments".parse().unwrap(), &mut reader, None, 0, &mut None, "stochastic language of alignments".parse().unwrap()).unwrap();
		let input0 = EbiInput::Object(object0, "stochastic language of alignments".parse().unwrap());
		let mut reader = MultipleReader::from_file(File::open("./testfiles/a-b.slang").unwrap());
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
		// to indicate that this test is expected to fail, add the following to src/tests/fallible_test_list.rs:
		/* 
		(
			&EBI_CONFORMANCE_NON_STOCHASTIC_SET_ALIGNMENTS,
			&[
				"trait finite language#./testfiles/ba-aa-ab.slang",
				"trait semantics#./testfiles/seq(a-xor(b-c)).sptree"			
			]
		),
		*/
		
		let mut reader = MultipleReader::from_file(File::open("./testfiles/ba-aa-ab.slang").unwrap());
		let (object0, file_handler0) = ebi_input::read_as_trait(&("finite language".parse().unwrap()), &mut reader, None, 0).unwrap();
		let input0 = EbiInput::Trait(object0, file_handler0);
		let mut reader = MultipleReader::from_file(File::open("./testfiles/seq(a-xor(b-c)).sptree").unwrap());
		let (object1, file_handler1) = ebi_input::read_as_trait(&("semantics".parse().unwrap()), &mut reader, None, 0).unwrap();
		let input1 = EbiInput::Trait(object1, file_handler1);
		let inputs = vec![input0, input1];

		if let EbiCommand::Command{execute, output_type, ..} = crate::ebi_commands::ebi_command_conformance_non_stochastic::EBI_CONFORMANCE_NON_STOCHASTIC_SET_ALIGNMENTS {
			match (execute)(inputs, None) {
				Ok(output) => {
					output.test_activity_key();
					assert_eq!(&output.get_type(), output_type);
				}
				Err(e) => Err(e).unwrap(),
			}
		}
	}
	#[test]
	pub fn ebi_confns_setali_test_1() {
		// to indicate that this test is expected to fail, add the following to src/tests/fallible_test_list.rs:
		/* 
		(
			&EBI_CONFORMANCE_NON_STOCHASTIC_SET_ALIGNMENTS,
			&[
				"trait finite language#./testfiles/ba-aa-ab.slang",
				"trait semantics#./testfiles/ba-aa-ab.slang"			
			]
		),
		*/
		
		let mut reader = MultipleReader::from_file(File::open("./testfiles/ba-aa-ab.slang").unwrap());
		let (object0, file_handler0) = ebi_input::read_as_trait(&("finite language".parse().unwrap()), &mut reader, None, 0).unwrap();
		let input0 = EbiInput::Trait(object0, file_handler0);
		let mut reader = MultipleReader::from_file(File::open("./testfiles/ba-aa-ab.slang").unwrap());
		let (object1, file_handler1) = ebi_input::read_as_trait(&("semantics".parse().unwrap()), &mut reader, None, 0).unwrap();
		let input1 = EbiInput::Trait(object1, file_handler1);
		let inputs = vec![input0, input1];

		if let EbiCommand::Command{execute, output_type, ..} = crate::ebi_commands::ebi_command_conformance_non_stochastic::EBI_CONFORMANCE_NON_STOCHASTIC_SET_ALIGNMENTS {
			match (execute)(inputs, None) {
				Ok(output) => {
					output.test_activity_key();
					assert_eq!(&output.get_type(), output_type);
				}
				Err(e) => Err(e).unwrap(),
			}
		}
	}
	#[test]
	pub fn ebi_confns_setali_test_2() {
		// to indicate that this test is expected to fail, add the following to src/tests/fallible_test_list.rs:
		/* 
		(
			&EBI_CONFORMANCE_NON_STOCHASTIC_SET_ALIGNMENTS,
			&[
				"trait finite language#./testfiles/ba-aa-ab.slang",
				"trait semantics#./testfiles/model.bpmn"			
			]
		),
		*/
		
		let mut reader = MultipleReader::from_file(File::open("./testfiles/ba-aa-ab.slang").unwrap());
		let (object0, file_handler0) = ebi_input::read_as_trait(&("finite language".parse().unwrap()), &mut reader, None, 0).unwrap();
		let input0 = EbiInput::Trait(object0, file_handler0);
		let mut reader = MultipleReader::from_file(File::open("./testfiles/model.bpmn").unwrap());
		let (object1, file_handler1) = ebi_input::read_as_trait(&("semantics".parse().unwrap()), &mut reader, None, 0).unwrap();
		let input1 = EbiInput::Trait(object1, file_handler1);
		let inputs = vec![input0, input1];

		if let EbiCommand::Command{execute, output_type, ..} = crate::ebi_commands::ebi_command_conformance_non_stochastic::EBI_CONFORMANCE_NON_STOCHASTIC_SET_ALIGNMENTS {
			match (execute)(inputs, None) {
				Ok(output) => {
					output.test_activity_key();
					assert_eq!(&output.get_type(), output_type);
				}
				Err(e) => Err(e).unwrap(),
			}
		}
	}
	#[test]
	pub fn ebi_confns_setali_test_3() {
		// to indicate that this test is expected to fail, add the following to src/tests/fallible_test_list.rs:
		/* 
		(
			&EBI_CONFORMANCE_NON_STOCHASTIC_SET_ALIGNMENTS,
			&[
				"trait finite language#./testfiles/ba-aa-ab.slang",
				"trait semantics#./testfiles/a-b.slang"			
			]
		),
		*/
		
		let mut reader = MultipleReader::from_file(File::open("./testfiles/ba-aa-ab.slang").unwrap());
		let (object0, file_handler0) = ebi_input::read_as_trait(&("finite language".parse().unwrap()), &mut reader, None, 0).unwrap();
		let input0 = EbiInput::Trait(object0, file_handler0);
		let mut reader = MultipleReader::from_file(File::open("./testfiles/a-b.slang").unwrap());
		let (object1, file_handler1) = ebi_input::read_as_trait(&("semantics".parse().unwrap()), &mut reader, None, 0).unwrap();
		let input1 = EbiInput::Trait(object1, file_handler1);
		let inputs = vec![input0, input1];

		if let EbiCommand::Command{execute, output_type, ..} = crate::ebi_commands::ebi_command_conformance_non_stochastic::EBI_CONFORMANCE_NON_STOCHASTIC_SET_ALIGNMENTS {
			match (execute)(inputs, None) {
				Ok(output) => {
					output.test_activity_key();
					assert_eq!(&output.get_type(), output_type);
				}
				Err(e) => Err(e).unwrap(),
			}
		}
	}


	// ==== command tfit ====
	#[test]
	pub fn ebi_confns_tfit_test_0() {
		// to indicate that this test is expected to fail, add the following to src/tests/fallible_test_list.rs:
		/* 
		(
			&EBI_CONFORMANCE_NON_STOCHASTIC_TRACE_FITNESS,
			&[
				"object stochastic language of alignments#./testfiles/aa-ab-ba.sali"			
			]
		),
		*/
		
		let mut reader = MultipleReader::from_file(File::open("./testfiles/aa-ab-ba.sali").unwrap());
		let object0 = read_as_object_with_file_handler(&"stochastic language of alignments".parse().unwrap(), &mut reader, None, 0, &mut None, "stochastic language of alignments".parse().unwrap()).unwrap();
		let input0 = EbiInput::Object(object0, "stochastic language of alignments".parse().unwrap());
		let inputs = vec![input0];

		if let EbiCommand::Command{execute, output_type, ..} = crate::ebi_commands::ebi_command_conformance_non_stochastic::EBI_CONFORMANCE_NON_STOCHASTIC_TRACE_FITNESS {
			match (execute)(inputs, None) {
				Ok(output) => {
					output.test_activity_key();
					assert_eq!(&output.get_type(), output_type);
				}
				Err(e) => Err(e).unwrap(),
			}
		}
	}


	// ==== group conv ====


	// ==== command bpmn ====
	#[test]
	pub fn ebi_conv_bpmn_test_0() {
		// to indicate that this test is expected to fail, add the following to src/tests/fallible_test_list.rs:
		/* 
		(
			&EBI_CONVERT_BPMN,
			&[
				"object business process model and notation#./testfiles/seq(a-xor(b-c)).sptree"			
			]
		),
		*/
		
		let mut reader = MultipleReader::from_file(File::open("./testfiles/seq(a-xor(b-c)).sptree").unwrap());
		let object0 = read_as_object_with_file_handler(&"business process model and notation".parse().unwrap(), &mut reader, None, 0, &mut None, "stochastic process tree".parse().unwrap()).unwrap();
		let input0 = EbiInput::Object(object0, "stochastic process tree".parse().unwrap());
		let inputs = vec![input0];

		if let EbiCommand::Command{execute, output_type, ..} = crate::ebi_commands::ebi_command_convert::EBI_CONVERT_BPMN {
			match (execute)(inputs, None) {
				Ok(output) => {
					output.test_activity_key();
					assert_eq!(&output.get_type(), output_type);
				}
				Err(e) => Err(e).unwrap(),
			}
		}
	}
	#[test]
	pub fn ebi_conv_bpmn_test_1() {
		// to indicate that this test is expected to fail, add the following to src/tests/fallible_test_list.rs:
		/* 
		(
			&EBI_CONVERT_BPMN,
			&[
				"object business process model and notation#./testfiles/model.bpmn"			
			]
		),
		*/
		
		let mut reader = MultipleReader::from_file(File::open("./testfiles/model.bpmn").unwrap());
		let object0 = read_as_object_with_file_handler(&"business process model and notation".parse().unwrap(), &mut reader, None, 0, &mut None, "business process model and notation".parse().unwrap()).unwrap();
		let input0 = EbiInput::Object(object0, "business process model and notation".parse().unwrap());
		let inputs = vec![input0];

		if let EbiCommand::Command{execute, output_type, ..} = crate::ebi_commands::ebi_command_convert::EBI_CONVERT_BPMN {
			match (execute)(inputs, None) {
				Ok(output) => {
					output.test_activity_key();
					assert_eq!(&output.get_type(), output_type);
				}
				Err(e) => Err(e).unwrap(),
			}
		}
	}
	#[test]
	pub fn ebi_conv_bpmn_test_2() {
		// to indicate that this test is expected to fail, add the following to src/tests/fallible_test_list.rs:
		/* 
		(
			&EBI_CONVERT_BPMN,
			&[
				"object business process model and notation#./testfiles/a-b_star.dfm"			
			]
		),
		*/
		
		let mut reader = MultipleReader::from_file(File::open("./testfiles/a-b_star.dfm").unwrap());
		let object0 = read_as_object_with_file_handler(&"business process model and notation".parse().unwrap(), &mut reader, None, 0, &mut None, "directly follows model".parse().unwrap()).unwrap();
		let input0 = EbiInput::Object(object0, "directly follows model".parse().unwrap());
		let inputs = vec![input0];

		if let EbiCommand::Command{execute, output_type, ..} = crate::ebi_commands::ebi_command_convert::EBI_CONVERT_BPMN {
			match (execute)(inputs, None) {
				Ok(output) => {
					output.test_activity_key();
					assert_eq!(&output.get_type(), output_type);
				}
				Err(e) => Err(e).unwrap(),
			}
		}
	}
	#[test]
	pub fn ebi_conv_bpmn_test_3() {
		// to indicate that this test is expected to fail, add the following to src/tests/fallible_test_list.rs:
		/* 
		(
			&EBI_CONVERT_BPMN,
			&[
				"object business process model and notation#./testfiles/bpic12-a.xes.gz-dfg.dfg"			
			]
		),
		*/
		
		let mut reader = MultipleReader::from_file(File::open("./testfiles/bpic12-a.xes.gz-dfg.dfg").unwrap());
		let object0 = read_as_object_with_file_handler(&"business process model and notation".parse().unwrap(), &mut reader, None, 0, &mut None, "directly follows graph".parse().unwrap()).unwrap();
		let input0 = EbiInput::Object(object0, "directly follows graph".parse().unwrap());
		let inputs = vec![input0];

		if let EbiCommand::Command{execute, output_type, ..} = crate::ebi_commands::ebi_command_convert::EBI_CONVERT_BPMN {
			match (execute)(inputs, None) {
				Ok(output) => {
					output.test_activity_key();
					assert_eq!(&output.get_type(), output_type);
				}
				Err(e) => Err(e).unwrap(),
			}
		}
	}


	// ==== command lang ====
	#[test]
	pub fn ebi_conv_lang_test_0() {
		// to indicate that this test is expected to fail, add the following to src/tests/fallible_test_list.rs:
		/* 
		(
			&EBI_CONVERT_LANG,
			&[
				"object finite language#./testfiles/empty.xes"			
			]
		),
		*/
		
		let mut reader = MultipleReader::from_file(File::open("./testfiles/empty.xes").unwrap());
		let object0 = read_as_object_with_file_handler(&"finite language".parse().unwrap(), &mut reader, None, 0, &mut None, "extensible event stream".parse().unwrap()).unwrap();
		let input0 = EbiInput::Object(object0, "extensible event stream".parse().unwrap());
		let inputs = vec![input0];

		if let EbiCommand::Command{execute, output_type, ..} = crate::ebi_commands::ebi_command_convert::EBI_CONVERT_LANG {
			match (execute)(inputs, None) {
				Ok(output) => {
					output.test_activity_key();
					assert_eq!(&output.get_type(), output_type);
				}
				Err(e) => Err(e).unwrap(),
			}
		}
	}
	#[test]
	pub fn ebi_conv_lang_test_1() {
		// to indicate that this test is expected to fail, add the following to src/tests/fallible_test_list.rs:
		/* 
		(
			&EBI_CONVERT_LANG,
			&[
				"object finite language#./testfiles/empty.lang"			
			]
		),
		*/
		
		let mut reader = MultipleReader::from_file(File::open("./testfiles/empty.lang").unwrap());
		let object0 = read_as_object_with_file_handler(&"finite language".parse().unwrap(), &mut reader, None, 0, &mut None, "finite language".parse().unwrap()).unwrap();
		let input0 = EbiInput::Object(object0, "finite language".parse().unwrap());
		let inputs = vec![input0];

		if let EbiCommand::Command{execute, output_type, ..} = crate::ebi_commands::ebi_command_convert::EBI_CONVERT_LANG {
			match (execute)(inputs, None) {
				Ok(output) => {
					output.test_activity_key();
					assert_eq!(&output.get_type(), output_type);
				}
				Err(e) => Err(e).unwrap(),
			}
		}
	}
	#[test]
	pub fn ebi_conv_lang_test_2() {
		// to indicate that this test is expected to fail, add the following to src/tests/fallible_test_list.rs:
		/* 
		(
			&EBI_CONVERT_LANG,
			&[
				"object finite language#./testfiles/bb.lang"			
			]
		),
		*/
		
		let mut reader = MultipleReader::from_file(File::open("./testfiles/bb.lang").unwrap());
		let object0 = read_as_object_with_file_handler(&"finite language".parse().unwrap(), &mut reader, None, 0, &mut None, "finite language".parse().unwrap()).unwrap();
		let input0 = EbiInput::Object(object0, "finite language".parse().unwrap());
		let inputs = vec![input0];

		if let EbiCommand::Command{execute, output_type, ..} = crate::ebi_commands::ebi_command_convert::EBI_CONVERT_LANG {
			match (execute)(inputs, None) {
				Ok(output) => {
					output.test_activity_key();
					assert_eq!(&output.get_type(), output_type);
				}
				Err(e) => Err(e).unwrap(),
			}
		}
	}
	#[test]
	pub fn ebi_conv_lang_test_3() {
		// to indicate that this test is expected to fail, add the following to src/tests/fallible_test_list.rs:
		/* 
		(
			&EBI_CONVERT_LANG,
			&[
				"object finite language#./testfiles/aa.lang"			
			]
		),
		*/
		
		let mut reader = MultipleReader::from_file(File::open("./testfiles/aa.lang").unwrap());
		let object0 = read_as_object_with_file_handler(&"finite language".parse().unwrap(), &mut reader, None, 0, &mut None, "finite language".parse().unwrap()).unwrap();
		let input0 = EbiInput::Object(object0, "finite language".parse().unwrap());
		let inputs = vec![input0];

		if let EbiCommand::Command{execute, output_type, ..} = crate::ebi_commands::ebi_command_convert::EBI_CONVERT_LANG {
			match (execute)(inputs, None) {
				Ok(output) => {
					output.test_activity_key();
					assert_eq!(&output.get_type(), output_type);
				}
				Err(e) => Err(e).unwrap(),
			}
		}
	}


	// ==== command log ====
	#[test]
	pub fn ebi_conv_log_test_0() {
		// to indicate that this test is expected to fail, add the following to src/tests/fallible_test_list.rs:
		/* 
		(
			&EBI_CONVERT_LOG,
			&[
				"object event log#./testfiles/empty.xes"			
			]
		),
		*/
		
		let mut reader = MultipleReader::from_file(File::open("./testfiles/empty.xes").unwrap());
		let object0 = read_as_object_with_file_handler(&"event log".parse().unwrap(), &mut reader, None, 0, &mut None, "extensible event stream".parse().unwrap()).unwrap();
		let input0 = EbiInput::Object(object0, "extensible event stream".parse().unwrap());
		let inputs = vec![input0];

		if let EbiCommand::Command{execute, output_type, ..} = crate::ebi_commands::ebi_command_convert::EBI_CONVERT_LOG {
			match (execute)(inputs, None) {
				Ok(output) => {
					output.test_activity_key();
					assert_eq!(&output.get_type(), output_type);
				}
				Err(e) => Err(e).unwrap(),
			}
		}
	}
	#[test]
	pub fn ebi_conv_log_test_1() {
		// to indicate that this test is expected to fail, add the following to src/tests/fallible_test_list.rs:
		/* 
		(
			&EBI_CONVERT_LOG,
			&[
				"object event log#./testfiles/simple_log_markovian_abstraction.xes"			
			]
		),
		*/
		
		let mut reader = MultipleReader::from_file(File::open("./testfiles/simple_log_markovian_abstraction.xes").unwrap());
		let object0 = read_as_object_with_file_handler(&"event log".parse().unwrap(), &mut reader, None, 0, &mut None, "extensible event stream".parse().unwrap()).unwrap();
		let input0 = EbiInput::Object(object0, "extensible event stream".parse().unwrap());
		let inputs = vec![input0];

		if let EbiCommand::Command{execute, output_type, ..} = crate::ebi_commands::ebi_command_convert::EBI_CONVERT_LOG {
			match (execute)(inputs, None) {
				Ok(output) => {
					output.test_activity_key();
					assert_eq!(&output.get_type(), output_type);
				}
				Err(e) => Err(e).unwrap(),
			}
		}
	}
	#[test]
	pub fn ebi_conv_log_test_2() {
		// to indicate that this test is expected to fail, add the following to src/tests/fallible_test_list.rs:
		/* 
		(
			&EBI_CONVERT_LOG,
			&[
				"object event log#./testfiles/a-b-double.xes"			
			]
		),
		*/
		
		let mut reader = MultipleReader::from_file(File::open("./testfiles/a-b-double.xes").unwrap());
		let object0 = read_as_object_with_file_handler(&"event log".parse().unwrap(), &mut reader, None, 0, &mut None, "extensible event stream".parse().unwrap()).unwrap();
		let input0 = EbiInput::Object(object0, "extensible event stream".parse().unwrap());
		let inputs = vec![input0];

		if let EbiCommand::Command{execute, output_type, ..} = crate::ebi_commands::ebi_command_convert::EBI_CONVERT_LOG {
			match (execute)(inputs, None) {
				Ok(output) => {
					output.test_activity_key();
					assert_eq!(&output.get_type(), output_type);
				}
				Err(e) => Err(e).unwrap(),
			}
		}
	}
	#[test]
	pub fn ebi_conv_log_test_3() {
		// to indicate that this test is expected to fail, add the following to src/tests/fallible_test_list.rs:
		/* 
		(
			&EBI_CONVERT_LOG,
			&[
				"object event log#./testfiles/a-b.xes.gz"			
			]
		),
		*/
		
		let mut reader = MultipleReader::from_file(File::open("./testfiles/a-b.xes.gz").unwrap());
		let object0 = read_as_object_with_file_handler(&"event log".parse().unwrap(), &mut reader, None, 0, &mut None, "compressed event log".parse().unwrap()).unwrap();
		let input0 = EbiInput::Object(object0, "compressed event log".parse().unwrap());
		let inputs = vec![input0];

		if let EbiCommand::Command{execute, output_type, ..} = crate::ebi_commands::ebi_command_convert::EBI_CONVERT_LOG {
			match (execute)(inputs, None) {
				Ok(output) => {
					output.test_activity_key();
					assert_eq!(&output.get_type(), output_type);
				}
				Err(e) => Err(e).unwrap(),
			}
		}
	}


	// ==== command lpn ====
	#[test]
	pub fn ebi_conv_lpn_test_0() {
		// to indicate that this test is expected to fail, add the following to src/tests/fallible_test_list.rs:
		/* 
		(
			&EBI_CONVERT_LPN,
			&[
				"object labelled Petri net#./testfiles/seq(a-xor(b-c)).sptree"			
			]
		),
		*/
		
		let mut reader = MultipleReader::from_file(File::open("./testfiles/seq(a-xor(b-c)).sptree").unwrap());
		let object0 = read_as_object_with_file_handler(&"labelled Petri net".parse().unwrap(), &mut reader, None, 0, &mut None, "stochastic process tree".parse().unwrap()).unwrap();
		let input0 = EbiInput::Object(object0, "stochastic process tree".parse().unwrap());
		let inputs = vec![input0];

		if let EbiCommand::Command{execute, output_type, ..} = crate::ebi_commands::ebi_command_convert::EBI_CONVERT_LPN {
			match (execute)(inputs, None) {
				Ok(output) => {
					output.test_activity_key();
					assert_eq!(&output.get_type(), output_type);
				}
				Err(e) => Err(e).unwrap(),
			}
		}
	}
	#[test]
	pub fn ebi_conv_lpn_test_1() {
		// to indicate that this test is expected to fail, add the following to src/tests/fallible_test_list.rs:
		/* 
		(
			&EBI_CONVERT_LPN,
			&[
				"object labelled Petri net#./testfiles/a-b_star.dfm"			
			]
		),
		*/
		
		let mut reader = MultipleReader::from_file(File::open("./testfiles/a-b_star.dfm").unwrap());
		let object0 = read_as_object_with_file_handler(&"labelled Petri net".parse().unwrap(), &mut reader, None, 0, &mut None, "directly follows model".parse().unwrap()).unwrap();
		let input0 = EbiInput::Object(object0, "directly follows model".parse().unwrap());
		let inputs = vec![input0];

		if let EbiCommand::Command{execute, output_type, ..} = crate::ebi_commands::ebi_command_convert::EBI_CONVERT_LPN {
			match (execute)(inputs, None) {
				Ok(output) => {
					output.test_activity_key();
					assert_eq!(&output.get_type(), output_type);
				}
				Err(e) => Err(e).unwrap(),
			}
		}
	}
	#[test]
	pub fn ebi_conv_lpn_test_2() {
		// to indicate that this test is expected to fail, add the following to src/tests/fallible_test_list.rs:
		/* 
		(
			&EBI_CONVERT_LPN,
			&[
				"object labelled Petri net#./testfiles/bpic12-a.xes.gz-dfg.dfg"			
			]
		),
		*/
		
		let mut reader = MultipleReader::from_file(File::open("./testfiles/bpic12-a.xes.gz-dfg.dfg").unwrap());
		let object0 = read_as_object_with_file_handler(&"labelled Petri net".parse().unwrap(), &mut reader, None, 0, &mut None, "directly follows graph".parse().unwrap()).unwrap();
		let input0 = EbiInput::Object(object0, "directly follows graph".parse().unwrap());
		let inputs = vec![input0];

		if let EbiCommand::Command{execute, output_type, ..} = crate::ebi_commands::ebi_command_convert::EBI_CONVERT_LPN {
			match (execute)(inputs, None) {
				Ok(output) => {
					output.test_activity_key();
					assert_eq!(&output.get_type(), output_type);
				}
				Err(e) => Err(e).unwrap(),
			}
		}
	}
	#[test]
	pub fn ebi_conv_lpn_test_3() {
		// to indicate that this test is expected to fail, add the following to src/tests/fallible_test_list.rs:
		/* 
		(
			&EBI_CONVERT_LPN,
			&[
				"object labelled Petri net#./testfiles/BPI_Challenge_2013_closed_problems.xes.gz-dfg.dfg"			
			]
		),
		*/
		
		let mut reader = MultipleReader::from_file(File::open("./testfiles/BPI_Challenge_2013_closed_problems.xes.gz-dfg.dfg").unwrap());
		let object0 = read_as_object_with_file_handler(&"labelled Petri net".parse().unwrap(), &mut reader, None, 0, &mut None, "directly follows graph".parse().unwrap()).unwrap();
		let input0 = EbiInput::Object(object0, "directly follows graph".parse().unwrap());
		let inputs = vec![input0];

		if let EbiCommand::Command{execute, output_type, ..} = crate::ebi_commands::ebi_command_convert::EBI_CONVERT_LPN {
			match (execute)(inputs, None) {
				Ok(output) => {
					output.test_activity_key();
					assert_eq!(&output.get_type(), output_type);
				}
				Err(e) => Err(e).unwrap(),
			}
		}
	}


	// ==== command slang ====
	#[test]
	pub fn ebi_conv_slang_test_0() {
		// to indicate that this test is expected to fail, add the following to src/tests/fallible_test_list.rs:
		/* 
		(
			&EBI_CONVERT_SLANG,
			&[
				"object finite stochastic language#./testfiles/ba-aa-ab.slang"			
			]
		),
		*/
		
		let mut reader = MultipleReader::from_file(File::open("./testfiles/ba-aa-ab.slang").unwrap());
		let object0 = read_as_object_with_file_handler(&"finite stochastic language".parse().unwrap(), &mut reader, None, 0, &mut None, "finite stochastic language".parse().unwrap()).unwrap();
		let input0 = EbiInput::Object(object0, "finite stochastic language".parse().unwrap());
		let inputs = vec![input0];

		if let EbiCommand::Command{execute, output_type, ..} = crate::ebi_commands::ebi_command_convert::EBI_CONVERT_SLANG {
			match (execute)(inputs, None) {
				Ok(output) => {
					output.test_activity_key();
					assert_eq!(&output.get_type(), output_type);
				}
				Err(e) => Err(e).unwrap(),
			}
		}
	}
	#[test]
	pub fn ebi_conv_slang_test_1() {
		// to indicate that this test is expected to fail, add the following to src/tests/fallible_test_list.rs:
		/* 
		(
			&EBI_CONVERT_SLANG,
			&[
				"object finite stochastic language#./testfiles/a-b.slang"			
			]
		),
		*/
		
		let mut reader = MultipleReader::from_file(File::open("./testfiles/a-b.slang").unwrap());
		let object0 = read_as_object_with_file_handler(&"finite stochastic language".parse().unwrap(), &mut reader, None, 0, &mut None, "finite stochastic language".parse().unwrap()).unwrap();
		let input0 = EbiInput::Object(object0, "finite stochastic language".parse().unwrap());
		let inputs = vec![input0];

		if let EbiCommand::Command{execute, output_type, ..} = crate::ebi_commands::ebi_command_convert::EBI_CONVERT_SLANG {
			match (execute)(inputs, None) {
				Ok(output) => {
					output.test_activity_key();
					assert_eq!(&output.get_type(), output_type);
				}
				Err(e) => Err(e).unwrap(),
			}
		}
	}
	#[test]
	pub fn ebi_conv_slang_test_2() {
		// to indicate that this test is expected to fail, add the following to src/tests/fallible_test_list.rs:
		/* 
		(
			&EBI_CONVERT_SLANG,
			&[
				"object finite stochastic language#./testfiles/empty.xes"			
			]
		),
		*/
		
		let mut reader = MultipleReader::from_file(File::open("./testfiles/empty.xes").unwrap());
		let object0 = read_as_object_with_file_handler(&"finite stochastic language".parse().unwrap(), &mut reader, None, 0, &mut None, "extensible event stream".parse().unwrap()).unwrap();
		let input0 = EbiInput::Object(object0, "extensible event stream".parse().unwrap());
		let inputs = vec![input0];

		if let EbiCommand::Command{execute, output_type, ..} = crate::ebi_commands::ebi_command_convert::EBI_CONVERT_SLANG {
			match (execute)(inputs, None) {
				Ok(output) => {
					output.test_activity_key();
					assert_eq!(&output.get_type(), output_type);
				}
				Err(e) => Err(e).unwrap(),
			}
		}
	}
	#[test]
	pub fn ebi_conv_slang_test_3() {
		// to indicate that this test is expected to fail, add the following to src/tests/fallible_test_list.rs:
		/* 
		(
			&EBI_CONVERT_SLANG,
			&[
				"object finite stochastic language#./testfiles/acb-abc-ad-aded-adeded-adededed.slang"			
			]
		),
		*/
		
		let mut reader = MultipleReader::from_file(File::open("./testfiles/acb-abc-ad-aded-adeded-adededed.slang").unwrap());
		let object0 = read_as_object_with_file_handler(&"finite stochastic language".parse().unwrap(), &mut reader, None, 0, &mut None, "finite stochastic language".parse().unwrap()).unwrap();
		let input0 = EbiInput::Object(object0, "finite stochastic language".parse().unwrap());
		let inputs = vec![input0];

		if let EbiCommand::Command{execute, output_type, ..} = crate::ebi_commands::ebi_command_convert::EBI_CONVERT_SLANG {
			match (execute)(inputs, None) {
				Ok(output) => {
					output.test_activity_key();
					assert_eq!(&output.get_type(), output_type);
				}
				Err(e) => Err(e).unwrap(),
			}
		}
	}


	// ==== command sdfa ====
	#[test]
	pub fn ebi_conv_sdfa_test_0() {
		// to indicate that this test is expected to fail, add the following to src/tests/fallible_test_list.rs:
		/* 
		(
			&EBI_CONVERT_SDFA,
			&[
				"object stochastic deterministic finite automaton#./testfiles/ba-aa-ab.slang"			
			]
		),
		*/
		
		let mut reader = MultipleReader::from_file(File::open("./testfiles/ba-aa-ab.slang").unwrap());
		let object0 = read_as_object_with_file_handler(&"stochastic deterministic finite automaton".parse().unwrap(), &mut reader, None, 0, &mut None, "finite stochastic language".parse().unwrap()).unwrap();
		let input0 = EbiInput::Object(object0, "finite stochastic language".parse().unwrap());
		let inputs = vec![input0];

		if let EbiCommand::Command{execute, output_type, ..} = crate::ebi_commands::ebi_command_convert::EBI_CONVERT_SDFA {
			match (execute)(inputs, None) {
				Ok(output) => {
					output.test_activity_key();
					assert_eq!(&output.get_type(), output_type);
				}
				Err(e) => Err(e).unwrap(),
			}
		}
	}
	#[test]
	pub fn ebi_conv_sdfa_test_1() {
		// to indicate that this test is expected to fail, add the following to src/tests/fallible_test_list.rs:
		/* 
		(
			&EBI_CONVERT_SDFA,
			&[
				"object stochastic deterministic finite automaton#./testfiles/a-b.slang"			
			]
		),
		*/
		
		let mut reader = MultipleReader::from_file(File::open("./testfiles/a-b.slang").unwrap());
		let object0 = read_as_object_with_file_handler(&"stochastic deterministic finite automaton".parse().unwrap(), &mut reader, None, 0, &mut None, "finite stochastic language".parse().unwrap()).unwrap();
		let input0 = EbiInput::Object(object0, "finite stochastic language".parse().unwrap());
		let inputs = vec![input0];

		if let EbiCommand::Command{execute, output_type, ..} = crate::ebi_commands::ebi_command_convert::EBI_CONVERT_SDFA {
			match (execute)(inputs, None) {
				Ok(output) => {
					output.test_activity_key();
					assert_eq!(&output.get_type(), output_type);
				}
				Err(e) => Err(e).unwrap(),
			}
		}
	}
	#[test]
	pub fn ebi_conv_sdfa_test_2() {
		// to indicate that this test is expected to fail, add the following to src/tests/fallible_test_list.rs:
		/* 
		(
			&EBI_CONVERT_SDFA,
			&[
				"object stochastic deterministic finite automaton#./testfiles/empty.xes"			
			]
		),
		*/
		
		let mut reader = MultipleReader::from_file(File::open("./testfiles/empty.xes").unwrap());
		let object0 = read_as_object_with_file_handler(&"stochastic deterministic finite automaton".parse().unwrap(), &mut reader, None, 0, &mut None, "extensible event stream".parse().unwrap()).unwrap();
		let input0 = EbiInput::Object(object0, "extensible event stream".parse().unwrap());
		let inputs = vec![input0];

		if let EbiCommand::Command{execute, output_type, ..} = crate::ebi_commands::ebi_command_convert::EBI_CONVERT_SDFA {
			match (execute)(inputs, None) {
				Ok(output) => {
					output.test_activity_key();
					assert_eq!(&output.get_type(), output_type);
				}
				Err(e) => Err(e).unwrap(),
			}
		}
	}
	#[test]
	pub fn ebi_conv_sdfa_test_3() {
		// to indicate that this test is expected to fail, add the following to src/tests/fallible_test_list.rs:
		/* 
		(
			&EBI_CONVERT_SDFA,
			&[
				"object stochastic deterministic finite automaton#./testfiles/acb-abc-ad-aded-adeded-adededed.slang"			
			]
		),
		*/
		
		let mut reader = MultipleReader::from_file(File::open("./testfiles/acb-abc-ad-aded-adeded-adededed.slang").unwrap());
		let object0 = read_as_object_with_file_handler(&"stochastic deterministic finite automaton".parse().unwrap(), &mut reader, None, 0, &mut None, "finite stochastic language".parse().unwrap()).unwrap();
		let input0 = EbiInput::Object(object0, "finite stochastic language".parse().unwrap());
		let inputs = vec![input0];

		if let EbiCommand::Command{execute, output_type, ..} = crate::ebi_commands::ebi_command_convert::EBI_CONVERT_SDFA {
			match (execute)(inputs, None) {
				Ok(output) => {
					output.test_activity_key();
					assert_eq!(&output.get_type(), output_type);
				}
				Err(e) => Err(e).unwrap(),
			}
		}
	}


	// ==== command slpn ====
	#[test]
	pub fn ebi_conv_slpn_test_0() {
		// to indicate that this test is expected to fail, add the following to src/tests/fallible_test_list.rs:
		/* 
		(
			&EBI_CONVERT_SLPN,
			&[
				"object stochastic labelled Petri net#./testfiles/bpic12-a.xes.gz-dfg.dfg"			
			]
		),
		*/
		
		let mut reader = MultipleReader::from_file(File::open("./testfiles/bpic12-a.xes.gz-dfg.dfg").unwrap());
		let object0 = read_as_object_with_file_handler(&"stochastic labelled Petri net".parse().unwrap(), &mut reader, None, 0, &mut None, "directly follows graph".parse().unwrap()).unwrap();
		let input0 = EbiInput::Object(object0, "directly follows graph".parse().unwrap());
		let inputs = vec![input0];

		if let EbiCommand::Command{execute, output_type, ..} = crate::ebi_commands::ebi_command_convert::EBI_CONVERT_SLPN {
			match (execute)(inputs, None) {
				Ok(output) => {
					output.test_activity_key();
					assert_eq!(&output.get_type(), output_type);
				}
				Err(e) => Err(e).unwrap(),
			}
		}
	}
	#[test]
	pub fn ebi_conv_slpn_test_1() {
		// to indicate that this test is expected to fail, add the following to src/tests/fallible_test_list.rs:
		/* 
		(
			&EBI_CONVERT_SLPN,
			&[
				"object stochastic labelled Petri net#./testfiles/BPI_Challenge_2013_closed_problems.xes.gz-dfg.dfg"			
			]
		),
		*/
		
		let mut reader = MultipleReader::from_file(File::open("./testfiles/BPI_Challenge_2013_closed_problems.xes.gz-dfg.dfg").unwrap());
		let object0 = read_as_object_with_file_handler(&"stochastic labelled Petri net".parse().unwrap(), &mut reader, None, 0, &mut None, "directly follows graph".parse().unwrap()).unwrap();
		let input0 = EbiInput::Object(object0, "directly follows graph".parse().unwrap());
		let inputs = vec![input0];

		if let EbiCommand::Command{execute, output_type, ..} = crate::ebi_commands::ebi_command_convert::EBI_CONVERT_SLPN {
			match (execute)(inputs, None) {
				Ok(output) => {
					output.test_activity_key();
					assert_eq!(&output.get_type(), output_type);
				}
				Err(e) => Err(e).unwrap(),
			}
		}
	}
	#[test]
	pub fn ebi_conv_slpn_test_2() {
		// to indicate that this test is expected to fail, add the following to src/tests/fallible_test_list.rs:
		/* 
		(
			&EBI_CONVERT_SLPN,
			&[
				"object stochastic labelled Petri net#./testfiles/a-aa-bb.slpn"			
			]
		),
		*/
		
		let mut reader = MultipleReader::from_file(File::open("./testfiles/a-aa-bb.slpn").unwrap());
		let object0 = read_as_object_with_file_handler(&"stochastic labelled Petri net".parse().unwrap(), &mut reader, None, 0, &mut None, "stochastic labelled Petri net".parse().unwrap()).unwrap();
		let input0 = EbiInput::Object(object0, "stochastic labelled Petri net".parse().unwrap());
		let inputs = vec![input0];

		if let EbiCommand::Command{execute, output_type, ..} = crate::ebi_commands::ebi_command_convert::EBI_CONVERT_SLPN {
			match (execute)(inputs, None) {
				Ok(output) => {
					output.test_activity_key();
					assert_eq!(&output.get_type(), output_type);
				}
				Err(e) => Err(e).unwrap(),
			}
		}
	}
	#[test]
	pub fn ebi_conv_slpn_test_3() {
		// to indicate that this test is expected to fail, add the following to src/tests/fallible_test_list.rs:
		/* 
		(
			&EBI_CONVERT_SLPN,
			&[
				"object stochastic labelled Petri net#./testfiles/simple_markovian_abstraction.slpn"			
			]
		),
		*/
		
		let mut reader = MultipleReader::from_file(File::open("./testfiles/simple_markovian_abstraction.slpn").unwrap());
		let object0 = read_as_object_with_file_handler(&"stochastic labelled Petri net".parse().unwrap(), &mut reader, None, 0, &mut None, "stochastic labelled Petri net".parse().unwrap()).unwrap();
		let input0 = EbiInput::Object(object0, "stochastic labelled Petri net".parse().unwrap());
		let inputs = vec![input0];

		if let EbiCommand::Command{execute, output_type, ..} = crate::ebi_commands::ebi_command_convert::EBI_CONVERT_SLPN {
			match (execute)(inputs, None) {
				Ok(output) => {
					output.test_activity_key();
					assert_eq!(&output.get_type(), output_type);
				}
				Err(e) => Err(e).unwrap(),
			}
		}
	}


	// ==== command snfa ====
	#[test]
	pub fn ebi_conv_snfa_test_0() {
		// to indicate that this test is expected to fail, add the following to src/tests/fallible_test_list.rs:
		/* 
		(
			&EBI_CONVERT_SNFA,
			&[
				"object stochastic non-deterministic finite automaton#./testfiles/seq(a-xor(b-c)).sptree"			
			]
		),
		*/
		
		let mut reader = MultipleReader::from_file(File::open("./testfiles/seq(a-xor(b-c)).sptree").unwrap());
		let object0 = read_as_object_with_file_handler(&"stochastic non-deterministic finite automaton".parse().unwrap(), &mut reader, None, 0, &mut None, "stochastic process tree".parse().unwrap()).unwrap();
		let input0 = EbiInput::Object(object0, "stochastic process tree".parse().unwrap());
		let inputs = vec![input0];

		if let EbiCommand::Command{execute, output_type, ..} = crate::ebi_commands::ebi_command_convert::EBI_CONVERT_SNFA {
			match (execute)(inputs, None) {
				Ok(output) => {
					output.test_activity_key();
					assert_eq!(&output.get_type(), output_type);
				}
				Err(e) => Err(e).unwrap(),
			}
		}
	}
	#[test]
	pub fn ebi_conv_snfa_test_1() {
		// to indicate that this test is expected to fail, add the following to src/tests/fallible_test_list.rs:
		/* 
		(
			&EBI_CONVERT_SNFA,
			&[
				"object stochastic non-deterministic finite automaton#./testfiles/ba-aa-ab.slang"			
			]
		),
		*/
		
		let mut reader = MultipleReader::from_file(File::open("./testfiles/ba-aa-ab.slang").unwrap());
		let object0 = read_as_object_with_file_handler(&"stochastic non-deterministic finite automaton".parse().unwrap(), &mut reader, None, 0, &mut None, "finite stochastic language".parse().unwrap()).unwrap();
		let input0 = EbiInput::Object(object0, "finite stochastic language".parse().unwrap());
		let inputs = vec![input0];

		if let EbiCommand::Command{execute, output_type, ..} = crate::ebi_commands::ebi_command_convert::EBI_CONVERT_SNFA {
			match (execute)(inputs, None) {
				Ok(output) => {
					output.test_activity_key();
					assert_eq!(&output.get_type(), output_type);
				}
				Err(e) => Err(e).unwrap(),
			}
		}
	}
	#[test]
	pub fn ebi_conv_snfa_test_2() {
		// to indicate that this test is expected to fail, add the following to src/tests/fallible_test_list.rs:
		/* 
		(
			&EBI_CONVERT_SNFA,
			&[
				"object stochastic non-deterministic finite automaton#./testfiles/a-b.slang"			
			]
		),
		*/
		
		let mut reader = MultipleReader::from_file(File::open("./testfiles/a-b.slang").unwrap());
		let object0 = read_as_object_with_file_handler(&"stochastic non-deterministic finite automaton".parse().unwrap(), &mut reader, None, 0, &mut None, "finite stochastic language".parse().unwrap()).unwrap();
		let input0 = EbiInput::Object(object0, "finite stochastic language".parse().unwrap());
		let inputs = vec![input0];

		if let EbiCommand::Command{execute, output_type, ..} = crate::ebi_commands::ebi_command_convert::EBI_CONVERT_SNFA {
			match (execute)(inputs, None) {
				Ok(output) => {
					output.test_activity_key();
					assert_eq!(&output.get_type(), output_type);
				}
				Err(e) => Err(e).unwrap(),
			}
		}
	}
	#[test]
	pub fn ebi_conv_snfa_test_3() {
		// to indicate that this test is expected to fail, add the following to src/tests/fallible_test_list.rs:
		/* 
		(
			&EBI_CONVERT_SNFA,
			&[
				"object stochastic non-deterministic finite automaton#./testfiles/empty.xes"			
			]
		),
		*/
		
		let mut reader = MultipleReader::from_file(File::open("./testfiles/empty.xes").unwrap());
		let object0 = read_as_object_with_file_handler(&"stochastic non-deterministic finite automaton".parse().unwrap(), &mut reader, None, 0, &mut None, "extensible event stream".parse().unwrap()).unwrap();
		let input0 = EbiInput::Object(object0, "extensible event stream".parse().unwrap());
		let inputs = vec![input0];

		if let EbiCommand::Command{execute, output_type, ..} = crate::ebi_commands::ebi_command_convert::EBI_CONVERT_SNFA {
			match (execute)(inputs, None) {
				Ok(output) => {
					output.test_activity_key();
					assert_eq!(&output.get_type(), output_type);
				}
				Err(e) => Err(e).unwrap(),
			}
		}
	}


	// ==== group disc ====


	// ==== group ali ====


	// ==== command sbpmn ====
	#[test]
	pub fn ebi_disc_ali_sbpmn_test_0() {
		// to indicate that this test is expected to fail, add the following to src/tests/fallible_test_list.rs:
		/* 
		(
			&EBI_DISCOVER_ALIGNMENTS_BPMN,
			&[
				"trait finite stochastic language#./testfiles/ba-aa-ab.slang",
				"object business process model and notation#./testfiles/seq(a-xor(b-c)).sptree"			
			]
		),
		*/
		
		let mut reader = MultipleReader::from_file(File::open("./testfiles/ba-aa-ab.slang").unwrap());
		let (object0, file_handler0) = ebi_input::read_as_trait(&("finite stochastic language".parse().unwrap()), &mut reader, None, 0).unwrap();
		let input0 = EbiInput::Trait(object0, file_handler0);
		let mut reader = MultipleReader::from_file(File::open("./testfiles/seq(a-xor(b-c)).sptree").unwrap());
		let object1 = read_as_object_with_file_handler(&"business process model and notation".parse().unwrap(), &mut reader, None, 0, &mut None, "stochastic process tree".parse().unwrap()).unwrap();
		let input1 = EbiInput::Object(object1, "stochastic process tree".parse().unwrap());
		let inputs = vec![input0, input1];

		if let EbiCommand::Command{execute, output_type, ..} = crate::ebi_commands::ebi_command_discover::EBI_DISCOVER_ALIGNMENTS_BPMN {
			match (execute)(inputs, None) {
				Ok(output) => {
					output.test_activity_key();
					assert_eq!(&output.get_type(), output_type);
				}
				Err(e) => Err(e).unwrap(),
			}
		}
	}
	#[test]
	pub fn ebi_disc_ali_sbpmn_test_1() {
		// to indicate that this test is expected to fail, add the following to src/tests/fallible_test_list.rs:
		/* 
		(
			&EBI_DISCOVER_ALIGNMENTS_BPMN,
			&[
				"trait finite stochastic language#./testfiles/ba-aa-ab.slang",
				"object business process model and notation#./testfiles/model.bpmn"			
			]
		),
		*/
		
		let mut reader = MultipleReader::from_file(File::open("./testfiles/ba-aa-ab.slang").unwrap());
		let (object0, file_handler0) = ebi_input::read_as_trait(&("finite stochastic language".parse().unwrap()), &mut reader, None, 0).unwrap();
		let input0 = EbiInput::Trait(object0, file_handler0);
		let mut reader = MultipleReader::from_file(File::open("./testfiles/model.bpmn").unwrap());
		let object1 = read_as_object_with_file_handler(&"business process model and notation".parse().unwrap(), &mut reader, None, 0, &mut None, "business process model and notation".parse().unwrap()).unwrap();
		let input1 = EbiInput::Object(object1, "business process model and notation".parse().unwrap());
		let inputs = vec![input0, input1];

		if let EbiCommand::Command{execute, output_type, ..} = crate::ebi_commands::ebi_command_discover::EBI_DISCOVER_ALIGNMENTS_BPMN {
			match (execute)(inputs, None) {
				Ok(output) => {
					output.test_activity_key();
					assert_eq!(&output.get_type(), output_type);
				}
				Err(e) => Err(e).unwrap(),
			}
		}
	}
	#[test]
	pub fn ebi_disc_ali_sbpmn_test_2() {
		// to indicate that this test is expected to fail, add the following to src/tests/fallible_test_list.rs:
		/* 
		(
			&EBI_DISCOVER_ALIGNMENTS_BPMN,
			&[
				"trait finite stochastic language#./testfiles/ba-aa-ab.slang",
				"object business process model and notation#./testfiles/a-b_star.dfm"			
			]
		),
		*/
		
		let mut reader = MultipleReader::from_file(File::open("./testfiles/ba-aa-ab.slang").unwrap());
		let (object0, file_handler0) = ebi_input::read_as_trait(&("finite stochastic language".parse().unwrap()), &mut reader, None, 0).unwrap();
		let input0 = EbiInput::Trait(object0, file_handler0);
		let mut reader = MultipleReader::from_file(File::open("./testfiles/a-b_star.dfm").unwrap());
		let object1 = read_as_object_with_file_handler(&"business process model and notation".parse().unwrap(), &mut reader, None, 0, &mut None, "directly follows model".parse().unwrap()).unwrap();
		let input1 = EbiInput::Object(object1, "directly follows model".parse().unwrap());
		let inputs = vec![input0, input1];

		if let EbiCommand::Command{execute, output_type, ..} = crate::ebi_commands::ebi_command_discover::EBI_DISCOVER_ALIGNMENTS_BPMN {
			match (execute)(inputs, None) {
				Ok(output) => {
					output.test_activity_key();
					assert_eq!(&output.get_type(), output_type);
				}
				Err(e) => Err(e).unwrap(),
			}
		}
	}
	#[test]
	pub fn ebi_disc_ali_sbpmn_test_3() {
		// to indicate that this test is expected to fail, add the following to src/tests/fallible_test_list.rs:
		/* 
		(
			&EBI_DISCOVER_ALIGNMENTS_BPMN,
			&[
				"trait finite stochastic language#./testfiles/ba-aa-ab.slang",
				"object business process model and notation#./testfiles/bpic12-a.xes.gz-dfg.dfg"			
			]
		),
		*/
		
		let mut reader = MultipleReader::from_file(File::open("./testfiles/ba-aa-ab.slang").unwrap());
		let (object0, file_handler0) = ebi_input::read_as_trait(&("finite stochastic language".parse().unwrap()), &mut reader, None, 0).unwrap();
		let input0 = EbiInput::Trait(object0, file_handler0);
		let mut reader = MultipleReader::from_file(File::open("./testfiles/bpic12-a.xes.gz-dfg.dfg").unwrap());
		let object1 = read_as_object_with_file_handler(&"business process model and notation".parse().unwrap(), &mut reader, None, 0, &mut None, "directly follows graph".parse().unwrap()).unwrap();
		let input1 = EbiInput::Object(object1, "directly follows graph".parse().unwrap());
		let inputs = vec![input0, input1];

		if let EbiCommand::Command{execute, output_type, ..} = crate::ebi_commands::ebi_command_discover::EBI_DISCOVER_ALIGNMENTS_BPMN {
			match (execute)(inputs, None) {
				Ok(output) => {
					output.test_activity_key();
					assert_eq!(&output.get_type(), output_type);
				}
				Err(e) => Err(e).unwrap(),
			}
		}
	}


	// ==== command slpn ====
	#[test]
	pub fn ebi_disc_ali_slpn_test_0() {
		// to indicate that this test is expected to fail, add the following to src/tests/fallible_test_list.rs:
		/* 
		(
			&EBI_DISCOVER_ALIGNMENTS_SLPN,
			&[
				"trait finite stochastic language#./testfiles/ba-aa-ab.slang",
				"object labelled Petri net#./testfiles/seq(a-xor(b-c)).sptree"			
			]
		),
		*/
		
		let mut reader = MultipleReader::from_file(File::open("./testfiles/ba-aa-ab.slang").unwrap());
		let (object0, file_handler0) = ebi_input::read_as_trait(&("finite stochastic language".parse().unwrap()), &mut reader, None, 0).unwrap();
		let input0 = EbiInput::Trait(object0, file_handler0);
		let mut reader = MultipleReader::from_file(File::open("./testfiles/seq(a-xor(b-c)).sptree").unwrap());
		let object1 = read_as_object_with_file_handler(&"labelled Petri net".parse().unwrap(), &mut reader, None, 0, &mut None, "stochastic process tree".parse().unwrap()).unwrap();
		let input1 = EbiInput::Object(object1, "stochastic process tree".parse().unwrap());
		let inputs = vec![input0, input1];

		if let EbiCommand::Command{execute, output_type, ..} = crate::ebi_commands::ebi_command_discover::EBI_DISCOVER_ALIGNMENTS_SLPN {
			match (execute)(inputs, None) {
				Ok(output) => {
					output.test_activity_key();
					assert_eq!(&output.get_type(), output_type);
				}
				Err(e) => Err(e).unwrap(),
			}
		}
	}
	#[test]
	pub fn ebi_disc_ali_slpn_test_1() {
		// to indicate that this test is expected to fail, add the following to src/tests/fallible_test_list.rs:
		/* 
		(
			&EBI_DISCOVER_ALIGNMENTS_SLPN,
			&[
				"trait finite stochastic language#./testfiles/ba-aa-ab.slang",
				"object labelled Petri net#./testfiles/a-b_star.dfm"			
			]
		),
		*/
		
		let mut reader = MultipleReader::from_file(File::open("./testfiles/ba-aa-ab.slang").unwrap());
		let (object0, file_handler0) = ebi_input::read_as_trait(&("finite stochastic language".parse().unwrap()), &mut reader, None, 0).unwrap();
		let input0 = EbiInput::Trait(object0, file_handler0);
		let mut reader = MultipleReader::from_file(File::open("./testfiles/a-b_star.dfm").unwrap());
		let object1 = read_as_object_with_file_handler(&"labelled Petri net".parse().unwrap(), &mut reader, None, 0, &mut None, "directly follows model".parse().unwrap()).unwrap();
		let input1 = EbiInput::Object(object1, "directly follows model".parse().unwrap());
		let inputs = vec![input0, input1];

		if let EbiCommand::Command{execute, output_type, ..} = crate::ebi_commands::ebi_command_discover::EBI_DISCOVER_ALIGNMENTS_SLPN {
			match (execute)(inputs, None) {
				Ok(output) => {
					output.test_activity_key();
					assert_eq!(&output.get_type(), output_type);
				}
				Err(e) => Err(e).unwrap(),
			}
		}
	}
	#[test]
	pub fn ebi_disc_ali_slpn_test_2() {
		// to indicate that this test is expected to fail, add the following to src/tests/fallible_test_list.rs:
		/* 
		(
			&EBI_DISCOVER_ALIGNMENTS_SLPN,
			&[
				"trait finite stochastic language#./testfiles/ba-aa-ab.slang",
				"object labelled Petri net#./testfiles/bpic12-a.xes.gz-dfg.dfg"			
			]
		),
		*/
		
		let mut reader = MultipleReader::from_file(File::open("./testfiles/ba-aa-ab.slang").unwrap());
		let (object0, file_handler0) = ebi_input::read_as_trait(&("finite stochastic language".parse().unwrap()), &mut reader, None, 0).unwrap();
		let input0 = EbiInput::Trait(object0, file_handler0);
		let mut reader = MultipleReader::from_file(File::open("./testfiles/bpic12-a.xes.gz-dfg.dfg").unwrap());
		let object1 = read_as_object_with_file_handler(&"labelled Petri net".parse().unwrap(), &mut reader, None, 0, &mut None, "directly follows graph".parse().unwrap()).unwrap();
		let input1 = EbiInput::Object(object1, "directly follows graph".parse().unwrap());
		let inputs = vec![input0, input1];

		if let EbiCommand::Command{execute, output_type, ..} = crate::ebi_commands::ebi_command_discover::EBI_DISCOVER_ALIGNMENTS_SLPN {
			match (execute)(inputs, None) {
				Ok(output) => {
					output.test_activity_key();
					assert_eq!(&output.get_type(), output_type);
				}
				Err(e) => Err(e).unwrap(),
			}
		}
	}
	#[test]
	pub fn ebi_disc_ali_slpn_test_3() {
		// to indicate that this test is expected to fail, add the following to src/tests/fallible_test_list.rs:
		/* 
		(
			&EBI_DISCOVER_ALIGNMENTS_SLPN,
			&[
				"trait finite stochastic language#./testfiles/ba-aa-ab.slang",
				"object labelled Petri net#./testfiles/BPI_Challenge_2013_closed_problems.xes.gz-dfg.dfg"			
			]
		),
		*/
		
		let mut reader = MultipleReader::from_file(File::open("./testfiles/ba-aa-ab.slang").unwrap());
		let (object0, file_handler0) = ebi_input::read_as_trait(&("finite stochastic language".parse().unwrap()), &mut reader, None, 0).unwrap();
		let input0 = EbiInput::Trait(object0, file_handler0);
		let mut reader = MultipleReader::from_file(File::open("./testfiles/BPI_Challenge_2013_closed_problems.xes.gz-dfg.dfg").unwrap());
		let object1 = read_as_object_with_file_handler(&"labelled Petri net".parse().unwrap(), &mut reader, None, 0, &mut None, "directly follows graph".parse().unwrap()).unwrap();
		let input1 = EbiInput::Object(object1, "directly follows graph".parse().unwrap());
		let inputs = vec![input0, input1];

		if let EbiCommand::Command{execute, output_type, ..} = crate::ebi_commands::ebi_command_discover::EBI_DISCOVER_ALIGNMENTS_SLPN {
			match (execute)(inputs, None) {
				Ok(output) => {
					output.test_activity_key();
					assert_eq!(&output.get_type(), output_type);
				}
				Err(e) => Err(e).unwrap(),
			}
		}
	}


	// ==== command dfg ====
	#[test]
	pub fn ebi_disc_dfg_test_0() {
		// to indicate that this test is expected to fail, add the following to src/tests/fallible_test_list.rs:
		/* 
		(
			&EBI_DISCOVER_DIRECTLY_FOLLOWS,
			&[
				"trait event log#./testfiles/empty.xes",
				"fraction 1"			
			]
		),
		*/
		
		let mut reader = MultipleReader::from_file(File::open("./testfiles/empty.xes").unwrap());
		let (object0, file_handler0) = ebi_input::read_as_trait(&("event log".parse().unwrap()), &mut reader, None, 0).unwrap();
		let input0 = EbiInput::Trait(object0, file_handler0);
		let input1 = EbiInput::Fraction("1".parse().unwrap(), &TEST_INPUT_TYPE_FRACTION);
		let inputs = vec![input0, input1];

		if let EbiCommand::Command{execute, output_type, ..} = crate::ebi_commands::ebi_command_discover::EBI_DISCOVER_DIRECTLY_FOLLOWS {
			match (execute)(inputs, None) {
				Ok(output) => {
					output.test_activity_key();
					assert_eq!(&output.get_type(), output_type);
				}
				Err(e) => Err(e).unwrap(),
			}
		}
	}
	#[test]
	pub fn ebi_disc_dfg_test_1() {
		// to indicate that this test is expected to fail, add the following to src/tests/fallible_test_list.rs:
		/* 
		(
			&EBI_DISCOVER_DIRECTLY_FOLLOWS,
			&[
				"trait event log#./testfiles/simple_log_markovian_abstraction.xes",
				"fraction 1"			
			]
		),
		*/
		
		let mut reader = MultipleReader::from_file(File::open("./testfiles/simple_log_markovian_abstraction.xes").unwrap());
		let (object0, file_handler0) = ebi_input::read_as_trait(&("event log".parse().unwrap()), &mut reader, None, 0).unwrap();
		let input0 = EbiInput::Trait(object0, file_handler0);
		let input1 = EbiInput::Fraction("1".parse().unwrap(), &TEST_INPUT_TYPE_FRACTION);
		let inputs = vec![input0, input1];

		if let EbiCommand::Command{execute, output_type, ..} = crate::ebi_commands::ebi_command_discover::EBI_DISCOVER_DIRECTLY_FOLLOWS {
			match (execute)(inputs, None) {
				Ok(output) => {
					output.test_activity_key();
					assert_eq!(&output.get_type(), output_type);
				}
				Err(e) => Err(e).unwrap(),
			}
		}
	}
	#[test]
	pub fn ebi_disc_dfg_test_2() {
		// to indicate that this test is expected to fail, add the following to src/tests/fallible_test_list.rs:
		/* 
		(
			&EBI_DISCOVER_DIRECTLY_FOLLOWS,
			&[
				"trait event log#./testfiles/a-b-double.xes",
				"fraction 1"			
			]
		),
		*/
		
		let mut reader = MultipleReader::from_file(File::open("./testfiles/a-b-double.xes").unwrap());
		let (object0, file_handler0) = ebi_input::read_as_trait(&("event log".parse().unwrap()), &mut reader, None, 0).unwrap();
		let input0 = EbiInput::Trait(object0, file_handler0);
		let input1 = EbiInput::Fraction("1".parse().unwrap(), &TEST_INPUT_TYPE_FRACTION);
		let inputs = vec![input0, input1];

		if let EbiCommand::Command{execute, output_type, ..} = crate::ebi_commands::ebi_command_discover::EBI_DISCOVER_DIRECTLY_FOLLOWS {
			match (execute)(inputs, None) {
				Ok(output) => {
					output.test_activity_key();
					assert_eq!(&output.get_type(), output_type);
				}
				Err(e) => Err(e).unwrap(),
			}
		}
	}
	#[test]
	pub fn ebi_disc_dfg_test_3() {
		// to indicate that this test is expected to fail, add the following to src/tests/fallible_test_list.rs:
		/* 
		(
			&EBI_DISCOVER_DIRECTLY_FOLLOWS,
			&[
				"trait event log#./testfiles/a-b.xes.gz",
				"fraction 1"			
			]
		),
		*/
		
		let mut reader = MultipleReader::from_file(File::open("./testfiles/a-b.xes.gz").unwrap());
		let (object0, file_handler0) = ebi_input::read_as_trait(&("event log".parse().unwrap()), &mut reader, None, 0).unwrap();
		let input0 = EbiInput::Trait(object0, file_handler0);
		let input1 = EbiInput::Fraction("1".parse().unwrap(), &TEST_INPUT_TYPE_FRACTION);
		let inputs = vec![input0, input1];

		if let EbiCommand::Command{execute, output_type, ..} = crate::ebi_commands::ebi_command_discover::EBI_DISCOVER_DIRECTLY_FOLLOWS {
			match (execute)(inputs, None) {
				Ok(output) => {
					output.test_activity_key();
					assert_eq!(&output.get_type(), output_type);
				}
				Err(e) => Err(e).unwrap(),
			}
		}
	}


	// ==== group occ ====


	// ==== command sbpmn ====
	#[test]
	pub fn ebi_disc_occ_sbpmn_test_0() {
		// to indicate that this test is expected to fail, add the following to src/tests/fallible_test_list.rs:
		/* 
		(
			&EBI_DISCOVER_OCCURRENCE_SBPMN,
			&[
				"trait finite stochastic language#./testfiles/ba-aa-ab.slang",
				"object business process model and notation#./testfiles/seq(a-xor(b-c)).sptree"			
			]
		),
		*/
		
		let mut reader = MultipleReader::from_file(File::open("./testfiles/ba-aa-ab.slang").unwrap());
		let (object0, file_handler0) = ebi_input::read_as_trait(&("finite stochastic language".parse().unwrap()), &mut reader, None, 0).unwrap();
		let input0 = EbiInput::Trait(object0, file_handler0);
		let mut reader = MultipleReader::from_file(File::open("./testfiles/seq(a-xor(b-c)).sptree").unwrap());
		let object1 = read_as_object_with_file_handler(&"business process model and notation".parse().unwrap(), &mut reader, None, 0, &mut None, "stochastic process tree".parse().unwrap()).unwrap();
		let input1 = EbiInput::Object(object1, "stochastic process tree".parse().unwrap());
		let inputs = vec![input0, input1];

		if let EbiCommand::Command{execute, output_type, ..} = crate::ebi_commands::ebi_command_discover::EBI_DISCOVER_OCCURRENCE_SBPMN {
			match (execute)(inputs, None) {
				Ok(output) => {
					output.test_activity_key();
					assert_eq!(&output.get_type(), output_type);
				}
				Err(e) => Err(e).unwrap(),
			}
		}
	}
	#[test]
	pub fn ebi_disc_occ_sbpmn_test_1() {
		// to indicate that this test is expected to fail, add the following to src/tests/fallible_test_list.rs:
		/* 
		(
			&EBI_DISCOVER_OCCURRENCE_SBPMN,
			&[
				"trait finite stochastic language#./testfiles/ba-aa-ab.slang",
				"object business process model and notation#./testfiles/model.bpmn"			
			]
		),
		*/
		
		let mut reader = MultipleReader::from_file(File::open("./testfiles/ba-aa-ab.slang").unwrap());
		let (object0, file_handler0) = ebi_input::read_as_trait(&("finite stochastic language".parse().unwrap()), &mut reader, None, 0).unwrap();
		let input0 = EbiInput::Trait(object0, file_handler0);
		let mut reader = MultipleReader::from_file(File::open("./testfiles/model.bpmn").unwrap());
		let object1 = read_as_object_with_file_handler(&"business process model and notation".parse().unwrap(), &mut reader, None, 0, &mut None, "business process model and notation".parse().unwrap()).unwrap();
		let input1 = EbiInput::Object(object1, "business process model and notation".parse().unwrap());
		let inputs = vec![input0, input1];

		if let EbiCommand::Command{execute, output_type, ..} = crate::ebi_commands::ebi_command_discover::EBI_DISCOVER_OCCURRENCE_SBPMN {
			match (execute)(inputs, None) {
				Ok(output) => {
					output.test_activity_key();
					assert_eq!(&output.get_type(), output_type);
				}
				Err(e) => Err(e).unwrap(),
			}
		}
	}
	#[test]
	pub fn ebi_disc_occ_sbpmn_test_2() {
		// to indicate that this test is expected to fail, add the following to src/tests/fallible_test_list.rs:
		/* 
		(
			&EBI_DISCOVER_OCCURRENCE_SBPMN,
			&[
				"trait finite stochastic language#./testfiles/ba-aa-ab.slang",
				"object business process model and notation#./testfiles/a-b_star.dfm"			
			]
		),
		*/
		
		let mut reader = MultipleReader::from_file(File::open("./testfiles/ba-aa-ab.slang").unwrap());
		let (object0, file_handler0) = ebi_input::read_as_trait(&("finite stochastic language".parse().unwrap()), &mut reader, None, 0).unwrap();
		let input0 = EbiInput::Trait(object0, file_handler0);
		let mut reader = MultipleReader::from_file(File::open("./testfiles/a-b_star.dfm").unwrap());
		let object1 = read_as_object_with_file_handler(&"business process model and notation".parse().unwrap(), &mut reader, None, 0, &mut None, "directly follows model".parse().unwrap()).unwrap();
		let input1 = EbiInput::Object(object1, "directly follows model".parse().unwrap());
		let inputs = vec![input0, input1];

		if let EbiCommand::Command{execute, output_type, ..} = crate::ebi_commands::ebi_command_discover::EBI_DISCOVER_OCCURRENCE_SBPMN {
			match (execute)(inputs, None) {
				Ok(output) => {
					output.test_activity_key();
					assert_eq!(&output.get_type(), output_type);
				}
				Err(e) => Err(e).unwrap(),
			}
		}
	}
	#[test]
	pub fn ebi_disc_occ_sbpmn_test_3() {
		// to indicate that this test is expected to fail, add the following to src/tests/fallible_test_list.rs:
		/* 
		(
			&EBI_DISCOVER_OCCURRENCE_SBPMN,
			&[
				"trait finite stochastic language#./testfiles/ba-aa-ab.slang",
				"object business process model and notation#./testfiles/bpic12-a.xes.gz-dfg.dfg"			
			]
		),
		*/
		
		let mut reader = MultipleReader::from_file(File::open("./testfiles/ba-aa-ab.slang").unwrap());
		let (object0, file_handler0) = ebi_input::read_as_trait(&("finite stochastic language".parse().unwrap()), &mut reader, None, 0).unwrap();
		let input0 = EbiInput::Trait(object0, file_handler0);
		let mut reader = MultipleReader::from_file(File::open("./testfiles/bpic12-a.xes.gz-dfg.dfg").unwrap());
		let object1 = read_as_object_with_file_handler(&"business process model and notation".parse().unwrap(), &mut reader, None, 0, &mut None, "directly follows graph".parse().unwrap()).unwrap();
		let input1 = EbiInput::Object(object1, "directly follows graph".parse().unwrap());
		let inputs = vec![input0, input1];

		if let EbiCommand::Command{execute, output_type, ..} = crate::ebi_commands::ebi_command_discover::EBI_DISCOVER_OCCURRENCE_SBPMN {
			match (execute)(inputs, None) {
				Ok(output) => {
					output.test_activity_key();
					assert_eq!(&output.get_type(), output_type);
				}
				Err(e) => Err(e).unwrap(),
			}
		}
	}


	// ==== command slpn ====
	#[test]
	pub fn ebi_disc_occ_slpn_test_0() {
		// to indicate that this test is expected to fail, add the following to src/tests/fallible_test_list.rs:
		/* 
		(
			&EBI_DISCOVER_OCCURRENCE_SLPN,
			&[
				"trait finite stochastic language#./testfiles/ba-aa-ab.slang",
				"object labelled Petri net#./testfiles/seq(a-xor(b-c)).sptree"			
			]
		),
		*/
		
		let mut reader = MultipleReader::from_file(File::open("./testfiles/ba-aa-ab.slang").unwrap());
		let (object0, file_handler0) = ebi_input::read_as_trait(&("finite stochastic language".parse().unwrap()), &mut reader, None, 0).unwrap();
		let input0 = EbiInput::Trait(object0, file_handler0);
		let mut reader = MultipleReader::from_file(File::open("./testfiles/seq(a-xor(b-c)).sptree").unwrap());
		let object1 = read_as_object_with_file_handler(&"labelled Petri net".parse().unwrap(), &mut reader, None, 0, &mut None, "stochastic process tree".parse().unwrap()).unwrap();
		let input1 = EbiInput::Object(object1, "stochastic process tree".parse().unwrap());
		let inputs = vec![input0, input1];

		if let EbiCommand::Command{execute, output_type, ..} = crate::ebi_commands::ebi_command_discover::EBI_DISCOVER_OCCURRENCE_SLPN {
			match (execute)(inputs, None) {
				Ok(output) => {
					output.test_activity_key();
					assert_eq!(&output.get_type(), output_type);
				}
				Err(e) => Err(e).unwrap(),
			}
		}
	}
	#[test]
	pub fn ebi_disc_occ_slpn_test_1() {
		// to indicate that this test is expected to fail, add the following to src/tests/fallible_test_list.rs:
		/* 
		(
			&EBI_DISCOVER_OCCURRENCE_SLPN,
			&[
				"trait finite stochastic language#./testfiles/ba-aa-ab.slang",
				"object labelled Petri net#./testfiles/a-b_star.dfm"			
			]
		),
		*/
		
		let mut reader = MultipleReader::from_file(File::open("./testfiles/ba-aa-ab.slang").unwrap());
		let (object0, file_handler0) = ebi_input::read_as_trait(&("finite stochastic language".parse().unwrap()), &mut reader, None, 0).unwrap();
		let input0 = EbiInput::Trait(object0, file_handler0);
		let mut reader = MultipleReader::from_file(File::open("./testfiles/a-b_star.dfm").unwrap());
		let object1 = read_as_object_with_file_handler(&"labelled Petri net".parse().unwrap(), &mut reader, None, 0, &mut None, "directly follows model".parse().unwrap()).unwrap();
		let input1 = EbiInput::Object(object1, "directly follows model".parse().unwrap());
		let inputs = vec![input0, input1];

		if let EbiCommand::Command{execute, output_type, ..} = crate::ebi_commands::ebi_command_discover::EBI_DISCOVER_OCCURRENCE_SLPN {
			match (execute)(inputs, None) {
				Ok(output) => {
					output.test_activity_key();
					assert_eq!(&output.get_type(), output_type);
				}
				Err(e) => Err(e).unwrap(),
			}
		}
	}
	#[test]
	pub fn ebi_disc_occ_slpn_test_2() {
		// to indicate that this test is expected to fail, add the following to src/tests/fallible_test_list.rs:
		/* 
		(
			&EBI_DISCOVER_OCCURRENCE_SLPN,
			&[
				"trait finite stochastic language#./testfiles/ba-aa-ab.slang",
				"object labelled Petri net#./testfiles/bpic12-a.xes.gz-dfg.dfg"			
			]
		),
		*/
		
		let mut reader = MultipleReader::from_file(File::open("./testfiles/ba-aa-ab.slang").unwrap());
		let (object0, file_handler0) = ebi_input::read_as_trait(&("finite stochastic language".parse().unwrap()), &mut reader, None, 0).unwrap();
		let input0 = EbiInput::Trait(object0, file_handler0);
		let mut reader = MultipleReader::from_file(File::open("./testfiles/bpic12-a.xes.gz-dfg.dfg").unwrap());
		let object1 = read_as_object_with_file_handler(&"labelled Petri net".parse().unwrap(), &mut reader, None, 0, &mut None, "directly follows graph".parse().unwrap()).unwrap();
		let input1 = EbiInput::Object(object1, "directly follows graph".parse().unwrap());
		let inputs = vec![input0, input1];

		if let EbiCommand::Command{execute, output_type, ..} = crate::ebi_commands::ebi_command_discover::EBI_DISCOVER_OCCURRENCE_SLPN {
			match (execute)(inputs, None) {
				Ok(output) => {
					output.test_activity_key();
					assert_eq!(&output.get_type(), output_type);
				}
				Err(e) => Err(e).unwrap(),
			}
		}
	}
	#[test]
	pub fn ebi_disc_occ_slpn_test_3() {
		// to indicate that this test is expected to fail, add the following to src/tests/fallible_test_list.rs:
		/* 
		(
			&EBI_DISCOVER_OCCURRENCE_SLPN,
			&[
				"trait finite stochastic language#./testfiles/ba-aa-ab.slang",
				"object labelled Petri net#./testfiles/BPI_Challenge_2013_closed_problems.xes.gz-dfg.dfg"			
			]
		),
		*/
		
		let mut reader = MultipleReader::from_file(File::open("./testfiles/ba-aa-ab.slang").unwrap());
		let (object0, file_handler0) = ebi_input::read_as_trait(&("finite stochastic language".parse().unwrap()), &mut reader, None, 0).unwrap();
		let input0 = EbiInput::Trait(object0, file_handler0);
		let mut reader = MultipleReader::from_file(File::open("./testfiles/BPI_Challenge_2013_closed_problems.xes.gz-dfg.dfg").unwrap());
		let object1 = read_as_object_with_file_handler(&"labelled Petri net".parse().unwrap(), &mut reader, None, 0, &mut None, "directly follows graph".parse().unwrap()).unwrap();
		let input1 = EbiInput::Object(object1, "directly follows graph".parse().unwrap());
		let inputs = vec![input0, input1];

		if let EbiCommand::Command{execute, output_type, ..} = crate::ebi_commands::ebi_command_discover::EBI_DISCOVER_OCCURRENCE_SLPN {
			match (execute)(inputs, None) {
				Ok(output) => {
					output.test_activity_key();
					assert_eq!(&output.get_type(), output_type);
				}
				Err(e) => Err(e).unwrap(),
			}
		}
	}


	// ==== command sptree ====
	#[test]
	pub fn ebi_disc_occ_sptree_test_0() {
		// to indicate that this test is expected to fail, add the following to src/tests/fallible_test_list.rs:
		/* 
		(
			&EBI_DISCOVER_OCCURRENCE_SPTREE,
			&[
				"trait finite stochastic language#./testfiles/ba-aa-ab.slang",
				"object process tree#./testfiles/seq(a-xor(b-c)).sptree"			
			]
		),
		*/
		
		let mut reader = MultipleReader::from_file(File::open("./testfiles/ba-aa-ab.slang").unwrap());
		let (object0, file_handler0) = ebi_input::read_as_trait(&("finite stochastic language".parse().unwrap()), &mut reader, None, 0).unwrap();
		let input0 = EbiInput::Trait(object0, file_handler0);
		let mut reader = MultipleReader::from_file(File::open("./testfiles/seq(a-xor(b-c)).sptree").unwrap());
		let object1 = read_as_object_with_file_handler(&"process tree".parse().unwrap(), &mut reader, None, 0, &mut None, "stochastic process tree".parse().unwrap()).unwrap();
		let input1 = EbiInput::Object(object1, "stochastic process tree".parse().unwrap());
		let inputs = vec![input0, input1];

		if let EbiCommand::Command{execute, output_type, ..} = crate::ebi_commands::ebi_command_discover::EBI_DISCOVER_OCCURRENCE_SPTREE {
			match (execute)(inputs, None) {
				Ok(output) => {
					output.test_activity_key();
					assert_eq!(&output.get_type(), output_type);
				}
				Err(e) => Err(e).unwrap(),
			}
		}
	}
	#[test]
	pub fn ebi_disc_occ_sptree_test_1() {
		// to indicate that this test is expected to fail, add the following to src/tests/fallible_test_list.rs:
		/* 
		(
			&EBI_DISCOVER_OCCURRENCE_SPTREE,
			&[
				"trait finite stochastic language#./testfiles/ba-aa-ab.slang",
				"object process tree#./testfiles/empty.ptree"			
			]
		),
		*/
		
		let mut reader = MultipleReader::from_file(File::open("./testfiles/ba-aa-ab.slang").unwrap());
		let (object0, file_handler0) = ebi_input::read_as_trait(&("finite stochastic language".parse().unwrap()), &mut reader, None, 0).unwrap();
		let input0 = EbiInput::Trait(object0, file_handler0);
		let mut reader = MultipleReader::from_file(File::open("./testfiles/empty.ptree").unwrap());
		let object1 = read_as_object_with_file_handler(&"process tree".parse().unwrap(), &mut reader, None, 0, &mut None, "process tree".parse().unwrap()).unwrap();
		let input1 = EbiInput::Object(object1, "process tree".parse().unwrap());
		let inputs = vec![input0, input1];

		if let EbiCommand::Command{execute, output_type, ..} = crate::ebi_commands::ebi_command_discover::EBI_DISCOVER_OCCURRENCE_SPTREE {
			match (execute)(inputs, None) {
				Ok(output) => {
					output.test_activity_key();
					assert_eq!(&output.get_type(), output_type);
				}
				Err(e) => Err(e).unwrap(),
			}
		}
	}
	#[test]
	pub fn ebi_disc_occ_sptree_test_2() {
		// to indicate that this test is expected to fail, add the following to src/tests/fallible_test_list.rs:
		/* 
		(
			&EBI_DISCOVER_OCCURRENCE_SPTREE,
			&[
				"trait finite stochastic language#./testfiles/ba-aa-ab.slang",
				"object process tree#./testfiles/aa-ab-ba.ptml"			
			]
		),
		*/
		
		let mut reader = MultipleReader::from_file(File::open("./testfiles/ba-aa-ab.slang").unwrap());
		let (object0, file_handler0) = ebi_input::read_as_trait(&("finite stochastic language".parse().unwrap()), &mut reader, None, 0).unwrap();
		let input0 = EbiInput::Trait(object0, file_handler0);
		let mut reader = MultipleReader::from_file(File::open("./testfiles/aa-ab-ba.ptml").unwrap());
		let object1 = read_as_object_with_file_handler(&"process tree".parse().unwrap(), &mut reader, None, 0, &mut None, "process tree markup language".parse().unwrap()).unwrap();
		let input1 = EbiInput::Object(object1, "process tree markup language".parse().unwrap());
		let inputs = vec![input0, input1];

		if let EbiCommand::Command{execute, output_type, ..} = crate::ebi_commands::ebi_command_discover::EBI_DISCOVER_OCCURRENCE_SPTREE {
			match (execute)(inputs, None) {
				Ok(output) => {
					output.test_activity_key();
					assert_eq!(&output.get_type(), output_type);
				}
				Err(e) => Err(e).unwrap(),
			}
		}
	}
	#[test]
	pub fn ebi_disc_occ_sptree_test_3() {
		// to indicate that this test is expected to fail, add the following to src/tests/fallible_test_list.rs:
		/* 
		(
			&EBI_DISCOVER_OCCURRENCE_SPTREE,
			&[
				"trait finite stochastic language#./testfiles/ba-aa-ab.slang",
				"object process tree#./testfiles/a-b-flower.ptree"			
			]
		),
		*/
		
		let mut reader = MultipleReader::from_file(File::open("./testfiles/ba-aa-ab.slang").unwrap());
		let (object0, file_handler0) = ebi_input::read_as_trait(&("finite stochastic language".parse().unwrap()), &mut reader, None, 0).unwrap();
		let input0 = EbiInput::Trait(object0, file_handler0);
		let mut reader = MultipleReader::from_file(File::open("./testfiles/a-b-flower.ptree").unwrap());
		let object1 = read_as_object_with_file_handler(&"process tree".parse().unwrap(), &mut reader, None, 0, &mut None, "process tree".parse().unwrap()).unwrap();
		let input1 = EbiInput::Object(object1, "process tree".parse().unwrap());
		let inputs = vec![input0, input1];

		if let EbiCommand::Command{execute, output_type, ..} = crate::ebi_commands::ebi_command_discover::EBI_DISCOVER_OCCURRENCE_SPTREE {
			match (execute)(inputs, None) {
				Ok(output) => {
					output.test_activity_key();
					assert_eq!(&output.get_type(), output_type);
				}
				Err(e) => Err(e).unwrap(),
			}
		}
	}


	// ==== group rnd ====


	// ==== command sbpmn ====
	#[test]
	pub fn ebi_disc_rnd_sbpmn_test_0() {
		// to indicate that this test is expected to fail, add the following to src/tests/fallible_test_list.rs:
		/* 
		(
			&EBI_DISCOVER_RANDOM_SBPMN,
			&[
				"object business process model and notation#./testfiles/seq(a-xor(b-c)).sptree"			
			]
		),
		*/
		
		let mut reader = MultipleReader::from_file(File::open("./testfiles/seq(a-xor(b-c)).sptree").unwrap());
		let object0 = read_as_object_with_file_handler(&"business process model and notation".parse().unwrap(), &mut reader, None, 0, &mut None, "stochastic process tree".parse().unwrap()).unwrap();
		let input0 = EbiInput::Object(object0, "stochastic process tree".parse().unwrap());
		let inputs = vec![input0];

		if let EbiCommand::Command{execute, output_type, ..} = crate::ebi_commands::ebi_command_discover::EBI_DISCOVER_RANDOM_SBPMN {
			match (execute)(inputs, None) {
				Ok(output) => {
					output.test_activity_key();
					assert_eq!(&output.get_type(), output_type);
				}
				Err(e) => Err(e).unwrap(),
			}
		}
	}
	#[test]
	pub fn ebi_disc_rnd_sbpmn_test_1() {
		// to indicate that this test is expected to fail, add the following to src/tests/fallible_test_list.rs:
		/* 
		(
			&EBI_DISCOVER_RANDOM_SBPMN,
			&[
				"object business process model and notation#./testfiles/model.bpmn"			
			]
		),
		*/
		
		let mut reader = MultipleReader::from_file(File::open("./testfiles/model.bpmn").unwrap());
		let object0 = read_as_object_with_file_handler(&"business process model and notation".parse().unwrap(), &mut reader, None, 0, &mut None, "business process model and notation".parse().unwrap()).unwrap();
		let input0 = EbiInput::Object(object0, "business process model and notation".parse().unwrap());
		let inputs = vec![input0];

		if let EbiCommand::Command{execute, output_type, ..} = crate::ebi_commands::ebi_command_discover::EBI_DISCOVER_RANDOM_SBPMN {
			match (execute)(inputs, None) {
				Ok(output) => {
					output.test_activity_key();
					assert_eq!(&output.get_type(), output_type);
				}
				Err(e) => Err(e).unwrap(),
			}
		}
	}
	#[test]
	pub fn ebi_disc_rnd_sbpmn_test_2() {
		// to indicate that this test is expected to fail, add the following to src/tests/fallible_test_list.rs:
		/* 
		(
			&EBI_DISCOVER_RANDOM_SBPMN,
			&[
				"object business process model and notation#./testfiles/a-b_star.dfm"			
			]
		),
		*/
		
		let mut reader = MultipleReader::from_file(File::open("./testfiles/a-b_star.dfm").unwrap());
		let object0 = read_as_object_with_file_handler(&"business process model and notation".parse().unwrap(), &mut reader, None, 0, &mut None, "directly follows model".parse().unwrap()).unwrap();
		let input0 = EbiInput::Object(object0, "directly follows model".parse().unwrap());
		let inputs = vec![input0];

		if let EbiCommand::Command{execute, output_type, ..} = crate::ebi_commands::ebi_command_discover::EBI_DISCOVER_RANDOM_SBPMN {
			match (execute)(inputs, None) {
				Ok(output) => {
					output.test_activity_key();
					assert_eq!(&output.get_type(), output_type);
				}
				Err(e) => Err(e).unwrap(),
			}
		}
	}
	#[test]
	pub fn ebi_disc_rnd_sbpmn_test_3() {
		// to indicate that this test is expected to fail, add the following to src/tests/fallible_test_list.rs:
		/* 
		(
			&EBI_DISCOVER_RANDOM_SBPMN,
			&[
				"object business process model and notation#./testfiles/bpic12-a.xes.gz-dfg.dfg"			
			]
		),
		*/
		
		let mut reader = MultipleReader::from_file(File::open("./testfiles/bpic12-a.xes.gz-dfg.dfg").unwrap());
		let object0 = read_as_object_with_file_handler(&"business process model and notation".parse().unwrap(), &mut reader, None, 0, &mut None, "directly follows graph".parse().unwrap()).unwrap();
		let input0 = EbiInput::Object(object0, "directly follows graph".parse().unwrap());
		let inputs = vec![input0];

		if let EbiCommand::Command{execute, output_type, ..} = crate::ebi_commands::ebi_command_discover::EBI_DISCOVER_RANDOM_SBPMN {
			match (execute)(inputs, None) {
				Ok(output) => {
					output.test_activity_key();
					assert_eq!(&output.get_type(), output_type);
				}
				Err(e) => Err(e).unwrap(),
			}
		}
	}


	// ==== command slpn ====
	#[test]
	pub fn ebi_disc_rnd_slpn_test_0() {
		// to indicate that this test is expected to fail, add the following to src/tests/fallible_test_list.rs:
		/* 
		(
			&EBI_DISCOVER_RANDOM_SLPN,
			&[
				"object labelled Petri net#./testfiles/seq(a-xor(b-c)).sptree"			
			]
		),
		*/
		
		let mut reader = MultipleReader::from_file(File::open("./testfiles/seq(a-xor(b-c)).sptree").unwrap());
		let object0 = read_as_object_with_file_handler(&"labelled Petri net".parse().unwrap(), &mut reader, None, 0, &mut None, "stochastic process tree".parse().unwrap()).unwrap();
		let input0 = EbiInput::Object(object0, "stochastic process tree".parse().unwrap());
		let inputs = vec![input0];

		if let EbiCommand::Command{execute, output_type, ..} = crate::ebi_commands::ebi_command_discover::EBI_DISCOVER_RANDOM_SLPN {
			match (execute)(inputs, None) {
				Ok(output) => {
					output.test_activity_key();
					assert_eq!(&output.get_type(), output_type);
				}
				Err(e) => Err(e).unwrap(),
			}
		}
	}
	#[test]
	pub fn ebi_disc_rnd_slpn_test_1() {
		// to indicate that this test is expected to fail, add the following to src/tests/fallible_test_list.rs:
		/* 
		(
			&EBI_DISCOVER_RANDOM_SLPN,
			&[
				"object labelled Petri net#./testfiles/a-b_star.dfm"			
			]
		),
		*/
		
		let mut reader = MultipleReader::from_file(File::open("./testfiles/a-b_star.dfm").unwrap());
		let object0 = read_as_object_with_file_handler(&"labelled Petri net".parse().unwrap(), &mut reader, None, 0, &mut None, "directly follows model".parse().unwrap()).unwrap();
		let input0 = EbiInput::Object(object0, "directly follows model".parse().unwrap());
		let inputs = vec![input0];

		if let EbiCommand::Command{execute, output_type, ..} = crate::ebi_commands::ebi_command_discover::EBI_DISCOVER_RANDOM_SLPN {
			match (execute)(inputs, None) {
				Ok(output) => {
					output.test_activity_key();
					assert_eq!(&output.get_type(), output_type);
				}
				Err(e) => Err(e).unwrap(),
			}
		}
	}
	#[test]
	pub fn ebi_disc_rnd_slpn_test_2() {
		// to indicate that this test is expected to fail, add the following to src/tests/fallible_test_list.rs:
		/* 
		(
			&EBI_DISCOVER_RANDOM_SLPN,
			&[
				"object labelled Petri net#./testfiles/bpic12-a.xes.gz-dfg.dfg"			
			]
		),
		*/
		
		let mut reader = MultipleReader::from_file(File::open("./testfiles/bpic12-a.xes.gz-dfg.dfg").unwrap());
		let object0 = read_as_object_with_file_handler(&"labelled Petri net".parse().unwrap(), &mut reader, None, 0, &mut None, "directly follows graph".parse().unwrap()).unwrap();
		let input0 = EbiInput::Object(object0, "directly follows graph".parse().unwrap());
		let inputs = vec![input0];

		if let EbiCommand::Command{execute, output_type, ..} = crate::ebi_commands::ebi_command_discover::EBI_DISCOVER_RANDOM_SLPN {
			match (execute)(inputs, None) {
				Ok(output) => {
					output.test_activity_key();
					assert_eq!(&output.get_type(), output_type);
				}
				Err(e) => Err(e).unwrap(),
			}
		}
	}
	#[test]
	pub fn ebi_disc_rnd_slpn_test_3() {
		// to indicate that this test is expected to fail, add the following to src/tests/fallible_test_list.rs:
		/* 
		(
			&EBI_DISCOVER_RANDOM_SLPN,
			&[
				"object labelled Petri net#./testfiles/BPI_Challenge_2013_closed_problems.xes.gz-dfg.dfg"			
			]
		),
		*/
		
		let mut reader = MultipleReader::from_file(File::open("./testfiles/BPI_Challenge_2013_closed_problems.xes.gz-dfg.dfg").unwrap());
		let object0 = read_as_object_with_file_handler(&"labelled Petri net".parse().unwrap(), &mut reader, None, 0, &mut None, "directly follows graph".parse().unwrap()).unwrap();
		let input0 = EbiInput::Object(object0, "directly follows graph".parse().unwrap());
		let inputs = vec![input0];

		if let EbiCommand::Command{execute, output_type, ..} = crate::ebi_commands::ebi_command_discover::EBI_DISCOVER_RANDOM_SLPN {
			match (execute)(inputs, None) {
				Ok(output) => {
					output.test_activity_key();
					assert_eq!(&output.get_type(), output_type);
				}
				Err(e) => Err(e).unwrap(),
			}
		}
	}


	// ==== command sptree ====
	#[test]
	pub fn ebi_disc_rnd_sptree_test_0() {
		// to indicate that this test is expected to fail, add the following to src/tests/fallible_test_list.rs:
		/* 
		(
			&EBI_DISCOVER_RANDOM_SPTREE,
			&[
				"object process tree#./testfiles/seq(a-xor(b-c)).sptree"			
			]
		),
		*/
		
		let mut reader = MultipleReader::from_file(File::open("./testfiles/seq(a-xor(b-c)).sptree").unwrap());
		let object0 = read_as_object_with_file_handler(&"process tree".parse().unwrap(), &mut reader, None, 0, &mut None, "stochastic process tree".parse().unwrap()).unwrap();
		let input0 = EbiInput::Object(object0, "stochastic process tree".parse().unwrap());
		let inputs = vec![input0];

		if let EbiCommand::Command{execute, output_type, ..} = crate::ebi_commands::ebi_command_discover::EBI_DISCOVER_RANDOM_SPTREE {
			match (execute)(inputs, None) {
				Ok(output) => {
					output.test_activity_key();
					assert_eq!(&output.get_type(), output_type);
				}
				Err(e) => Err(e).unwrap(),
			}
		}
	}
	#[test]
	pub fn ebi_disc_rnd_sptree_test_1() {
		// to indicate that this test is expected to fail, add the following to src/tests/fallible_test_list.rs:
		/* 
		(
			&EBI_DISCOVER_RANDOM_SPTREE,
			&[
				"object process tree#./testfiles/empty.ptree"			
			]
		),
		*/
		
		let mut reader = MultipleReader::from_file(File::open("./testfiles/empty.ptree").unwrap());
		let object0 = read_as_object_with_file_handler(&"process tree".parse().unwrap(), &mut reader, None, 0, &mut None, "process tree".parse().unwrap()).unwrap();
		let input0 = EbiInput::Object(object0, "process tree".parse().unwrap());
		let inputs = vec![input0];

		if let EbiCommand::Command{execute, output_type, ..} = crate::ebi_commands::ebi_command_discover::EBI_DISCOVER_RANDOM_SPTREE {
			match (execute)(inputs, None) {
				Ok(output) => {
					output.test_activity_key();
					assert_eq!(&output.get_type(), output_type);
				}
				Err(e) => Err(e).unwrap(),
			}
		}
	}
	#[test]
	pub fn ebi_disc_rnd_sptree_test_2() {
		// to indicate that this test is expected to fail, add the following to src/tests/fallible_test_list.rs:
		/* 
		(
			&EBI_DISCOVER_RANDOM_SPTREE,
			&[
				"object process tree#./testfiles/aa-ab-ba.ptml"			
			]
		),
		*/
		
		let mut reader = MultipleReader::from_file(File::open("./testfiles/aa-ab-ba.ptml").unwrap());
		let object0 = read_as_object_with_file_handler(&"process tree".parse().unwrap(), &mut reader, None, 0, &mut None, "process tree markup language".parse().unwrap()).unwrap();
		let input0 = EbiInput::Object(object0, "process tree markup language".parse().unwrap());
		let inputs = vec![input0];

		if let EbiCommand::Command{execute, output_type, ..} = crate::ebi_commands::ebi_command_discover::EBI_DISCOVER_RANDOM_SPTREE {
			match (execute)(inputs, None) {
				Ok(output) => {
					output.test_activity_key();
					assert_eq!(&output.get_type(), output_type);
				}
				Err(e) => Err(e).unwrap(),
			}
		}
	}
	#[test]
	pub fn ebi_disc_rnd_sptree_test_3() {
		// to indicate that this test is expected to fail, add the following to src/tests/fallible_test_list.rs:
		/* 
		(
			&EBI_DISCOVER_RANDOM_SPTREE,
			&[
				"object process tree#./testfiles/a-b-flower.ptree"			
			]
		),
		*/
		
		let mut reader = MultipleReader::from_file(File::open("./testfiles/a-b-flower.ptree").unwrap());
		let object0 = read_as_object_with_file_handler(&"process tree".parse().unwrap(), &mut reader, None, 0, &mut None, "process tree".parse().unwrap()).unwrap();
		let input0 = EbiInput::Object(object0, "process tree".parse().unwrap());
		let inputs = vec![input0];

		if let EbiCommand::Command{execute, output_type, ..} = crate::ebi_commands::ebi_command_discover::EBI_DISCOVER_RANDOM_SPTREE {
			match (execute)(inputs, None) {
				Ok(output) => {
					output.test_activity_key();
					assert_eq!(&output.get_type(), output_type);
				}
				Err(e) => Err(e).unwrap(),
			}
		}
	}


	// ==== group uni ====


	// ==== command sbpmn ====
	#[test]
	pub fn ebi_disc_uni_sbpmn_test_0() {
		// to indicate that this test is expected to fail, add the following to src/tests/fallible_test_list.rs:
		/* 
		(
			&EBI_DISCOVER_UNIFORM_SBPMN,
			&[
				"object business process model and notation#./testfiles/seq(a-xor(b-c)).sptree"			
			]
		),
		*/
		
		let mut reader = MultipleReader::from_file(File::open("./testfiles/seq(a-xor(b-c)).sptree").unwrap());
		let object0 = read_as_object_with_file_handler(&"business process model and notation".parse().unwrap(), &mut reader, None, 0, &mut None, "stochastic process tree".parse().unwrap()).unwrap();
		let input0 = EbiInput::Object(object0, "stochastic process tree".parse().unwrap());
		let inputs = vec![input0];

		if let EbiCommand::Command{execute, output_type, ..} = crate::ebi_commands::ebi_command_discover::EBI_DISCOVER_UNIFORM_SBPMN {
			match (execute)(inputs, None) {
				Ok(output) => {
					output.test_activity_key();
					assert_eq!(&output.get_type(), output_type);
				}
				Err(e) => Err(e).unwrap(),
			}
		}
	}
	#[test]
	pub fn ebi_disc_uni_sbpmn_test_1() {
		// to indicate that this test is expected to fail, add the following to src/tests/fallible_test_list.rs:
		/* 
		(
			&EBI_DISCOVER_UNIFORM_SBPMN,
			&[
				"object business process model and notation#./testfiles/model.bpmn"			
			]
		),
		*/
		
		let mut reader = MultipleReader::from_file(File::open("./testfiles/model.bpmn").unwrap());
		let object0 = read_as_object_with_file_handler(&"business process model and notation".parse().unwrap(), &mut reader, None, 0, &mut None, "business process model and notation".parse().unwrap()).unwrap();
		let input0 = EbiInput::Object(object0, "business process model and notation".parse().unwrap());
		let inputs = vec![input0];

		if let EbiCommand::Command{execute, output_type, ..} = crate::ebi_commands::ebi_command_discover::EBI_DISCOVER_UNIFORM_SBPMN {
			match (execute)(inputs, None) {
				Ok(output) => {
					output.test_activity_key();
					assert_eq!(&output.get_type(), output_type);
				}
				Err(e) => Err(e).unwrap(),
			}
		}
	}
	#[test]
	pub fn ebi_disc_uni_sbpmn_test_2() {
		// to indicate that this test is expected to fail, add the following to src/tests/fallible_test_list.rs:
		/* 
		(
			&EBI_DISCOVER_UNIFORM_SBPMN,
			&[
				"object business process model and notation#./testfiles/a-b_star.dfm"			
			]
		),
		*/
		
		let mut reader = MultipleReader::from_file(File::open("./testfiles/a-b_star.dfm").unwrap());
		let object0 = read_as_object_with_file_handler(&"business process model and notation".parse().unwrap(), &mut reader, None, 0, &mut None, "directly follows model".parse().unwrap()).unwrap();
		let input0 = EbiInput::Object(object0, "directly follows model".parse().unwrap());
		let inputs = vec![input0];

		if let EbiCommand::Command{execute, output_type, ..} = crate::ebi_commands::ebi_command_discover::EBI_DISCOVER_UNIFORM_SBPMN {
			match (execute)(inputs, None) {
				Ok(output) => {
					output.test_activity_key();
					assert_eq!(&output.get_type(), output_type);
				}
				Err(e) => Err(e).unwrap(),
			}
		}
	}
	#[test]
	pub fn ebi_disc_uni_sbpmn_test_3() {
		// to indicate that this test is expected to fail, add the following to src/tests/fallible_test_list.rs:
		/* 
		(
			&EBI_DISCOVER_UNIFORM_SBPMN,
			&[
				"object business process model and notation#./testfiles/bpic12-a.xes.gz-dfg.dfg"			
			]
		),
		*/
		
		let mut reader = MultipleReader::from_file(File::open("./testfiles/bpic12-a.xes.gz-dfg.dfg").unwrap());
		let object0 = read_as_object_with_file_handler(&"business process model and notation".parse().unwrap(), &mut reader, None, 0, &mut None, "directly follows graph".parse().unwrap()).unwrap();
		let input0 = EbiInput::Object(object0, "directly follows graph".parse().unwrap());
		let inputs = vec![input0];

		if let EbiCommand::Command{execute, output_type, ..} = crate::ebi_commands::ebi_command_discover::EBI_DISCOVER_UNIFORM_SBPMN {
			match (execute)(inputs, None) {
				Ok(output) => {
					output.test_activity_key();
					assert_eq!(&output.get_type(), output_type);
				}
				Err(e) => Err(e).unwrap(),
			}
		}
	}


	// ==== command slpn ====
	#[test]
	pub fn ebi_disc_uni_slpn_test_0() {
		// to indicate that this test is expected to fail, add the following to src/tests/fallible_test_list.rs:
		/* 
		(
			&EBI_DISCOVER_UNIFORM_SLPN,
			&[
				"object labelled Petri net#./testfiles/seq(a-xor(b-c)).sptree"			
			]
		),
		*/
		
		let mut reader = MultipleReader::from_file(File::open("./testfiles/seq(a-xor(b-c)).sptree").unwrap());
		let object0 = read_as_object_with_file_handler(&"labelled Petri net".parse().unwrap(), &mut reader, None, 0, &mut None, "stochastic process tree".parse().unwrap()).unwrap();
		let input0 = EbiInput::Object(object0, "stochastic process tree".parse().unwrap());
		let inputs = vec![input0];

		if let EbiCommand::Command{execute, output_type, ..} = crate::ebi_commands::ebi_command_discover::EBI_DISCOVER_UNIFORM_SLPN {
			match (execute)(inputs, None) {
				Ok(output) => {
					output.test_activity_key();
					assert_eq!(&output.get_type(), output_type);
				}
				Err(e) => Err(e).unwrap(),
			}
		}
	}
	#[test]
	pub fn ebi_disc_uni_slpn_test_1() {
		// to indicate that this test is expected to fail, add the following to src/tests/fallible_test_list.rs:
		/* 
		(
			&EBI_DISCOVER_UNIFORM_SLPN,
			&[
				"object labelled Petri net#./testfiles/a-b_star.dfm"			
			]
		),
		*/
		
		let mut reader = MultipleReader::from_file(File::open("./testfiles/a-b_star.dfm").unwrap());
		let object0 = read_as_object_with_file_handler(&"labelled Petri net".parse().unwrap(), &mut reader, None, 0, &mut None, "directly follows model".parse().unwrap()).unwrap();
		let input0 = EbiInput::Object(object0, "directly follows model".parse().unwrap());
		let inputs = vec![input0];

		if let EbiCommand::Command{execute, output_type, ..} = crate::ebi_commands::ebi_command_discover::EBI_DISCOVER_UNIFORM_SLPN {
			match (execute)(inputs, None) {
				Ok(output) => {
					output.test_activity_key();
					assert_eq!(&output.get_type(), output_type);
				}
				Err(e) => Err(e).unwrap(),
			}
		}
	}
	#[test]
	pub fn ebi_disc_uni_slpn_test_2() {
		// to indicate that this test is expected to fail, add the following to src/tests/fallible_test_list.rs:
		/* 
		(
			&EBI_DISCOVER_UNIFORM_SLPN,
			&[
				"object labelled Petri net#./testfiles/bpic12-a.xes.gz-dfg.dfg"			
			]
		),
		*/
		
		let mut reader = MultipleReader::from_file(File::open("./testfiles/bpic12-a.xes.gz-dfg.dfg").unwrap());
		let object0 = read_as_object_with_file_handler(&"labelled Petri net".parse().unwrap(), &mut reader, None, 0, &mut None, "directly follows graph".parse().unwrap()).unwrap();
		let input0 = EbiInput::Object(object0, "directly follows graph".parse().unwrap());
		let inputs = vec![input0];

		if let EbiCommand::Command{execute, output_type, ..} = crate::ebi_commands::ebi_command_discover::EBI_DISCOVER_UNIFORM_SLPN {
			match (execute)(inputs, None) {
				Ok(output) => {
					output.test_activity_key();
					assert_eq!(&output.get_type(), output_type);
				}
				Err(e) => Err(e).unwrap(),
			}
		}
	}
	#[test]
	pub fn ebi_disc_uni_slpn_test_3() {
		// to indicate that this test is expected to fail, add the following to src/tests/fallible_test_list.rs:
		/* 
		(
			&EBI_DISCOVER_UNIFORM_SLPN,
			&[
				"object labelled Petri net#./testfiles/BPI_Challenge_2013_closed_problems.xes.gz-dfg.dfg"			
			]
		),
		*/
		
		let mut reader = MultipleReader::from_file(File::open("./testfiles/BPI_Challenge_2013_closed_problems.xes.gz-dfg.dfg").unwrap());
		let object0 = read_as_object_with_file_handler(&"labelled Petri net".parse().unwrap(), &mut reader, None, 0, &mut None, "directly follows graph".parse().unwrap()).unwrap();
		let input0 = EbiInput::Object(object0, "directly follows graph".parse().unwrap());
		let inputs = vec![input0];

		if let EbiCommand::Command{execute, output_type, ..} = crate::ebi_commands::ebi_command_discover::EBI_DISCOVER_UNIFORM_SLPN {
			match (execute)(inputs, None) {
				Ok(output) => {
					output.test_activity_key();
					assert_eq!(&output.get_type(), output_type);
				}
				Err(e) => Err(e).unwrap(),
			}
		}
	}


	// ==== command sptree ====
	#[test]
	pub fn ebi_disc_uni_sptree_test_0() {
		// to indicate that this test is expected to fail, add the following to src/tests/fallible_test_list.rs:
		/* 
		(
			&EBI_DISCOVER_UNIFORM_SPTREE,
			&[
				"object process tree#./testfiles/seq(a-xor(b-c)).sptree"			
			]
		),
		*/
		
		let mut reader = MultipleReader::from_file(File::open("./testfiles/seq(a-xor(b-c)).sptree").unwrap());
		let object0 = read_as_object_with_file_handler(&"process tree".parse().unwrap(), &mut reader, None, 0, &mut None, "stochastic process tree".parse().unwrap()).unwrap();
		let input0 = EbiInput::Object(object0, "stochastic process tree".parse().unwrap());
		let inputs = vec![input0];

		if let EbiCommand::Command{execute, output_type, ..} = crate::ebi_commands::ebi_command_discover::EBI_DISCOVER_UNIFORM_SPTREE {
			match (execute)(inputs, None) {
				Ok(output) => {
					output.test_activity_key();
					assert_eq!(&output.get_type(), output_type);
				}
				Err(e) => Err(e).unwrap(),
			}
		}
	}
	#[test]
	pub fn ebi_disc_uni_sptree_test_1() {
		// to indicate that this test is expected to fail, add the following to src/tests/fallible_test_list.rs:
		/* 
		(
			&EBI_DISCOVER_UNIFORM_SPTREE,
			&[
				"object process tree#./testfiles/empty.ptree"			
			]
		),
		*/
		
		let mut reader = MultipleReader::from_file(File::open("./testfiles/empty.ptree").unwrap());
		let object0 = read_as_object_with_file_handler(&"process tree".parse().unwrap(), &mut reader, None, 0, &mut None, "process tree".parse().unwrap()).unwrap();
		let input0 = EbiInput::Object(object0, "process tree".parse().unwrap());
		let inputs = vec![input0];

		if let EbiCommand::Command{execute, output_type, ..} = crate::ebi_commands::ebi_command_discover::EBI_DISCOVER_UNIFORM_SPTREE {
			match (execute)(inputs, None) {
				Ok(output) => {
					output.test_activity_key();
					assert_eq!(&output.get_type(), output_type);
				}
				Err(e) => Err(e).unwrap(),
			}
		}
	}
	#[test]
	pub fn ebi_disc_uni_sptree_test_2() {
		// to indicate that this test is expected to fail, add the following to src/tests/fallible_test_list.rs:
		/* 
		(
			&EBI_DISCOVER_UNIFORM_SPTREE,
			&[
				"object process tree#./testfiles/aa-ab-ba.ptml"			
			]
		),
		*/
		
		let mut reader = MultipleReader::from_file(File::open("./testfiles/aa-ab-ba.ptml").unwrap());
		let object0 = read_as_object_with_file_handler(&"process tree".parse().unwrap(), &mut reader, None, 0, &mut None, "process tree markup language".parse().unwrap()).unwrap();
		let input0 = EbiInput::Object(object0, "process tree markup language".parse().unwrap());
		let inputs = vec![input0];

		if let EbiCommand::Command{execute, output_type, ..} = crate::ebi_commands::ebi_command_discover::EBI_DISCOVER_UNIFORM_SPTREE {
			match (execute)(inputs, None) {
				Ok(output) => {
					output.test_activity_key();
					assert_eq!(&output.get_type(), output_type);
				}
				Err(e) => Err(e).unwrap(),
			}
		}
	}
	#[test]
	pub fn ebi_disc_uni_sptree_test_3() {
		// to indicate that this test is expected to fail, add the following to src/tests/fallible_test_list.rs:
		/* 
		(
			&EBI_DISCOVER_UNIFORM_SPTREE,
			&[
				"object process tree#./testfiles/a-b-flower.ptree"			
			]
		),
		*/
		
		let mut reader = MultipleReader::from_file(File::open("./testfiles/a-b-flower.ptree").unwrap());
		let object0 = read_as_object_with_file_handler(&"process tree".parse().unwrap(), &mut reader, None, 0, &mut None, "process tree".parse().unwrap()).unwrap();
		let input0 = EbiInput::Object(object0, "process tree".parse().unwrap());
		let inputs = vec![input0];

		if let EbiCommand::Command{execute, output_type, ..} = crate::ebi_commands::ebi_command_discover::EBI_DISCOVER_UNIFORM_SPTREE {
			match (execute)(inputs, None) {
				Ok(output) => {
					output.test_activity_key();
					assert_eq!(&output.get_type(), output_type);
				}
				Err(e) => Err(e).unwrap(),
			}
		}
	}


	// ==== group dins ====


	// ==== group flw ====


	// ==== command dfa ====
	#[test]
	pub fn ebi_dins_flw_dfa_test_0() {
		// to indicate that this test is expected to fail, add the following to src/tests/fallible_test_list.rs:
		/* 
		(
			&EBI_DISCOVER_NON_STOCHASTIC_FLOWER_DFA,
			&[
				"trait finite language#./testfiles/ba-aa-ab.slang"			
			]
		),
		*/
		
		let mut reader = MultipleReader::from_file(File::open("./testfiles/ba-aa-ab.slang").unwrap());
		let (object0, file_handler0) = ebi_input::read_as_trait(&("finite language".parse().unwrap()), &mut reader, None, 0).unwrap();
		let input0 = EbiInput::Trait(object0, file_handler0);
		let inputs = vec![input0];

		if let EbiCommand::Command{execute, output_type, ..} = crate::ebi_commands::ebi_command_discover_non_stochastic::EBI_DISCOVER_NON_STOCHASTIC_FLOWER_DFA {
			match (execute)(inputs, None) {
				Ok(output) => {
					output.test_activity_key();
					assert_eq!(&output.get_type(), output_type);
				}
				Err(e) => Err(e).unwrap(),
			}
		}
	}
	#[test]
	pub fn ebi_dins_flw_dfa_test_1() {
		// to indicate that this test is expected to fail, add the following to src/tests/fallible_test_list.rs:
		/* 
		(
			&EBI_DISCOVER_NON_STOCHASTIC_FLOWER_DFA,
			&[
				"trait finite language#./testfiles/a-b.slang"			
			]
		),
		*/
		
		let mut reader = MultipleReader::from_file(File::open("./testfiles/a-b.slang").unwrap());
		let (object0, file_handler0) = ebi_input::read_as_trait(&("finite language".parse().unwrap()), &mut reader, None, 0).unwrap();
		let input0 = EbiInput::Trait(object0, file_handler0);
		let inputs = vec![input0];

		if let EbiCommand::Command{execute, output_type, ..} = crate::ebi_commands::ebi_command_discover_non_stochastic::EBI_DISCOVER_NON_STOCHASTIC_FLOWER_DFA {
			match (execute)(inputs, None) {
				Ok(output) => {
					output.test_activity_key();
					assert_eq!(&output.get_type(), output_type);
				}
				Err(e) => Err(e).unwrap(),
			}
		}
	}
	#[test]
	pub fn ebi_dins_flw_dfa_test_2() {
		// to indicate that this test is expected to fail, add the following to src/tests/fallible_test_list.rs:
		/* 
		(
			&EBI_DISCOVER_NON_STOCHASTIC_FLOWER_DFA,
			&[
				"trait finite language#./testfiles/empty.xes"			
			]
		),
		*/
		
		let mut reader = MultipleReader::from_file(File::open("./testfiles/empty.xes").unwrap());
		let (object0, file_handler0) = ebi_input::read_as_trait(&("finite language".parse().unwrap()), &mut reader, None, 0).unwrap();
		let input0 = EbiInput::Trait(object0, file_handler0);
		let inputs = vec![input0];

		if let EbiCommand::Command{execute, output_type, ..} = crate::ebi_commands::ebi_command_discover_non_stochastic::EBI_DISCOVER_NON_STOCHASTIC_FLOWER_DFA {
			match (execute)(inputs, None) {
				Ok(output) => {
					output.test_activity_key();
					assert_eq!(&output.get_type(), output_type);
				}
				Err(e) => Err(e).unwrap(),
			}
		}
	}
	#[test]
	pub fn ebi_dins_flw_dfa_test_3() {
		// to indicate that this test is expected to fail, add the following to src/tests/fallible_test_list.rs:
		/* 
		(
			&EBI_DISCOVER_NON_STOCHASTIC_FLOWER_DFA,
			&[
				"trait finite language#./testfiles/acb-abc-ad-aded-adeded-adededed.slang"			
			]
		),
		*/
		
		let mut reader = MultipleReader::from_file(File::open("./testfiles/acb-abc-ad-aded-adeded-adededed.slang").unwrap());
		let (object0, file_handler0) = ebi_input::read_as_trait(&("finite language".parse().unwrap()), &mut reader, None, 0).unwrap();
		let input0 = EbiInput::Trait(object0, file_handler0);
		let inputs = vec![input0];

		if let EbiCommand::Command{execute, output_type, ..} = crate::ebi_commands::ebi_command_discover_non_stochastic::EBI_DISCOVER_NON_STOCHASTIC_FLOWER_DFA {
			match (execute)(inputs, None) {
				Ok(output) => {
					output.test_activity_key();
					assert_eq!(&output.get_type(), output_type);
				}
				Err(e) => Err(e).unwrap(),
			}
		}
	}


	// ==== command ptree ====
	#[test]
	pub fn ebi_dins_flw_ptree_test_0() {
		// to indicate that this test is expected to fail, add the following to src/tests/fallible_test_list.rs:
		/* 
		(
			&EBI_DISCOVER_NON_STOCHASTIC_FLOWER_TREE,
			&[
				"trait finite language#./testfiles/ba-aa-ab.slang"			
			]
		),
		*/
		
		let mut reader = MultipleReader::from_file(File::open("./testfiles/ba-aa-ab.slang").unwrap());
		let (object0, file_handler0) = ebi_input::read_as_trait(&("finite language".parse().unwrap()), &mut reader, None, 0).unwrap();
		let input0 = EbiInput::Trait(object0, file_handler0);
		let inputs = vec![input0];

		if let EbiCommand::Command{execute, output_type, ..} = crate::ebi_commands::ebi_command_discover_non_stochastic::EBI_DISCOVER_NON_STOCHASTIC_FLOWER_TREE {
			match (execute)(inputs, None) {
				Ok(output) => {
					output.test_activity_key();
					assert_eq!(&output.get_type(), output_type);
				}
				Err(e) => Err(e).unwrap(),
			}
		}
	}
	#[test]
	pub fn ebi_dins_flw_ptree_test_1() {
		// to indicate that this test is expected to fail, add the following to src/tests/fallible_test_list.rs:
		/* 
		(
			&EBI_DISCOVER_NON_STOCHASTIC_FLOWER_TREE,
			&[
				"trait finite language#./testfiles/a-b.slang"			
			]
		),
		*/
		
		let mut reader = MultipleReader::from_file(File::open("./testfiles/a-b.slang").unwrap());
		let (object0, file_handler0) = ebi_input::read_as_trait(&("finite language".parse().unwrap()), &mut reader, None, 0).unwrap();
		let input0 = EbiInput::Trait(object0, file_handler0);
		let inputs = vec![input0];

		if let EbiCommand::Command{execute, output_type, ..} = crate::ebi_commands::ebi_command_discover_non_stochastic::EBI_DISCOVER_NON_STOCHASTIC_FLOWER_TREE {
			match (execute)(inputs, None) {
				Ok(output) => {
					output.test_activity_key();
					assert_eq!(&output.get_type(), output_type);
				}
				Err(e) => Err(e).unwrap(),
			}
		}
	}
	#[test]
	pub fn ebi_dins_flw_ptree_test_2() {
		// to indicate that this test is expected to fail, add the following to src/tests/fallible_test_list.rs:
		/* 
		(
			&EBI_DISCOVER_NON_STOCHASTIC_FLOWER_TREE,
			&[
				"trait finite language#./testfiles/empty.xes"			
			]
		),
		*/
		
		let mut reader = MultipleReader::from_file(File::open("./testfiles/empty.xes").unwrap());
		let (object0, file_handler0) = ebi_input::read_as_trait(&("finite language".parse().unwrap()), &mut reader, None, 0).unwrap();
		let input0 = EbiInput::Trait(object0, file_handler0);
		let inputs = vec![input0];

		if let EbiCommand::Command{execute, output_type, ..} = crate::ebi_commands::ebi_command_discover_non_stochastic::EBI_DISCOVER_NON_STOCHASTIC_FLOWER_TREE {
			match (execute)(inputs, None) {
				Ok(output) => {
					output.test_activity_key();
					assert_eq!(&output.get_type(), output_type);
				}
				Err(e) => Err(e).unwrap(),
			}
		}
	}
	#[test]
	pub fn ebi_dins_flw_ptree_test_3() {
		// to indicate that this test is expected to fail, add the following to src/tests/fallible_test_list.rs:
		/* 
		(
			&EBI_DISCOVER_NON_STOCHASTIC_FLOWER_TREE,
			&[
				"trait finite language#./testfiles/acb-abc-ad-aded-adeded-adededed.slang"			
			]
		),
		*/
		
		let mut reader = MultipleReader::from_file(File::open("./testfiles/acb-abc-ad-aded-adeded-adededed.slang").unwrap());
		let (object0, file_handler0) = ebi_input::read_as_trait(&("finite language".parse().unwrap()), &mut reader, None, 0).unwrap();
		let input0 = EbiInput::Trait(object0, file_handler0);
		let inputs = vec![input0];

		if let EbiCommand::Command{execute, output_type, ..} = crate::ebi_commands::ebi_command_discover_non_stochastic::EBI_DISCOVER_NON_STOCHASTIC_FLOWER_TREE {
			match (execute)(inputs, None) {
				Ok(output) => {
					output.test_activity_key();
					assert_eq!(&output.get_type(), output_type);
				}
				Err(e) => Err(e).unwrap(),
			}
		}
	}


	// ==== group pfxt ====


	// ==== command dfa ====
	#[test]
	pub fn ebi_dins_pfxt_dfa_test_0() {
		// to indicate that this test is expected to fail, add the following to src/tests/fallible_test_list.rs:
		/* 
		(
			&EBI_DISCOVER_NON_STOCHASTIC_TREE_DFA,
			&[
				"trait finite language#./testfiles/ba-aa-ab.slang"			
			]
		),
		*/
		
		let mut reader = MultipleReader::from_file(File::open("./testfiles/ba-aa-ab.slang").unwrap());
		let (object0, file_handler0) = ebi_input::read_as_trait(&("finite language".parse().unwrap()), &mut reader, None, 0).unwrap();
		let input0 = EbiInput::Trait(object0, file_handler0);
		let inputs = vec![input0];

		if let EbiCommand::Command{execute, output_type, ..} = crate::ebi_commands::ebi_command_discover_non_stochastic::EBI_DISCOVER_NON_STOCHASTIC_TREE_DFA {
			match (execute)(inputs, None) {
				Ok(output) => {
					output.test_activity_key();
					assert_eq!(&output.get_type(), output_type);
				}
				Err(e) => Err(e).unwrap(),
			}
		}
	}
	#[test]
	pub fn ebi_dins_pfxt_dfa_test_1() {
		// to indicate that this test is expected to fail, add the following to src/tests/fallible_test_list.rs:
		/* 
		(
			&EBI_DISCOVER_NON_STOCHASTIC_TREE_DFA,
			&[
				"trait finite language#./testfiles/a-b.slang"			
			]
		),
		*/
		
		let mut reader = MultipleReader::from_file(File::open("./testfiles/a-b.slang").unwrap());
		let (object0, file_handler0) = ebi_input::read_as_trait(&("finite language".parse().unwrap()), &mut reader, None, 0).unwrap();
		let input0 = EbiInput::Trait(object0, file_handler0);
		let inputs = vec![input0];

		if let EbiCommand::Command{execute, output_type, ..} = crate::ebi_commands::ebi_command_discover_non_stochastic::EBI_DISCOVER_NON_STOCHASTIC_TREE_DFA {
			match (execute)(inputs, None) {
				Ok(output) => {
					output.test_activity_key();
					assert_eq!(&output.get_type(), output_type);
				}
				Err(e) => Err(e).unwrap(),
			}
		}
	}
	#[test]
	pub fn ebi_dins_pfxt_dfa_test_2() {
		// to indicate that this test is expected to fail, add the following to src/tests/fallible_test_list.rs:
		/* 
		(
			&EBI_DISCOVER_NON_STOCHASTIC_TREE_DFA,
			&[
				"trait finite language#./testfiles/empty.xes"			
			]
		),
		*/
		
		let mut reader = MultipleReader::from_file(File::open("./testfiles/empty.xes").unwrap());
		let (object0, file_handler0) = ebi_input::read_as_trait(&("finite language".parse().unwrap()), &mut reader, None, 0).unwrap();
		let input0 = EbiInput::Trait(object0, file_handler0);
		let inputs = vec![input0];

		if let EbiCommand::Command{execute, output_type, ..} = crate::ebi_commands::ebi_command_discover_non_stochastic::EBI_DISCOVER_NON_STOCHASTIC_TREE_DFA {
			match (execute)(inputs, None) {
				Ok(output) => {
					output.test_activity_key();
					assert_eq!(&output.get_type(), output_type);
				}
				Err(e) => Err(e).unwrap(),
			}
		}
	}
	#[test]
	pub fn ebi_dins_pfxt_dfa_test_3() {
		// to indicate that this test is expected to fail, add the following to src/tests/fallible_test_list.rs:
		/* 
		(
			&EBI_DISCOVER_NON_STOCHASTIC_TREE_DFA,
			&[
				"trait finite language#./testfiles/acb-abc-ad-aded-adeded-adededed.slang"			
			]
		),
		*/
		
		let mut reader = MultipleReader::from_file(File::open("./testfiles/acb-abc-ad-aded-adeded-adededed.slang").unwrap());
		let (object0, file_handler0) = ebi_input::read_as_trait(&("finite language".parse().unwrap()), &mut reader, None, 0).unwrap();
		let input0 = EbiInput::Trait(object0, file_handler0);
		let inputs = vec![input0];

		if let EbiCommand::Command{execute, output_type, ..} = crate::ebi_commands::ebi_command_discover_non_stochastic::EBI_DISCOVER_NON_STOCHASTIC_TREE_DFA {
			match (execute)(inputs, None) {
				Ok(output) => {
					output.test_activity_key();
					assert_eq!(&output.get_type(), output_type);
				}
				Err(e) => Err(e).unwrap(),
			}
		}
	}


	// ==== command ptree ====
	#[test]
	pub fn ebi_dins_pfxt_ptree_test_0() {
		// to indicate that this test is expected to fail, add the following to src/tests/fallible_test_list.rs:
		/* 
		(
			&EBI_DISCOVER_NON_STOCHASTIC_TREE_TREE,
			&[
				"trait finite language#./testfiles/ba-aa-ab.slang"			
			]
		),
		*/
		
		let mut reader = MultipleReader::from_file(File::open("./testfiles/ba-aa-ab.slang").unwrap());
		let (object0, file_handler0) = ebi_input::read_as_trait(&("finite language".parse().unwrap()), &mut reader, None, 0).unwrap();
		let input0 = EbiInput::Trait(object0, file_handler0);
		let inputs = vec![input0];

		if let EbiCommand::Command{execute, output_type, ..} = crate::ebi_commands::ebi_command_discover_non_stochastic::EBI_DISCOVER_NON_STOCHASTIC_TREE_TREE {
			match (execute)(inputs, None) {
				Ok(output) => {
					output.test_activity_key();
					assert_eq!(&output.get_type(), output_type);
				}
				Err(e) => Err(e).unwrap(),
			}
		}
	}
	#[test]
	pub fn ebi_dins_pfxt_ptree_test_1() {
		// to indicate that this test is expected to fail, add the following to src/tests/fallible_test_list.rs:
		/* 
		(
			&EBI_DISCOVER_NON_STOCHASTIC_TREE_TREE,
			&[
				"trait finite language#./testfiles/a-b.slang"			
			]
		),
		*/
		
		let mut reader = MultipleReader::from_file(File::open("./testfiles/a-b.slang").unwrap());
		let (object0, file_handler0) = ebi_input::read_as_trait(&("finite language".parse().unwrap()), &mut reader, None, 0).unwrap();
		let input0 = EbiInput::Trait(object0, file_handler0);
		let inputs = vec![input0];

		if let EbiCommand::Command{execute, output_type, ..} = crate::ebi_commands::ebi_command_discover_non_stochastic::EBI_DISCOVER_NON_STOCHASTIC_TREE_TREE {
			match (execute)(inputs, None) {
				Ok(output) => {
					output.test_activity_key();
					assert_eq!(&output.get_type(), output_type);
				}
				Err(e) => Err(e).unwrap(),
			}
		}
	}
	#[test]
	pub fn ebi_dins_pfxt_ptree_test_2() {
		// to indicate that this test is expected to fail, add the following to src/tests/fallible_test_list.rs:
		/* 
		(
			&EBI_DISCOVER_NON_STOCHASTIC_TREE_TREE,
			&[
				"trait finite language#./testfiles/empty.xes"			
			]
		),
		*/
		
		let mut reader = MultipleReader::from_file(File::open("./testfiles/empty.xes").unwrap());
		let (object0, file_handler0) = ebi_input::read_as_trait(&("finite language".parse().unwrap()), &mut reader, None, 0).unwrap();
		let input0 = EbiInput::Trait(object0, file_handler0);
		let inputs = vec![input0];

		if let EbiCommand::Command{execute, output_type, ..} = crate::ebi_commands::ebi_command_discover_non_stochastic::EBI_DISCOVER_NON_STOCHASTIC_TREE_TREE {
			match (execute)(inputs, None) {
				Ok(output) => {
					output.test_activity_key();
					assert_eq!(&output.get_type(), output_type);
				}
				Err(e) => Err(e).unwrap(),
			}
		}
	}
	#[test]
	pub fn ebi_dins_pfxt_ptree_test_3() {
		// to indicate that this test is expected to fail, add the following to src/tests/fallible_test_list.rs:
		/* 
		(
			&EBI_DISCOVER_NON_STOCHASTIC_TREE_TREE,
			&[
				"trait finite language#./testfiles/acb-abc-ad-aded-adeded-adededed.slang"			
			]
		),
		*/
		
		let mut reader = MultipleReader::from_file(File::open("./testfiles/acb-abc-ad-aded-adeded-adededed.slang").unwrap());
		let (object0, file_handler0) = ebi_input::read_as_trait(&("finite language".parse().unwrap()), &mut reader, None, 0).unwrap();
		let input0 = EbiInput::Trait(object0, file_handler0);
		let inputs = vec![input0];

		if let EbiCommand::Command{execute, output_type, ..} = crate::ebi_commands::ebi_command_discover_non_stochastic::EBI_DISCOVER_NON_STOCHASTIC_TREE_TREE {
			match (execute)(inputs, None) {
				Ok(output) => {
					output.test_activity_key();
					assert_eq!(&output.get_type(), output_type);
				}
				Err(e) => Err(e).unwrap(),
			}
		}
	}


	// ==== command tm ====
	#[test]
	pub fn ebi_dins_tm_test_0() {
		// to indicate that this test is expected to fail, add the following to src/tests/fallible_test_list.rs:
		/* 
		(
			&EBI_DISCOVER_NON_STOCHASTIC_TRACE_MODEL,
			&[
				"trait finite language#./testfiles/ba-aa-ab.slang"			
			]
		),
		*/
		
		let mut reader = MultipleReader::from_file(File::open("./testfiles/ba-aa-ab.slang").unwrap());
		let (object0, file_handler0) = ebi_input::read_as_trait(&("finite language".parse().unwrap()), &mut reader, None, 0).unwrap();
		let input0 = EbiInput::Trait(object0, file_handler0);
		let inputs = vec![input0];

		if let EbiCommand::Command{execute, output_type, ..} = crate::ebi_commands::ebi_command_discover_non_stochastic::EBI_DISCOVER_NON_STOCHASTIC_TRACE_MODEL {
			match (execute)(inputs, None) {
				Ok(output) => {
					output.test_activity_key();
					assert_eq!(&output.get_type(), output_type);
				}
				Err(e) => Err(e).unwrap(),
			}
		}
	}
	#[test]
	pub fn ebi_dins_tm_test_1() {
		// to indicate that this test is expected to fail, add the following to src/tests/fallible_test_list.rs:
		/* 
		(
			&EBI_DISCOVER_NON_STOCHASTIC_TRACE_MODEL,
			&[
				"trait finite language#./testfiles/a-b.slang"			
			]
		),
		*/
		
		let mut reader = MultipleReader::from_file(File::open("./testfiles/a-b.slang").unwrap());
		let (object0, file_handler0) = ebi_input::read_as_trait(&("finite language".parse().unwrap()), &mut reader, None, 0).unwrap();
		let input0 = EbiInput::Trait(object0, file_handler0);
		let inputs = vec![input0];

		if let EbiCommand::Command{execute, output_type, ..} = crate::ebi_commands::ebi_command_discover_non_stochastic::EBI_DISCOVER_NON_STOCHASTIC_TRACE_MODEL {
			match (execute)(inputs, None) {
				Ok(output) => {
					output.test_activity_key();
					assert_eq!(&output.get_type(), output_type);
				}
				Err(e) => Err(e).unwrap(),
			}
		}
	}
	#[test]
	pub fn ebi_dins_tm_test_2() {
		// to indicate that this test is expected to fail, add the following to src/tests/fallible_test_list.rs:
		/* 
		(
			&EBI_DISCOVER_NON_STOCHASTIC_TRACE_MODEL,
			&[
				"trait finite language#./testfiles/empty.xes"			
			]
		),
		*/
		
		let mut reader = MultipleReader::from_file(File::open("./testfiles/empty.xes").unwrap());
		let (object0, file_handler0) = ebi_input::read_as_trait(&("finite language".parse().unwrap()), &mut reader, None, 0).unwrap();
		let input0 = EbiInput::Trait(object0, file_handler0);
		let inputs = vec![input0];

		if let EbiCommand::Command{execute, output_type, ..} = crate::ebi_commands::ebi_command_discover_non_stochastic::EBI_DISCOVER_NON_STOCHASTIC_TRACE_MODEL {
			match (execute)(inputs, None) {
				Ok(output) => {
					output.test_activity_key();
					assert_eq!(&output.get_type(), output_type);
				}
				Err(e) => Err(e).unwrap(),
			}
		}
	}
	#[test]
	pub fn ebi_dins_tm_test_3() {
		// to indicate that this test is expected to fail, add the following to src/tests/fallible_test_list.rs:
		/* 
		(
			&EBI_DISCOVER_NON_STOCHASTIC_TRACE_MODEL,
			&[
				"trait finite language#./testfiles/acb-abc-ad-aded-adeded-adededed.slang"			
			]
		),
		*/
		
		let mut reader = MultipleReader::from_file(File::open("./testfiles/acb-abc-ad-aded-adeded-adededed.slang").unwrap());
		let (object0, file_handler0) = ebi_input::read_as_trait(&("finite language".parse().unwrap()), &mut reader, None, 0).unwrap();
		let input0 = EbiInput::Trait(object0, file_handler0);
		let inputs = vec![input0];

		if let EbiCommand::Command{execute, output_type, ..} = crate::ebi_commands::ebi_command_discover_non_stochastic::EBI_DISCOVER_NON_STOCHASTIC_TRACE_MODEL {
			match (execute)(inputs, None) {
				Ok(output) => {
					output.test_activity_key();
					assert_eq!(&output.get_type(), output_type);
				}
				Err(e) => Err(e).unwrap(),
			}
		}
	}


	// ==== group fil ====


	// ==== group tr ====


	// ==== command empty ====
	#[test]
	pub fn ebi_fil_tr_empty_test_0() {
		// to indicate that this test is expected to fail, add the following to src/tests/fallible_test_list.rs:
		/* 
		(
			&EBI_FILTER_TRACES_EMPTY,
			&[
				"object XES event log#./testfiles/empty.xes"			
			]
		),
		*/
		
		let mut reader = MultipleReader::from_file(File::open("./testfiles/empty.xes").unwrap());
		let object0 = read_as_object_with_file_handler(&"XES event log".parse().unwrap(), &mut reader, None, 0, &mut None, "extensible event stream".parse().unwrap()).unwrap();
		let input0 = EbiInput::Object(object0, "extensible event stream".parse().unwrap());
		let inputs = vec![input0];

		if let EbiCommand::Command{execute, output_type, ..} = crate::ebi_commands::ebi_command_filter::EBI_FILTER_TRACES_EMPTY {
			match (execute)(inputs, None) {
				Ok(output) => {
					output.test_activity_key();
					assert_eq!(&output.get_type(), output_type);
				}
				Err(e) => Err(e).unwrap(),
			}
		}
	}
	#[test]
	pub fn ebi_fil_tr_empty_test_1() {
		// to indicate that this test is expected to fail, add the following to src/tests/fallible_test_list.rs:
		/* 
		(
			&EBI_FILTER_TRACES_EMPTY,
			&[
				"object XES event log#./testfiles/simple_log_markovian_abstraction.xes"			
			]
		),
		*/
		
		let mut reader = MultipleReader::from_file(File::open("./testfiles/simple_log_markovian_abstraction.xes").unwrap());
		let object0 = read_as_object_with_file_handler(&"XES event log".parse().unwrap(), &mut reader, None, 0, &mut None, "extensible event stream".parse().unwrap()).unwrap();
		let input0 = EbiInput::Object(object0, "extensible event stream".parse().unwrap());
		let inputs = vec![input0];

		if let EbiCommand::Command{execute, output_type, ..} = crate::ebi_commands::ebi_command_filter::EBI_FILTER_TRACES_EMPTY {
			match (execute)(inputs, None) {
				Ok(output) => {
					output.test_activity_key();
					assert_eq!(&output.get_type(), output_type);
				}
				Err(e) => Err(e).unwrap(),
			}
		}
	}
	#[test]
	pub fn ebi_fil_tr_empty_test_2() {
		// to indicate that this test is expected to fail, add the following to src/tests/fallible_test_list.rs:
		/* 
		(
			&EBI_FILTER_TRACES_EMPTY,
			&[
				"object XES event log#./testfiles/a-b-double.xes"			
			]
		),
		*/
		
		let mut reader = MultipleReader::from_file(File::open("./testfiles/a-b-double.xes").unwrap());
		let object0 = read_as_object_with_file_handler(&"XES event log".parse().unwrap(), &mut reader, None, 0, &mut None, "extensible event stream".parse().unwrap()).unwrap();
		let input0 = EbiInput::Object(object0, "extensible event stream".parse().unwrap());
		let inputs = vec![input0];

		if let EbiCommand::Command{execute, output_type, ..} = crate::ebi_commands::ebi_command_filter::EBI_FILTER_TRACES_EMPTY {
			match (execute)(inputs, None) {
				Ok(output) => {
					output.test_activity_key();
					assert_eq!(&output.get_type(), output_type);
				}
				Err(e) => Err(e).unwrap(),
			}
		}
	}
	#[test]
	pub fn ebi_fil_tr_empty_test_3() {
		// to indicate that this test is expected to fail, add the following to src/tests/fallible_test_list.rs:
		/* 
		(
			&EBI_FILTER_TRACES_EMPTY,
			&[
				"object XES event log#./testfiles/a-b.xes.gz"			
			]
		),
		*/
		
		let mut reader = MultipleReader::from_file(File::open("./testfiles/a-b.xes.gz").unwrap());
		let object0 = read_as_object_with_file_handler(&"XES event log".parse().unwrap(), &mut reader, None, 0, &mut None, "compressed event log".parse().unwrap()).unwrap();
		let input0 = EbiInput::Object(object0, "compressed event log".parse().unwrap());
		let inputs = vec![input0];

		if let EbiCommand::Command{execute, output_type, ..} = crate::ebi_commands::ebi_command_filter::EBI_FILTER_TRACES_EMPTY {
			match (execute)(inputs, None) {
				Ok(output) => {
					output.test_activity_key();
					assert_eq!(&output.get_type(), output_type);
				}
				Err(e) => Err(e).unwrap(),
			}
		}
	}


	// ==== command len ====
	#[test]
	pub fn ebi_fil_tr_len_test_0() {
		// to indicate that this test is expected to fail, add the following to src/tests/fallible_test_list.rs:
		/* 
		(
			&EBI_FILTER_TRACES_LENGTH,
			&[
				"object XES event log#./testfiles/empty.xes",
				"string <",
				"usize 0"			
			]
		),
		*/
		
		let mut reader = MultipleReader::from_file(File::open("./testfiles/empty.xes").unwrap());
		let object0 = read_as_object_with_file_handler(&"XES event log".parse().unwrap(), &mut reader, None, 0, &mut None, "extensible event stream".parse().unwrap()).unwrap();
		let input0 = EbiInput::Object(object0, "extensible event stream".parse().unwrap());
		let input1 = EbiInput::String("<".to_string(), &TEST_INPUT_TYPE_STRING);
		let input2 = EbiInput::Usize(0, &TEST_INPUT_TYPE_USIZE);
		let inputs = vec![input0, input1, input2];

		if let EbiCommand::Command{execute, output_type, ..} = crate::ebi_commands::ebi_command_filter::EBI_FILTER_TRACES_LENGTH {
			match (execute)(inputs, None) {
				Ok(output) => {
					output.test_activity_key();
					assert_eq!(&output.get_type(), output_type);
				}
				Err(e) => Err(e).unwrap(),
			}
		}
	}
	#[test]
	pub fn ebi_fil_tr_len_test_1() {
		// to indicate that this test is expected to fail, add the following to src/tests/fallible_test_list.rs:
		/* 
		(
			&EBI_FILTER_TRACES_LENGTH,
			&[
				"object XES event log#./testfiles/simple_log_markovian_abstraction.xes",
				"string <",
				"usize 0"			
			]
		),
		*/
		
		let mut reader = MultipleReader::from_file(File::open("./testfiles/simple_log_markovian_abstraction.xes").unwrap());
		let object0 = read_as_object_with_file_handler(&"XES event log".parse().unwrap(), &mut reader, None, 0, &mut None, "extensible event stream".parse().unwrap()).unwrap();
		let input0 = EbiInput::Object(object0, "extensible event stream".parse().unwrap());
		let input1 = EbiInput::String("<".to_string(), &TEST_INPUT_TYPE_STRING);
		let input2 = EbiInput::Usize(0, &TEST_INPUT_TYPE_USIZE);
		let inputs = vec![input0, input1, input2];

		if let EbiCommand::Command{execute, output_type, ..} = crate::ebi_commands::ebi_command_filter::EBI_FILTER_TRACES_LENGTH {
			match (execute)(inputs, None) {
				Ok(output) => {
					output.test_activity_key();
					assert_eq!(&output.get_type(), output_type);
				}
				Err(e) => Err(e).unwrap(),
			}
		}
	}
	#[test]
	pub fn ebi_fil_tr_len_test_2() {
		// to indicate that this test is expected to fail, add the following to src/tests/fallible_test_list.rs:
		/* 
		(
			&EBI_FILTER_TRACES_LENGTH,
			&[
				"object XES event log#./testfiles/a-b-double.xes",
				"string <",
				"usize 0"			
			]
		),
		*/
		
		let mut reader = MultipleReader::from_file(File::open("./testfiles/a-b-double.xes").unwrap());
		let object0 = read_as_object_with_file_handler(&"XES event log".parse().unwrap(), &mut reader, None, 0, &mut None, "extensible event stream".parse().unwrap()).unwrap();
		let input0 = EbiInput::Object(object0, "extensible event stream".parse().unwrap());
		let input1 = EbiInput::String("<".to_string(), &TEST_INPUT_TYPE_STRING);
		let input2 = EbiInput::Usize(0, &TEST_INPUT_TYPE_USIZE);
		let inputs = vec![input0, input1, input2];

		if let EbiCommand::Command{execute, output_type, ..} = crate::ebi_commands::ebi_command_filter::EBI_FILTER_TRACES_LENGTH {
			match (execute)(inputs, None) {
				Ok(output) => {
					output.test_activity_key();
					assert_eq!(&output.get_type(), output_type);
				}
				Err(e) => Err(e).unwrap(),
			}
		}
	}
	#[test]
	pub fn ebi_fil_tr_len_test_3() {
		// to indicate that this test is expected to fail, add the following to src/tests/fallible_test_list.rs:
		/* 
		(
			&EBI_FILTER_TRACES_LENGTH,
			&[
				"object XES event log#./testfiles/a-b.xes.gz",
				"string <",
				"usize 0"			
			]
		),
		*/
		
		let mut reader = MultipleReader::from_file(File::open("./testfiles/a-b.xes.gz").unwrap());
		let object0 = read_as_object_with_file_handler(&"XES event log".parse().unwrap(), &mut reader, None, 0, &mut None, "compressed event log".parse().unwrap()).unwrap();
		let input0 = EbiInput::Object(object0, "compressed event log".parse().unwrap());
		let input1 = EbiInput::String("<".to_string(), &TEST_INPUT_TYPE_STRING);
		let input2 = EbiInput::Usize(0, &TEST_INPUT_TYPE_USIZE);
		let inputs = vec![input0, input1, input2];

		if let EbiCommand::Command{execute, output_type, ..} = crate::ebi_commands::ebi_command_filter::EBI_FILTER_TRACES_LENGTH {
			match (execute)(inputs, None) {
				Ok(output) => {
					output.test_activity_key();
					assert_eq!(&output.get_type(), output_type);
				}
				Err(e) => Err(e).unwrap(),
			}
		}
	}


	// ==== group event ====


	// ==== command act ====
	#[test]
	pub fn ebi_fil_tr_event_act_test_0() {
		// to indicate that this test is expected to fail, add the following to src/tests/fallible_test_list.rs:
		/* 
		(
			&EBI_FILTER_TRACES_EVENT_ACTIVITY,
			&[
				"object XES event log#./testfiles/empty.xes",
				"string any",
				"string some string"			
			]
		),
		*/
		
		let mut reader = MultipleReader::from_file(File::open("./testfiles/empty.xes").unwrap());
		let object0 = read_as_object_with_file_handler(&"XES event log".parse().unwrap(), &mut reader, None, 0, &mut None, "extensible event stream".parse().unwrap()).unwrap();
		let input0 = EbiInput::Object(object0, "extensible event stream".parse().unwrap());
		let input1 = EbiInput::String("any".to_string(), &TEST_INPUT_TYPE_STRING);
		let input2 = EbiInput::String("some string".to_string(), &TEST_INPUT_TYPE_STRING);
		let inputs = vec![input0, input1, input2];

		if let EbiCommand::Command{execute, output_type, ..} = crate::ebi_commands::ebi_command_filter::EBI_FILTER_TRACES_EVENT_ACTIVITY {
			match (execute)(inputs, None) {
				Ok(output) => {
					output.test_activity_key();
					assert_eq!(&output.get_type(), output_type);
				}
				Err(e) => Err(e).unwrap(),
			}
		}
	}
	#[test]
	pub fn ebi_fil_tr_event_act_test_1() {
		// to indicate that this test is expected to fail, add the following to src/tests/fallible_test_list.rs:
		/* 
		(
			&EBI_FILTER_TRACES_EVENT_ACTIVITY,
			&[
				"object XES event log#./testfiles/simple_log_markovian_abstraction.xes",
				"string any",
				"string some string"			
			]
		),
		*/
		
		let mut reader = MultipleReader::from_file(File::open("./testfiles/simple_log_markovian_abstraction.xes").unwrap());
		let object0 = read_as_object_with_file_handler(&"XES event log".parse().unwrap(), &mut reader, None, 0, &mut None, "extensible event stream".parse().unwrap()).unwrap();
		let input0 = EbiInput::Object(object0, "extensible event stream".parse().unwrap());
		let input1 = EbiInput::String("any".to_string(), &TEST_INPUT_TYPE_STRING);
		let input2 = EbiInput::String("some string".to_string(), &TEST_INPUT_TYPE_STRING);
		let inputs = vec![input0, input1, input2];

		if let EbiCommand::Command{execute, output_type, ..} = crate::ebi_commands::ebi_command_filter::EBI_FILTER_TRACES_EVENT_ACTIVITY {
			match (execute)(inputs, None) {
				Ok(output) => {
					output.test_activity_key();
					assert_eq!(&output.get_type(), output_type);
				}
				Err(e) => Err(e).unwrap(),
			}
		}
	}
	#[test]
	pub fn ebi_fil_tr_event_act_test_2() {
		// to indicate that this test is expected to fail, add the following to src/tests/fallible_test_list.rs:
		/* 
		(
			&EBI_FILTER_TRACES_EVENT_ACTIVITY,
			&[
				"object XES event log#./testfiles/a-b-double.xes",
				"string any",
				"string some string"			
			]
		),
		*/
		
		let mut reader = MultipleReader::from_file(File::open("./testfiles/a-b-double.xes").unwrap());
		let object0 = read_as_object_with_file_handler(&"XES event log".parse().unwrap(), &mut reader, None, 0, &mut None, "extensible event stream".parse().unwrap()).unwrap();
		let input0 = EbiInput::Object(object0, "extensible event stream".parse().unwrap());
		let input1 = EbiInput::String("any".to_string(), &TEST_INPUT_TYPE_STRING);
		let input2 = EbiInput::String("some string".to_string(), &TEST_INPUT_TYPE_STRING);
		let inputs = vec![input0, input1, input2];

		if let EbiCommand::Command{execute, output_type, ..} = crate::ebi_commands::ebi_command_filter::EBI_FILTER_TRACES_EVENT_ACTIVITY {
			match (execute)(inputs, None) {
				Ok(output) => {
					output.test_activity_key();
					assert_eq!(&output.get_type(), output_type);
				}
				Err(e) => Err(e).unwrap(),
			}
		}
	}
	#[test]
	pub fn ebi_fil_tr_event_act_test_3() {
		// to indicate that this test is expected to fail, add the following to src/tests/fallible_test_list.rs:
		/* 
		(
			&EBI_FILTER_TRACES_EVENT_ACTIVITY,
			&[
				"object XES event log#./testfiles/a-b.xes.gz",
				"string any",
				"string some string"			
			]
		),
		*/
		
		let mut reader = MultipleReader::from_file(File::open("./testfiles/a-b.xes.gz").unwrap());
		let object0 = read_as_object_with_file_handler(&"XES event log".parse().unwrap(), &mut reader, None, 0, &mut None, "compressed event log".parse().unwrap()).unwrap();
		let input0 = EbiInput::Object(object0, "compressed event log".parse().unwrap());
		let input1 = EbiInput::String("any".to_string(), &TEST_INPUT_TYPE_STRING);
		let input2 = EbiInput::String("some string".to_string(), &TEST_INPUT_TYPE_STRING);
		let inputs = vec![input0, input1, input2];

		if let EbiCommand::Command{execute, output_type, ..} = crate::ebi_commands::ebi_command_filter::EBI_FILTER_TRACES_EVENT_ACTIVITY {
			match (execute)(inputs, None) {
				Ok(output) => {
					output.test_activity_key();
					assert_eq!(&output.get_type(), output_type);
				}
				Err(e) => Err(e).unwrap(),
			}
		}
	}


	// ==== group it ====


	// ==== group docs ====










	// ==== command info ====
	#[test]
	pub fn ebi_info_test_0() {
		// to indicate that this test is expected to fail, add the following to src/tests/fallible_test_list.rs:
		/* 
		(
			&EBI_INFO,
			&[
				"object stochastic process tree#./testfiles/seq(a-xor(b-c)).sptree"			
			]
		),
		*/
		
		let mut reader = MultipleReader::from_file(File::open("./testfiles/seq(a-xor(b-c)).sptree").unwrap());
		let object0 = read_as_object_with_file_handler(&"stochastic process tree".parse().unwrap(), &mut reader, None, 0, &mut None, "stochastic process tree".parse().unwrap()).unwrap();
		let input0 = EbiInput::Object(object0, "stochastic process tree".parse().unwrap());
		let inputs = vec![input0];

		if let EbiCommand::Command{execute, output_type, ..} = crate::ebi_commands::ebi_command_info::EBI_INFO {
			match (execute)(inputs, None) {
				Ok(output) => {
					output.test_activity_key();
					assert_eq!(&output.get_type(), output_type);
				}
				Err(e) => Err(e).unwrap(),
			}
		}
	}
	#[test]
	pub fn ebi_info_test_1() {
		// to indicate that this test is expected to fail, add the following to src/tests/fallible_test_list.rs:
		/* 
		(
			&EBI_INFO,
			&[
				"object finite stochastic language#./testfiles/ba-aa-ab.slang"			
			]
		),
		*/
		
		let mut reader = MultipleReader::from_file(File::open("./testfiles/ba-aa-ab.slang").unwrap());
		let object0 = read_as_object_with_file_handler(&"finite stochastic language".parse().unwrap(), &mut reader, None, 0, &mut None, "finite stochastic language".parse().unwrap()).unwrap();
		let input0 = EbiInput::Object(object0, "finite stochastic language".parse().unwrap());
		let inputs = vec![input0];

		if let EbiCommand::Command{execute, output_type, ..} = crate::ebi_commands::ebi_command_info::EBI_INFO {
			match (execute)(inputs, None) {
				Ok(output) => {
					output.test_activity_key();
					assert_eq!(&output.get_type(), output_type);
				}
				Err(e) => Err(e).unwrap(),
			}
		}
	}
	#[test]
	pub fn ebi_info_test_2() {
		// to indicate that this test is expected to fail, add the following to src/tests/fallible_test_list.rs:
		/* 
		(
			&EBI_INFO,
			&[
				"object business process model and notation#./testfiles/model.bpmn"			
			]
		),
		*/
		
		let mut reader = MultipleReader::from_file(File::open("./testfiles/model.bpmn").unwrap());
		let object0 = read_as_object_with_file_handler(&"business process model and notation".parse().unwrap(), &mut reader, None, 0, &mut None, "business process model and notation".parse().unwrap()).unwrap();
		let input0 = EbiInput::Object(object0, "business process model and notation".parse().unwrap());
		let inputs = vec![input0];

		if let EbiCommand::Command{execute, output_type, ..} = crate::ebi_commands::ebi_command_info::EBI_INFO {
			match (execute)(inputs, None) {
				Ok(output) => {
					output.test_activity_key();
					assert_eq!(&output.get_type(), output_type);
				}
				Err(e) => Err(e).unwrap(),
			}
		}
	}
	#[test]
	pub fn ebi_info_test_3() {
		// to indicate that this test is expected to fail, add the following to src/tests/fallible_test_list.rs:
		/* 
		(
			&EBI_INFO,
			&[
				"object finite stochastic language#./testfiles/a-b.slang"			
			]
		),
		*/
		
		let mut reader = MultipleReader::from_file(File::open("./testfiles/a-b.slang").unwrap());
		let object0 = read_as_object_with_file_handler(&"finite stochastic language".parse().unwrap(), &mut reader, None, 0, &mut None, "finite stochastic language".parse().unwrap()).unwrap();
		let input0 = EbiInput::Object(object0, "finite stochastic language".parse().unwrap());
		let inputs = vec![input0];

		if let EbiCommand::Command{execute, output_type, ..} = crate::ebi_commands::ebi_command_info::EBI_INFO {
			match (execute)(inputs, None) {
				Ok(output) => {
					output.test_activity_key();
					assert_eq!(&output.get_type(), output_type);
				}
				Err(e) => Err(e).unwrap(),
			}
		}
	}


	// ==== group prob ====


	// ==== command log ====
	#[test]
	pub fn ebi_prob_log_test_0() {
		// to indicate that this test is expected to fail, add the following to src/tests/fallible_test_list.rs:
		/* 
		(
			&EBI_PROBABILITY_LOG,
			&[
				"trait queriable stochastic language#./testfiles/seq(a-xor(b-c)).sptree",
				"trait finite language#./testfiles/ba-aa-ab.slang"			
			]
		),
		*/
		
		let mut reader = MultipleReader::from_file(File::open("./testfiles/seq(a-xor(b-c)).sptree").unwrap());
		let (object0, file_handler0) = ebi_input::read_as_trait(&("queriable stochastic language".parse().unwrap()), &mut reader, None, 0).unwrap();
		let input0 = EbiInput::Trait(object0, file_handler0);
		let mut reader = MultipleReader::from_file(File::open("./testfiles/ba-aa-ab.slang").unwrap());
		let (object1, file_handler1) = ebi_input::read_as_trait(&("finite language".parse().unwrap()), &mut reader, None, 0).unwrap();
		let input1 = EbiInput::Trait(object1, file_handler1);
		let inputs = vec![input0, input1];

		if let EbiCommand::Command{execute, output_type, ..} = crate::ebi_commands::ebi_command_probability::EBI_PROBABILITY_LOG {
			match (execute)(inputs, None) {
				Ok(output) => {
					output.test_activity_key();
					assert_eq!(&output.get_type(), output_type);
				}
				Err(e) => Err(e).unwrap(),
			}
		}
	}
	#[test]
	pub fn ebi_prob_log_test_1() {
		// to indicate that this test is expected to fail, add the following to src/tests/fallible_test_list.rs:
		/* 
		(
			&EBI_PROBABILITY_LOG,
			&[
				"trait queriable stochastic language#./testfiles/seq(a-xor(b-c)).sptree",
				"trait finite language#./testfiles/a-b.slang"			
			]
		),
		*/
		
		let mut reader = MultipleReader::from_file(File::open("./testfiles/seq(a-xor(b-c)).sptree").unwrap());
		let (object0, file_handler0) = ebi_input::read_as_trait(&("queriable stochastic language".parse().unwrap()), &mut reader, None, 0).unwrap();
		let input0 = EbiInput::Trait(object0, file_handler0);
		let mut reader = MultipleReader::from_file(File::open("./testfiles/a-b.slang").unwrap());
		let (object1, file_handler1) = ebi_input::read_as_trait(&("finite language".parse().unwrap()), &mut reader, None, 0).unwrap();
		let input1 = EbiInput::Trait(object1, file_handler1);
		let inputs = vec![input0, input1];

		if let EbiCommand::Command{execute, output_type, ..} = crate::ebi_commands::ebi_command_probability::EBI_PROBABILITY_LOG {
			match (execute)(inputs, None) {
				Ok(output) => {
					output.test_activity_key();
					assert_eq!(&output.get_type(), output_type);
				}
				Err(e) => Err(e).unwrap(),
			}
		}
	}
	#[test]
	pub fn ebi_prob_log_test_2() {
		// to indicate that this test is expected to fail, add the following to src/tests/fallible_test_list.rs:
		/* 
		(
			&EBI_PROBABILITY_LOG,
			&[
				"trait queriable stochastic language#./testfiles/seq(a-xor(b-c)).sptree",
				"trait finite language#./testfiles/empty.xes"			
			]
		),
		*/
		
		let mut reader = MultipleReader::from_file(File::open("./testfiles/seq(a-xor(b-c)).sptree").unwrap());
		let (object0, file_handler0) = ebi_input::read_as_trait(&("queriable stochastic language".parse().unwrap()), &mut reader, None, 0).unwrap();
		let input0 = EbiInput::Trait(object0, file_handler0);
		let mut reader = MultipleReader::from_file(File::open("./testfiles/empty.xes").unwrap());
		let (object1, file_handler1) = ebi_input::read_as_trait(&("finite language".parse().unwrap()), &mut reader, None, 0).unwrap();
		let input1 = EbiInput::Trait(object1, file_handler1);
		let inputs = vec![input0, input1];

		if let EbiCommand::Command{execute, output_type, ..} = crate::ebi_commands::ebi_command_probability::EBI_PROBABILITY_LOG {
			match (execute)(inputs, None) {
				Ok(output) => {
					output.test_activity_key();
					assert_eq!(&output.get_type(), output_type);
				}
				Err(e) => Err(e).unwrap(),
			}
		}
	}
	#[test]
	pub fn ebi_prob_log_test_3() {
		// to indicate that this test is expected to fail, add the following to src/tests/fallible_test_list.rs:
		/* 
		(
			&EBI_PROBABILITY_LOG,
			&[
				"trait queriable stochastic language#./testfiles/seq(a-xor(b-c)).sptree",
				"trait finite language#./testfiles/acb-abc-ad-aded-adeded-adededed.slang"			
			]
		),
		*/
		
		let mut reader = MultipleReader::from_file(File::open("./testfiles/seq(a-xor(b-c)).sptree").unwrap());
		let (object0, file_handler0) = ebi_input::read_as_trait(&("queriable stochastic language".parse().unwrap()), &mut reader, None, 0).unwrap();
		let input0 = EbiInput::Trait(object0, file_handler0);
		let mut reader = MultipleReader::from_file(File::open("./testfiles/acb-abc-ad-aded-adeded-adededed.slang").unwrap());
		let (object1, file_handler1) = ebi_input::read_as_trait(&("finite language".parse().unwrap()), &mut reader, None, 0).unwrap();
		let input1 = EbiInput::Trait(object1, file_handler1);
		let inputs = vec![input0, input1];

		if let EbiCommand::Command{execute, output_type, ..} = crate::ebi_commands::ebi_command_probability::EBI_PROBABILITY_LOG {
			match (execute)(inputs, None) {
				Ok(output) => {
					output.test_activity_key();
					assert_eq!(&output.get_type(), output_type);
				}
				Err(e) => Err(e).unwrap(),
			}
		}
	}




	// ==== group sam ====


	// ==== command folds ====
	#[test]
	pub fn ebi_sam_folds_test_0() {
		// to indicate that this test is expected to fail, add the following to src/tests/fallible_test_list.rs:
		/* 
		(
			&EBI_SAMPLE_FOLDS,
			&[
				"object event log#./testfiles/empty.xes",
				"usize 1",
				"usize 0",
				"usize 0"			
			]
		),
		*/
		
		let mut reader = MultipleReader::from_file(File::open("./testfiles/empty.xes").unwrap());
		let object0 = read_as_object_with_file_handler(&"event log".parse().unwrap(), &mut reader, None, 0, &mut None, "extensible event stream".parse().unwrap()).unwrap();
		let input0 = EbiInput::Object(object0, "extensible event stream".parse().unwrap());
		let input1 = EbiInput::Usize(1, &TEST_INPUT_TYPE_USIZE);
		let input2 = EbiInput::Usize(0, &TEST_INPUT_TYPE_USIZE);
		let input3 = EbiInput::Usize(0, &TEST_INPUT_TYPE_USIZE);
		let inputs = vec![input0, input1, input2, input3];

		if let EbiCommand::Command{execute, output_type, ..} = crate::ebi_commands::ebi_command_sample::EBI_SAMPLE_FOLDS {
			match (execute)(inputs, None) {
				Ok(output) => {
					output.test_activity_key();
					assert_eq!(&output.get_type(), output_type);
				}
				Err(e) => Err(e).unwrap(),
			}
		}
	}
	#[test]
	pub fn ebi_sam_folds_test_1() {
		// to indicate that this test is expected to fail, add the following to src/tests/fallible_test_list.rs:
		/* 
		(
			&EBI_SAMPLE_FOLDS,
			&[
				"object event log#./testfiles/simple_log_markovian_abstraction.xes",
				"usize 1",
				"usize 0",
				"usize 0"			
			]
		),
		*/
		
		let mut reader = MultipleReader::from_file(File::open("./testfiles/simple_log_markovian_abstraction.xes").unwrap());
		let object0 = read_as_object_with_file_handler(&"event log".parse().unwrap(), &mut reader, None, 0, &mut None, "extensible event stream".parse().unwrap()).unwrap();
		let input0 = EbiInput::Object(object0, "extensible event stream".parse().unwrap());
		let input1 = EbiInput::Usize(1, &TEST_INPUT_TYPE_USIZE);
		let input2 = EbiInput::Usize(0, &TEST_INPUT_TYPE_USIZE);
		let input3 = EbiInput::Usize(0, &TEST_INPUT_TYPE_USIZE);
		let inputs = vec![input0, input1, input2, input3];

		if let EbiCommand::Command{execute, output_type, ..} = crate::ebi_commands::ebi_command_sample::EBI_SAMPLE_FOLDS {
			match (execute)(inputs, None) {
				Ok(output) => {
					output.test_activity_key();
					assert_eq!(&output.get_type(), output_type);
				}
				Err(e) => Err(e).unwrap(),
			}
		}
	}
	#[test]
	pub fn ebi_sam_folds_test_2() {
		// to indicate that this test is expected to fail, add the following to src/tests/fallible_test_list.rs:
		/* 
		(
			&EBI_SAMPLE_FOLDS,
			&[
				"object event log#./testfiles/a-b-double.xes",
				"usize 1",
				"usize 0",
				"usize 0"			
			]
		),
		*/
		
		let mut reader = MultipleReader::from_file(File::open("./testfiles/a-b-double.xes").unwrap());
		let object0 = read_as_object_with_file_handler(&"event log".parse().unwrap(), &mut reader, None, 0, &mut None, "extensible event stream".parse().unwrap()).unwrap();
		let input0 = EbiInput::Object(object0, "extensible event stream".parse().unwrap());
		let input1 = EbiInput::Usize(1, &TEST_INPUT_TYPE_USIZE);
		let input2 = EbiInput::Usize(0, &TEST_INPUT_TYPE_USIZE);
		let input3 = EbiInput::Usize(0, &TEST_INPUT_TYPE_USIZE);
		let inputs = vec![input0, input1, input2, input3];

		if let EbiCommand::Command{execute, output_type, ..} = crate::ebi_commands::ebi_command_sample::EBI_SAMPLE_FOLDS {
			match (execute)(inputs, None) {
				Ok(output) => {
					output.test_activity_key();
					assert_eq!(&output.get_type(), output_type);
				}
				Err(e) => Err(e).unwrap(),
			}
		}
	}
	#[test]
	pub fn ebi_sam_folds_test_3() {
		// to indicate that this test is expected to fail, add the following to src/tests/fallible_test_list.rs:
		/* 
		(
			&EBI_SAMPLE_FOLDS,
			&[
				"object event log#./testfiles/a-b.xes.gz",
				"usize 1",
				"usize 0",
				"usize 0"			
			]
		),
		*/
		
		let mut reader = MultipleReader::from_file(File::open("./testfiles/a-b.xes.gz").unwrap());
		let object0 = read_as_object_with_file_handler(&"event log".parse().unwrap(), &mut reader, None, 0, &mut None, "compressed event log".parse().unwrap()).unwrap();
		let input0 = EbiInput::Object(object0, "compressed event log".parse().unwrap());
		let input1 = EbiInput::Usize(1, &TEST_INPUT_TYPE_USIZE);
		let input2 = EbiInput::Usize(0, &TEST_INPUT_TYPE_USIZE);
		let input3 = EbiInput::Usize(0, &TEST_INPUT_TYPE_USIZE);
		let inputs = vec![input0, input1, input2, input3];

		if let EbiCommand::Command{execute, output_type, ..} = crate::ebi_commands::ebi_command_sample::EBI_SAMPLE_FOLDS {
			match (execute)(inputs, None) {
				Ok(output) => {
					output.test_activity_key();
					assert_eq!(&output.get_type(), output_type);
				}
				Err(e) => Err(e).unwrap(),
			}
		}
	}


	// ==== command potr ====
	#[test]
	pub fn ebi_sam_potr_test_0() {
		// to indicate that this test is expected to fail, add the following to src/tests/fallible_test_list.rs:
		/* 
		(
			&EBI_SAMPLE_PARTIALLY_ORDERED_TRACES,
			&[
				"object stochastic business process model and notation#./testfiles/model.sbpmn",
				"usize 1"			
			]
		),
		*/
		
		let mut reader = MultipleReader::from_file(File::open("./testfiles/model.sbpmn").unwrap());
		let object0 = read_as_object_with_file_handler(&"stochastic business process model and notation".parse().unwrap(), &mut reader, None, 0, &mut None, "stochastic business process model and notation".parse().unwrap()).unwrap();
		let input0 = EbiInput::Object(object0, "stochastic business process model and notation".parse().unwrap());
		let input1 = EbiInput::Usize(1, &TEST_INPUT_TYPE_USIZE);
		let inputs = vec![input0, input1];

		if let EbiCommand::Command{execute, output_type, ..} = crate::ebi_commands::ebi_command_sample::EBI_SAMPLE_PARTIALLY_ORDERED_TRACES {
			match (execute)(inputs, None) {
				Ok(output) => {
					output.test_activity_key();
					assert_eq!(&output.get_type(), output_type);
				}
				Err(e) => Err(e).unwrap(),
			}
		}
	}
	#[test]
	pub fn ebi_sam_potr_test_1() {
		// to indicate that this test is expected to fail, add the following to src/tests/fallible_test_list.rs:
		/* 
		(
			&EBI_SAMPLE_PARTIALLY_ORDERED_TRACES,
			&[
				"object stochastic business process model and notation#./testfiles/flower.sbpmn",
				"usize 1"			
			]
		),
		*/
		
		let mut reader = MultipleReader::from_file(File::open("./testfiles/flower.sbpmn").unwrap());
		let object0 = read_as_object_with_file_handler(&"stochastic business process model and notation".parse().unwrap(), &mut reader, None, 0, &mut None, "stochastic business process model and notation".parse().unwrap()).unwrap();
		let input0 = EbiInput::Object(object0, "stochastic business process model and notation".parse().unwrap());
		let input1 = EbiInput::Usize(1, &TEST_INPUT_TYPE_USIZE);
		let inputs = vec![input0, input1];

		if let EbiCommand::Command{execute, output_type, ..} = crate::ebi_commands::ebi_command_sample::EBI_SAMPLE_PARTIALLY_ORDERED_TRACES {
			match (execute)(inputs, None) {
				Ok(output) => {
					output.test_activity_key();
					assert_eq!(&output.get_type(), output_type);
				}
				Err(e) => Err(e).unwrap(),
			}
		}
	}


	// ==== command tra ====
	#[test]
	pub fn ebi_sam_tra_test_0() {
		// to indicate that this test is expected to fail, add the following to src/tests/fallible_test_list.rs:
		/* 
		(
			&EBI_SAMPLE_TRACES,
			&[
				"trait finite stochastic language#./testfiles/ba-aa-ab.slang",
				"usize 1"			
			]
		),
		*/
		
		let mut reader = MultipleReader::from_file(File::open("./testfiles/ba-aa-ab.slang").unwrap());
		let (object0, file_handler0) = ebi_input::read_as_trait(&("finite stochastic language".parse().unwrap()), &mut reader, None, 0).unwrap();
		let input0 = EbiInput::Trait(object0, file_handler0);
		let input1 = EbiInput::Usize(1, &TEST_INPUT_TYPE_USIZE);
		let inputs = vec![input0, input1];

		if let EbiCommand::Command{execute, output_type, ..} = crate::ebi_commands::ebi_command_sample::EBI_SAMPLE_TRACES {
			match (execute)(inputs, None) {
				Ok(output) => {
					output.test_activity_key();
					assert_eq!(&output.get_type(), output_type);
				}
				Err(e) => Err(e).unwrap(),
			}
		}
	}
	#[test]
	pub fn ebi_sam_tra_test_1() {
		// to indicate that this test is expected to fail, add the following to src/tests/fallible_test_list.rs:
		/* 
		(
			&EBI_SAMPLE_TRACES,
			&[
				"trait finite stochastic language#./testfiles/a-b.slang",
				"usize 1"			
			]
		),
		*/
		
		let mut reader = MultipleReader::from_file(File::open("./testfiles/a-b.slang").unwrap());
		let (object0, file_handler0) = ebi_input::read_as_trait(&("finite stochastic language".parse().unwrap()), &mut reader, None, 0).unwrap();
		let input0 = EbiInput::Trait(object0, file_handler0);
		let input1 = EbiInput::Usize(1, &TEST_INPUT_TYPE_USIZE);
		let inputs = vec![input0, input1];

		if let EbiCommand::Command{execute, output_type, ..} = crate::ebi_commands::ebi_command_sample::EBI_SAMPLE_TRACES {
			match (execute)(inputs, None) {
				Ok(output) => {
					output.test_activity_key();
					assert_eq!(&output.get_type(), output_type);
				}
				Err(e) => Err(e).unwrap(),
			}
		}
	}
	#[test]
	pub fn ebi_sam_tra_test_2() {
		// this test has been indicated as to be expected to fail
		
		let mut reader = MultipleReader::from_file(File::open("./testfiles/empty.xes").unwrap());
		let (object0, file_handler0) = ebi_input::read_as_trait(&("finite stochastic language".parse().unwrap()), &mut reader, None, 0).unwrap();
		let input0 = EbiInput::Trait(object0, file_handler0);
		let input1 = EbiInput::Usize(1, &TEST_INPUT_TYPE_USIZE);
		let inputs = vec![input0, input1];

		if let EbiCommand::Command{execute, ..} = crate::ebi_commands::ebi_command_sample::EBI_SAMPLE_TRACES {
			assert!(((execute)(inputs, None)).is_err())
		}
	}
	#[test]
	pub fn ebi_sam_tra_test_3() {
		// to indicate that this test is expected to fail, add the following to src/tests/fallible_test_list.rs:
		/* 
		(
			&EBI_SAMPLE_TRACES,
			&[
				"trait finite stochastic language#./testfiles/acb-abc-ad-aded-adeded-adededed.slang",
				"usize 1"			
			]
		),
		*/
		
		let mut reader = MultipleReader::from_file(File::open("./testfiles/acb-abc-ad-aded-adeded-adededed.slang").unwrap());
		let (object0, file_handler0) = ebi_input::read_as_trait(&("finite stochastic language".parse().unwrap()), &mut reader, None, 0).unwrap();
		let input0 = EbiInput::Trait(object0, file_handler0);
		let input1 = EbiInput::Usize(1, &TEST_INPUT_TYPE_USIZE);
		let inputs = vec![input0, input1];

		if let EbiCommand::Command{execute, output_type, ..} = crate::ebi_commands::ebi_command_sample::EBI_SAMPLE_TRACES {
			match (execute)(inputs, None) {
				Ok(output) => {
					output.test_activity_key();
					assert_eq!(&output.get_type(), output_type);
				}
				Err(e) => Err(e).unwrap(),
			}
		}
	}


	// ==== group tst ====


	// ==== command btst ====
	#[test]
	pub fn ebi_tst_btst_test_0() {
		// to indicate that this test is expected to fail, add the following to src/tests/fallible_test_list.rs:
		/* 
		(
			&EBI_TEST_BOOTSTRAP,
			&[
				"trait finite stochastic language#./testfiles/ba-aa-ab.slang",
				"trait finite stochastic language#./testfiles/ba-aa-ab.slang",
				"usize 10",
				"fraction 0.05"			
			]
		),
		*/
		
		let mut reader = MultipleReader::from_file(File::open("./testfiles/ba-aa-ab.slang").unwrap());
		let (object0, file_handler0) = ebi_input::read_as_trait(&("finite stochastic language".parse().unwrap()), &mut reader, None, 0).unwrap();
		let input0 = EbiInput::Trait(object0, file_handler0);
		let mut reader = MultipleReader::from_file(File::open("./testfiles/ba-aa-ab.slang").unwrap());
		let (object1, file_handler1) = ebi_input::read_as_trait(&("finite stochastic language".parse().unwrap()), &mut reader, None, 0).unwrap();
		let input1 = EbiInput::Trait(object1, file_handler1);
		let input2 = EbiInput::Usize(10, &TEST_INPUT_TYPE_USIZE);
		let input3 = EbiInput::Fraction("0.05".parse().unwrap(), &TEST_INPUT_TYPE_FRACTION);
		let inputs = vec![input0, input1, input2, input3];

		if let EbiCommand::Command{execute, output_type, ..} = crate::ebi_commands::ebi_command_test::EBI_TEST_BOOTSTRAP {
			match (execute)(inputs, None) {
				Ok(output) => {
					output.test_activity_key();
					assert_eq!(&output.get_type(), output_type);
				}
				Err(e) => Err(e).unwrap(),
			}
		}
	}
	#[test]
	pub fn ebi_tst_btst_test_1() {
		// to indicate that this test is expected to fail, add the following to src/tests/fallible_test_list.rs:
		/* 
		(
			&EBI_TEST_BOOTSTRAP,
			&[
				"trait finite stochastic language#./testfiles/ba-aa-ab.slang",
				"trait finite stochastic language#./testfiles/a-b.slang",
				"usize 10",
				"fraction 0.05"			
			]
		),
		*/
		
		let mut reader = MultipleReader::from_file(File::open("./testfiles/ba-aa-ab.slang").unwrap());
		let (object0, file_handler0) = ebi_input::read_as_trait(&("finite stochastic language".parse().unwrap()), &mut reader, None, 0).unwrap();
		let input0 = EbiInput::Trait(object0, file_handler0);
		let mut reader = MultipleReader::from_file(File::open("./testfiles/a-b.slang").unwrap());
		let (object1, file_handler1) = ebi_input::read_as_trait(&("finite stochastic language".parse().unwrap()), &mut reader, None, 0).unwrap();
		let input1 = EbiInput::Trait(object1, file_handler1);
		let input2 = EbiInput::Usize(10, &TEST_INPUT_TYPE_USIZE);
		let input3 = EbiInput::Fraction("0.05".parse().unwrap(), &TEST_INPUT_TYPE_FRACTION);
		let inputs = vec![input0, input1, input2, input3];

		if let EbiCommand::Command{execute, output_type, ..} = crate::ebi_commands::ebi_command_test::EBI_TEST_BOOTSTRAP {
			match (execute)(inputs, None) {
				Ok(output) => {
					output.test_activity_key();
					assert_eq!(&output.get_type(), output_type);
				}
				Err(e) => Err(e).unwrap(),
			}
		}
	}
	#[test]
	pub fn ebi_tst_btst_test_2() {
		// this test has been indicated as to be expected to fail
		
		let mut reader = MultipleReader::from_file(File::open("./testfiles/ba-aa-ab.slang").unwrap());
		let (object0, file_handler0) = ebi_input::read_as_trait(&("finite stochastic language".parse().unwrap()), &mut reader, None, 0).unwrap();
		let input0 = EbiInput::Trait(object0, file_handler0);
		let mut reader = MultipleReader::from_file(File::open("./testfiles/empty.xes").unwrap());
		let (object1, file_handler1) = ebi_input::read_as_trait(&("finite stochastic language".parse().unwrap()), &mut reader, None, 0).unwrap();
		let input1 = EbiInput::Trait(object1, file_handler1);
		let input2 = EbiInput::Usize(10, &TEST_INPUT_TYPE_USIZE);
		let input3 = EbiInput::Fraction("0.05".parse().unwrap(), &TEST_INPUT_TYPE_FRACTION);
		let inputs = vec![input0, input1, input2, input3];

		if let EbiCommand::Command{execute, ..} = crate::ebi_commands::ebi_command_test::EBI_TEST_BOOTSTRAP {
			assert!(((execute)(inputs, None)).is_err())
		}
	}
	#[test]
	pub fn ebi_tst_btst_test_3() {
		// to indicate that this test is expected to fail, add the following to src/tests/fallible_test_list.rs:
		/* 
		(
			&EBI_TEST_BOOTSTRAP,
			&[
				"trait finite stochastic language#./testfiles/ba-aa-ab.slang",
				"trait finite stochastic language#./testfiles/acb-abc-ad-aded-adeded-adededed.slang",
				"usize 10",
				"fraction 0.05"			
			]
		),
		*/
		
		let mut reader = MultipleReader::from_file(File::open("./testfiles/ba-aa-ab.slang").unwrap());
		let (object0, file_handler0) = ebi_input::read_as_trait(&("finite stochastic language".parse().unwrap()), &mut reader, None, 0).unwrap();
		let input0 = EbiInput::Trait(object0, file_handler0);
		let mut reader = MultipleReader::from_file(File::open("./testfiles/acb-abc-ad-aded-adeded-adededed.slang").unwrap());
		let (object1, file_handler1) = ebi_input::read_as_trait(&("finite stochastic language".parse().unwrap()), &mut reader, None, 0).unwrap();
		let input1 = EbiInput::Trait(object1, file_handler1);
		let input2 = EbiInput::Usize(10, &TEST_INPUT_TYPE_USIZE);
		let input3 = EbiInput::Fraction("0.05".parse().unwrap(), &TEST_INPUT_TYPE_FRACTION);
		let inputs = vec![input0, input1, input2, input3];

		if let EbiCommand::Command{execute, output_type, ..} = crate::ebi_commands::ebi_command_test::EBI_TEST_BOOTSTRAP {
			match (execute)(inputs, None) {
				Ok(output) => {
					output.test_activity_key();
					assert_eq!(&output.get_type(), output_type);
				}
				Err(e) => Err(e).unwrap(),
			}
		}
	}


	// ==== command lcat ====
	#[test]
	pub fn ebi_tst_lcat_test_0() {
		// this test has been indicated as to be expected to fail
		
		let mut reader = MultipleReader::from_file(File::open("./testfiles/empty.xes").unwrap());
		let (object0, file_handler0) = ebi_input::read_as_trait(&("event log with trace attributes".parse().unwrap()), &mut reader, None, 0).unwrap();
		let input0 = EbiInput::Trait(object0, file_handler0);
		let input1 = EbiInput::String("some string".to_string(), &TEST_INPUT_TYPE_STRING);
		let input2 = EbiInput::Usize(10, &TEST_INPUT_TYPE_USIZE);
		let input3 = EbiInput::Fraction("0.05".parse().unwrap(), &TEST_INPUT_TYPE_FRACTION);
		let inputs = vec![input0, input1, input2, input3];

		if let EbiCommand::Command{execute, ..} = crate::ebi_commands::ebi_command_test::EBI_TEST_LOG_ATTRIBUTE {
			assert!(((execute)(inputs, None)).is_err())
		}
	}
	#[test]
	pub fn ebi_tst_lcat_test_1() {
		// this test has been indicated as to be expected to fail
		
		let mut reader = MultipleReader::from_file(File::open("./testfiles/simple_log_markovian_abstraction.xes").unwrap());
		let (object0, file_handler0) = ebi_input::read_as_trait(&("event log with trace attributes".parse().unwrap()), &mut reader, None, 0).unwrap();
		let input0 = EbiInput::Trait(object0, file_handler0);
		let input1 = EbiInput::String("some string".to_string(), &TEST_INPUT_TYPE_STRING);
		let input2 = EbiInput::Usize(10, &TEST_INPUT_TYPE_USIZE);
		let input3 = EbiInput::Fraction("0.05".parse().unwrap(), &TEST_INPUT_TYPE_FRACTION);
		let inputs = vec![input0, input1, input2, input3];

		if let EbiCommand::Command{execute, ..} = crate::ebi_commands::ebi_command_test::EBI_TEST_LOG_ATTRIBUTE {
			assert!(((execute)(inputs, None)).is_err())
		}
	}
	#[test]
	pub fn ebi_tst_lcat_test_2() {
		// this test has been indicated as to be expected to fail
		
		let mut reader = MultipleReader::from_file(File::open("./testfiles/a-b-double.xes").unwrap());
		let (object0, file_handler0) = ebi_input::read_as_trait(&("event log with trace attributes".parse().unwrap()), &mut reader, None, 0).unwrap();
		let input0 = EbiInput::Trait(object0, file_handler0);
		let input1 = EbiInput::String("some string".to_string(), &TEST_INPUT_TYPE_STRING);
		let input2 = EbiInput::Usize(10, &TEST_INPUT_TYPE_USIZE);
		let input3 = EbiInput::Fraction("0.05".parse().unwrap(), &TEST_INPUT_TYPE_FRACTION);
		let inputs = vec![input0, input1, input2, input3];

		if let EbiCommand::Command{execute, ..} = crate::ebi_commands::ebi_command_test::EBI_TEST_LOG_ATTRIBUTE {
			assert!(((execute)(inputs, None)).is_err())
		}
	}
	#[test]
	pub fn ebi_tst_lcat_test_3() {
		// this test has been indicated as to be expected to fail
		
		let mut reader = MultipleReader::from_file(File::open("./testfiles/a-b.xes.gz").unwrap());
		let (object0, file_handler0) = ebi_input::read_as_trait(&("event log with trace attributes".parse().unwrap()), &mut reader, None, 0).unwrap();
		let input0 = EbiInput::Trait(object0, file_handler0);
		let input1 = EbiInput::String("some string".to_string(), &TEST_INPUT_TYPE_STRING);
		let input2 = EbiInput::Usize(10, &TEST_INPUT_TYPE_USIZE);
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
		// to indicate that this test is expected to fail, add the following to src/tests/fallible_test_list.rs:
		/* 
		(
			&EBI_VISUALISE_GRAPH,
			&[
				"trait graphable#./testfiles/seq(a-xor(b-c)).sptree"			
			]
		),
		*/
		
		let mut reader = MultipleReader::from_file(File::open("./testfiles/seq(a-xor(b-c)).sptree").unwrap());
		let (object0, file_handler0) = ebi_input::read_as_trait(&("graphable".parse().unwrap()), &mut reader, None, 0).unwrap();
		let input0 = EbiInput::Trait(object0, file_handler0);
		let inputs = vec![input0];

		if let EbiCommand::Command{execute, output_type, ..} = crate::ebi_commands::ebi_command_visualise::EBI_VISUALISE_GRAPH {
			match (execute)(inputs, None) {
				Ok(output) => {
					output.test_activity_key();
					assert_eq!(&output.get_type(), output_type);
				}
				Err(e) => Err(e).unwrap(),
			}
		}
	}
	#[test]
	pub fn ebi_vis_graph_test_1() {
		// to indicate that this test is expected to fail, add the following to src/tests/fallible_test_list.rs:
		/* 
		(
			&EBI_VISUALISE_GRAPH,
			&[
				"trait graphable#./testfiles/model.bpmn"			
			]
		),
		*/
		
		let mut reader = MultipleReader::from_file(File::open("./testfiles/model.bpmn").unwrap());
		let (object0, file_handler0) = ebi_input::read_as_trait(&("graphable".parse().unwrap()), &mut reader, None, 0).unwrap();
		let input0 = EbiInput::Trait(object0, file_handler0);
		let inputs = vec![input0];

		if let EbiCommand::Command{execute, output_type, ..} = crate::ebi_commands::ebi_command_visualise::EBI_VISUALISE_GRAPH {
			match (execute)(inputs, None) {
				Ok(output) => {
					output.test_activity_key();
					assert_eq!(&output.get_type(), output_type);
				}
				Err(e) => Err(e).unwrap(),
			}
		}
	}
	#[test]
	pub fn ebi_vis_graph_test_2() {
		// to indicate that this test is expected to fail, add the following to src/tests/fallible_test_list.rs:
		/* 
		(
			&EBI_VISUALISE_GRAPH,
			&[
				"trait graphable#./testfiles/a-b_star.dfm"			
			]
		),
		*/
		
		let mut reader = MultipleReader::from_file(File::open("./testfiles/a-b_star.dfm").unwrap());
		let (object0, file_handler0) = ebi_input::read_as_trait(&("graphable".parse().unwrap()), &mut reader, None, 0).unwrap();
		let input0 = EbiInput::Trait(object0, file_handler0);
		let inputs = vec![input0];

		if let EbiCommand::Command{execute, output_type, ..} = crate::ebi_commands::ebi_command_visualise::EBI_VISUALISE_GRAPH {
			match (execute)(inputs, None) {
				Ok(output) => {
					output.test_activity_key();
					assert_eq!(&output.get_type(), output_type);
				}
				Err(e) => Err(e).unwrap(),
			}
		}
	}
	#[test]
	pub fn ebi_vis_graph_test_3() {
		// to indicate that this test is expected to fail, add the following to src/tests/fallible_test_list.rs:
		/* 
		(
			&EBI_VISUALISE_GRAPH,
			&[
				"trait graphable#./testfiles/bpic12-a.xes.gz-dfg.dfg"			
			]
		),
		*/
		
		let mut reader = MultipleReader::from_file(File::open("./testfiles/bpic12-a.xes.gz-dfg.dfg").unwrap());
		let (object0, file_handler0) = ebi_input::read_as_trait(&("graphable".parse().unwrap()), &mut reader, None, 0).unwrap();
		let input0 = EbiInput::Trait(object0, file_handler0);
		let inputs = vec![input0];

		if let EbiCommand::Command{execute, output_type, ..} = crate::ebi_commands::ebi_command_visualise::EBI_VISUALISE_GRAPH {
			match (execute)(inputs, None) {
				Ok(output) => {
					output.test_activity_key();
					assert_eq!(&output.get_type(), output_type);
				}
				Err(e) => Err(e).unwrap(),
			}
		}
	}


	// ==== command txt ====
	#[test]
	pub fn ebi_vis_txt_test_0() {
		// to indicate that this test is expected to fail, add the following to src/tests/fallible_test_list.rs:
		/* 
		(
			&EBI_VISUALISE_TEXT,
			&[
				"object stochastic process tree#./testfiles/seq(a-xor(b-c)).sptree"			
			]
		),
		*/
		if ebi_objects::ebi_arithmetic::is_exact_globally() {
			return;
		}
		let mut reader = MultipleReader::from_file(File::open("./testfiles/seq(a-xor(b-c)).sptree").unwrap());
		let object0 = read_as_object_with_file_handler(&"stochastic process tree".parse().unwrap(), &mut reader, None, 0, &mut None, "stochastic process tree".parse().unwrap()).unwrap();
		let input0 = EbiInput::Object(object0, "stochastic process tree".parse().unwrap());
		let inputs = vec![input0];

		if let EbiCommand::Command{execute, output_type, ..} = crate::ebi_commands::ebi_command_visualise::EBI_VISUALISE_TEXT {
			match (execute)(inputs, None) {
				Ok(output) => {
					output.test_activity_key();
					assert_eq!(&output.get_type(), output_type);
				}
				Err(e) => Err(e).unwrap(),
			}
		}
	}
	#[test]
	pub fn ebi_vis_txt_test_1() {
		// to indicate that this test is expected to fail, add the following to src/tests/fallible_test_list.rs:
		/* 
		(
			&EBI_VISUALISE_TEXT,
			&[
				"object finite stochastic language#./testfiles/ba-aa-ab.slang"			
			]
		),
		*/
		if ebi_objects::ebi_arithmetic::is_exact_globally() {
			return;
		}
		let mut reader = MultipleReader::from_file(File::open("./testfiles/ba-aa-ab.slang").unwrap());
		let object0 = read_as_object_with_file_handler(&"finite stochastic language".parse().unwrap(), &mut reader, None, 0, &mut None, "finite stochastic language".parse().unwrap()).unwrap();
		let input0 = EbiInput::Object(object0, "finite stochastic language".parse().unwrap());
		let inputs = vec![input0];

		if let EbiCommand::Command{execute, output_type, ..} = crate::ebi_commands::ebi_command_visualise::EBI_VISUALISE_TEXT {
			match (execute)(inputs, None) {
				Ok(output) => {
					output.test_activity_key();
					assert_eq!(&output.get_type(), output_type);
				}
				Err(e) => Err(e).unwrap(),
			}
		}
	}
	#[test]
	pub fn ebi_vis_txt_test_2() {
		// to indicate that this test is expected to fail, add the following to src/tests/fallible_test_list.rs:
		/* 
		(
			&EBI_VISUALISE_TEXT,
			&[
				"object business process model and notation#./testfiles/model.bpmn"			
			]
		),
		*/
		if ebi_objects::ebi_arithmetic::is_exact_globally() {
			return;
		}
		let mut reader = MultipleReader::from_file(File::open("./testfiles/model.bpmn").unwrap());
		let object0 = read_as_object_with_file_handler(&"business process model and notation".parse().unwrap(), &mut reader, None, 0, &mut None, "business process model and notation".parse().unwrap()).unwrap();
		let input0 = EbiInput::Object(object0, "business process model and notation".parse().unwrap());
		let inputs = vec![input0];

		if let EbiCommand::Command{execute, output_type, ..} = crate::ebi_commands::ebi_command_visualise::EBI_VISUALISE_TEXT {
			match (execute)(inputs, None) {
				Ok(output) => {
					output.test_activity_key();
					assert_eq!(&output.get_type(), output_type);
				}
				Err(e) => Err(e).unwrap(),
			}
		}
	}
	#[test]
	pub fn ebi_vis_txt_test_3() {
		// to indicate that this test is expected to fail, add the following to src/tests/fallible_test_list.rs:
		/* 
		(
			&EBI_VISUALISE_TEXT,
			&[
				"object finite stochastic language#./testfiles/a-b.slang"			
			]
		),
		*/
		if ebi_objects::ebi_arithmetic::is_exact_globally() {
			return;
		}
		let mut reader = MultipleReader::from_file(File::open("./testfiles/a-b.slang").unwrap());
		let object0 = read_as_object_with_file_handler(&"finite stochastic language".parse().unwrap(), &mut reader, None, 0, &mut None, "finite stochastic language".parse().unwrap()).unwrap();
		let input0 = EbiInput::Object(object0, "finite stochastic language".parse().unwrap());
		let inputs = vec![input0];

		if let EbiCommand::Command{execute, output_type, ..} = crate::ebi_commands::ebi_command_visualise::EBI_VISUALISE_TEXT {
			match (execute)(inputs, None) {
				Ok(output) => {
					output.test_activity_key();
					assert_eq!(&output.get_type(), output_type);
				}
				Err(e) => Err(e).unwrap(),
			}
		}
	}
}
