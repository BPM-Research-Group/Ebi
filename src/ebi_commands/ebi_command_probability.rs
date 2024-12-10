use anyhow::{anyhow, Context};
use clap::{value_parser, Arg, ArgAction};
use std::error::Error;
use csv::Writer;
use peak_alloc::PeakAlloc;

use crate::{ebi_framework::{ebi_command::EbiCommand, ebi_input::EbiInputType, ebi_object::{EbiObject, EbiObjectType}, ebi_output::{EbiOutput, EbiOutputType}, ebi_trait::EbiTrait}, ebi_traits::{ebi_trait_finite_language::EbiTraitFiniteLanguage, ebi_trait_finite_stochastic_language::EbiTraitFiniteStochasticLanguage, ebi_trait_queriable_stochastic_language::EbiTraitQueriableStochasticLanguage, ebi_trait_stochastic_semantics::EbiTraitStochasticSemantics}, follower_semantics::FollowerSemantics, math::fraction::Fraction, techniques::explain_trace::ExplainTrace};

#[global_allocator]
static PEAK_ALLOC: PeakAlloc = PeakAlloc;

pub const EBI_PROBABILITY: EbiCommand = EbiCommand::Group { 
    name_short: "prob",
    name_long: Some("probability"),
    explanation_short: "Compute the probability of a trace or specification on the model.", 
    explanation_long: None, 
    children: &[
        &EBI_PROBABILITY_MODEL,
        &EBI_PROBABILITY_TRACE,
        &EBI_PROBABILITY_EXPLAIN_TRACE,
        &EBI_PROBABILITY_EXPLAIN_TRACE_WITH_REUSE,
        &EBI_PROBABILITY_EXPLAIN_TRACE_WITHOUT_REUSE,
        &EBI_PROBABILITY_EXPLAIN_TRACE_WITHOUT_HEURISTIC,
        &EBI_PROBABILITY_EXPLAIN_LOG,
        &EBI_PROBABILITY_EXPLAIN_LOG_WITHOUT_H
    ]
};

pub const EBI_PROBABILITY_MODEL: EbiCommand = EbiCommand::Command { 
    name_short: "mod", 
    name_long: Some("model"), 
    explanation_short: "Compute the probability that a queriable stochastic language (stochastic model) produces any trace of the model.", 
    explanation_long: None, 
    latex_link: Some("~\\cite{DBLP:journals/is/LeemansMM24}"), 
    cli_command: None, 
    exact_arithmetic: true, 
    input_types: &[ 
        &[ &EbiInputType::Trait(EbiTrait::QueriableStochasticLanguage) ], 
        &[ &EbiInputType::Trait(EbiTrait::FiniteLanguage) ] 
    ], 
    input_names: &[ "FILE_1", "FILE_2" ], 
    input_helps: &[ "The queriable stochastic language (model).", "The finite language (log)." ], 
    execute: |mut inputs, _| {
        let model: Box<dyn EbiTraitQueriableStochasticLanguage> = inputs.remove(0).to_type::<dyn EbiTraitQueriableStochasticLanguage>()?;
        let log = inputs.remove(0).to_type::<dyn EbiTraitFiniteLanguage>()?;
        
        let mut sum = Fraction::zero();
        for trace in log.iter() {
            sum += model.get_probability(&FollowerSemantics::Trace(&trace)).with_context(|| format!("cannot compute probability of trace {:?}", trace))?;
        }
        return Ok(EbiOutput::Fraction(sum));
    }, 
    output_type: &EbiOutputType::Fraction,
};

pub const EBI_PROBABILITY_TRACE: EbiCommand = EbiCommand::Command { 
    name_short: "trac", 
    name_long: Some("trace"), 
    explanation_short: "Compute the probability of a trace in a queriable stochastic language (model).", 
    explanation_long: None, 
    latex_link: Some("~\\cite{DBLP:journals/is/LeemansMM24}"), 
    cli_command: Some(|command| {
        command.arg(Arg::new("trace")
            .action(ArgAction::Set)
            .value_name("TRACE")
            .help("The trace.")
            .required(true)
            .value_parser(value_parser!(String))
            .num_args(0..))
    }), 
    exact_arithmetic: true, 
    input_types: &[ 
        &[ &EbiInputType::Trait(EbiTrait::QueriableStochasticLanguage) ] 
    ], 
    input_names: &[ "FILE" ], 
    input_helps: &[ "The queriable stochastic language (model)." ], 
    execute: |mut inputs, cli_matches| {
        let mut model = inputs.remove(0).to_type::<dyn EbiTraitQueriableStochasticLanguage>()?;
        if let Some(x) = cli_matches.unwrap().get_many::<String>("trace") {
            let t: Vec<&String> = x.collect();
            let trace = t.into_iter().map(|activity| activity.as_str()).collect::<Vec<_>>();
            let trace = model.get_activity_key_mut().process_trace_ref(&trace);

            log::trace!("compute probability of trace {:?}", trace);
        
            let result = model.get_probability(&FollowerSemantics::Trace(&trace)).with_context(|| format!("cannot compute probability of trace {:?}", trace))?;
            return Ok(EbiOutput::Fraction(result));
        } else {
            return Err(anyhow!("no trace given"));
        }
    }, 
    output_type: &EbiOutputType::Fraction,
};


pub const EBI_PROBABILITY_EXPLAIN_TRACE_WITH_REUSE: EbiCommand = EbiCommand::Command { 
    name_short: "exptra_with_reuse", 
    name_long: Some("explain-trace-with-reuse"), 
    explanation_short: "Compute the most likely explanation of a trace given the stochastic model.", 
    explanation_long: None, 
    latex_link: None, 
    cli_command: Some(|command| {
        command.arg(Arg::new("trace")
            .action(ArgAction::Set)
            .value_name("TRACE")
            .help("The trace.")
            .required(true)
            .value_parser(value_parser!(String))
            .num_args(0..))
    }), 
    exact_arithmetic: true, 
    input_types: &[ 
        &[ &EbiInputType::Trait(EbiTrait::StochasticSemantics) ],
        &[ &EbiInputType::Fraction ]
    ], 
    input_names: &[ "FILE", "VALUE" ], 
    input_helps: &[ "The model.", "Balance between 0 (=only consider deviations) to 1 (=only consider weight in the model)" ], 
    execute: |mut inputs, cli_matches| {
        let mut semantics = inputs.remove(0).to_type::<EbiTraitStochasticSemantics>()?;
        let balance = inputs.remove(0).to_type::<Fraction>()?;
        if let Some(x) = cli_matches.unwrap().get_many::<String>("trace") {
            let t: Vec<&String> = x.collect();
            let trace = t.into_iter().map(|activity| activity.as_str()).collect::<Vec<_>>();
            let trace = semantics.get_activity_key_mut().process_trace_ref(&trace);

            log::trace!("explain the trace {:?} given the model", trace);
            use std::time::Instant;
            let now = Instant::now();
            let result = semantics.explain_trace_with_reuse(&trace, &balance).with_context(|| format!("cannot explain the trace {:?}", trace))?;
            let elapsed = now.elapsed();
            println!("Elapsed: {:.5?} and trace length:{}", elapsed, trace.len());
            return Ok(EbiOutput::Object(EbiObject::Alignments(result)));
        } else {
            return Err(anyhow!("no trace given"));
        }
    }, 
    output_type: &EbiOutputType::ObjectType(EbiObjectType::Alignments),
};


pub const EBI_PROBABILITY_EXPLAIN_TRACE_WITHOUT_REUSE: EbiCommand = EbiCommand::Command { 
    name_short: "exptra_without_reuse", 
    name_long: Some("explain-trace-without-reuse"), 
    explanation_short: "Compute the most likely explanation of a trace given the stochastic model.", 
    explanation_long: None, 
    latex_link: None, 
    cli_command: Some(|command| {
        command.arg(Arg::new("trace")
            .action(ArgAction::Set)
            .value_name("TRACE")
            .help("The trace.")
            .required(true)
            .value_parser(value_parser!(String))
            .num_args(0..))
    }), 
    exact_arithmetic: true, 
    input_types: &[ 
        &[ &EbiInputType::Trait(EbiTrait::StochasticSemantics) ],
        &[ &EbiInputType::Fraction ]
    ], 
    input_names: &[ "FILE", "VALUE" ], 
    input_helps: &[ "The model.", "Balance between 0 (=only consider deviations) to 1 (=only consider weight in the model)" ], 
    execute: |mut inputs, cli_matches| {
        let mut semantics = inputs.remove(0).to_type::<EbiTraitStochasticSemantics>()?;
        let balance = inputs.remove(0).to_type::<Fraction>()?;
        if let Some(x) = cli_matches.unwrap().get_many::<String>("trace") {
            let t: Vec<&String> = x.collect();
            let trace = t.into_iter().map(|activity| activity.as_str()).collect::<Vec<_>>();
            let trace = semantics.get_activity_key_mut().process_trace_ref(&trace);

            log::trace!("explain the trace {:?} given the model", trace);
            use std::time::Instant;
            let now = Instant::now();
            let result = semantics.explain_trace_without_reuse(&trace, &balance).with_context(|| format!("cannot explain the trace {:?}", trace))?;
            let elapsed = now.elapsed();
            println!("Elapsed: {:.5?} and trace length:{}", elapsed, trace.len());
            return Ok(EbiOutput::Object(EbiObject::Alignments(result)));
        } else {
            return Err(anyhow!("no trace given"));
        }
    }, 
    output_type: &EbiOutputType::ObjectType(EbiObjectType::Alignments),
};


pub const EBI_PROBABILITY_EXPLAIN_TRACE: EbiCommand = EbiCommand::Command { 
    name_short: "exptra", 
    name_long: Some("explain-trace"), 
    explanation_short: "Compute the most likely explanation of a trace given the stochastic model.", 
    explanation_long: None, 
    latex_link: None, 
    cli_command: Some(|command| {
        command.arg(Arg::new("trace")
            .action(ArgAction::Set)
            .value_name("TRACE")
            .help("The trace.")
            .required(true)
            .value_parser(value_parser!(String))
            .num_args(0..))
    }), 
    exact_arithmetic: true, 
    input_types: &[ 
        &[ &EbiInputType::Trait(EbiTrait::StochasticSemantics) ],
        &[ &EbiInputType::Fraction ]
    ], 
    input_names: &[ "FILE", "VALUE" ], 
    input_helps: &[ "The model.", "Balance between 0 (=only consider deviations) to 1 (=only consider weight in the model)" ], 
    execute: |mut inputs, cli_matches| {
        let mut semantics = inputs.remove(0).to_type::<EbiTraitStochasticSemantics>()?;
        let balance = inputs.remove(0).to_type::<Fraction>()?;
        if let Some(x) = cli_matches.unwrap().get_many::<String>("trace") {
            let t: Vec<&String> = x.collect();
            let trace = t.into_iter().map(|activity| activity.as_str()).collect::<Vec<_>>();
            let trace = semantics.get_activity_key_mut().process_trace_ref(&trace);

            log::trace!("explain the trace {:?} given the model", trace);
            use std::time::Instant;
            let now = Instant::now();
            let result = semantics.explain_trace(&trace, &balance).with_context(|| format!("cannot explain the trace {:?}", trace))?;
            let elapsed = now.elapsed();
            println!("Elapsed: {:.5?} and trace length:{}", elapsed, trace.len());
            return Ok(EbiOutput::Object(EbiObject::Alignments(result)));
        } else {
            return Err(anyhow!("no trace given"));
        }
    }, 
    output_type: &EbiOutputType::ObjectType(EbiObjectType::Alignments),
};

pub const EBI_PROBABILITY_EXPLAIN_TRACE_WITHOUT_HEURISTIC: EbiCommand = EbiCommand::Command { 
    name_short: "exptra_without_heuristic", 
    name_long: Some("explain-trace-without-heuristic"), 
    explanation_short: "Compute the most likely explanation of a trace given the stochastic model.", 
    explanation_long: None, 
    latex_link: None, 
    cli_command: Some(|command| {
        command.arg(Arg::new("trace")
            .action(ArgAction::Set)
            .value_name("TRACE")
            .help("The trace.")
            .required(true)
            .value_parser(value_parser!(String))
            .num_args(0..))
    }), 
    exact_arithmetic: true, 
    input_types: &[ 
        &[ &EbiInputType::Trait(EbiTrait::StochasticSemantics) ],
        &[ &EbiInputType::Fraction ]
    ], 
    input_names: &[ "FILE", "VALUE" ], 
    input_helps: &[ "The model.", "Balance between 0 (=only consider deviations) to 1 (=only consider weight in the model)" ], 
    execute: |mut inputs, cli_matches| {
        let mut semantics = inputs.remove(0).to_type::<EbiTraitStochasticSemantics>()?;
        let balance = inputs.remove(0).to_type::<Fraction>()?;
        if let Some(x) = cli_matches.unwrap().get_many::<String>("trace") {
            let t: Vec<&String> = x.collect();
            let trace = t.into_iter().map(|activity| activity.as_str()).collect::<Vec<_>>();
            let trace = semantics.get_activity_key_mut().process_trace_ref(&trace);

            log::trace!("explain the trace {:?} given the model", trace);
            use std::time::Instant;
            let now = Instant::now();
            let result = semantics.explain_trace_without_heuristic(&trace, &balance).with_context(|| format!("cannot explain the trace {:?}", trace))?;
            let elapsed = now.elapsed();
            println!("Elapsed: {:.5?} and trace length:{}", elapsed, trace.len());
            return Ok(EbiOutput::Object(EbiObject::Alignments(result)));
        } else {
            return Err(anyhow!("no trace given"));
        }
    }, 
    output_type: &EbiOutputType::ObjectType(EbiObjectType::Alignments),
};


pub const EBI_PROBABILITY_EXPLAIN_LOG: EbiCommand = EbiCommand::Command { 
    name_short: "explog", 
    name_long: Some("explain-log"), 
    explanation_short: "Compute the most likely explanation of each trace in a log given the stochastic model.", 
    explanation_long: None, 
    latex_link: None, 
    cli_command: None, 
    exact_arithmetic: true, 
    input_types: &[ 
        &[ &EbiInputType::Trait(EbiTrait::StochasticSemantics) ],
        &[ &EbiInputType::Fraction ],
        &[&EbiInputType::Trait(EbiTrait::FiniteStochasticLanguage)], 
    ], 
    input_names: &[ "FILE", "VALUE", "FILE"], 
    input_helps: &[ "The model.", "Balance between 0 (=only consider deviations) to 1 (=only consider weight in the model)", "The log." ], 
    execute: |mut inputs, cli_matches| {
        let semantics = inputs.remove(0).to_type::<EbiTraitStochasticSemantics>()?;
        let balance = inputs.remove(0).to_type::<Fraction>()?;
        let log = inputs.remove(0).to_type::<dyn EbiTraitFiniteStochasticLanguage>()?;
        let mut trace_len_vec = vec![];
        let mut time_vec = vec![];

        let mut states_vec = vec![];

        for (trace, _probability) in log.iter_trace_probability() {
            println!("trace len:{} and prob: {:?}", trace.len(), _probability);

            trace_len_vec.push(trace.len());
            use std::time::Instant;
            let now = Instant::now();
            let states_num = semantics.explain_log(&trace, &balance).with_context(|| format!("cannot explain the trace {:?}", trace))?;
            let elapsed = now.elapsed();

            states_vec.push(states_num);
            println!("Elapsed: {:.5?} and trace length:{}", elapsed, trace.len());
            time_vec.push(elapsed.as_secs_f64().to_string());
        }
        save_vectors_to_csv(&trace_len_vec, &time_vec, &states_vec, balance.fraction_to_f64().unwrap(),"data/application_df09_a=0.1_reuse.csv");
        Ok(EbiOutput::Fraction(Fraction::zero()))
    }, 

    output_type: &EbiOutputType::Fraction
};


pub const EBI_PROBABILITY_EXPLAIN_LOG_WITHOUT_H: EbiCommand = EbiCommand::Command { 
    name_short: "explog_without_h", 
    name_long: Some("explain-log-without-h"), 
    explanation_short: "Compute the most likely explanation of each trace in a log given the stochastic model.", 
    explanation_long: None, 
    latex_link: None, 
    cli_command: None, 
    exact_arithmetic: true, 
    input_types: &[ 
        &[ &EbiInputType::Trait(EbiTrait::StochasticSemantics) ],
        &[ &EbiInputType::Fraction ],
        &[&EbiInputType::Trait(EbiTrait::FiniteStochasticLanguage)], 
    ], 
    input_names: &[ "FILE", "VALUE", "FILE"], 
    input_helps: &[ "The model.", "Balance between 0 (=only consider deviations) to 1 (=only consider weight in the model)", "The log." ], 
    execute: |mut inputs, cli_matches| {
        let semantics = inputs.remove(0).to_type::<EbiTraitStochasticSemantics>()?;
        let balance = inputs.remove(0).to_type::<Fraction>()?;
        let log = inputs.remove(0).to_type::<dyn EbiTraitFiniteStochasticLanguage>()?;
        let mut trace_len_vec = vec![];
        let mut time_vec = vec![];
        let mut states_vec = vec![];

        for (trace, _probability) in log.iter_trace_probability() {
            trace_len_vec.push(trace.len());
            use std::time::Instant;
            let now = Instant::now();            
            let states_num = semantics.explain_log_without_h(&trace, &balance).with_context(|| format!("cannot explain the trace {:?}", trace))?;
            let elapsed = now.elapsed();
            states_vec.push(states_num);
            println!("Elapsed: {:.5?} and trace length:{}", elapsed, trace.len());
            time_vec.push(elapsed.as_secs_f64().to_string());

        }
        save_vectors_to_csv(&trace_len_vec, &time_vec, &states_vec, balance.fraction_to_f64().unwrap(),"data/domestic_df09_a=0.1_noheuristic.csv");
        Ok(EbiOutput::Fraction(Fraction::zero()))
    }, 
    output_type: &EbiOutputType::Fraction
};

fn save_vectors_to_csv<T: std::fmt::Display>(
    vec1: &[T],
    vec2: &[String],
    vec4: &[(usize,f64,f64)],
    balance:f64,
    filename: &str
) -> Result<(), Box<dyn Error>> {
    // Create a new CSV writer
    let mut writer = Writer::from_path(filename)?;

    // Write headers (optional)
    writer.write_record(&["Trace length", "Time", "States", "Cost", "Probability", "Balance"])?;

    // Iterate through the vectors and write rows
    for i in 0..vec1.len().min(vec2.len()) {
        writer.write_record(&[
            vec1[i].to_string(),
            vec2[i].to_string(),
            vec4[i].0.to_string(),
            vec4[i].1.to_string(),
            vec4[i].2.to_string(),
            balance.to_string(),
        ])?;
    }

    // Flush the writer to ensure all data is written
    writer.flush()?;

    Ok(())
}