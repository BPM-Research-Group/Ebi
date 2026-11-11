#![allow(unsafe_op_in_unsafe_fn)]
#![allow(unused_variables)]

// This file has been automatically generated. Manual changes will be overridden.

use crate::{ebi_framework::ebi_command::EbiCommand, surfaces::java::{java_export::ExportableToJava, java_link::import_or_load}};
use ebi_objects::anyhow::{anyhow, Context};
use jni::{
    EnvUnowned, jni_mangle, objects::{JClass, JString},
};

#[jni_mangle("Ebi")]
pub fn analyse_all_traces<'caller>(mut unowned_env: EnvUnowned<'caller>, _class: JClass<'caller>, arg0: JString<'caller>) -> JString<'caller> {
    unowned_env.with_env(|env| -> Result<_, jni::errors::Error> {
        ebi_objects::ebi_arithmetic::exact::set_exact_globally(true);
        let command: &&EbiCommand = &&crate::ebi_commands::ebi_command_analyse::EBI_ANALYSE_ALL;
        let input_types = match **command {
            EbiCommand::Command { input_types, .. } => input_types,
            _ => {
                env.throw("Expected a command.")?;
                return Ok(JString::null())
            },
        };

        let input0 = match import_or_load(arg0, input_types[0], 0).with_context(|| anyhow!("Could not import argument 0")) {
                    Ok(x) => x,
                    Err(err) => {
                        env.throw(err.to_string().as_str())?;
                        return Ok(JString::null())
                    }
                };
    

        let inputs = vec![input0];

        // Execute the command.
        let output = match command.execute_with_inputs(inputs) {
            Ok(result) => result,
            Err(err) => {
                env.throw(format!("Command error: {}", err.to_string().as_str()))?;
                return Ok(JString::null());
            }
        };
        
        output.export_to_java(env)
    }).resolve::<jni::errors::ThrowRuntimeExAndDefault>()
}

#[jni_mangle("Ebi")]
pub fn analyse_cohort_analysis<'caller>(mut unowned_env: EnvUnowned<'caller>, _class: JClass<'caller>, arg0: JString<'caller>, arg1: JString<'caller>, arg2: JString<'caller>) -> JString<'caller> {
    unowned_env.with_env(|env| -> Result<_, jni::errors::Error> {
        ebi_objects::ebi_arithmetic::exact::set_exact_globally(true);
        let command: &&EbiCommand = &&crate::ebi_commands::ebi_command_analyse::EBI_ANALYSE_COHORT_ANALYSIS;
        let input_types = match **command {
            EbiCommand::Command { input_types, .. } => input_types,
            _ => {
                env.throw("Expected a command.")?;
                return Ok(JString::null())
            },
        };

        let input0 = match import_or_load(arg0, input_types[0], 0).with_context(|| anyhow!("Could not import argument 0")) {
                    Ok(x) => x,
                    Err(err) => {
                        env.throw(err.to_string().as_str())?;
                        return Ok(JString::null())
                    }
                };
    
let input1 = match import_or_load(arg1, input_types[1], 1).with_context(|| anyhow!("Could not import argument 1")) {
                    Ok(x) => x,
                    Err(err) => {
                        env.throw(err.to_string().as_str())?;
                        return Ok(JString::null())
                    }
                };
    
let input2 = match import_or_load(arg2, input_types[2], 2).with_context(|| anyhow!("Could not import argument 2")) {
                    Ok(x) => x,
                    Err(err) => {
                        env.throw(err.to_string().as_str())?;
                        return Ok(JString::null())
                    }
                };
    

        let inputs = vec![input0, input1, input2];

        // Execute the command.
        let output = match command.execute_with_inputs(inputs) {
            Ok(result) => result,
            Err(err) => {
                env.throw(format!("Command error: {}", err.to_string().as_str()))?;
                return Ok(JString::null());
            }
        };
        
        output.export_to_java(env)
    }).resolve::<jni::errors::ThrowRuntimeExAndDefault>()
}

#[jni_mangle("Ebi")]
pub fn analyse_completeness<'caller>(mut unowned_env: EnvUnowned<'caller>, _class: JClass<'caller>, arg0: JString<'caller>) -> JString<'caller> {
    unowned_env.with_env(|env| -> Result<_, jni::errors::Error> {
        ebi_objects::ebi_arithmetic::exact::set_exact_globally(true);
        let command: &&EbiCommand = &&crate::ebi_commands::ebi_command_analyse::EBI_ANALYSE_COMPLETENESS;
        let input_types = match **command {
            EbiCommand::Command { input_types, .. } => input_types,
            _ => {
                env.throw("Expected a command.")?;
                return Ok(JString::null())
            },
        };

        let input0 = match import_or_load(arg0, input_types[0], 0).with_context(|| anyhow!("Could not import argument 0")) {
                    Ok(x) => x,
                    Err(err) => {
                        env.throw(err.to_string().as_str())?;
                        return Ok(JString::null())
                    }
                };
    

        let inputs = vec![input0];

        // Execute the command.
        let output = match command.execute_with_inputs(inputs) {
            Ok(result) => result,
            Err(err) => {
                env.throw(format!("Command error: {}", err.to_string().as_str()))?;
                return Ok(JString::null());
            }
        };
        
        output.export_to_java(env)
    }).resolve::<jni::errors::ThrowRuntimeExAndDefault>()
}

#[jni_mangle("Ebi")]
pub fn analyse_coverage<'caller>(mut unowned_env: EnvUnowned<'caller>, _class: JClass<'caller>, arg0: JString<'caller>, arg1: JString<'caller>) -> JString<'caller> {
    unowned_env.with_env(|env| -> Result<_, jni::errors::Error> {
        ebi_objects::ebi_arithmetic::exact::set_exact_globally(true);
        let command: &&EbiCommand = &&crate::ebi_commands::ebi_command_analyse::EBI_ANALYSE_COVERAGE;
        let input_types = match **command {
            EbiCommand::Command { input_types, .. } => input_types,
            _ => {
                env.throw("Expected a command.")?;
                return Ok(JString::null())
            },
        };

        let input0 = match import_or_load(arg0, input_types[0], 0).with_context(|| anyhow!("Could not import argument 0")) {
                    Ok(x) => x,
                    Err(err) => {
                        env.throw(err.to_string().as_str())?;
                        return Ok(JString::null())
                    }
                };
    
let input1 = match import_or_load(arg1, input_types[1], 1).with_context(|| anyhow!("Could not import argument 1")) {
                    Ok(x) => x,
                    Err(err) => {
                        env.throw(err.to_string().as_str())?;
                        return Ok(JString::null())
                    }
                };
    

        let inputs = vec![input0, input1];

        // Execute the command.
        let output = match command.execute_with_inputs(inputs) {
            Ok(result) => result,
            Err(err) => {
                env.throw(format!("Command error: {}", err.to_string().as_str()))?;
                return Ok(JString::null());
            }
        };
        
        output.export_to_java(env)
    }).resolve::<jni::errors::ThrowRuntimeExAndDefault>()
}

#[jni_mangle("Ebi")]
pub fn analyse_directly_follows_edge_difference<'caller>(mut unowned_env: EnvUnowned<'caller>, _class: JClass<'caller>, arg0: JString<'caller>, arg1: JString<'caller>) -> JString<'caller> {
    unowned_env.with_env(|env| -> Result<_, jni::errors::Error> {
        ebi_objects::ebi_arithmetic::exact::set_exact_globally(true);
        let command: &&EbiCommand = &&crate::ebi_commands::ebi_command_analyse::EBI_ANALYSE_DIRECTLY_FOLLOWS_EDGE_DIFFERENCE;
        let input_types = match **command {
            EbiCommand::Command { input_types, .. } => input_types,
            _ => {
                env.throw("Expected a command.")?;
                return Ok(JString::null())
            },
        };

        let input0 = match import_or_load(arg0, input_types[0], 0).with_context(|| anyhow!("Could not import argument 0")) {
                    Ok(x) => x,
                    Err(err) => {
                        env.throw(err.to_string().as_str())?;
                        return Ok(JString::null())
                    }
                };
    
let input1 = match import_or_load(arg1, input_types[1], 1).with_context(|| anyhow!("Could not import argument 1")) {
                    Ok(x) => x,
                    Err(err) => {
                        env.throw(err.to_string().as_str())?;
                        return Ok(JString::null())
                    }
                };
    

        let inputs = vec![input0, input1];

        // Execute the command.
        let output = match command.execute_with_inputs(inputs) {
            Ok(result) => result,
            Err(err) => {
                env.throw(format!("Command error: {}", err.to_string().as_str()))?;
                return Ok(JString::null());
            }
        };
        
        output.export_to_java(env)
    }).resolve::<jni::errors::ThrowRuntimeExAndDefault>()
}

#[jni_mangle("Ebi")]
pub fn analyse_directly_follows_edge_difference_no_frequencies<'caller>(mut unowned_env: EnvUnowned<'caller>, _class: JClass<'caller>, arg0: JString<'caller>, arg1: JString<'caller>) -> JString<'caller> {
    unowned_env.with_env(|env| -> Result<_, jni::errors::Error> {
        ebi_objects::ebi_arithmetic::exact::set_exact_globally(true);
        let command: &&EbiCommand = &&crate::ebi_commands::ebi_command_analyse::EBI_ANALYSE_DIRECTLY_FOLLOWS_EDGE_DIFFERENCE_NO_FREQUENCIES;
        let input_types = match **command {
            EbiCommand::Command { input_types, .. } => input_types,
            _ => {
                env.throw("Expected a command.")?;
                return Ok(JString::null())
            },
        };

        let input0 = match import_or_load(arg0, input_types[0], 0).with_context(|| anyhow!("Could not import argument 0")) {
                    Ok(x) => x,
                    Err(err) => {
                        env.throw(err.to_string().as_str())?;
                        return Ok(JString::null())
                    }
                };
    
let input1 = match import_or_load(arg1, input_types[1], 1).with_context(|| anyhow!("Could not import argument 1")) {
                    Ok(x) => x,
                    Err(err) => {
                        env.throw(err.to_string().as_str())?;
                        return Ok(JString::null())
                    }
                };
    

        let inputs = vec![input0, input1];

        // Execute the command.
        let output = match command.execute_with_inputs(inputs) {
            Ok(result) => result,
            Err(err) => {
                env.throw(format!("Command error: {}", err.to_string().as_str()))?;
                return Ok(JString::null());
            }
        };
        
        output.export_to_java(env)
    }).resolve::<jni::errors::ThrowRuntimeExAndDefault>()
}

#[jni_mangle("Ebi")]
pub fn analyse_entropy<'caller>(mut unowned_env: EnvUnowned<'caller>, _class: JClass<'caller>, arg0: JString<'caller>) -> JString<'caller> {
    unowned_env.with_env(|env| -> Result<_, jni::errors::Error> {
        ebi_objects::ebi_arithmetic::exact::set_exact_globally(true);
        let command: &&EbiCommand = &&crate::ebi_commands::ebi_command_analyse::EBI_ANALYSE_ENTROPY;
        let input_types = match **command {
            EbiCommand::Command { input_types, .. } => input_types,
            _ => {
                env.throw("Expected a command.")?;
                return Ok(JString::null())
            },
        };

        let input0 = match import_or_load(arg0, input_types[0], 0).with_context(|| anyhow!("Could not import argument 0")) {
                    Ok(x) => x,
                    Err(err) => {
                        env.throw(err.to_string().as_str())?;
                        return Ok(JString::null())
                    }
                };
    

        let inputs = vec![input0];

        // Execute the command.
        let output = match command.execute_with_inputs(inputs) {
            Ok(result) => result,
            Err(err) => {
                env.throw(format!("Command error: {}", err.to_string().as_str()))?;
                return Ok(JString::null());
            }
        };
        
        output.export_to_java(env)
    }).resolve::<jni::errors::ThrowRuntimeExAndDefault>()
}

#[jni_mangle("Ebi")]
pub fn analyse_medoid<'caller>(mut unowned_env: EnvUnowned<'caller>, _class: JClass<'caller>, arg0: JString<'caller>, arg1: JString<'caller>) -> JString<'caller> {
    unowned_env.with_env(|env| -> Result<_, jni::errors::Error> {
        ebi_objects::ebi_arithmetic::exact::set_exact_globally(true);
        let command: &&EbiCommand = &&crate::ebi_commands::ebi_command_analyse::EBI_ANALYSE_MEDOID;
        let input_types = match **command {
            EbiCommand::Command { input_types, .. } => input_types,
            _ => {
                env.throw("Expected a command.")?;
                return Ok(JString::null())
            },
        };

        let input0 = match import_or_load(arg0, input_types[0], 0).with_context(|| anyhow!("Could not import argument 0")) {
                    Ok(x) => x,
                    Err(err) => {
                        env.throw(err.to_string().as_str())?;
                        return Ok(JString::null())
                    }
                };
    
let input1 = match import_or_load(arg1, input_types[1], 1).with_context(|| anyhow!("Could not import argument 1")) {
                    Ok(x) => x,
                    Err(err) => {
                        env.throw(err.to_string().as_str())?;
                        return Ok(JString::null())
                    }
                };
    

        let inputs = vec![input0, input1];

        // Execute the command.
        let output = match command.execute_with_inputs(inputs) {
            Ok(result) => result,
            Err(err) => {
                env.throw(format!("Command error: {}", err.to_string().as_str()))?;
                return Ok(JString::null());
            }
        };
        
        output.export_to_java(env)
    }).resolve::<jni::errors::ThrowRuntimeExAndDefault>()
}

#[jni_mangle("Ebi")]
pub fn analyse_minimum_probability_traces<'caller>(mut unowned_env: EnvUnowned<'caller>, _class: JClass<'caller>, arg0: JString<'caller>, arg1: JString<'caller>) -> JString<'caller> {
    unowned_env.with_env(|env| -> Result<_, jni::errors::Error> {
        ebi_objects::ebi_arithmetic::exact::set_exact_globally(true);
        let command: &&EbiCommand = &&crate::ebi_commands::ebi_command_analyse::EBI_ANALYSE_MINPROB;
        let input_types = match **command {
            EbiCommand::Command { input_types, .. } => input_types,
            _ => {
                env.throw("Expected a command.")?;
                return Ok(JString::null())
            },
        };

        let input0 = match import_or_load(arg0, input_types[0], 0).with_context(|| anyhow!("Could not import argument 0")) {
                    Ok(x) => x,
                    Err(err) => {
                        env.throw(err.to_string().as_str())?;
                        return Ok(JString::null())
                    }
                };
    
let input1 = match import_or_load(arg1, input_types[1], 1).with_context(|| anyhow!("Could not import argument 1")) {
                    Ok(x) => x,
                    Err(err) => {
                        env.throw(err.to_string().as_str())?;
                        return Ok(JString::null())
                    }
                };
    

        let inputs = vec![input0, input1];

        // Execute the command.
        let output = match command.execute_with_inputs(inputs) {
            Ok(result) => result,
            Err(err) => {
                env.throw(format!("Command error: {}", err.to_string().as_str()))?;
                return Ok(JString::null());
            }
        };
        
        output.export_to_java(env)
    }).resolve::<jni::errors::ThrowRuntimeExAndDefault>()
}

#[jni_mangle("Ebi")]
pub fn analyse_mode<'caller>(mut unowned_env: EnvUnowned<'caller>, _class: JClass<'caller>, arg0: JString<'caller>) -> JString<'caller> {
    unowned_env.with_env(|env| -> Result<_, jni::errors::Error> {
        ebi_objects::ebi_arithmetic::exact::set_exact_globally(true);
        let command: &&EbiCommand = &&crate::ebi_commands::ebi_command_analyse::EBI_ANALYSE_MODE;
        let input_types = match **command {
            EbiCommand::Command { input_types, .. } => input_types,
            _ => {
                env.throw("Expected a command.")?;
                return Ok(JString::null())
            },
        };

        let input0 = match import_or_load(arg0, input_types[0], 0).with_context(|| anyhow!("Could not import argument 0")) {
                    Ok(x) => x,
                    Err(err) => {
                        env.throw(err.to_string().as_str())?;
                        return Ok(JString::null())
                    }
                };
    

        let inputs = vec![input0];

        // Execute the command.
        let output = match command.execute_with_inputs(inputs) {
            Ok(result) => result,
            Err(err) => {
                env.throw(format!("Command error: {}", err.to_string().as_str()))?;
                return Ok(JString::null());
            }
        };
        
        output.export_to_java(env)
    }).resolve::<jni::errors::ThrowRuntimeExAndDefault>()
}

#[jni_mangle("Ebi")]
pub fn analyse_most_likely_traces<'caller>(mut unowned_env: EnvUnowned<'caller>, _class: JClass<'caller>, arg0: JString<'caller>, arg1: JString<'caller>) -> JString<'caller> {
    unowned_env.with_env(|env| -> Result<_, jni::errors::Error> {
        ebi_objects::ebi_arithmetic::exact::set_exact_globally(true);
        let command: &&EbiCommand = &&crate::ebi_commands::ebi_command_analyse::EBI_ANALYSE_MOSTLIKELY;
        let input_types = match **command {
            EbiCommand::Command { input_types, .. } => input_types,
            _ => {
                env.throw("Expected a command.")?;
                return Ok(JString::null())
            },
        };

        let input0 = match import_or_load(arg0, input_types[0], 0).with_context(|| anyhow!("Could not import argument 0")) {
                    Ok(x) => x,
                    Err(err) => {
                        env.throw(err.to_string().as_str())?;
                        return Ok(JString::null())
                    }
                };
    
let input1 = match import_or_load(arg1, input_types[1], 1).with_context(|| anyhow!("Could not import argument 1")) {
                    Ok(x) => x,
                    Err(err) => {
                        env.throw(err.to_string().as_str())?;
                        return Ok(JString::null())
                    }
                };
    

        let inputs = vec![input0, input1];

        // Execute the command.
        let output = match command.execute_with_inputs(inputs) {
            Ok(result) => result,
            Err(err) => {
                env.throw(format!("Command error: {}", err.to_string().as_str()))?;
                return Ok(JString::null());
            }
        };
        
        output.export_to_java(env)
    }).resolve::<jni::errors::ThrowRuntimeExAndDefault>()
}

#[jni_mangle("Ebi")]
pub fn analyse_variety<'caller>(mut unowned_env: EnvUnowned<'caller>, _class: JClass<'caller>, arg0: JString<'caller>) -> JString<'caller> {
    unowned_env.with_env(|env| -> Result<_, jni::errors::Error> {
        ebi_objects::ebi_arithmetic::exact::set_exact_globally(true);
        let command: &&EbiCommand = &&crate::ebi_commands::ebi_command_analyse::EBI_ANALYSE_VARIETY;
        let input_types = match **command {
            EbiCommand::Command { input_types, .. } => input_types,
            _ => {
                env.throw("Expected a command.")?;
                return Ok(JString::null())
            },
        };

        let input0 = match import_or_load(arg0, input_types[0], 0).with_context(|| anyhow!("Could not import argument 0")) {
                    Ok(x) => x,
                    Err(err) => {
                        env.throw(err.to_string().as_str())?;
                        return Ok(JString::null())
                    }
                };
    

        let inputs = vec![input0];

        // Execute the command.
        let output = match command.execute_with_inputs(inputs) {
            Ok(result) => result,
            Err(err) => {
                env.throw(format!("Command error: {}", err.to_string().as_str()))?;
                return Ok(JString::null());
            }
        };
        
        output.export_to_java(env)
    }).resolve::<jni::errors::ThrowRuntimeExAndDefault>()
}

#[jni_mangle("Ebi")]
pub fn analyse_non_stochastic_activities<'caller>(mut unowned_env: EnvUnowned<'caller>, _class: JClass<'caller>, arg0: JString<'caller>) -> JString<'caller> {
    unowned_env.with_env(|env| -> Result<_, jni::errors::Error> {
        ebi_objects::ebi_arithmetic::exact::set_exact_globally(true);
        let command: &&EbiCommand = &&crate::ebi_commands::ebi_command_analyse_non_stochastic::EBI_ANALYSE_NON_STOCHASTIC_ACTIVITIES;
        let input_types = match **command {
            EbiCommand::Command { input_types, .. } => input_types,
            _ => {
                env.throw("Expected a command.")?;
                return Ok(JString::null())
            },
        };

        let input0 = match import_or_load(arg0, input_types[0], 0).with_context(|| anyhow!("Could not import argument 0")) {
                    Ok(x) => x,
                    Err(err) => {
                        env.throw(err.to_string().as_str())?;
                        return Ok(JString::null())
                    }
                };
    

        let inputs = vec![input0];

        // Execute the command.
        let output = match command.execute_with_inputs(inputs) {
            Ok(result) => result,
            Err(err) => {
                env.throw(format!("Command error: {}", err.to_string().as_str()))?;
                return Ok(JString::null());
            }
        };
        
        output.export_to_java(env)
    }).resolve::<jni::errors::ThrowRuntimeExAndDefault>()
}

#[jni_mangle("Ebi")]
pub fn analyse_non_stochastic_any_traces<'caller>(mut unowned_env: EnvUnowned<'caller>, _class: JClass<'caller>, arg0: JString<'caller>) -> JString<'caller> {
    unowned_env.with_env(|env| -> Result<_, jni::errors::Error> {
        ebi_objects::ebi_arithmetic::exact::set_exact_globally(true);
        let command: &&EbiCommand = &&crate::ebi_commands::ebi_command_analyse_non_stochastic::EBI_ANALYSE_NON_STOCHASTIC_ANY_TRACES;
        let input_types = match **command {
            EbiCommand::Command { input_types, .. } => input_types,
            _ => {
                env.throw("Expected a command.")?;
                return Ok(JString::null())
            },
        };

        let input0 = match import_or_load(arg0, input_types[0], 0).with_context(|| anyhow!("Could not import argument 0")) {
                    Ok(x) => x,
                    Err(err) => {
                        env.throw(err.to_string().as_str())?;
                        return Ok(JString::null())
                    }
                };
    

        let inputs = vec![input0];

        // Execute the command.
        let output = match command.execute_with_inputs(inputs) {
            Ok(result) => result,
            Err(err) => {
                env.throw(format!("Command error: {}", err.to_string().as_str()))?;
                return Ok(JString::null());
            }
        };
        
        output.export_to_java(env)
    }).resolve::<jni::errors::ThrowRuntimeExAndDefault>()
}

#[jni_mangle("Ebi")]
pub fn analyse_non_stochastic_bounded<'caller>(mut unowned_env: EnvUnowned<'caller>, _class: JClass<'caller>, arg0: JString<'caller>) -> JString<'caller> {
    unowned_env.with_env(|env| -> Result<_, jni::errors::Error> {
        ebi_objects::ebi_arithmetic::exact::set_exact_globally(true);
        let command: &&EbiCommand = &&crate::ebi_commands::ebi_command_analyse_non_stochastic::EBI_ANALYSE_NON_STOCHASTIC_BOUNDED;
        let input_types = match **command {
            EbiCommand::Command { input_types, .. } => input_types,
            _ => {
                env.throw("Expected a command.")?;
                return Ok(JString::null())
            },
        };

        let input0 = match import_or_load(arg0, input_types[0], 0).with_context(|| anyhow!("Could not import argument 0")) {
                    Ok(x) => x,
                    Err(err) => {
                        env.throw(err.to_string().as_str())?;
                        return Ok(JString::null())
                    }
                };
    

        let inputs = vec![input0];

        // Execute the command.
        let output = match command.execute_with_inputs(inputs) {
            Ok(result) => result,
            Err(err) => {
                env.throw(format!("Command error: {}", err.to_string().as_str()))?;
                return Ok(JString::null());
            }
        };
        
        output.export_to_java(env)
    }).resolve::<jni::errors::ThrowRuntimeExAndDefault>()
}

#[jni_mangle("Ebi")]
pub fn analyse_non_stochastic_cluster<'caller>(mut unowned_env: EnvUnowned<'caller>, _class: JClass<'caller>, arg0: JString<'caller>, arg1: JString<'caller>) -> JString<'caller> {
    unowned_env.with_env(|env| -> Result<_, jni::errors::Error> {
        ebi_objects::ebi_arithmetic::exact::set_exact_globally(true);
        let command: &&EbiCommand = &&crate::ebi_commands::ebi_command_analyse_non_stochastic::EBI_ANALYSE_NON_STOCHASTIC_CLUSTER;
        let input_types = match **command {
            EbiCommand::Command { input_types, .. } => input_types,
            _ => {
                env.throw("Expected a command.")?;
                return Ok(JString::null())
            },
        };

        let input0 = match import_or_load(arg0, input_types[0], 0).with_context(|| anyhow!("Could not import argument 0")) {
                    Ok(x) => x,
                    Err(err) => {
                        env.throw(err.to_string().as_str())?;
                        return Ok(JString::null())
                    }
                };
    
let input1 = match import_or_load(arg1, input_types[1], 1).with_context(|| anyhow!("Could not import argument 1")) {
                    Ok(x) => x,
                    Err(err) => {
                        env.throw(err.to_string().as_str())?;
                        return Ok(JString::null())
                    }
                };
    

        let inputs = vec![input0, input1];

        // Execute the command.
        let output = match command.execute_with_inputs(inputs) {
            Ok(result) => result,
            Err(err) => {
                env.throw(format!("Command error: {}", err.to_string().as_str()))?;
                return Ok(JString::null());
            }
        };
        
        output.export_to_java(env)
    }).resolve::<jni::errors::ThrowRuntimeExAndDefault>()
}

#[jni_mangle("Ebi")]
pub fn analyse_non_stochastic_empty_traces<'caller>(mut unowned_env: EnvUnowned<'caller>, _class: JClass<'caller>, arg0: JString<'caller>) -> JString<'caller> {
    unowned_env.with_env(|env| -> Result<_, jni::errors::Error> {
        ebi_objects::ebi_arithmetic::exact::set_exact_globally(true);
        let command: &&EbiCommand = &&crate::ebi_commands::ebi_command_analyse_non_stochastic::EBI_ANALYSE_NON_STOCHASTIC_EMPTY_TRACES;
        let input_types = match **command {
            EbiCommand::Command { input_types, .. } => input_types,
            _ => {
                env.throw("Expected a command.")?;
                return Ok(JString::null())
            },
        };

        let input0 = match import_or_load(arg0, input_types[0], 0).with_context(|| anyhow!("Could not import argument 0")) {
                    Ok(x) => x,
                    Err(err) => {
                        env.throw(err.to_string().as_str())?;
                        return Ok(JString::null())
                    }
                };
    

        let inputs = vec![input0];

        // Execute the command.
        let output = match command.execute_with_inputs(inputs) {
            Ok(result) => result,
            Err(err) => {
                env.throw(format!("Command error: {}", err.to_string().as_str()))?;
                return Ok(JString::null());
            }
        };
        
        output.export_to_java(env)
    }).resolve::<jni::errors::ThrowRuntimeExAndDefault>()
}

#[jni_mangle("Ebi")]
pub fn analyse_non_stochastic_executions<'caller>(mut unowned_env: EnvUnowned<'caller>, _class: JClass<'caller>, arg0: JString<'caller>, arg1: JString<'caller>) -> JString<'caller> {
    unowned_env.with_env(|env| -> Result<_, jni::errors::Error> {
        ebi_objects::ebi_arithmetic::exact::set_exact_globally(true);
        let command: &&EbiCommand = &&crate::ebi_commands::ebi_command_analyse_non_stochastic::EBI_ANALYSE_NON_STOCHASTIC_EXECUTIONS;
        let input_types = match **command {
            EbiCommand::Command { input_types, .. } => input_types,
            _ => {
                env.throw("Expected a command.")?;
                return Ok(JString::null())
            },
        };

        let input0 = match import_or_load(arg0, input_types[0], 0).with_context(|| anyhow!("Could not import argument 0")) {
                    Ok(x) => x,
                    Err(err) => {
                        env.throw(err.to_string().as_str())?;
                        return Ok(JString::null())
                    }
                };
    
let input1 = match import_or_load(arg1, input_types[1], 1).with_context(|| anyhow!("Could not import argument 1")) {
                    Ok(x) => x,
                    Err(err) => {
                        env.throw(err.to_string().as_str())?;
                        return Ok(JString::null())
                    }
                };
    

        let inputs = vec![input0, input1];

        // Execute the command.
        let output = match command.execute_with_inputs(inputs) {
            Ok(result) => result,
            Err(err) => {
                env.throw(format!("Command error: {}", err.to_string().as_str()))?;
                return Ok(JString::null());
            }
        };
        
        output.export_to_java(env)
    }).resolve::<jni::errors::ThrowRuntimeExAndDefault>()
}

#[jni_mangle("Ebi")]
pub fn analyse_non_stochastic_infinitely_many_traces<'caller>(mut unowned_env: EnvUnowned<'caller>, _class: JClass<'caller>, arg0: JString<'caller>) -> JString<'caller> {
    unowned_env.with_env(|env| -> Result<_, jni::errors::Error> {
        ebi_objects::ebi_arithmetic::exact::set_exact_globally(true);
        let command: &&EbiCommand = &&crate::ebi_commands::ebi_command_analyse_non_stochastic::EBI_ANALYSE_NON_STOCHASTIC_INFINITELY_MANY_TRACES;
        let input_types = match **command {
            EbiCommand::Command { input_types, .. } => input_types,
            _ => {
                env.throw("Expected a command.")?;
                return Ok(JString::null())
            },
        };

        let input0 = match import_or_load(arg0, input_types[0], 0).with_context(|| anyhow!("Could not import argument 0")) {
                    Ok(x) => x,
                    Err(err) => {
                        env.throw(err.to_string().as_str())?;
                        return Ok(JString::null())
                    }
                };
    

        let inputs = vec![input0];

        // Execute the command.
        let output = match command.execute_with_inputs(inputs) {
            Ok(result) => result,
            Err(err) => {
                env.throw(format!("Command error: {}", err.to_string().as_str()))?;
                return Ok(JString::null());
            }
        };
        
        output.export_to_java(env)
    }).resolve::<jni::errors::ThrowRuntimeExAndDefault>()
}

#[jni_mangle("Ebi")]
pub fn analyse_non_stochastic_medoid<'caller>(mut unowned_env: EnvUnowned<'caller>, _class: JClass<'caller>, arg0: JString<'caller>, arg1: JString<'caller>) -> JString<'caller> {
    unowned_env.with_env(|env| -> Result<_, jni::errors::Error> {
        ebi_objects::ebi_arithmetic::exact::set_exact_globally(true);
        let command: &&EbiCommand = &&crate::ebi_commands::ebi_command_analyse_non_stochastic::EBI_ANALYSE_NON_STOCHASTIC_MEDOID;
        let input_types = match **command {
            EbiCommand::Command { input_types, .. } => input_types,
            _ => {
                env.throw("Expected a command.")?;
                return Ok(JString::null())
            },
        };

        let input0 = match import_or_load(arg0, input_types[0], 0).with_context(|| anyhow!("Could not import argument 0")) {
                    Ok(x) => x,
                    Err(err) => {
                        env.throw(err.to_string().as_str())?;
                        return Ok(JString::null())
                    }
                };
    
let input1 = match import_or_load(arg1, input_types[1], 1).with_context(|| anyhow!("Could not import argument 1")) {
                    Ok(x) => x,
                    Err(err) => {
                        env.throw(err.to_string().as_str())?;
                        return Ok(JString::null())
                    }
                };
    

        let inputs = vec![input0, input1];

        // Execute the command.
        let output = match command.execute_with_inputs(inputs) {
            Ok(result) => result,
            Err(err) => {
                env.throw(format!("Command error: {}", err.to_string().as_str()))?;
                return Ok(JString::null());
            }
        };
        
        output.export_to_java(env)
    }).resolve::<jni::errors::ThrowRuntimeExAndDefault>()
}

#[jni_mangle("Ebi")]
pub fn analyse_non_stochastic_timestamps_ordered<'caller>(mut unowned_env: EnvUnowned<'caller>, _class: JClass<'caller>, arg0: JString<'caller>) -> JString<'caller> {
    unowned_env.with_env(|env| -> Result<_, jni::errors::Error> {
        ebi_objects::ebi_arithmetic::exact::set_exact_globally(true);
        let command: &&EbiCommand = &&crate::ebi_commands::ebi_command_analyse_non_stochastic::EBI_ANALYSE_NON_STOCHASTIC_TIMESTAMPS_ORDERED;
        let input_types = match **command {
            EbiCommand::Command { input_types, .. } => input_types,
            _ => {
                env.throw("Expected a command.")?;
                return Ok(JString::null())
            },
        };

        let input0 = match import_or_load(arg0, input_types[0], 0).with_context(|| anyhow!("Could not import argument 0")) {
                    Ok(x) => x,
                    Err(err) => {
                        env.throw(err.to_string().as_str())?;
                        return Ok(JString::null())
                    }
                };
    

        let inputs = vec![input0];

        // Execute the command.
        let output = match command.execute_with_inputs(inputs) {
            Ok(result) => result,
            Err(err) => {
                env.throw(format!("Command error: {}", err.to_string().as_str()))?;
                return Ok(JString::null());
            }
        };
        
        output.export_to_java(env)
    }).resolve::<jni::errors::ThrowRuntimeExAndDefault>()
}

#[jni_mangle("Ebi")]
pub fn association_all_trace_attributes<'caller>(mut unowned_env: EnvUnowned<'caller>, _class: JClass<'caller>, arg0: JString<'caller>, arg1: JString<'caller>) -> JString<'caller> {
    unowned_env.with_env(|env| -> Result<_, jni::errors::Error> {
        ebi_objects::ebi_arithmetic::exact::set_exact_globally(true);
        let command: &&EbiCommand = &&crate::ebi_commands::ebi_command_association::EBI_ASSOCIATION_ATTRIBUTES;
        let input_types = match **command {
            EbiCommand::Command { input_types, .. } => input_types,
            _ => {
                env.throw("Expected a command.")?;
                return Ok(JString::null())
            },
        };

        let input0 = match import_or_load(arg0, input_types[0], 0).with_context(|| anyhow!("Could not import argument 0")) {
                    Ok(x) => x,
                    Err(err) => {
                        env.throw(err.to_string().as_str())?;
                        return Ok(JString::null())
                    }
                };
    
let input1 = match import_or_load(arg1, input_types[1], 1).with_context(|| anyhow!("Could not import argument 1")) {
                    Ok(x) => x,
                    Err(err) => {
                        env.throw(err.to_string().as_str())?;
                        return Ok(JString::null())
                    }
                };
    

        let inputs = vec![input0, input1];

        // Execute the command.
        let output = match command.execute_with_inputs(inputs) {
            Ok(result) => result,
            Err(err) => {
                env.throw(format!("Command error: {}", err.to_string().as_str()))?;
                return Ok(JString::null());
            }
        };
        
        output.export_to_java(env)
    }).resolve::<jni::errors::ThrowRuntimeExAndDefault>()
}

#[jni_mangle("Ebi")]
pub fn association_trace_attribute<'caller>(mut unowned_env: EnvUnowned<'caller>, _class: JClass<'caller>, arg0: JString<'caller>, arg1: JString<'caller>, arg2: JString<'caller>) -> JString<'caller> {
    unowned_env.with_env(|env| -> Result<_, jni::errors::Error> {
        ebi_objects::ebi_arithmetic::exact::set_exact_globally(true);
        let command: &&EbiCommand = &&crate::ebi_commands::ebi_command_association::EBI_ASSOCIATION_ATTRIBUTE;
        let input_types = match **command {
            EbiCommand::Command { input_types, .. } => input_types,
            _ => {
                env.throw("Expected a command.")?;
                return Ok(JString::null())
            },
        };

        let input0 = match import_or_load(arg0, input_types[0], 0).with_context(|| anyhow!("Could not import argument 0")) {
                    Ok(x) => x,
                    Err(err) => {
                        env.throw(err.to_string().as_str())?;
                        return Ok(JString::null())
                    }
                };
    
let input1 = match import_or_load(arg1, input_types[1], 1).with_context(|| anyhow!("Could not import argument 1")) {
                    Ok(x) => x,
                    Err(err) => {
                        env.throw(err.to_string().as_str())?;
                        return Ok(JString::null())
                    }
                };
    
let input2 = match import_or_load(arg2, input_types[2], 2).with_context(|| anyhow!("Could not import argument 2")) {
                    Ok(x) => x,
                    Err(err) => {
                        env.throw(err.to_string().as_str())?;
                        return Ok(JString::null())
                    }
                };
    

        let inputs = vec![input0, input1, input2];

        // Execute the command.
        let output = match command.execute_with_inputs(inputs) {
            Ok(result) => result,
            Err(err) => {
                env.throw(format!("Command error: {}", err.to_string().as_str()))?;
                return Ok(JString::null());
            }
        };
        
        output.export_to_java(env)
    }).resolve::<jni::errors::ThrowRuntimeExAndDefault>()
}

#[jni_mangle("Ebi")]
pub fn conformance_chi_squared<'caller>(mut unowned_env: EnvUnowned<'caller>, _class: JClass<'caller>, arg0: JString<'caller>, arg1: JString<'caller>) -> JString<'caller> {
    unowned_env.with_env(|env| -> Result<_, jni::errors::Error> {
        ebi_objects::ebi_arithmetic::exact::set_exact_globally(true);
        let command: &&EbiCommand = &&crate::ebi_commands::ebi_command_conformance::EBI_CONFORMANCE_CHI_SQUARED;
        let input_types = match **command {
            EbiCommand::Command { input_types, .. } => input_types,
            _ => {
                env.throw("Expected a command.")?;
                return Ok(JString::null())
            },
        };

        let input0 = match import_or_load(arg0, input_types[0], 0).with_context(|| anyhow!("Could not import argument 0")) {
                    Ok(x) => x,
                    Err(err) => {
                        env.throw(err.to_string().as_str())?;
                        return Ok(JString::null())
                    }
                };
    
let input1 = match import_or_load(arg1, input_types[1], 1).with_context(|| anyhow!("Could not import argument 1")) {
                    Ok(x) => x,
                    Err(err) => {
                        env.throw(err.to_string().as_str())?;
                        return Ok(JString::null())
                    }
                };
    

        let inputs = vec![input0, input1];

        // Execute the command.
        let output = match command.execute_with_inputs(inputs) {
            Ok(result) => result,
            Err(err) => {
                env.throw(format!("Command error: {}", err.to_string().as_str()))?;
                return Ok(JString::null());
            }
        };
        
        output.export_to_java(env)
    }).resolve::<jni::errors::ThrowRuntimeExAndDefault>()
}

#[jni_mangle("Ebi")]
pub fn conformance_chi_squared_sample<'caller>(mut unowned_env: EnvUnowned<'caller>, _class: JClass<'caller>, arg0: JString<'caller>, arg1: JString<'caller>, arg2: JString<'caller>) -> JString<'caller> {
    unowned_env.with_env(|env| -> Result<_, jni::errors::Error> {
        ebi_objects::ebi_arithmetic::exact::set_exact_globally(true);
        let command: &&EbiCommand = &&crate::ebi_commands::ebi_command_conformance::EBI_CONFORMANCE_CHI_SQUARED_SAMPLE;
        let input_types = match **command {
            EbiCommand::Command { input_types, .. } => input_types,
            _ => {
                env.throw("Expected a command.")?;
                return Ok(JString::null())
            },
        };

        let input0 = match import_or_load(arg0, input_types[0], 0).with_context(|| anyhow!("Could not import argument 0")) {
                    Ok(x) => x,
                    Err(err) => {
                        env.throw(err.to_string().as_str())?;
                        return Ok(JString::null())
                    }
                };
    
let input1 = match import_or_load(arg1, input_types[1], 1).with_context(|| anyhow!("Could not import argument 1")) {
                    Ok(x) => x,
                    Err(err) => {
                        env.throw(err.to_string().as_str())?;
                        return Ok(JString::null())
                    }
                };
    
let input2 = match import_or_load(arg2, input_types[2], 2).with_context(|| anyhow!("Could not import argument 2")) {
                    Ok(x) => x,
                    Err(err) => {
                        env.throw(err.to_string().as_str())?;
                        return Ok(JString::null())
                    }
                };
    

        let inputs = vec![input0, input1, input2];

        // Execute the command.
        let output = match command.execute_with_inputs(inputs) {
            Ok(result) => result,
            Err(err) => {
                env.throw(format!("Command error: {}", err.to_string().as_str()))?;
                return Ok(JString::null());
            }
        };
        
        output.export_to_java(env)
    }).resolve::<jni::errors::ThrowRuntimeExAndDefault>()
}

#[jni_mangle("Ebi")]
pub fn conformance_earth_movers<'caller>(mut unowned_env: EnvUnowned<'caller>, _class: JClass<'caller>, arg0: JString<'caller>, arg1: JString<'caller>) -> JString<'caller> {
    unowned_env.with_env(|env| -> Result<_, jni::errors::Error> {
        ebi_objects::ebi_arithmetic::exact::set_exact_globally(true);
        let command: &&EbiCommand = &&crate::ebi_commands::ebi_command_conformance::EBI_CONFORMANCE_EARTH_MOVERS;
        let input_types = match **command {
            EbiCommand::Command { input_types, .. } => input_types,
            _ => {
                env.throw("Expected a command.")?;
                return Ok(JString::null())
            },
        };

        let input0 = match import_or_load(arg0, input_types[0], 0).with_context(|| anyhow!("Could not import argument 0")) {
                    Ok(x) => x,
                    Err(err) => {
                        env.throw(err.to_string().as_str())?;
                        return Ok(JString::null())
                    }
                };
    
let input1 = match import_or_load(arg1, input_types[1], 1).with_context(|| anyhow!("Could not import argument 1")) {
                    Ok(x) => x,
                    Err(err) => {
                        env.throw(err.to_string().as_str())?;
                        return Ok(JString::null())
                    }
                };
    

        let inputs = vec![input0, input1];

        // Execute the command.
        let output = match command.execute_with_inputs(inputs) {
            Ok(result) => result,
            Err(err) => {
                env.throw(format!("Command error: {}", err.to_string().as_str()))?;
                return Ok(JString::null());
            }
        };
        
        output.export_to_java(env)
    }).resolve::<jni::errors::ThrowRuntimeExAndDefault>()
}

#[jni_mangle("Ebi")]
pub fn conformance_earth_movers_sample<'caller>(mut unowned_env: EnvUnowned<'caller>, _class: JClass<'caller>, arg0: JString<'caller>, arg1: JString<'caller>, arg2: JString<'caller>) -> JString<'caller> {
    unowned_env.with_env(|env| -> Result<_, jni::errors::Error> {
        ebi_objects::ebi_arithmetic::exact::set_exact_globally(true);
        let command: &&EbiCommand = &&crate::ebi_commands::ebi_command_conformance::EBI_CONFORMANCE_EARTH_MOVERS_SAMPLE;
        let input_types = match **command {
            EbiCommand::Command { input_types, .. } => input_types,
            _ => {
                env.throw("Expected a command.")?;
                return Ok(JString::null())
            },
        };

        let input0 = match import_or_load(arg0, input_types[0], 0).with_context(|| anyhow!("Could not import argument 0")) {
                    Ok(x) => x,
                    Err(err) => {
                        env.throw(err.to_string().as_str())?;
                        return Ok(JString::null())
                    }
                };
    
let input1 = match import_or_load(arg1, input_types[1], 1).with_context(|| anyhow!("Could not import argument 1")) {
                    Ok(x) => x,
                    Err(err) => {
                        env.throw(err.to_string().as_str())?;
                        return Ok(JString::null())
                    }
                };
    
let input2 = match import_or_load(arg2, input_types[2], 2).with_context(|| anyhow!("Could not import argument 2")) {
                    Ok(x) => x,
                    Err(err) => {
                        env.throw(err.to_string().as_str())?;
                        return Ok(JString::null())
                    }
                };
    

        let inputs = vec![input0, input1, input2];

        // Execute the command.
        let output = match command.execute_with_inputs(inputs) {
            Ok(result) => result,
            Err(err) => {
                env.throw(format!("Command error: {}", err.to_string().as_str()))?;
                return Ok(JString::null());
            }
        };
        
        output.export_to_java(env)
    }).resolve::<jni::errors::ThrowRuntimeExAndDefault>()
}

#[jni_mangle("Ebi")]
pub fn conformance_entropic_relevance<'caller>(mut unowned_env: EnvUnowned<'caller>, _class: JClass<'caller>, arg0: JString<'caller>, arg1: JString<'caller>) -> JString<'caller> {
    unowned_env.with_env(|env| -> Result<_, jni::errors::Error> {
        ebi_objects::ebi_arithmetic::exact::set_exact_globally(true);
        let command: &&EbiCommand = &&crate::ebi_commands::ebi_command_conformance::EBI_CONFORMANCE_ENTROPIC_RELEVANCE;
        let input_types = match **command {
            EbiCommand::Command { input_types, .. } => input_types,
            _ => {
                env.throw("Expected a command.")?;
                return Ok(JString::null())
            },
        };

        let input0 = match import_or_load(arg0, input_types[0], 0).with_context(|| anyhow!("Could not import argument 0")) {
                    Ok(x) => x,
                    Err(err) => {
                        env.throw(err.to_string().as_str())?;
                        return Ok(JString::null())
                    }
                };
    
let input1 = match import_or_load(arg1, input_types[1], 1).with_context(|| anyhow!("Could not import argument 1")) {
                    Ok(x) => x,
                    Err(err) => {
                        env.throw(err.to_string().as_str())?;
                        return Ok(JString::null())
                    }
                };
    

        let inputs = vec![input0, input1];

        // Execute the command.
        let output = match command.execute_with_inputs(inputs) {
            Ok(result) => result,
            Err(err) => {
                env.throw(format!("Command error: {}", err.to_string().as_str()))?;
                return Ok(JString::null());
            }
        };
        
        output.export_to_java(env)
    }).resolve::<jni::errors::ThrowRuntimeExAndDefault>()
}

#[jni_mangle("Ebi")]
pub fn conformance_gain_precision<'caller>(mut unowned_env: EnvUnowned<'caller>, _class: JClass<'caller>, arg0: JString<'caller>, arg1: JString<'caller>, arg2: JString<'caller>) -> JString<'caller> {
    unowned_env.with_env(|env| -> Result<_, jni::errors::Error> {
        ebi_objects::ebi_arithmetic::exact::set_exact_globally(false);
        let command: &&EbiCommand = &&crate::ebi_commands::ebi_command_conformance::EBI_CONFORMANCE_GAIN_PRECISION;
        let input_types = match **command {
            EbiCommand::Command { input_types, .. } => input_types,
            _ => {
                env.throw("Expected a command.")?;
                return Ok(JString::null())
            },
        };

        let input0 = match import_or_load(arg0, input_types[0], 0).with_context(|| anyhow!("Could not import argument 0")) {
                    Ok(x) => x,
                    Err(err) => {
                        env.throw(err.to_string().as_str())?;
                        return Ok(JString::null())
                    }
                };
    
let input1 = match import_or_load(arg1, input_types[1], 1).with_context(|| anyhow!("Could not import argument 1")) {
                    Ok(x) => x,
                    Err(err) => {
                        env.throw(err.to_string().as_str())?;
                        return Ok(JString::null())
                    }
                };
    
let input2 = match import_or_load(arg2, input_types[2], 2).with_context(|| anyhow!("Could not import argument 2")) {
                    Ok(x) => x,
                    Err(err) => {
                        env.throw(err.to_string().as_str())?;
                        return Ok(JString::null())
                    }
                };
    

        let inputs = vec![input0, input1, input2];

        // Execute the command.
        let output = match command.execute_with_inputs(inputs) {
            Ok(result) => result,
            Err(err) => {
                env.throw(format!("Command error: {}", err.to_string().as_str()))?;
                return Ok(JString::null());
            }
        };
        
        output.export_to_java(env)
    }).resolve::<jni::errors::ThrowRuntimeExAndDefault>()
}

#[jni_mangle("Ebi")]
pub fn conformance_gain_recall<'caller>(mut unowned_env: EnvUnowned<'caller>, _class: JClass<'caller>, arg0: JString<'caller>, arg1: JString<'caller>, arg2: JString<'caller>) -> JString<'caller> {
    unowned_env.with_env(|env| -> Result<_, jni::errors::Error> {
        ebi_objects::ebi_arithmetic::exact::set_exact_globally(false);
        let command: &&EbiCommand = &&crate::ebi_commands::ebi_command_conformance::EBI_CONFORMANCE_GAIN_RECALL;
        let input_types = match **command {
            EbiCommand::Command { input_types, .. } => input_types,
            _ => {
                env.throw("Expected a command.")?;
                return Ok(JString::null())
            },
        };

        let input0 = match import_or_load(arg0, input_types[0], 0).with_context(|| anyhow!("Could not import argument 0")) {
                    Ok(x) => x,
                    Err(err) => {
                        env.throw(err.to_string().as_str())?;
                        return Ok(JString::null())
                    }
                };
    
let input1 = match import_or_load(arg1, input_types[1], 1).with_context(|| anyhow!("Could not import argument 1")) {
                    Ok(x) => x,
                    Err(err) => {
                        env.throw(err.to_string().as_str())?;
                        return Ok(JString::null())
                    }
                };
    
let input2 = match import_or_load(arg2, input_types[2], 2).with_context(|| anyhow!("Could not import argument 2")) {
                    Ok(x) => x,
                    Err(err) => {
                        env.throw(err.to_string().as_str())?;
                        return Ok(JString::null())
                    }
                };
    

        let inputs = vec![input0, input1, input2];

        // Execute the command.
        let output = match command.execute_with_inputs(inputs) {
            Ok(result) => result,
            Err(err) => {
                env.throw(format!("Command error: {}", err.to_string().as_str()))?;
                return Ok(JString::null());
            }
        };
        
        output.export_to_java(env)
    }).resolve::<jni::errors::ThrowRuntimeExAndDefault>()
}

#[jni_mangle("Ebi")]
pub fn conformance_hellinger<'caller>(mut unowned_env: EnvUnowned<'caller>, _class: JClass<'caller>, arg0: JString<'caller>, arg1: JString<'caller>) -> JString<'caller> {
    unowned_env.with_env(|env| -> Result<_, jni::errors::Error> {
        ebi_objects::ebi_arithmetic::exact::set_exact_globally(true);
        let command: &&EbiCommand = &&crate::ebi_commands::ebi_command_conformance::EBI_CONFORMANCE_HELLINGER;
        let input_types = match **command {
            EbiCommand::Command { input_types, .. } => input_types,
            _ => {
                env.throw("Expected a command.")?;
                return Ok(JString::null())
            },
        };

        let input0 = match import_or_load(arg0, input_types[0], 0).with_context(|| anyhow!("Could not import argument 0")) {
                    Ok(x) => x,
                    Err(err) => {
                        env.throw(err.to_string().as_str())?;
                        return Ok(JString::null())
                    }
                };
    
let input1 = match import_or_load(arg1, input_types[1], 1).with_context(|| anyhow!("Could not import argument 1")) {
                    Ok(x) => x,
                    Err(err) => {
                        env.throw(err.to_string().as_str())?;
                        return Ok(JString::null())
                    }
                };
    

        let inputs = vec![input0, input1];

        // Execute the command.
        let output = match command.execute_with_inputs(inputs) {
            Ok(result) => result,
            Err(err) => {
                env.throw(format!("Command error: {}", err.to_string().as_str()))?;
                return Ok(JString::null());
            }
        };
        
        output.export_to_java(env)
    }).resolve::<jni::errors::ThrowRuntimeExAndDefault>()
}

#[jni_mangle("Ebi")]
pub fn conformance_hellinger_sample<'caller>(mut unowned_env: EnvUnowned<'caller>, _class: JClass<'caller>, arg0: JString<'caller>, arg1: JString<'caller>, arg2: JString<'caller>) -> JString<'caller> {
    unowned_env.with_env(|env| -> Result<_, jni::errors::Error> {
        ebi_objects::ebi_arithmetic::exact::set_exact_globally(true);
        let command: &&EbiCommand = &&crate::ebi_commands::ebi_command_conformance::EBI_CONFORMANCE_HELLINGER_SAMPLE;
        let input_types = match **command {
            EbiCommand::Command { input_types, .. } => input_types,
            _ => {
                env.throw("Expected a command.")?;
                return Ok(JString::null())
            },
        };

        let input0 = match import_or_load(arg0, input_types[0], 0).with_context(|| anyhow!("Could not import argument 0")) {
                    Ok(x) => x,
                    Err(err) => {
                        env.throw(err.to_string().as_str())?;
                        return Ok(JString::null())
                    }
                };
    
let input1 = match import_or_load(arg1, input_types[1], 1).with_context(|| anyhow!("Could not import argument 1")) {
                    Ok(x) => x,
                    Err(err) => {
                        env.throw(err.to_string().as_str())?;
                        return Ok(JString::null())
                    }
                };
    
let input2 = match import_or_load(arg2, input_types[2], 2).with_context(|| anyhow!("Could not import argument 2")) {
                    Ok(x) => x,
                    Err(err) => {
                        env.throw(err.to_string().as_str())?;
                        return Ok(JString::null())
                    }
                };
    

        let inputs = vec![input0, input1, input2];

        // Execute the command.
        let output = match command.execute_with_inputs(inputs) {
            Ok(result) => result,
            Err(err) => {
                env.throw(format!("Command error: {}", err.to_string().as_str()))?;
                return Ok(JString::null());
            }
        };
        
        output.export_to_java(env)
    }).resolve::<jni::errors::ThrowRuntimeExAndDefault>()
}

#[jni_mangle("Ebi")]
pub fn conformance_jensen_shannon<'caller>(mut unowned_env: EnvUnowned<'caller>, _class: JClass<'caller>, arg0: JString<'caller>, arg1: JString<'caller>) -> JString<'caller> {
    unowned_env.with_env(|env| -> Result<_, jni::errors::Error> {
        ebi_objects::ebi_arithmetic::exact::set_exact_globally(false);
        let command: &&EbiCommand = &&crate::ebi_commands::ebi_command_conformance::EBI_CONFORMANCE_JSSC;
        let input_types = match **command {
            EbiCommand::Command { input_types, .. } => input_types,
            _ => {
                env.throw("Expected a command.")?;
                return Ok(JString::null())
            },
        };

        let input0 = match import_or_load(arg0, input_types[0], 0).with_context(|| anyhow!("Could not import argument 0")) {
                    Ok(x) => x,
                    Err(err) => {
                        env.throw(err.to_string().as_str())?;
                        return Ok(JString::null())
                    }
                };
    
let input1 = match import_or_load(arg1, input_types[1], 1).with_context(|| anyhow!("Could not import argument 1")) {
                    Ok(x) => x,
                    Err(err) => {
                        env.throw(err.to_string().as_str())?;
                        return Ok(JString::null())
                    }
                };
    

        let inputs = vec![input0, input1];

        // Execute the command.
        let output = match command.execute_with_inputs(inputs) {
            Ok(result) => result,
            Err(err) => {
                env.throw(format!("Command error: {}", err.to_string().as_str()))?;
                return Ok(JString::null());
            }
        };
        
        output.export_to_java(env)
    }).resolve::<jni::errors::ThrowRuntimeExAndDefault>()
}

#[jni_mangle("Ebi")]
pub fn conformance_jensen_shannon_sample<'caller>(mut unowned_env: EnvUnowned<'caller>, _class: JClass<'caller>, arg0: JString<'caller>, arg1: JString<'caller>, arg2: JString<'caller>) -> JString<'caller> {
    unowned_env.with_env(|env| -> Result<_, jni::errors::Error> {
        ebi_objects::ebi_arithmetic::exact::set_exact_globally(false);
        let command: &&EbiCommand = &&crate::ebi_commands::ebi_command_conformance::EBI_CONFORMANCE_JSSC_SAMPLE;
        let input_types = match **command {
            EbiCommand::Command { input_types, .. } => input_types,
            _ => {
                env.throw("Expected a command.")?;
                return Ok(JString::null())
            },
        };

        let input0 = match import_or_load(arg0, input_types[0], 0).with_context(|| anyhow!("Could not import argument 0")) {
                    Ok(x) => x,
                    Err(err) => {
                        env.throw(err.to_string().as_str())?;
                        return Ok(JString::null())
                    }
                };
    
let input1 = match import_or_load(arg1, input_types[1], 1).with_context(|| anyhow!("Could not import argument 1")) {
                    Ok(x) => x,
                    Err(err) => {
                        env.throw(err.to_string().as_str())?;
                        return Ok(JString::null())
                    }
                };
    
let input2 = match import_or_load(arg2, input_types[2], 2).with_context(|| anyhow!("Could not import argument 2")) {
                    Ok(x) => x,
                    Err(err) => {
                        env.throw(err.to_string().as_str())?;
                        return Ok(JString::null())
                    }
                };
    

        let inputs = vec![input0, input1, input2];

        // Execute the command.
        let output = match command.execute_with_inputs(inputs) {
            Ok(result) => result,
            Err(err) => {
                env.throw(format!("Command error: {}", err.to_string().as_str()))?;
                return Ok(JString::null());
            }
        };
        
        output.export_to_java(env)
    }).resolve::<jni::errors::ThrowRuntimeExAndDefault>()
}

#[jni_mangle("Ebi")]
pub fn conformance_markovian<'caller>(mut unowned_env: EnvUnowned<'caller>, _class: JClass<'caller>, arg0: JString<'caller>, arg1: JString<'caller>, arg2: JString<'caller>, arg3: JString<'caller>) -> JString<'caller> {
    unowned_env.with_env(|env| -> Result<_, jni::errors::Error> {
        ebi_objects::ebi_arithmetic::exact::set_exact_globally(true);
        let command: &&EbiCommand = &&crate::ebi_commands::ebi_command_conformance::EBI_CONFORMANCE_MARKOVIAN;
        let input_types = match **command {
            EbiCommand::Command { input_types, .. } => input_types,
            _ => {
                env.throw("Expected a command.")?;
                return Ok(JString::null())
            },
        };

        let input0 = match import_or_load(arg0, input_types[0], 0).with_context(|| anyhow!("Could not import argument 0")) {
                    Ok(x) => x,
                    Err(err) => {
                        env.throw(err.to_string().as_str())?;
                        return Ok(JString::null())
                    }
                };
    
let input1 = match import_or_load(arg1, input_types[1], 1).with_context(|| anyhow!("Could not import argument 1")) {
                    Ok(x) => x,
                    Err(err) => {
                        env.throw(err.to_string().as_str())?;
                        return Ok(JString::null())
                    }
                };
    
let input2 = match import_or_load(arg2, input_types[2], 2).with_context(|| anyhow!("Could not import argument 2")) {
                    Ok(x) => x,
                    Err(err) => {
                        env.throw(err.to_string().as_str())?;
                        return Ok(JString::null())
                    }
                };
    
let input3 = match import_or_load(arg3, input_types[3], 3).with_context(|| anyhow!("Could not import argument 3")) {
                    Ok(x) => x,
                    Err(err) => {
                        env.throw(err.to_string().as_str())?;
                        return Ok(JString::null())
                    }
                };
    

        let inputs = vec![input0, input1, input2, input3];

        // Execute the command.
        let output = match command.execute_with_inputs(inputs) {
            Ok(result) => result,
            Err(err) => {
                env.throw(format!("Command error: {}", err.to_string().as_str()))?;
                return Ok(JString::null());
            }
        };
        
        output.export_to_java(env)
    }).resolve::<jni::errors::ThrowRuntimeExAndDefault>()
}

#[jni_mangle("Ebi")]
pub fn conformance_unit_earth_movers<'caller>(mut unowned_env: EnvUnowned<'caller>, _class: JClass<'caller>, arg0: JString<'caller>, arg1: JString<'caller>) -> JString<'caller> {
    unowned_env.with_env(|env| -> Result<_, jni::errors::Error> {
        ebi_objects::ebi_arithmetic::exact::set_exact_globally(true);
        let command: &&EbiCommand = &&crate::ebi_commands::ebi_command_conformance::EBI_CONFORMANCE_UEMSC;
        let input_types = match **command {
            EbiCommand::Command { input_types, .. } => input_types,
            _ => {
                env.throw("Expected a command.")?;
                return Ok(JString::null())
            },
        };

        let input0 = match import_or_load(arg0, input_types[0], 0).with_context(|| anyhow!("Could not import argument 0")) {
                    Ok(x) => x,
                    Err(err) => {
                        env.throw(err.to_string().as_str())?;
                        return Ok(JString::null())
                    }
                };
    
let input1 = match import_or_load(arg1, input_types[1], 1).with_context(|| anyhow!("Could not import argument 1")) {
                    Ok(x) => x,
                    Err(err) => {
                        env.throw(err.to_string().as_str())?;
                        return Ok(JString::null())
                    }
                };
    

        let inputs = vec![input0, input1];

        // Execute the command.
        let output = match command.execute_with_inputs(inputs) {
            Ok(result) => result,
            Err(err) => {
                env.throw(format!("Command error: {}", err.to_string().as_str()))?;
                return Ok(JString::null());
            }
        };
        
        output.export_to_java(env)
    }).resolve::<jni::errors::ThrowRuntimeExAndDefault>()
}

#[jni_mangle("Ebi")]
pub fn conformance_unit_earth_movers_sample<'caller>(mut unowned_env: EnvUnowned<'caller>, _class: JClass<'caller>, arg0: JString<'caller>, arg1: JString<'caller>, arg2: JString<'caller>) -> JString<'caller> {
    unowned_env.with_env(|env| -> Result<_, jni::errors::Error> {
        ebi_objects::ebi_arithmetic::exact::set_exact_globally(true);
        let command: &&EbiCommand = &&crate::ebi_commands::ebi_command_conformance::EBI_CONFORMANCE_UEMSC_SAMPLE;
        let input_types = match **command {
            EbiCommand::Command { input_types, .. } => input_types,
            _ => {
                env.throw("Expected a command.")?;
                return Ok(JString::null())
            },
        };

        let input0 = match import_or_load(arg0, input_types[0], 0).with_context(|| anyhow!("Could not import argument 0")) {
                    Ok(x) => x,
                    Err(err) => {
                        env.throw(err.to_string().as_str())?;
                        return Ok(JString::null())
                    }
                };
    
let input1 = match import_or_load(arg1, input_types[1], 1).with_context(|| anyhow!("Could not import argument 1")) {
                    Ok(x) => x,
                    Err(err) => {
                        env.throw(err.to_string().as_str())?;
                        return Ok(JString::null())
                    }
                };
    
let input2 = match import_or_load(arg2, input_types[2], 2).with_context(|| anyhow!("Could not import argument 2")) {
                    Ok(x) => x,
                    Err(err) => {
                        env.throw(err.to_string().as_str())?;
                        return Ok(JString::null())
                    }
                };
    

        let inputs = vec![input0, input1, input2];

        // Execute the command.
        let output = match command.execute_with_inputs(inputs) {
            Ok(result) => result,
            Err(err) => {
                env.throw(format!("Command error: {}", err.to_string().as_str()))?;
                return Ok(JString::null());
            }
        };
        
        output.export_to_java(env)
    }).resolve::<jni::errors::ThrowRuntimeExAndDefault>()
}

#[jni_mangle("Ebi")]
pub fn conformance_non_stochastic_alignments<'caller>(mut unowned_env: EnvUnowned<'caller>, _class: JClass<'caller>, arg0: JString<'caller>, arg1: JString<'caller>) -> JString<'caller> {
    unowned_env.with_env(|env| -> Result<_, jni::errors::Error> {
        ebi_objects::ebi_arithmetic::exact::set_exact_globally(true);
        let command: &&EbiCommand = &&crate::ebi_commands::ebi_command_conformance_non_stochastic::EBI_CONFORMANCE_NON_STOCHASTIC_ALIGNMENTS;
        let input_types = match **command {
            EbiCommand::Command { input_types, .. } => input_types,
            _ => {
                env.throw("Expected a command.")?;
                return Ok(JString::null())
            },
        };

        let input0 = match import_or_load(arg0, input_types[0], 0).with_context(|| anyhow!("Could not import argument 0")) {
                    Ok(x) => x,
                    Err(err) => {
                        env.throw(err.to_string().as_str())?;
                        return Ok(JString::null())
                    }
                };
    
let input1 = match import_or_load(arg1, input_types[1], 1).with_context(|| anyhow!("Could not import argument 1")) {
                    Ok(x) => x,
                    Err(err) => {
                        env.throw(err.to_string().as_str())?;
                        return Ok(JString::null())
                    }
                };
    

        let inputs = vec![input0, input1];

        // Execute the command.
        let output = match command.execute_with_inputs(inputs) {
            Ok(result) => result,
            Err(err) => {
                env.throw(format!("Command error: {}", err.to_string().as_str()))?;
                return Ok(JString::null());
            }
        };
        
        output.export_to_java(env)
    }).resolve::<jni::errors::ThrowRuntimeExAndDefault>()
}

#[jni_mangle("Ebi")]
pub fn conformance_non_stochastic_escaping_edges_precision<'caller>(mut unowned_env: EnvUnowned<'caller>, _class: JClass<'caller>, arg0: JString<'caller>, arg1: JString<'caller>) -> JString<'caller> {
    unowned_env.with_env(|env| -> Result<_, jni::errors::Error> {
        ebi_objects::ebi_arithmetic::exact::set_exact_globally(true);
        let command: &&EbiCommand = &&crate::ebi_commands::ebi_command_conformance_non_stochastic::EBI_CONFORMANCE_NON_STOCHASTIC_ESCAPING_EDGES_PRECISION;
        let input_types = match **command {
            EbiCommand::Command { input_types, .. } => input_types,
            _ => {
                env.throw("Expected a command.")?;
                return Ok(JString::null())
            },
        };

        let input0 = match import_or_load(arg0, input_types[0], 0).with_context(|| anyhow!("Could not import argument 0")) {
                    Ok(x) => x,
                    Err(err) => {
                        env.throw(err.to_string().as_str())?;
                        return Ok(JString::null())
                    }
                };
    
let input1 = match import_or_load(arg1, input_types[1], 1).with_context(|| anyhow!("Could not import argument 1")) {
                    Ok(x) => x,
                    Err(err) => {
                        env.throw(err.to_string().as_str())?;
                        return Ok(JString::null())
                    }
                };
    

        let inputs = vec![input0, input1];

        // Execute the command.
        let output = match command.execute_with_inputs(inputs) {
            Ok(result) => result,
            Err(err) => {
                env.throw(format!("Command error: {}", err.to_string().as_str()))?;
                return Ok(JString::null());
            }
        };
        
        output.export_to_java(env)
    }).resolve::<jni::errors::ThrowRuntimeExAndDefault>()
}

#[jni_mangle("Ebi")]
pub fn conformance_non_stochastic_fitting_traces<'caller>(mut unowned_env: EnvUnowned<'caller>, _class: JClass<'caller>, arg0: JString<'caller>) -> JString<'caller> {
    unowned_env.with_env(|env| -> Result<_, jni::errors::Error> {
        ebi_objects::ebi_arithmetic::exact::set_exact_globally(true);
        let command: &&EbiCommand = &&crate::ebi_commands::ebi_command_conformance_non_stochastic::EBI_CONFORMANCE_NON_STOCHASTIC_FITTING_TRACES;
        let input_types = match **command {
            EbiCommand::Command { input_types, .. } => input_types,
            _ => {
                env.throw("Expected a command.")?;
                return Ok(JString::null())
            },
        };

        let input0 = match import_or_load(arg0, input_types[0], 0).with_context(|| anyhow!("Could not import argument 0")) {
                    Ok(x) => x,
                    Err(err) => {
                        env.throw(err.to_string().as_str())?;
                        return Ok(JString::null())
                    }
                };
    

        let inputs = vec![input0];

        // Execute the command.
        let output = match command.execute_with_inputs(inputs) {
            Ok(result) => result,
            Err(err) => {
                env.throw(format!("Command error: {}", err.to_string().as_str()))?;
                return Ok(JString::null());
            }
        };
        
        output.export_to_java(env)
    }).resolve::<jni::errors::ThrowRuntimeExAndDefault>()
}

#[jni_mangle("Ebi")]
pub fn conformance_non_stochastic_set_alignments<'caller>(mut unowned_env: EnvUnowned<'caller>, _class: JClass<'caller>, arg0: JString<'caller>, arg1: JString<'caller>) -> JString<'caller> {
    unowned_env.with_env(|env| -> Result<_, jni::errors::Error> {
        ebi_objects::ebi_arithmetic::exact::set_exact_globally(true);
        let command: &&EbiCommand = &&crate::ebi_commands::ebi_command_conformance_non_stochastic::EBI_CONFORMANCE_NON_STOCHASTIC_SET_ALIGNMENTS;
        let input_types = match **command {
            EbiCommand::Command { input_types, .. } => input_types,
            _ => {
                env.throw("Expected a command.")?;
                return Ok(JString::null())
            },
        };

        let input0 = match import_or_load(arg0, input_types[0], 0).with_context(|| anyhow!("Could not import argument 0")) {
                    Ok(x) => x,
                    Err(err) => {
                        env.throw(err.to_string().as_str())?;
                        return Ok(JString::null())
                    }
                };
    
let input1 = match import_or_load(arg1, input_types[1], 1).with_context(|| anyhow!("Could not import argument 1")) {
                    Ok(x) => x,
                    Err(err) => {
                        env.throw(err.to_string().as_str())?;
                        return Ok(JString::null())
                    }
                };
    

        let inputs = vec![input0, input1];

        // Execute the command.
        let output = match command.execute_with_inputs(inputs) {
            Ok(result) => result,
            Err(err) => {
                env.throw(format!("Command error: {}", err.to_string().as_str()))?;
                return Ok(JString::null());
            }
        };
        
        output.export_to_java(env)
    }).resolve::<jni::errors::ThrowRuntimeExAndDefault>()
}

#[jni_mangle("Ebi")]
pub fn conformance_non_stochastic_trace_fitness<'caller>(mut unowned_env: EnvUnowned<'caller>, _class: JClass<'caller>, arg0: JString<'caller>) -> JString<'caller> {
    unowned_env.with_env(|env| -> Result<_, jni::errors::Error> {
        ebi_objects::ebi_arithmetic::exact::set_exact_globally(true);
        let command: &&EbiCommand = &&crate::ebi_commands::ebi_command_conformance_non_stochastic::EBI_CONFORMANCE_NON_STOCHASTIC_TRACE_FITNESS;
        let input_types = match **command {
            EbiCommand::Command { input_types, .. } => input_types,
            _ => {
                env.throw("Expected a command.")?;
                return Ok(JString::null())
            },
        };

        let input0 = match import_or_load(arg0, input_types[0], 0).with_context(|| anyhow!("Could not import argument 0")) {
                    Ok(x) => x,
                    Err(err) => {
                        env.throw(err.to_string().as_str())?;
                        return Ok(JString::null())
                    }
                };
    

        let inputs = vec![input0];

        // Execute the command.
        let output = match command.execute_with_inputs(inputs) {
            Ok(result) => result,
            Err(err) => {
                env.throw(format!("Command error: {}", err.to_string().as_str()))?;
                return Ok(JString::null());
            }
        };
        
        output.export_to_java(env)
    }).resolve::<jni::errors::ThrowRuntimeExAndDefault>()
}

#[jni_mangle("Ebi")]
pub fn convert_business_process_model_and_notation<'caller>(mut unowned_env: EnvUnowned<'caller>, _class: JClass<'caller>, arg0: JString<'caller>) -> JString<'caller> {
    unowned_env.with_env(|env| -> Result<_, jni::errors::Error> {
        ebi_objects::ebi_arithmetic::exact::set_exact_globally(true);
        let command: &&EbiCommand = &&crate::ebi_commands::ebi_command_convert::EBI_CONVERT_BPMN;
        let input_types = match **command {
            EbiCommand::Command { input_types, .. } => input_types,
            _ => {
                env.throw("Expected a command.")?;
                return Ok(JString::null())
            },
        };

        let input0 = match import_or_load(arg0, input_types[0], 0).with_context(|| anyhow!("Could not import argument 0")) {
                    Ok(x) => x,
                    Err(err) => {
                        env.throw(err.to_string().as_str())?;
                        return Ok(JString::null())
                    }
                };
    

        let inputs = vec![input0];

        // Execute the command.
        let output = match command.execute_with_inputs(inputs) {
            Ok(result) => result,
            Err(err) => {
                env.throw(format!("Command error: {}", err.to_string().as_str()))?;
                return Ok(JString::null());
            }
        };
        
        output.export_to_java(env)
    }).resolve::<jni::errors::ThrowRuntimeExAndDefault>()
}

#[jni_mangle("Ebi")]
pub fn convert_finite_language<'caller>(mut unowned_env: EnvUnowned<'caller>, _class: JClass<'caller>, arg0: JString<'caller>) -> JString<'caller> {
    unowned_env.with_env(|env| -> Result<_, jni::errors::Error> {
        ebi_objects::ebi_arithmetic::exact::set_exact_globally(true);
        let command: &&EbiCommand = &&crate::ebi_commands::ebi_command_convert::EBI_CONVERT_LANG;
        let input_types = match **command {
            EbiCommand::Command { input_types, .. } => input_types,
            _ => {
                env.throw("Expected a command.")?;
                return Ok(JString::null())
            },
        };

        let input0 = match import_or_load(arg0, input_types[0], 0).with_context(|| anyhow!("Could not import argument 0")) {
                    Ok(x) => x,
                    Err(err) => {
                        env.throw(err.to_string().as_str())?;
                        return Ok(JString::null())
                    }
                };
    

        let inputs = vec![input0];

        // Execute the command.
        let output = match command.execute_with_inputs(inputs) {
            Ok(result) => result,
            Err(err) => {
                env.throw(format!("Command error: {}", err.to_string().as_str()))?;
                return Ok(JString::null());
            }
        };
        
        output.export_to_java(env)
    }).resolve::<jni::errors::ThrowRuntimeExAndDefault>()
}

#[jni_mangle("Ebi")]
pub fn convert_finite_stochastic_language<'caller>(mut unowned_env: EnvUnowned<'caller>, _class: JClass<'caller>, arg0: JString<'caller>) -> JString<'caller> {
    unowned_env.with_env(|env| -> Result<_, jni::errors::Error> {
        ebi_objects::ebi_arithmetic::exact::set_exact_globally(true);
        let command: &&EbiCommand = &&crate::ebi_commands::ebi_command_convert::EBI_CONVERT_SLANG;
        let input_types = match **command {
            EbiCommand::Command { input_types, .. } => input_types,
            _ => {
                env.throw("Expected a command.")?;
                return Ok(JString::null())
            },
        };

        let input0 = match import_or_load(arg0, input_types[0], 0).with_context(|| anyhow!("Could not import argument 0")) {
                    Ok(x) => x,
                    Err(err) => {
                        env.throw(err.to_string().as_str())?;
                        return Ok(JString::null())
                    }
                };
    

        let inputs = vec![input0];

        // Execute the command.
        let output = match command.execute_with_inputs(inputs) {
            Ok(result) => result,
            Err(err) => {
                env.throw(format!("Command error: {}", err.to_string().as_str()))?;
                return Ok(JString::null());
            }
        };
        
        output.export_to_java(env)
    }).resolve::<jni::errors::ThrowRuntimeExAndDefault>()
}

#[jni_mangle("Ebi")]
pub fn convert_labelled_petri_net<'caller>(mut unowned_env: EnvUnowned<'caller>, _class: JClass<'caller>, arg0: JString<'caller>) -> JString<'caller> {
    unowned_env.with_env(|env| -> Result<_, jni::errors::Error> {
        ebi_objects::ebi_arithmetic::exact::set_exact_globally(true);
        let command: &&EbiCommand = &&crate::ebi_commands::ebi_command_convert::EBI_CONVERT_LPN;
        let input_types = match **command {
            EbiCommand::Command { input_types, .. } => input_types,
            _ => {
                env.throw("Expected a command.")?;
                return Ok(JString::null())
            },
        };

        let input0 = match import_or_load(arg0, input_types[0], 0).with_context(|| anyhow!("Could not import argument 0")) {
                    Ok(x) => x,
                    Err(err) => {
                        env.throw(err.to_string().as_str())?;
                        return Ok(JString::null())
                    }
                };
    

        let inputs = vec![input0];

        // Execute the command.
        let output = match command.execute_with_inputs(inputs) {
            Ok(result) => result,
            Err(err) => {
                env.throw(format!("Command error: {}", err.to_string().as_str()))?;
                return Ok(JString::null());
            }
        };
        
        output.export_to_java(env)
    }).resolve::<jni::errors::ThrowRuntimeExAndDefault>()
}

#[jni_mangle("Ebi")]
pub fn convert_log<'caller>(mut unowned_env: EnvUnowned<'caller>, _class: JClass<'caller>, arg0: JString<'caller>) -> JString<'caller> {
    unowned_env.with_env(|env| -> Result<_, jni::errors::Error> {
        ebi_objects::ebi_arithmetic::exact::set_exact_globally(true);
        let command: &&EbiCommand = &&crate::ebi_commands::ebi_command_convert::EBI_CONVERT_LOG;
        let input_types = match **command {
            EbiCommand::Command { input_types, .. } => input_types,
            _ => {
                env.throw("Expected a command.")?;
                return Ok(JString::null())
            },
        };

        let input0 = match import_or_load(arg0, input_types[0], 0).with_context(|| anyhow!("Could not import argument 0")) {
                    Ok(x) => x,
                    Err(err) => {
                        env.throw(err.to_string().as_str())?;
                        return Ok(JString::null())
                    }
                };
    

        let inputs = vec![input0];

        // Execute the command.
        let output = match command.execute_with_inputs(inputs) {
            Ok(result) => result,
            Err(err) => {
                env.throw(format!("Command error: {}", err.to_string().as_str()))?;
                return Ok(JString::null());
            }
        };
        
        output.export_to_java(env)
    }).resolve::<jni::errors::ThrowRuntimeExAndDefault>()
}

#[jni_mangle("Ebi")]
pub fn convert_stochastic_deterministic_finite_automaton<'caller>(mut unowned_env: EnvUnowned<'caller>, _class: JClass<'caller>, arg0: JString<'caller>) -> JString<'caller> {
    unowned_env.with_env(|env| -> Result<_, jni::errors::Error> {
        ebi_objects::ebi_arithmetic::exact::set_exact_globally(true);
        let command: &&EbiCommand = &&crate::ebi_commands::ebi_command_convert::EBI_CONVERT_SDFA;
        let input_types = match **command {
            EbiCommand::Command { input_types, .. } => input_types,
            _ => {
                env.throw("Expected a command.")?;
                return Ok(JString::null())
            },
        };

        let input0 = match import_or_load(arg0, input_types[0], 0).with_context(|| anyhow!("Could not import argument 0")) {
                    Ok(x) => x,
                    Err(err) => {
                        env.throw(err.to_string().as_str())?;
                        return Ok(JString::null())
                    }
                };
    

        let inputs = vec![input0];

        // Execute the command.
        let output = match command.execute_with_inputs(inputs) {
            Ok(result) => result,
            Err(err) => {
                env.throw(format!("Command error: {}", err.to_string().as_str()))?;
                return Ok(JString::null());
            }
        };
        
        output.export_to_java(env)
    }).resolve::<jni::errors::ThrowRuntimeExAndDefault>()
}

#[jni_mangle("Ebi")]
pub fn convert_stochastic_directly_follows_model<'caller>(mut unowned_env: EnvUnowned<'caller>, _class: JClass<'caller>, arg0: JString<'caller>) -> JString<'caller> {
    unowned_env.with_env(|env| -> Result<_, jni::errors::Error> {
        ebi_objects::ebi_arithmetic::exact::set_exact_globally(true);
        let command: &&EbiCommand = &&crate::ebi_commands::ebi_command_convert::EBI_CONVERT_SDFM;
        let input_types = match **command {
            EbiCommand::Command { input_types, .. } => input_types,
            _ => {
                env.throw("Expected a command.")?;
                return Ok(JString::null())
            },
        };

        let input0 = match import_or_load(arg0, input_types[0], 0).with_context(|| anyhow!("Could not import argument 0")) {
                    Ok(x) => x,
                    Err(err) => {
                        env.throw(err.to_string().as_str())?;
                        return Ok(JString::null())
                    }
                };
    

        let inputs = vec![input0];

        // Execute the command.
        let output = match command.execute_with_inputs(inputs) {
            Ok(result) => result,
            Err(err) => {
                env.throw(format!("Command error: {}", err.to_string().as_str()))?;
                return Ok(JString::null());
            }
        };
        
        output.export_to_java(env)
    }).resolve::<jni::errors::ThrowRuntimeExAndDefault>()
}

#[jni_mangle("Ebi")]
pub fn convert_stochastic_labelled_petri_net<'caller>(mut unowned_env: EnvUnowned<'caller>, _class: JClass<'caller>, arg0: JString<'caller>) -> JString<'caller> {
    unowned_env.with_env(|env| -> Result<_, jni::errors::Error> {
        ebi_objects::ebi_arithmetic::exact::set_exact_globally(true);
        let command: &&EbiCommand = &&crate::ebi_commands::ebi_command_convert::EBI_CONVERT_SLPN;
        let input_types = match **command {
            EbiCommand::Command { input_types, .. } => input_types,
            _ => {
                env.throw("Expected a command.")?;
                return Ok(JString::null())
            },
        };

        let input0 = match import_or_load(arg0, input_types[0], 0).with_context(|| anyhow!("Could not import argument 0")) {
                    Ok(x) => x,
                    Err(err) => {
                        env.throw(err.to_string().as_str())?;
                        return Ok(JString::null())
                    }
                };
    

        let inputs = vec![input0];

        // Execute the command.
        let output = match command.execute_with_inputs(inputs) {
            Ok(result) => result,
            Err(err) => {
                env.throw(format!("Command error: {}", err.to_string().as_str()))?;
                return Ok(JString::null());
            }
        };
        
        output.export_to_java(env)
    }).resolve::<jni::errors::ThrowRuntimeExAndDefault>()
}

#[jni_mangle("Ebi")]
pub fn convert_stochastic_nondeterministic_finite_automaton<'caller>(mut unowned_env: EnvUnowned<'caller>, _class: JClass<'caller>, arg0: JString<'caller>) -> JString<'caller> {
    unowned_env.with_env(|env| -> Result<_, jni::errors::Error> {
        ebi_objects::ebi_arithmetic::exact::set_exact_globally(true);
        let command: &&EbiCommand = &&crate::ebi_commands::ebi_command_convert::EBI_CONVERT_SNFA;
        let input_types = match **command {
            EbiCommand::Command { input_types, .. } => input_types,
            _ => {
                env.throw("Expected a command.")?;
                return Ok(JString::null())
            },
        };

        let input0 = match import_or_load(arg0, input_types[0], 0).with_context(|| anyhow!("Could not import argument 0")) {
                    Ok(x) => x,
                    Err(err) => {
                        env.throw(err.to_string().as_str())?;
                        return Ok(JString::null())
                    }
                };
    

        let inputs = vec![input0];

        // Execute the command.
        let output = match command.execute_with_inputs(inputs) {
            Ok(result) => result,
            Err(err) => {
                env.throw(format!("Command error: {}", err.to_string().as_str()))?;
                return Ok(JString::null());
            }
        };
        
        output.export_to_java(env)
    }).resolve::<jni::errors::ThrowRuntimeExAndDefault>()
}

#[jni_mangle("Ebi")]
pub fn discover_alergia<'caller>(mut unowned_env: EnvUnowned<'caller>, _class: JClass<'caller>, arg0: JString<'caller>, arg1: JString<'caller>) -> JString<'caller> {
    unowned_env.with_env(|env| -> Result<_, jni::errors::Error> {
        ebi_objects::ebi_arithmetic::exact::set_exact_globally(true);
        let command: &&EbiCommand = &&crate::ebi_commands::ebi_command_discover::EBI_DISCOVER_ALERGIA;
        let input_types = match **command {
            EbiCommand::Command { input_types, .. } => input_types,
            _ => {
                env.throw("Expected a command.")?;
                return Ok(JString::null())
            },
        };

        let input0 = match import_or_load(arg0, input_types[0], 0).with_context(|| anyhow!("Could not import argument 0")) {
                    Ok(x) => x,
                    Err(err) => {
                        env.throw(err.to_string().as_str())?;
                        return Ok(JString::null())
                    }
                };
    
let input1 = match import_or_load(arg1, input_types[1], 1).with_context(|| anyhow!("Could not import argument 1")) {
                    Ok(x) => x,
                    Err(err) => {
                        env.throw(err.to_string().as_str())?;
                        return Ok(JString::null())
                    }
                };
    

        let inputs = vec![input0, input1];

        // Execute the command.
        let output = match command.execute_with_inputs(inputs) {
            Ok(result) => result,
            Err(err) => {
                env.throw(format!("Command error: {}", err.to_string().as_str()))?;
                return Ok(JString::null());
            }
        };
        
        output.export_to_java(env)
    }).resolve::<jni::errors::ThrowRuntimeExAndDefault>()
}

#[jni_mangle("Ebi")]
pub fn discover_alignments_stochastic_business_process_model_and_notation<'caller>(mut unowned_env: EnvUnowned<'caller>, _class: JClass<'caller>, arg0: JString<'caller>, arg1: JString<'caller>) -> JString<'caller> {
    unowned_env.with_env(|env| -> Result<_, jni::errors::Error> {
        ebi_objects::ebi_arithmetic::exact::set_exact_globally(true);
        let command: &&EbiCommand = &&crate::ebi_commands::ebi_command_discover::EBI_DISCOVER_ALIGNMENTS_BPMN;
        let input_types = match **command {
            EbiCommand::Command { input_types, .. } => input_types,
            _ => {
                env.throw("Expected a command.")?;
                return Ok(JString::null())
            },
        };

        let input0 = match import_or_load(arg0, input_types[0], 0).with_context(|| anyhow!("Could not import argument 0")) {
                    Ok(x) => x,
                    Err(err) => {
                        env.throw(err.to_string().as_str())?;
                        return Ok(JString::null())
                    }
                };
    
let input1 = match import_or_load(arg1, input_types[1], 1).with_context(|| anyhow!("Could not import argument 1")) {
                    Ok(x) => x,
                    Err(err) => {
                        env.throw(err.to_string().as_str())?;
                        return Ok(JString::null())
                    }
                };
    

        let inputs = vec![input0, input1];

        // Execute the command.
        let output = match command.execute_with_inputs(inputs) {
            Ok(result) => result,
            Err(err) => {
                env.throw(format!("Command error: {}", err.to_string().as_str()))?;
                return Ok(JString::null());
            }
        };
        
        output.export_to_java(env)
    }).resolve::<jni::errors::ThrowRuntimeExAndDefault>()
}

#[jni_mangle("Ebi")]
pub fn discover_alignments_stochastic_labelled_petri_nets<'caller>(mut unowned_env: EnvUnowned<'caller>, _class: JClass<'caller>, arg0: JString<'caller>, arg1: JString<'caller>) -> JString<'caller> {
    unowned_env.with_env(|env| -> Result<_, jni::errors::Error> {
        ebi_objects::ebi_arithmetic::exact::set_exact_globally(true);
        let command: &&EbiCommand = &&crate::ebi_commands::ebi_command_discover::EBI_DISCOVER_ALIGNMENTS_SLPN;
        let input_types = match **command {
            EbiCommand::Command { input_types, .. } => input_types,
            _ => {
                env.throw("Expected a command.")?;
                return Ok(JString::null())
            },
        };

        let input0 = match import_or_load(arg0, input_types[0], 0).with_context(|| anyhow!("Could not import argument 0")) {
                    Ok(x) => x,
                    Err(err) => {
                        env.throw(err.to_string().as_str())?;
                        return Ok(JString::null())
                    }
                };
    
let input1 = match import_or_load(arg1, input_types[1], 1).with_context(|| anyhow!("Could not import argument 1")) {
                    Ok(x) => x,
                    Err(err) => {
                        env.throw(err.to_string().as_str())?;
                        return Ok(JString::null())
                    }
                };
    

        let inputs = vec![input0, input1];

        // Execute the command.
        let output = match command.execute_with_inputs(inputs) {
            Ok(result) => result,
            Err(err) => {
                env.throw(format!("Command error: {}", err.to_string().as_str()))?;
                return Ok(JString::null());
            }
        };
        
        output.export_to_java(env)
    }).resolve::<jni::errors::ThrowRuntimeExAndDefault>()
}

#[jni_mangle("Ebi")]
pub fn discover_can_it_be_done_optimally<'caller>(mut unowned_env: EnvUnowned<'caller>, _class: JClass<'caller>, arg0: JString<'caller>, arg1: JString<'caller>) -> JString<'caller> {
    unowned_env.with_env(|env| -> Result<_, jni::errors::Error> {
        ebi_objects::ebi_arithmetic::exact::set_exact_globally(true);
        let command: &&EbiCommand = &&crate::ebi_commands::ebi_command_discover::EBI_DISCOVER_CIBDO;
        let input_types = match **command {
            EbiCommand::Command { input_types, .. } => input_types,
            _ => {
                env.throw("Expected a command.")?;
                return Ok(JString::null())
            },
        };

        let input0 = match import_or_load(arg0, input_types[0], 0).with_context(|| anyhow!("Could not import argument 0")) {
                    Ok(x) => x,
                    Err(err) => {
                        env.throw(err.to_string().as_str())?;
                        return Ok(JString::null())
                    }
                };
    
let input1 = match import_or_load(arg1, input_types[1], 1).with_context(|| anyhow!("Could not import argument 1")) {
                    Ok(x) => x,
                    Err(err) => {
                        env.throw(err.to_string().as_str())?;
                        return Ok(JString::null())
                    }
                };
    

        let inputs = vec![input0, input1];

        // Execute the command.
        let output = match command.execute_with_inputs(inputs) {
            Ok(result) => result,
            Err(err) => {
                env.throw(format!("Command error: {}", err.to_string().as_str()))?;
                return Ok(JString::null());
            }
        };
        
        output.export_to_java(env)
    }).resolve::<jni::errors::ThrowRuntimeExAndDefault>()
}

#[jni_mangle("Ebi")]
pub fn discover_directly_follows_graph<'caller>(mut unowned_env: EnvUnowned<'caller>, _class: JClass<'caller>, arg0: JString<'caller>, arg1: JString<'caller>) -> JString<'caller> {
    unowned_env.with_env(|env| -> Result<_, jni::errors::Error> {
        ebi_objects::ebi_arithmetic::exact::set_exact_globally(true);
        let command: &&EbiCommand = &&crate::ebi_commands::ebi_command_discover::EBI_DISCOVER_DIRECTLY_FOLLOWS;
        let input_types = match **command {
            EbiCommand::Command { input_types, .. } => input_types,
            _ => {
                env.throw("Expected a command.")?;
                return Ok(JString::null())
            },
        };

        let input0 = match import_or_load(arg0, input_types[0], 0).with_context(|| anyhow!("Could not import argument 0")) {
                    Ok(x) => x,
                    Err(err) => {
                        env.throw(err.to_string().as_str())?;
                        return Ok(JString::null())
                    }
                };
    
let input1 = match import_or_load(arg1, input_types[1], 1).with_context(|| anyhow!("Could not import argument 1")) {
                    Ok(x) => x,
                    Err(err) => {
                        env.throw(err.to_string().as_str())?;
                        return Ok(JString::null())
                    }
                };
    

        let inputs = vec![input0, input1];

        // Execute the command.
        let output = match command.execute_with_inputs(inputs) {
            Ok(result) => result,
            Err(err) => {
                env.throw(format!("Command error: {}", err.to_string().as_str()))?;
                return Ok(JString::null());
            }
        };
        
        output.export_to_java(env)
    }).resolve::<jni::errors::ThrowRuntimeExAndDefault>()
}

#[jni_mangle("Ebi")]
pub fn discover_occurrence_stochastic_business_process_model_and_notation<'caller>(mut unowned_env: EnvUnowned<'caller>, _class: JClass<'caller>, arg0: JString<'caller>, arg1: JString<'caller>) -> JString<'caller> {
    unowned_env.with_env(|env| -> Result<_, jni::errors::Error> {
        ebi_objects::ebi_arithmetic::exact::set_exact_globally(true);
        let command: &&EbiCommand = &&crate::ebi_commands::ebi_command_discover::EBI_DISCOVER_OCCURRENCE_SBPMN;
        let input_types = match **command {
            EbiCommand::Command { input_types, .. } => input_types,
            _ => {
                env.throw("Expected a command.")?;
                return Ok(JString::null())
            },
        };

        let input0 = match import_or_load(arg0, input_types[0], 0).with_context(|| anyhow!("Could not import argument 0")) {
                    Ok(x) => x,
                    Err(err) => {
                        env.throw(err.to_string().as_str())?;
                        return Ok(JString::null())
                    }
                };
    
let input1 = match import_or_load(arg1, input_types[1], 1).with_context(|| anyhow!("Could not import argument 1")) {
                    Ok(x) => x,
                    Err(err) => {
                        env.throw(err.to_string().as_str())?;
                        return Ok(JString::null())
                    }
                };
    

        let inputs = vec![input0, input1];

        // Execute the command.
        let output = match command.execute_with_inputs(inputs) {
            Ok(result) => result,
            Err(err) => {
                env.throw(format!("Command error: {}", err.to_string().as_str()))?;
                return Ok(JString::null());
            }
        };
        
        output.export_to_java(env)
    }).resolve::<jni::errors::ThrowRuntimeExAndDefault>()
}

#[jni_mangle("Ebi")]
pub fn discover_occurrence_stochastic_labelled_petri_net<'caller>(mut unowned_env: EnvUnowned<'caller>, _class: JClass<'caller>, arg0: JString<'caller>, arg1: JString<'caller>) -> JString<'caller> {
    unowned_env.with_env(|env| -> Result<_, jni::errors::Error> {
        ebi_objects::ebi_arithmetic::exact::set_exact_globally(true);
        let command: &&EbiCommand = &&crate::ebi_commands::ebi_command_discover::EBI_DISCOVER_OCCURRENCE_SLPN;
        let input_types = match **command {
            EbiCommand::Command { input_types, .. } => input_types,
            _ => {
                env.throw("Expected a command.")?;
                return Ok(JString::null())
            },
        };

        let input0 = match import_or_load(arg0, input_types[0], 0).with_context(|| anyhow!("Could not import argument 0")) {
                    Ok(x) => x,
                    Err(err) => {
                        env.throw(err.to_string().as_str())?;
                        return Ok(JString::null())
                    }
                };
    
let input1 = match import_or_load(arg1, input_types[1], 1).with_context(|| anyhow!("Could not import argument 1")) {
                    Ok(x) => x,
                    Err(err) => {
                        env.throw(err.to_string().as_str())?;
                        return Ok(JString::null())
                    }
                };
    

        let inputs = vec![input0, input1];

        // Execute the command.
        let output = match command.execute_with_inputs(inputs) {
            Ok(result) => result,
            Err(err) => {
                env.throw(format!("Command error: {}", err.to_string().as_str()))?;
                return Ok(JString::null());
            }
        };
        
        output.export_to_java(env)
    }).resolve::<jni::errors::ThrowRuntimeExAndDefault>()
}

#[jni_mangle("Ebi")]
pub fn discover_occurrence_stochastic_process_tree<'caller>(mut unowned_env: EnvUnowned<'caller>, _class: JClass<'caller>, arg0: JString<'caller>, arg1: JString<'caller>) -> JString<'caller> {
    unowned_env.with_env(|env| -> Result<_, jni::errors::Error> {
        ebi_objects::ebi_arithmetic::exact::set_exact_globally(true);
        let command: &&EbiCommand = &&crate::ebi_commands::ebi_command_discover::EBI_DISCOVER_OCCURRENCE_SPTREE;
        let input_types = match **command {
            EbiCommand::Command { input_types, .. } => input_types,
            _ => {
                env.throw("Expected a command.")?;
                return Ok(JString::null())
            },
        };

        let input0 = match import_or_load(arg0, input_types[0], 0).with_context(|| anyhow!("Could not import argument 0")) {
                    Ok(x) => x,
                    Err(err) => {
                        env.throw(err.to_string().as_str())?;
                        return Ok(JString::null())
                    }
                };
    
let input1 = match import_or_load(arg1, input_types[1], 1).with_context(|| anyhow!("Could not import argument 1")) {
                    Ok(x) => x,
                    Err(err) => {
                        env.throw(err.to_string().as_str())?;
                        return Ok(JString::null())
                    }
                };
    

        let inputs = vec![input0, input1];

        // Execute the command.
        let output = match command.execute_with_inputs(inputs) {
            Ok(result) => result,
            Err(err) => {
                env.throw(format!("Command error: {}", err.to_string().as_str()))?;
                return Ok(JString::null());
            }
        };
        
        output.export_to_java(env)
    }).resolve::<jni::errors::ThrowRuntimeExAndDefault>()
}

#[jni_mangle("Ebi")]
pub fn discover_random_stochastic_business_process_model_and_notation<'caller>(mut unowned_env: EnvUnowned<'caller>, _class: JClass<'caller>, arg0: JString<'caller>) -> JString<'caller> {
    unowned_env.with_env(|env| -> Result<_, jni::errors::Error> {
        ebi_objects::ebi_arithmetic::exact::set_exact_globally(true);
        let command: &&EbiCommand = &&crate::ebi_commands::ebi_command_discover::EBI_DISCOVER_RANDOM_SBPMN;
        let input_types = match **command {
            EbiCommand::Command { input_types, .. } => input_types,
            _ => {
                env.throw("Expected a command.")?;
                return Ok(JString::null())
            },
        };

        let input0 = match import_or_load(arg0, input_types[0], 0).with_context(|| anyhow!("Could not import argument 0")) {
                    Ok(x) => x,
                    Err(err) => {
                        env.throw(err.to_string().as_str())?;
                        return Ok(JString::null())
                    }
                };
    

        let inputs = vec![input0];

        // Execute the command.
        let output = match command.execute_with_inputs(inputs) {
            Ok(result) => result,
            Err(err) => {
                env.throw(format!("Command error: {}", err.to_string().as_str()))?;
                return Ok(JString::null());
            }
        };
        
        output.export_to_java(env)
    }).resolve::<jni::errors::ThrowRuntimeExAndDefault>()
}

#[jni_mangle("Ebi")]
pub fn discover_random_stochastic_labelled_petri_net<'caller>(mut unowned_env: EnvUnowned<'caller>, _class: JClass<'caller>, arg0: JString<'caller>) -> JString<'caller> {
    unowned_env.with_env(|env| -> Result<_, jni::errors::Error> {
        ebi_objects::ebi_arithmetic::exact::set_exact_globally(true);
        let command: &&EbiCommand = &&crate::ebi_commands::ebi_command_discover::EBI_DISCOVER_RANDOM_SLPN;
        let input_types = match **command {
            EbiCommand::Command { input_types, .. } => input_types,
            _ => {
                env.throw("Expected a command.")?;
                return Ok(JString::null())
            },
        };

        let input0 = match import_or_load(arg0, input_types[0], 0).with_context(|| anyhow!("Could not import argument 0")) {
                    Ok(x) => x,
                    Err(err) => {
                        env.throw(err.to_string().as_str())?;
                        return Ok(JString::null())
                    }
                };
    

        let inputs = vec![input0];

        // Execute the command.
        let output = match command.execute_with_inputs(inputs) {
            Ok(result) => result,
            Err(err) => {
                env.throw(format!("Command error: {}", err.to_string().as_str()))?;
                return Ok(JString::null());
            }
        };
        
        output.export_to_java(env)
    }).resolve::<jni::errors::ThrowRuntimeExAndDefault>()
}

#[jni_mangle("Ebi")]
pub fn discover_random_stochastic_process_tree<'caller>(mut unowned_env: EnvUnowned<'caller>, _class: JClass<'caller>, arg0: JString<'caller>) -> JString<'caller> {
    unowned_env.with_env(|env| -> Result<_, jni::errors::Error> {
        ebi_objects::ebi_arithmetic::exact::set_exact_globally(true);
        let command: &&EbiCommand = &&crate::ebi_commands::ebi_command_discover::EBI_DISCOVER_RANDOM_SPTREE;
        let input_types = match **command {
            EbiCommand::Command { input_types, .. } => input_types,
            _ => {
                env.throw("Expected a command.")?;
                return Ok(JString::null())
            },
        };

        let input0 = match import_or_load(arg0, input_types[0], 0).with_context(|| anyhow!("Could not import argument 0")) {
                    Ok(x) => x,
                    Err(err) => {
                        env.throw(err.to_string().as_str())?;
                        return Ok(JString::null())
                    }
                };
    

        let inputs = vec![input0];

        // Execute the command.
        let output = match command.execute_with_inputs(inputs) {
            Ok(result) => result,
            Err(err) => {
                env.throw(format!("Command error: {}", err.to_string().as_str()))?;
                return Ok(JString::null());
            }
        };
        
        output.export_to_java(env)
    }).resolve::<jni::errors::ThrowRuntimeExAndDefault>()
}

#[jni_mangle("Ebi")]
pub fn discover_uniform_stochastic_business_process_model_and_notation<'caller>(mut unowned_env: EnvUnowned<'caller>, _class: JClass<'caller>, arg0: JString<'caller>) -> JString<'caller> {
    unowned_env.with_env(|env| -> Result<_, jni::errors::Error> {
        ebi_objects::ebi_arithmetic::exact::set_exact_globally(true);
        let command: &&EbiCommand = &&crate::ebi_commands::ebi_command_discover::EBI_DISCOVER_UNIFORM_SBPMN;
        let input_types = match **command {
            EbiCommand::Command { input_types, .. } => input_types,
            _ => {
                env.throw("Expected a command.")?;
                return Ok(JString::null())
            },
        };

        let input0 = match import_or_load(arg0, input_types[0], 0).with_context(|| anyhow!("Could not import argument 0")) {
                    Ok(x) => x,
                    Err(err) => {
                        env.throw(err.to_string().as_str())?;
                        return Ok(JString::null())
                    }
                };
    

        let inputs = vec![input0];

        // Execute the command.
        let output = match command.execute_with_inputs(inputs) {
            Ok(result) => result,
            Err(err) => {
                env.throw(format!("Command error: {}", err.to_string().as_str()))?;
                return Ok(JString::null());
            }
        };
        
        output.export_to_java(env)
    }).resolve::<jni::errors::ThrowRuntimeExAndDefault>()
}

#[jni_mangle("Ebi")]
pub fn discover_uniform_stochastic_labelled_petri_net<'caller>(mut unowned_env: EnvUnowned<'caller>, _class: JClass<'caller>, arg0: JString<'caller>) -> JString<'caller> {
    unowned_env.with_env(|env| -> Result<_, jni::errors::Error> {
        ebi_objects::ebi_arithmetic::exact::set_exact_globally(true);
        let command: &&EbiCommand = &&crate::ebi_commands::ebi_command_discover::EBI_DISCOVER_UNIFORM_SLPN;
        let input_types = match **command {
            EbiCommand::Command { input_types, .. } => input_types,
            _ => {
                env.throw("Expected a command.")?;
                return Ok(JString::null())
            },
        };

        let input0 = match import_or_load(arg0, input_types[0], 0).with_context(|| anyhow!("Could not import argument 0")) {
                    Ok(x) => x,
                    Err(err) => {
                        env.throw(err.to_string().as_str())?;
                        return Ok(JString::null())
                    }
                };
    

        let inputs = vec![input0];

        // Execute the command.
        let output = match command.execute_with_inputs(inputs) {
            Ok(result) => result,
            Err(err) => {
                env.throw(format!("Command error: {}", err.to_string().as_str()))?;
                return Ok(JString::null());
            }
        };
        
        output.export_to_java(env)
    }).resolve::<jni::errors::ThrowRuntimeExAndDefault>()
}

#[jni_mangle("Ebi")]
pub fn discover_uniform_stochastic_process_tree<'caller>(mut unowned_env: EnvUnowned<'caller>, _class: JClass<'caller>, arg0: JString<'caller>) -> JString<'caller> {
    unowned_env.with_env(|env| -> Result<_, jni::errors::Error> {
        ebi_objects::ebi_arithmetic::exact::set_exact_globally(true);
        let command: &&EbiCommand = &&crate::ebi_commands::ebi_command_discover::EBI_DISCOVER_UNIFORM_SPTREE;
        let input_types = match **command {
            EbiCommand::Command { input_types, .. } => input_types,
            _ => {
                env.throw("Expected a command.")?;
                return Ok(JString::null())
            },
        };

        let input0 = match import_or_load(arg0, input_types[0], 0).with_context(|| anyhow!("Could not import argument 0")) {
                    Ok(x) => x,
                    Err(err) => {
                        env.throw(err.to_string().as_str())?;
                        return Ok(JString::null())
                    }
                };
    

        let inputs = vec![input0];

        // Execute the command.
        let output = match command.execute_with_inputs(inputs) {
            Ok(result) => result,
            Err(err) => {
                env.throw(format!("Command error: {}", err.to_string().as_str()))?;
                return Ok(JString::null());
            }
        };
        
        output.export_to_java(env)
    }).resolve::<jni::errors::ThrowRuntimeExAndDefault>()
}

#[jni_mangle("Ebi")]
pub fn discover_non_stochastic_flower_deterministic_finite_automaton<'caller>(mut unowned_env: EnvUnowned<'caller>, _class: JClass<'caller>, arg0: JString<'caller>) -> JString<'caller> {
    unowned_env.with_env(|env| -> Result<_, jni::errors::Error> {
        ebi_objects::ebi_arithmetic::exact::set_exact_globally(true);
        let command: &&EbiCommand = &&crate::ebi_commands::ebi_command_discover_non_stochastic::EBI_DISCOVER_NON_STOCHASTIC_FLOWER_DFA;
        let input_types = match **command {
            EbiCommand::Command { input_types, .. } => input_types,
            _ => {
                env.throw("Expected a command.")?;
                return Ok(JString::null())
            },
        };

        let input0 = match import_or_load(arg0, input_types[0], 0).with_context(|| anyhow!("Could not import argument 0")) {
                    Ok(x) => x,
                    Err(err) => {
                        env.throw(err.to_string().as_str())?;
                        return Ok(JString::null())
                    }
                };
    

        let inputs = vec![input0];

        // Execute the command.
        let output = match command.execute_with_inputs(inputs) {
            Ok(result) => result,
            Err(err) => {
                env.throw(format!("Command error: {}", err.to_string().as_str()))?;
                return Ok(JString::null());
            }
        };
        
        output.export_to_java(env)
    }).resolve::<jni::errors::ThrowRuntimeExAndDefault>()
}

#[jni_mangle("Ebi")]
pub fn discover_non_stochastic_flower_process_tree<'caller>(mut unowned_env: EnvUnowned<'caller>, _class: JClass<'caller>, arg0: JString<'caller>) -> JString<'caller> {
    unowned_env.with_env(|env| -> Result<_, jni::errors::Error> {
        ebi_objects::ebi_arithmetic::exact::set_exact_globally(true);
        let command: &&EbiCommand = &&crate::ebi_commands::ebi_command_discover_non_stochastic::EBI_DISCOVER_NON_STOCHASTIC_FLOWER_TREE;
        let input_types = match **command {
            EbiCommand::Command { input_types, .. } => input_types,
            _ => {
                env.throw("Expected a command.")?;
                return Ok(JString::null())
            },
        };

        let input0 = match import_or_load(arg0, input_types[0], 0).with_context(|| anyhow!("Could not import argument 0")) {
                    Ok(x) => x,
                    Err(err) => {
                        env.throw(err.to_string().as_str())?;
                        return Ok(JString::null())
                    }
                };
    

        let inputs = vec![input0];

        // Execute the command.
        let output = match command.execute_with_inputs(inputs) {
            Ok(result) => result,
            Err(err) => {
                env.throw(format!("Command error: {}", err.to_string().as_str()))?;
                return Ok(JString::null());
            }
        };
        
        output.export_to_java(env)
    }).resolve::<jni::errors::ThrowRuntimeExAndDefault>()
}

#[jni_mangle("Ebi")]
pub fn discover_non_stochastic_inductive_miner<'caller>(mut unowned_env: EnvUnowned<'caller>, _class: JClass<'caller>, arg0: JString<'caller>) -> JString<'caller> {
    unowned_env.with_env(|env| -> Result<_, jni::errors::Error> {
        ebi_objects::ebi_arithmetic::exact::set_exact_globally(true);
        let command: &&EbiCommand = &&crate::ebi_commands::ebi_command_discover_non_stochastic::EBI_DISCOVER_NON_STOCHASTIC_INDUCTIVE_MINER;
        let input_types = match **command {
            EbiCommand::Command { input_types, .. } => input_types,
            _ => {
                env.throw("Expected a command.")?;
                return Ok(JString::null())
            },
        };

        let input0 = match import_or_load(arg0, input_types[0], 0).with_context(|| anyhow!("Could not import argument 0")) {
                    Ok(x) => x,
                    Err(err) => {
                        env.throw(err.to_string().as_str())?;
                        return Ok(JString::null())
                    }
                };
    

        let inputs = vec![input0];

        // Execute the command.
        let output = match command.execute_with_inputs(inputs) {
            Ok(result) => result,
            Err(err) => {
                env.throw(format!("Command error: {}", err.to_string().as_str()))?;
                return Ok(JString::null());
            }
        };
        
        output.export_to_java(env)
    }).resolve::<jni::errors::ThrowRuntimeExAndDefault>()
}

#[jni_mangle("Ebi")]
pub fn discover_non_stochastic_inductive_miner_infrequent<'caller>(mut unowned_env: EnvUnowned<'caller>, _class: JClass<'caller>, arg0: JString<'caller>, arg1: JString<'caller>) -> JString<'caller> {
    unowned_env.with_env(|env| -> Result<_, jni::errors::Error> {
        ebi_objects::ebi_arithmetic::exact::set_exact_globally(true);
        let command: &&EbiCommand = &&crate::ebi_commands::ebi_command_discover_non_stochastic::EBI_DISCOVER_NON_STOCHASTIC_INDUCTIVE_MINER_INFREQUENT;
        let input_types = match **command {
            EbiCommand::Command { input_types, .. } => input_types,
            _ => {
                env.throw("Expected a command.")?;
                return Ok(JString::null())
            },
        };

        let input0 = match import_or_load(arg0, input_types[0], 0).with_context(|| anyhow!("Could not import argument 0")) {
                    Ok(x) => x,
                    Err(err) => {
                        env.throw(err.to_string().as_str())?;
                        return Ok(JString::null())
                    }
                };
    
let input1 = match import_or_load(arg1, input_types[1], 1).with_context(|| anyhow!("Could not import argument 1")) {
                    Ok(x) => x,
                    Err(err) => {
                        env.throw(err.to_string().as_str())?;
                        return Ok(JString::null())
                    }
                };
    

        let inputs = vec![input0, input1];

        // Execute the command.
        let output = match command.execute_with_inputs(inputs) {
            Ok(result) => result,
            Err(err) => {
                env.throw(format!("Command error: {}", err.to_string().as_str()))?;
                return Ok(JString::null());
            }
        };
        
        output.export_to_java(env)
    }).resolve::<jni::errors::ThrowRuntimeExAndDefault>()
}

#[jni_mangle("Ebi")]
pub fn discover_non_stochastic_prefix_tree_deterministic_finite_automaton<'caller>(mut unowned_env: EnvUnowned<'caller>, _class: JClass<'caller>, arg0: JString<'caller>) -> JString<'caller> {
    unowned_env.with_env(|env| -> Result<_, jni::errors::Error> {
        ebi_objects::ebi_arithmetic::exact::set_exact_globally(true);
        let command: &&EbiCommand = &&crate::ebi_commands::ebi_command_discover_non_stochastic::EBI_DISCOVER_NON_STOCHASTIC_TREE_DFA;
        let input_types = match **command {
            EbiCommand::Command { input_types, .. } => input_types,
            _ => {
                env.throw("Expected a command.")?;
                return Ok(JString::null())
            },
        };

        let input0 = match import_or_load(arg0, input_types[0], 0).with_context(|| anyhow!("Could not import argument 0")) {
                    Ok(x) => x,
                    Err(err) => {
                        env.throw(err.to_string().as_str())?;
                        return Ok(JString::null())
                    }
                };
    

        let inputs = vec![input0];

        // Execute the command.
        let output = match command.execute_with_inputs(inputs) {
            Ok(result) => result,
            Err(err) => {
                env.throw(format!("Command error: {}", err.to_string().as_str()))?;
                return Ok(JString::null());
            }
        };
        
        output.export_to_java(env)
    }).resolve::<jni::errors::ThrowRuntimeExAndDefault>()
}

#[jni_mangle("Ebi")]
pub fn discover_non_stochastic_prefix_tree_process_tree<'caller>(mut unowned_env: EnvUnowned<'caller>, _class: JClass<'caller>, arg0: JString<'caller>) -> JString<'caller> {
    unowned_env.with_env(|env| -> Result<_, jni::errors::Error> {
        ebi_objects::ebi_arithmetic::exact::set_exact_globally(true);
        let command: &&EbiCommand = &&crate::ebi_commands::ebi_command_discover_non_stochastic::EBI_DISCOVER_NON_STOCHASTIC_TREE_TREE;
        let input_types = match **command {
            EbiCommand::Command { input_types, .. } => input_types,
            _ => {
                env.throw("Expected a command.")?;
                return Ok(JString::null())
            },
        };

        let input0 = match import_or_load(arg0, input_types[0], 0).with_context(|| anyhow!("Could not import argument 0")) {
                    Ok(x) => x,
                    Err(err) => {
                        env.throw(err.to_string().as_str())?;
                        return Ok(JString::null())
                    }
                };
    

        let inputs = vec![input0];

        // Execute the command.
        let output = match command.execute_with_inputs(inputs) {
            Ok(result) => result,
            Err(err) => {
                env.throw(format!("Command error: {}", err.to_string().as_str()))?;
                return Ok(JString::null());
            }
        };
        
        output.export_to_java(env)
    }).resolve::<jni::errors::ThrowRuntimeExAndDefault>()
}

#[jni_mangle("Ebi")]
pub fn discover_non_stochastic_split_miner<'caller>(mut unowned_env: EnvUnowned<'caller>, _class: JClass<'caller>, arg0: JString<'caller>, arg1: JString<'caller>, arg2: JString<'caller>) -> JString<'caller> {
    unowned_env.with_env(|env| -> Result<_, jni::errors::Error> {
        ebi_objects::ebi_arithmetic::exact::set_exact_globally(true);
        let command: &&EbiCommand = &&crate::ebi_commands::ebi_command_discover_non_stochastic::EBI_DISCOVER_NON_STOCHASTIC_SPLIT_MINER;
        let input_types = match **command {
            EbiCommand::Command { input_types, .. } => input_types,
            _ => {
                env.throw("Expected a command.")?;
                return Ok(JString::null())
            },
        };

        let input0 = match import_or_load(arg0, input_types[0], 0).with_context(|| anyhow!("Could not import argument 0")) {
                    Ok(x) => x,
                    Err(err) => {
                        env.throw(err.to_string().as_str())?;
                        return Ok(JString::null())
                    }
                };
    
let input1 = match import_or_load(arg1, input_types[1], 1).with_context(|| anyhow!("Could not import argument 1")) {
                    Ok(x) => x,
                    Err(err) => {
                        env.throw(err.to_string().as_str())?;
                        return Ok(JString::null())
                    }
                };
    
let input2 = match import_or_load(arg2, input_types[2], 2).with_context(|| anyhow!("Could not import argument 2")) {
                    Ok(x) => x,
                    Err(err) => {
                        env.throw(err.to_string().as_str())?;
                        return Ok(JString::null())
                    }
                };
    

        let inputs = vec![input0, input1, input2];

        // Execute the command.
        let output = match command.execute_with_inputs(inputs) {
            Ok(result) => result,
            Err(err) => {
                env.throw(format!("Command error: {}", err.to_string().as_str()))?;
                return Ok(JString::null());
            }
        };
        
        output.export_to_java(env)
    }).resolve::<jni::errors::ThrowRuntimeExAndDefault>()
}

#[jni_mangle("Ebi")]
pub fn discover_non_stochastic_trace_model<'caller>(mut unowned_env: EnvUnowned<'caller>, _class: JClass<'caller>, arg0: JString<'caller>) -> JString<'caller> {
    unowned_env.with_env(|env| -> Result<_, jni::errors::Error> {
        ebi_objects::ebi_arithmetic::exact::set_exact_globally(true);
        let command: &&EbiCommand = &&crate::ebi_commands::ebi_command_discover_non_stochastic::EBI_DISCOVER_NON_STOCHASTIC_TRACE_MODEL;
        let input_types = match **command {
            EbiCommand::Command { input_types, .. } => input_types,
            _ => {
                env.throw("Expected a command.")?;
                return Ok(JString::null())
            },
        };

        let input0 = match import_or_load(arg0, input_types[0], 0).with_context(|| anyhow!("Could not import argument 0")) {
                    Ok(x) => x,
                    Err(err) => {
                        env.throw(err.to_string().as_str())?;
                        return Ok(JString::null())
                    }
                };
    

        let inputs = vec![input0];

        // Execute the command.
        let output = match command.execute_with_inputs(inputs) {
            Ok(result) => result,
            Err(err) => {
                env.throw(format!("Command error: {}", err.to_string().as_str()))?;
                return Ok(JString::null());
            }
        };
        
        output.export_to_java(env)
    }).resolve::<jni::errors::ThrowRuntimeExAndDefault>()
}

#[jni_mangle("Ebi")]
pub fn filter_language_traces_empty<'caller>(mut unowned_env: EnvUnowned<'caller>, _class: JClass<'caller>, arg0: JString<'caller>) -> JString<'caller> {
    unowned_env.with_env(|env| -> Result<_, jni::errors::Error> {
        ebi_objects::ebi_arithmetic::exact::set_exact_globally(true);
        let command: &&EbiCommand = &&crate::ebi_commands::ebi_command_filter::EBI_FILTER_LANG_TRACES_EMPTY;
        let input_types = match **command {
            EbiCommand::Command { input_types, .. } => input_types,
            _ => {
                env.throw("Expected a command.")?;
                return Ok(JString::null())
            },
        };

        let input0 = match import_or_load(arg0, input_types[0], 0).with_context(|| anyhow!("Could not import argument 0")) {
                    Ok(x) => x,
                    Err(err) => {
                        env.throw(err.to_string().as_str())?;
                        return Ok(JString::null())
                    }
                };
    

        let inputs = vec![input0];

        // Execute the command.
        let output = match command.execute_with_inputs(inputs) {
            Ok(result) => result,
            Err(err) => {
                env.throw(format!("Command error: {}", err.to_string().as_str()))?;
                return Ok(JString::null());
            }
        };
        
        output.export_to_java(env)
    }).resolve::<jni::errors::ThrowRuntimeExAndDefault>()
}

#[jni_mangle("Ebi")]
pub fn filter_language_traces_event_activity<'caller>(mut unowned_env: EnvUnowned<'caller>, _class: JClass<'caller>, arg0: JString<'caller>, arg1: JString<'caller>, arg2: JString<'caller>) -> JString<'caller> {
    unowned_env.with_env(|env| -> Result<_, jni::errors::Error> {
        ebi_objects::ebi_arithmetic::exact::set_exact_globally(true);
        let command: &&EbiCommand = &&crate::ebi_commands::ebi_command_filter::EBI_FILTER_LANG_TRACES_EVENT_ACTIVITY;
        let input_types = match **command {
            EbiCommand::Command { input_types, .. } => input_types,
            _ => {
                env.throw("Expected a command.")?;
                return Ok(JString::null())
            },
        };

        let input0 = match import_or_load(arg0, input_types[0], 0).with_context(|| anyhow!("Could not import argument 0")) {
                    Ok(x) => x,
                    Err(err) => {
                        env.throw(err.to_string().as_str())?;
                        return Ok(JString::null())
                    }
                };
    
let input1 = match import_or_load(arg1, input_types[1], 1).with_context(|| anyhow!("Could not import argument 1")) {
                    Ok(x) => x,
                    Err(err) => {
                        env.throw(err.to_string().as_str())?;
                        return Ok(JString::null())
                    }
                };
    
let input2 = match import_or_load(arg2, input_types[2], 2).with_context(|| anyhow!("Could not import argument 2")) {
                    Ok(x) => x,
                    Err(err) => {
                        env.throw(err.to_string().as_str())?;
                        return Ok(JString::null())
                    }
                };
    

        let inputs = vec![input0, input1, input2];

        // Execute the command.
        let output = match command.execute_with_inputs(inputs) {
            Ok(result) => result,
            Err(err) => {
                env.throw(format!("Command error: {}", err.to_string().as_str()))?;
                return Ok(JString::null());
            }
        };
        
        output.export_to_java(env)
    }).resolve::<jni::errors::ThrowRuntimeExAndDefault>()
}

#[jni_mangle("Ebi")]
pub fn filter_language_traces_length<'caller>(mut unowned_env: EnvUnowned<'caller>, _class: JClass<'caller>, arg0: JString<'caller>, arg1: JString<'caller>, arg2: JString<'caller>) -> JString<'caller> {
    unowned_env.with_env(|env| -> Result<_, jni::errors::Error> {
        ebi_objects::ebi_arithmetic::exact::set_exact_globally(true);
        let command: &&EbiCommand = &&crate::ebi_commands::ebi_command_filter::EBI_FILTER_LANG_TRACES_LENGTH;
        let input_types = match **command {
            EbiCommand::Command { input_types, .. } => input_types,
            _ => {
                env.throw("Expected a command.")?;
                return Ok(JString::null())
            },
        };

        let input0 = match import_or_load(arg0, input_types[0], 0).with_context(|| anyhow!("Could not import argument 0")) {
                    Ok(x) => x,
                    Err(err) => {
                        env.throw(err.to_string().as_str())?;
                        return Ok(JString::null())
                    }
                };
    
let input1 = match import_or_load(arg1, input_types[1], 1).with_context(|| anyhow!("Could not import argument 1")) {
                    Ok(x) => x,
                    Err(err) => {
                        env.throw(err.to_string().as_str())?;
                        return Ok(JString::null())
                    }
                };
    
let input2 = match import_or_load(arg2, input_types[2], 2).with_context(|| anyhow!("Could not import argument 2")) {
                    Ok(x) => x,
                    Err(err) => {
                        env.throw(err.to_string().as_str())?;
                        return Ok(JString::null())
                    }
                };
    

        let inputs = vec![input0, input1, input2];

        // Execute the command.
        let output = match command.execute_with_inputs(inputs) {
            Ok(result) => result,
            Err(err) => {
                env.throw(format!("Command error: {}", err.to_string().as_str()))?;
                return Ok(JString::null());
            }
        };
        
        output.export_to_java(env)
    }).resolve::<jni::errors::ThrowRuntimeExAndDefault>()
}

#[jni_mangle("Ebi")]
pub fn filter_log_traces_empty<'caller>(mut unowned_env: EnvUnowned<'caller>, _class: JClass<'caller>, arg0: JString<'caller>) -> JString<'caller> {
    unowned_env.with_env(|env| -> Result<_, jni::errors::Error> {
        ebi_objects::ebi_arithmetic::exact::set_exact_globally(true);
        let command: &&EbiCommand = &&crate::ebi_commands::ebi_command_filter::EBI_FILTER_LOG_TRACES_EMPTY;
        let input_types = match **command {
            EbiCommand::Command { input_types, .. } => input_types,
            _ => {
                env.throw("Expected a command.")?;
                return Ok(JString::null())
            },
        };

        let input0 = match import_or_load(arg0, input_types[0], 0).with_context(|| anyhow!("Could not import argument 0")) {
                    Ok(x) => x,
                    Err(err) => {
                        env.throw(err.to_string().as_str())?;
                        return Ok(JString::null())
                    }
                };
    

        let inputs = vec![input0];

        // Execute the command.
        let output = match command.execute_with_inputs(inputs) {
            Ok(result) => result,
            Err(err) => {
                env.throw(format!("Command error: {}", err.to_string().as_str()))?;
                return Ok(JString::null());
            }
        };
        
        output.export_to_java(env)
    }).resolve::<jni::errors::ThrowRuntimeExAndDefault>()
}

#[jni_mangle("Ebi")]
pub fn filter_log_traces_event_activity<'caller>(mut unowned_env: EnvUnowned<'caller>, _class: JClass<'caller>, arg0: JString<'caller>, arg1: JString<'caller>, arg2: JString<'caller>) -> JString<'caller> {
    unowned_env.with_env(|env| -> Result<_, jni::errors::Error> {
        ebi_objects::ebi_arithmetic::exact::set_exact_globally(true);
        let command: &&EbiCommand = &&crate::ebi_commands::ebi_command_filter::EBI_FILTER_LOG_TRACES_EVENT_ACTIVITY;
        let input_types = match **command {
            EbiCommand::Command { input_types, .. } => input_types,
            _ => {
                env.throw("Expected a command.")?;
                return Ok(JString::null())
            },
        };

        let input0 = match import_or_load(arg0, input_types[0], 0).with_context(|| anyhow!("Could not import argument 0")) {
                    Ok(x) => x,
                    Err(err) => {
                        env.throw(err.to_string().as_str())?;
                        return Ok(JString::null())
                    }
                };
    
let input1 = match import_or_load(arg1, input_types[1], 1).with_context(|| anyhow!("Could not import argument 1")) {
                    Ok(x) => x,
                    Err(err) => {
                        env.throw(err.to_string().as_str())?;
                        return Ok(JString::null())
                    }
                };
    
let input2 = match import_or_load(arg2, input_types[2], 2).with_context(|| anyhow!("Could not import argument 2")) {
                    Ok(x) => x,
                    Err(err) => {
                        env.throw(err.to_string().as_str())?;
                        return Ok(JString::null())
                    }
                };
    

        let inputs = vec![input0, input1, input2];

        // Execute the command.
        let output = match command.execute_with_inputs(inputs) {
            Ok(result) => result,
            Err(err) => {
                env.throw(format!("Command error: {}", err.to_string().as_str()))?;
                return Ok(JString::null());
            }
        };
        
        output.export_to_java(env)
    }).resolve::<jni::errors::ThrowRuntimeExAndDefault>()
}

#[jni_mangle("Ebi")]
pub fn filter_log_traces_length<'caller>(mut unowned_env: EnvUnowned<'caller>, _class: JClass<'caller>, arg0: JString<'caller>, arg1: JString<'caller>, arg2: JString<'caller>) -> JString<'caller> {
    unowned_env.with_env(|env| -> Result<_, jni::errors::Error> {
        ebi_objects::ebi_arithmetic::exact::set_exact_globally(true);
        let command: &&EbiCommand = &&crate::ebi_commands::ebi_command_filter::EBI_FILTER_LOG_TRACES_LENGTH;
        let input_types = match **command {
            EbiCommand::Command { input_types, .. } => input_types,
            _ => {
                env.throw("Expected a command.")?;
                return Ok(JString::null())
            },
        };

        let input0 = match import_or_load(arg0, input_types[0], 0).with_context(|| anyhow!("Could not import argument 0")) {
                    Ok(x) => x,
                    Err(err) => {
                        env.throw(err.to_string().as_str())?;
                        return Ok(JString::null())
                    }
                };
    
let input1 = match import_or_load(arg1, input_types[1], 1).with_context(|| anyhow!("Could not import argument 1")) {
                    Ok(x) => x,
                    Err(err) => {
                        env.throw(err.to_string().as_str())?;
                        return Ok(JString::null())
                    }
                };
    
let input2 = match import_or_load(arg2, input_types[2], 2).with_context(|| anyhow!("Could not import argument 2")) {
                    Ok(x) => x,
                    Err(err) => {
                        env.throw(err.to_string().as_str())?;
                        return Ok(JString::null())
                    }
                };
    

        let inputs = vec![input0, input1, input2];

        // Execute the command.
        let output = match command.execute_with_inputs(inputs) {
            Ok(result) => result,
            Err(err) => {
                env.throw(format!("Command error: {}", err.to_string().as_str()))?;
                return Ok(JString::null());
            }
        };
        
        output.export_to_java(env)
    }).resolve::<jni::errors::ThrowRuntimeExAndDefault>()
}

#[jni_mangle("Ebi")]
pub fn filter_stochastic_language_traces_empty<'caller>(mut unowned_env: EnvUnowned<'caller>, _class: JClass<'caller>, arg0: JString<'caller>) -> JString<'caller> {
    unowned_env.with_env(|env| -> Result<_, jni::errors::Error> {
        ebi_objects::ebi_arithmetic::exact::set_exact_globally(true);
        let command: &&EbiCommand = &&crate::ebi_commands::ebi_command_filter::EBI_FILTER_SLANG_TRACES_EMPTY;
        let input_types = match **command {
            EbiCommand::Command { input_types, .. } => input_types,
            _ => {
                env.throw("Expected a command.")?;
                return Ok(JString::null())
            },
        };

        let input0 = match import_or_load(arg0, input_types[0], 0).with_context(|| anyhow!("Could not import argument 0")) {
                    Ok(x) => x,
                    Err(err) => {
                        env.throw(err.to_string().as_str())?;
                        return Ok(JString::null())
                    }
                };
    

        let inputs = vec![input0];

        // Execute the command.
        let output = match command.execute_with_inputs(inputs) {
            Ok(result) => result,
            Err(err) => {
                env.throw(format!("Command error: {}", err.to_string().as_str()))?;
                return Ok(JString::null());
            }
        };
        
        output.export_to_java(env)
    }).resolve::<jni::errors::ThrowRuntimeExAndDefault>()
}

#[jni_mangle("Ebi")]
pub fn filter_stochastic_language_traces_event_activity<'caller>(mut unowned_env: EnvUnowned<'caller>, _class: JClass<'caller>, arg0: JString<'caller>, arg1: JString<'caller>, arg2: JString<'caller>) -> JString<'caller> {
    unowned_env.with_env(|env| -> Result<_, jni::errors::Error> {
        ebi_objects::ebi_arithmetic::exact::set_exact_globally(true);
        let command: &&EbiCommand = &&crate::ebi_commands::ebi_command_filter::EBI_FILTER_SLANG_TRACES_EVENT_ACTIVITY;
        let input_types = match **command {
            EbiCommand::Command { input_types, .. } => input_types,
            _ => {
                env.throw("Expected a command.")?;
                return Ok(JString::null())
            },
        };

        let input0 = match import_or_load(arg0, input_types[0], 0).with_context(|| anyhow!("Could not import argument 0")) {
                    Ok(x) => x,
                    Err(err) => {
                        env.throw(err.to_string().as_str())?;
                        return Ok(JString::null())
                    }
                };
    
let input1 = match import_or_load(arg1, input_types[1], 1).with_context(|| anyhow!("Could not import argument 1")) {
                    Ok(x) => x,
                    Err(err) => {
                        env.throw(err.to_string().as_str())?;
                        return Ok(JString::null())
                    }
                };
    
let input2 = match import_or_load(arg2, input_types[2], 2).with_context(|| anyhow!("Could not import argument 2")) {
                    Ok(x) => x,
                    Err(err) => {
                        env.throw(err.to_string().as_str())?;
                        return Ok(JString::null())
                    }
                };
    

        let inputs = vec![input0, input1, input2];

        // Execute the command.
        let output = match command.execute_with_inputs(inputs) {
            Ok(result) => result,
            Err(err) => {
                env.throw(format!("Command error: {}", err.to_string().as_str()))?;
                return Ok(JString::null());
            }
        };
        
        output.export_to_java(env)
    }).resolve::<jni::errors::ThrowRuntimeExAndDefault>()
}

#[jni_mangle("Ebi")]
pub fn filter_stochastic_language_traces_least_likely<'caller>(mut unowned_env: EnvUnowned<'caller>, _class: JClass<'caller>, arg0: JString<'caller>, arg1: JString<'caller>) -> JString<'caller> {
    unowned_env.with_env(|env| -> Result<_, jni::errors::Error> {
        ebi_objects::ebi_arithmetic::exact::set_exact_globally(true);
        let command: &&EbiCommand = &&crate::ebi_commands::ebi_command_filter::EBI_FILTER_SLANG_TRACES_LEAST_LIKELY;
        let input_types = match **command {
            EbiCommand::Command { input_types, .. } => input_types,
            _ => {
                env.throw("Expected a command.")?;
                return Ok(JString::null())
            },
        };

        let input0 = match import_or_load(arg0, input_types[0], 0).with_context(|| anyhow!("Could not import argument 0")) {
                    Ok(x) => x,
                    Err(err) => {
                        env.throw(err.to_string().as_str())?;
                        return Ok(JString::null())
                    }
                };
    
let input1 = match import_or_load(arg1, input_types[1], 1).with_context(|| anyhow!("Could not import argument 1")) {
                    Ok(x) => x,
                    Err(err) => {
                        env.throw(err.to_string().as_str())?;
                        return Ok(JString::null())
                    }
                };
    

        let inputs = vec![input0, input1];

        // Execute the command.
        let output = match command.execute_with_inputs(inputs) {
            Ok(result) => result,
            Err(err) => {
                env.throw(format!("Command error: {}", err.to_string().as_str()))?;
                return Ok(JString::null());
            }
        };
        
        output.export_to_java(env)
    }).resolve::<jni::errors::ThrowRuntimeExAndDefault>()
}

#[jni_mangle("Ebi")]
pub fn filter_stochastic_language_traces_length<'caller>(mut unowned_env: EnvUnowned<'caller>, _class: JClass<'caller>, arg0: JString<'caller>, arg1: JString<'caller>, arg2: JString<'caller>) -> JString<'caller> {
    unowned_env.with_env(|env| -> Result<_, jni::errors::Error> {
        ebi_objects::ebi_arithmetic::exact::set_exact_globally(true);
        let command: &&EbiCommand = &&crate::ebi_commands::ebi_command_filter::EBI_FILTER_SLANG_TRACES_LENGTH;
        let input_types = match **command {
            EbiCommand::Command { input_types, .. } => input_types,
            _ => {
                env.throw("Expected a command.")?;
                return Ok(JString::null())
            },
        };

        let input0 = match import_or_load(arg0, input_types[0], 0).with_context(|| anyhow!("Could not import argument 0")) {
                    Ok(x) => x,
                    Err(err) => {
                        env.throw(err.to_string().as_str())?;
                        return Ok(JString::null())
                    }
                };
    
let input1 = match import_or_load(arg1, input_types[1], 1).with_context(|| anyhow!("Could not import argument 1")) {
                    Ok(x) => x,
                    Err(err) => {
                        env.throw(err.to_string().as_str())?;
                        return Ok(JString::null())
                    }
                };
    
let input2 = match import_or_load(arg2, input_types[2], 2).with_context(|| anyhow!("Could not import argument 2")) {
                    Ok(x) => x,
                    Err(err) => {
                        env.throw(err.to_string().as_str())?;
                        return Ok(JString::null())
                    }
                };
    

        let inputs = vec![input0, input1, input2];

        // Execute the command.
        let output = match command.execute_with_inputs(inputs) {
            Ok(result) => result,
            Err(err) => {
                env.throw(format!("Command error: {}", err.to_string().as_str()))?;
                return Ok(JString::null());
            }
        };
        
        output.export_to_java(env)
    }).resolve::<jni::errors::ThrowRuntimeExAndDefault>()
}

#[jni_mangle("Ebi")]
pub fn filter_stochastic_language_traces_most_likely<'caller>(mut unowned_env: EnvUnowned<'caller>, _class: JClass<'caller>, arg0: JString<'caller>, arg1: JString<'caller>) -> JString<'caller> {
    unowned_env.with_env(|env| -> Result<_, jni::errors::Error> {
        ebi_objects::ebi_arithmetic::exact::set_exact_globally(true);
        let command: &&EbiCommand = &&crate::ebi_commands::ebi_command_filter::EBI_FILTER_SLANG_TRACES_MOST_LIKELY;
        let input_types = match **command {
            EbiCommand::Command { input_types, .. } => input_types,
            _ => {
                env.throw("Expected a command.")?;
                return Ok(JString::null())
            },
        };

        let input0 = match import_or_load(arg0, input_types[0], 0).with_context(|| anyhow!("Could not import argument 0")) {
                    Ok(x) => x,
                    Err(err) => {
                        env.throw(err.to_string().as_str())?;
                        return Ok(JString::null())
                    }
                };
    
let input1 = match import_or_load(arg1, input_types[1], 1).with_context(|| anyhow!("Could not import argument 1")) {
                    Ok(x) => x,
                    Err(err) => {
                        env.throw(err.to_string().as_str())?;
                        return Ok(JString::null())
                    }
                };
    

        let inputs = vec![input0, input1];

        // Execute the command.
        let output = match command.execute_with_inputs(inputs) {
            Ok(result) => result,
            Err(err) => {
                env.throw(format!("Command error: {}", err.to_string().as_str()))?;
                return Ok(JString::null());
            }
        };
        
        output.export_to_java(env)
    }).resolve::<jni::errors::ThrowRuntimeExAndDefault>()
}

#[jni_mangle("Ebi")]
pub fn information<'caller>(mut unowned_env: EnvUnowned<'caller>, _class: JClass<'caller>, arg0: JString<'caller>) -> JString<'caller> {
    unowned_env.with_env(|env| -> Result<_, jni::errors::Error> {
        ebi_objects::ebi_arithmetic::exact::set_exact_globally(true);
        let command: &&EbiCommand = &&crate::ebi_commands::ebi_command_info::EBI_INFO;
        let input_types = match **command {
            EbiCommand::Command { input_types, .. } => input_types,
            _ => {
                env.throw("Expected a command.")?;
                return Ok(JString::null())
            },
        };

        let input0 = match import_or_load(arg0, input_types[0], 0).with_context(|| anyhow!("Could not import argument 0")) {
                    Ok(x) => x,
                    Err(err) => {
                        env.throw(err.to_string().as_str())?;
                        return Ok(JString::null())
                    }
                };
    

        let inputs = vec![input0];

        // Execute the command.
        let output = match command.execute_with_inputs(inputs) {
            Ok(result) => result,
            Err(err) => {
                env.throw(format!("Command error: {}", err.to_string().as_str()))?;
                return Ok(JString::null());
            }
        };
        
        output.export_to_java(env)
    }).resolve::<jni::errors::ThrowRuntimeExAndDefault>()
}

#[jni_mangle("Ebi")]
pub fn probability_log<'caller>(mut unowned_env: EnvUnowned<'caller>, _class: JClass<'caller>, arg0: JString<'caller>, arg1: JString<'caller>) -> JString<'caller> {
    unowned_env.with_env(|env| -> Result<_, jni::errors::Error> {
        ebi_objects::ebi_arithmetic::exact::set_exact_globally(true);
        let command: &&EbiCommand = &&crate::ebi_commands::ebi_command_probability::EBI_PROBABILITY_LOG;
        let input_types = match **command {
            EbiCommand::Command { input_types, .. } => input_types,
            _ => {
                env.throw("Expected a command.")?;
                return Ok(JString::null())
            },
        };

        let input0 = match import_or_load(arg0, input_types[0], 0).with_context(|| anyhow!("Could not import argument 0")) {
                    Ok(x) => x,
                    Err(err) => {
                        env.throw(err.to_string().as_str())?;
                        return Ok(JString::null())
                    }
                };
    
let input1 = match import_or_load(arg1, input_types[1], 1).with_context(|| anyhow!("Could not import argument 1")) {
                    Ok(x) => x,
                    Err(err) => {
                        env.throw(err.to_string().as_str())?;
                        return Ok(JString::null())
                    }
                };
    

        let inputs = vec![input0, input1];

        // Execute the command.
        let output = match command.execute_with_inputs(inputs) {
            Ok(result) => result,
            Err(err) => {
                env.throw(format!("Command error: {}", err.to_string().as_str()))?;
                return Ok(JString::null());
            }
        };
        
        output.export_to_java(env)
    }).resolve::<jni::errors::ThrowRuntimeExAndDefault>()
}

#[jni_mangle("Ebi")]
pub fn reduce_labelled_petri_net<'caller>(mut unowned_env: EnvUnowned<'caller>, _class: JClass<'caller>, arg0: JString<'caller>) -> JString<'caller> {
    unowned_env.with_env(|env| -> Result<_, jni::errors::Error> {
        ebi_objects::ebi_arithmetic::exact::set_exact_globally(true);
        let command: &&EbiCommand = &&crate::ebi_commands::ebi_command_reduce::EBI_REDUCE_PETRI_NET;
        let input_types = match **command {
            EbiCommand::Command { input_types, .. } => input_types,
            _ => {
                env.throw("Expected a command.")?;
                return Ok(JString::null())
            },
        };

        let input0 = match import_or_load(arg0, input_types[0], 0).with_context(|| anyhow!("Could not import argument 0")) {
                    Ok(x) => x,
                    Err(err) => {
                        env.throw(err.to_string().as_str())?;
                        return Ok(JString::null())
                    }
                };
    

        let inputs = vec![input0];

        // Execute the command.
        let output = match command.execute_with_inputs(inputs) {
            Ok(result) => result,
            Err(err) => {
                env.throw(format!("Command error: {}", err.to_string().as_str()))?;
                return Ok(JString::null());
            }
        };
        
        output.export_to_java(env)
    }).resolve::<jni::errors::ThrowRuntimeExAndDefault>()
}

#[jni_mangle("Ebi")]
pub fn reduce_process_tree<'caller>(mut unowned_env: EnvUnowned<'caller>, _class: JClass<'caller>, arg0: JString<'caller>) -> JString<'caller> {
    unowned_env.with_env(|env| -> Result<_, jni::errors::Error> {
        ebi_objects::ebi_arithmetic::exact::set_exact_globally(true);
        let command: &&EbiCommand = &&crate::ebi_commands::ebi_command_reduce::EBI_REDUCE_PROCESS_TREE;
        let input_types = match **command {
            EbiCommand::Command { input_types, .. } => input_types,
            _ => {
                env.throw("Expected a command.")?;
                return Ok(JString::null())
            },
        };

        let input0 = match import_or_load(arg0, input_types[0], 0).with_context(|| anyhow!("Could not import argument 0")) {
                    Ok(x) => x,
                    Err(err) => {
                        env.throw(err.to_string().as_str())?;
                        return Ok(JString::null())
                    }
                };
    

        let inputs = vec![input0];

        // Execute the command.
        let output = match command.execute_with_inputs(inputs) {
            Ok(result) => result,
            Err(err) => {
                env.throw(format!("Command error: {}", err.to_string().as_str()))?;
                return Ok(JString::null());
            }
        };
        
        output.export_to_java(env)
    }).resolve::<jni::errors::ThrowRuntimeExAndDefault>()
}

#[jni_mangle("Ebi")]
pub fn sample_folds<'caller>(mut unowned_env: EnvUnowned<'caller>, _class: JClass<'caller>, arg0: JString<'caller>, arg1: JString<'caller>, arg2: JString<'caller>, arg3: JString<'caller>) -> JString<'caller> {
    unowned_env.with_env(|env| -> Result<_, jni::errors::Error> {
        ebi_objects::ebi_arithmetic::exact::set_exact_globally(true);
        let command: &&EbiCommand = &&crate::ebi_commands::ebi_command_sample::EBI_SAMPLE_FOLDS;
        let input_types = match **command {
            EbiCommand::Command { input_types, .. } => input_types,
            _ => {
                env.throw("Expected a command.")?;
                return Ok(JString::null())
            },
        };

        let input0 = match import_or_load(arg0, input_types[0], 0).with_context(|| anyhow!("Could not import argument 0")) {
                    Ok(x) => x,
                    Err(err) => {
                        env.throw(err.to_string().as_str())?;
                        return Ok(JString::null())
                    }
                };
    
let input1 = match import_or_load(arg1, input_types[1], 1).with_context(|| anyhow!("Could not import argument 1")) {
                    Ok(x) => x,
                    Err(err) => {
                        env.throw(err.to_string().as_str())?;
                        return Ok(JString::null())
                    }
                };
    
let input2 = match import_or_load(arg2, input_types[2], 2).with_context(|| anyhow!("Could not import argument 2")) {
                    Ok(x) => x,
                    Err(err) => {
                        env.throw(err.to_string().as_str())?;
                        return Ok(JString::null())
                    }
                };
    
let input3 = match import_or_load(arg3, input_types[3], 3).with_context(|| anyhow!("Could not import argument 3")) {
                    Ok(x) => x,
                    Err(err) => {
                        env.throw(err.to_string().as_str())?;
                        return Ok(JString::null())
                    }
                };
    

        let inputs = vec![input0, input1, input2, input3];

        // Execute the command.
        let output = match command.execute_with_inputs(inputs) {
            Ok(result) => result,
            Err(err) => {
                env.throw(format!("Command error: {}", err.to_string().as_str()))?;
                return Ok(JString::null());
            }
        };
        
        output.export_to_java(env)
    }).resolve::<jni::errors::ThrowRuntimeExAndDefault>()
}

#[jni_mangle("Ebi")]
pub fn sample_partially_ordered_traces<'caller>(mut unowned_env: EnvUnowned<'caller>, _class: JClass<'caller>, arg0: JString<'caller>, arg1: JString<'caller>) -> JString<'caller> {
    unowned_env.with_env(|env| -> Result<_, jni::errors::Error> {
        ebi_objects::ebi_arithmetic::exact::set_exact_globally(true);
        let command: &&EbiCommand = &&crate::ebi_commands::ebi_command_sample::EBI_SAMPLE_PARTIALLY_ORDERED_TRACES;
        let input_types = match **command {
            EbiCommand::Command { input_types, .. } => input_types,
            _ => {
                env.throw("Expected a command.")?;
                return Ok(JString::null())
            },
        };

        let input0 = match import_or_load(arg0, input_types[0], 0).with_context(|| anyhow!("Could not import argument 0")) {
                    Ok(x) => x,
                    Err(err) => {
                        env.throw(err.to_string().as_str())?;
                        return Ok(JString::null())
                    }
                };
    
let input1 = match import_or_load(arg1, input_types[1], 1).with_context(|| anyhow!("Could not import argument 1")) {
                    Ok(x) => x,
                    Err(err) => {
                        env.throw(err.to_string().as_str())?;
                        return Ok(JString::null())
                    }
                };
    

        let inputs = vec![input0, input1];

        // Execute the command.
        let output = match command.execute_with_inputs(inputs) {
            Ok(result) => result,
            Err(err) => {
                env.throw(format!("Command error: {}", err.to_string().as_str()))?;
                return Ok(JString::null());
            }
        };
        
        output.export_to_java(env)
    }).resolve::<jni::errors::ThrowRuntimeExAndDefault>()
}

#[jni_mangle("Ebi")]
pub fn sample_traces<'caller>(mut unowned_env: EnvUnowned<'caller>, _class: JClass<'caller>, arg0: JString<'caller>, arg1: JString<'caller>) -> JString<'caller> {
    unowned_env.with_env(|env| -> Result<_, jni::errors::Error> {
        ebi_objects::ebi_arithmetic::exact::set_exact_globally(true);
        let command: &&EbiCommand = &&crate::ebi_commands::ebi_command_sample::EBI_SAMPLE_TRACES;
        let input_types = match **command {
            EbiCommand::Command { input_types, .. } => input_types,
            _ => {
                env.throw("Expected a command.")?;
                return Ok(JString::null())
            },
        };

        let input0 = match import_or_load(arg0, input_types[0], 0).with_context(|| anyhow!("Could not import argument 0")) {
                    Ok(x) => x,
                    Err(err) => {
                        env.throw(err.to_string().as_str())?;
                        return Ok(JString::null())
                    }
                };
    
let input1 = match import_or_load(arg1, input_types[1], 1).with_context(|| anyhow!("Could not import argument 1")) {
                    Ok(x) => x,
                    Err(err) => {
                        env.throw(err.to_string().as_str())?;
                        return Ok(JString::null())
                    }
                };
    

        let inputs = vec![input0, input1];

        // Execute the command.
        let output = match command.execute_with_inputs(inputs) {
            Ok(result) => result,
            Err(err) => {
                env.throw(format!("Command error: {}", err.to_string().as_str()))?;
                return Ok(JString::null());
            }
        };
        
        output.export_to_java(env)
    }).resolve::<jni::errors::ThrowRuntimeExAndDefault>()
}

#[jni_mangle("Ebi")]
pub fn test_bootstrap_test<'caller>(mut unowned_env: EnvUnowned<'caller>, _class: JClass<'caller>, arg0: JString<'caller>, arg1: JString<'caller>, arg2: JString<'caller>, arg3: JString<'caller>) -> JString<'caller> {
    unowned_env.with_env(|env| -> Result<_, jni::errors::Error> {
        ebi_objects::ebi_arithmetic::exact::set_exact_globally(true);
        let command: &&EbiCommand = &&crate::ebi_commands::ebi_command_test::EBI_TEST_BOOTSTRAP;
        let input_types = match **command {
            EbiCommand::Command { input_types, .. } => input_types,
            _ => {
                env.throw("Expected a command.")?;
                return Ok(JString::null())
            },
        };

        let input0 = match import_or_load(arg0, input_types[0], 0).with_context(|| anyhow!("Could not import argument 0")) {
                    Ok(x) => x,
                    Err(err) => {
                        env.throw(err.to_string().as_str())?;
                        return Ok(JString::null())
                    }
                };
    
let input1 = match import_or_load(arg1, input_types[1], 1).with_context(|| anyhow!("Could not import argument 1")) {
                    Ok(x) => x,
                    Err(err) => {
                        env.throw(err.to_string().as_str())?;
                        return Ok(JString::null())
                    }
                };
    
let input2 = match import_or_load(arg2, input_types[2], 2).with_context(|| anyhow!("Could not import argument 2")) {
                    Ok(x) => x,
                    Err(err) => {
                        env.throw(err.to_string().as_str())?;
                        return Ok(JString::null())
                    }
                };
    
let input3 = match import_or_load(arg3, input_types[3], 3).with_context(|| anyhow!("Could not import argument 3")) {
                    Ok(x) => x,
                    Err(err) => {
                        env.throw(err.to_string().as_str())?;
                        return Ok(JString::null())
                    }
                };
    

        let inputs = vec![input0, input1, input2, input3];

        // Execute the command.
        let output = match command.execute_with_inputs(inputs) {
            Ok(result) => result,
            Err(err) => {
                env.throw(format!("Command error: {}", err.to_string().as_str()))?;
                return Ok(JString::null());
            }
        };
        
        output.export_to_java(env)
    }).resolve::<jni::errors::ThrowRuntimeExAndDefault>()
}

#[jni_mangle("Ebi")]
pub fn test_log_categorical_attribute<'caller>(mut unowned_env: EnvUnowned<'caller>, _class: JClass<'caller>, arg0: JString<'caller>, arg1: JString<'caller>, arg2: JString<'caller>, arg3: JString<'caller>) -> JString<'caller> {
    unowned_env.with_env(|env| -> Result<_, jni::errors::Error> {
        ebi_objects::ebi_arithmetic::exact::set_exact_globally(true);
        let command: &&EbiCommand = &&crate::ebi_commands::ebi_command_test::EBI_TEST_LOG_ATTRIBUTE;
        let input_types = match **command {
            EbiCommand::Command { input_types, .. } => input_types,
            _ => {
                env.throw("Expected a command.")?;
                return Ok(JString::null())
            },
        };

        let input0 = match import_or_load(arg0, input_types[0], 0).with_context(|| anyhow!("Could not import argument 0")) {
                    Ok(x) => x,
                    Err(err) => {
                        env.throw(err.to_string().as_str())?;
                        return Ok(JString::null())
                    }
                };
    
let input1 = match import_or_load(arg1, input_types[1], 1).with_context(|| anyhow!("Could not import argument 1")) {
                    Ok(x) => x,
                    Err(err) => {
                        env.throw(err.to_string().as_str())?;
                        return Ok(JString::null())
                    }
                };
    
let input2 = match import_or_load(arg2, input_types[2], 2).with_context(|| anyhow!("Could not import argument 2")) {
                    Ok(x) => x,
                    Err(err) => {
                        env.throw(err.to_string().as_str())?;
                        return Ok(JString::null())
                    }
                };
    
let input3 = match import_or_load(arg3, input_types[3], 3).with_context(|| anyhow!("Could not import argument 3")) {
                    Ok(x) => x,
                    Err(err) => {
                        env.throw(err.to_string().as_str())?;
                        return Ok(JString::null())
                    }
                };
    

        let inputs = vec![input0, input1, input2, input3];

        // Execute the command.
        let output = match command.execute_with_inputs(inputs) {
            Ok(result) => result,
            Err(err) => {
                env.throw(format!("Command error: {}", err.to_string().as_str()))?;
                return Ok(JString::null());
            }
        };
        
        output.export_to_java(env)
    }).resolve::<jni::errors::ThrowRuntimeExAndDefault>()
}

#[jni_mangle("Ebi")]
pub fn test_permutation_test_log_model<'caller>(mut unowned_env: EnvUnowned<'caller>, _class: JClass<'caller>, arg0: JString<'caller>, arg1: JString<'caller>, arg2: JString<'caller>, arg3: JString<'caller>) -> JString<'caller> {
    unowned_env.with_env(|env| -> Result<_, jni::errors::Error> {
        ebi_objects::ebi_arithmetic::exact::set_exact_globally(true);
        let command: &&EbiCommand = &&crate::ebi_commands::ebi_command_test::EBI_PERMUTATION_TEST_LOG_MODEL;
        let input_types = match **command {
            EbiCommand::Command { input_types, .. } => input_types,
            _ => {
                env.throw("Expected a command.")?;
                return Ok(JString::null())
            },
        };

        let input0 = match import_or_load(arg0, input_types[0], 0).with_context(|| anyhow!("Could not import argument 0")) {
                    Ok(x) => x,
                    Err(err) => {
                        env.throw(err.to_string().as_str())?;
                        return Ok(JString::null())
                    }
                };
    
let input1 = match import_or_load(arg1, input_types[1], 1).with_context(|| anyhow!("Could not import argument 1")) {
                    Ok(x) => x,
                    Err(err) => {
                        env.throw(err.to_string().as_str())?;
                        return Ok(JString::null())
                    }
                };
    
let input2 = match import_or_load(arg2, input_types[2], 2).with_context(|| anyhow!("Could not import argument 2")) {
                    Ok(x) => x,
                    Err(err) => {
                        env.throw(err.to_string().as_str())?;
                        return Ok(JString::null())
                    }
                };
    
let input3 = match import_or_load(arg3, input_types[3], 3).with_context(|| anyhow!("Could not import argument 3")) {
                    Ok(x) => x,
                    Err(err) => {
                        env.throw(err.to_string().as_str())?;
                        return Ok(JString::null())
                    }
                };
    

        let inputs = vec![input0, input1, input2, input3];

        // Execute the command.
        let output = match command.execute_with_inputs(inputs) {
            Ok(result) => result,
            Err(err) => {
                env.throw(format!("Command error: {}", err.to_string().as_str()))?;
                return Ok(JString::null());
            }
        };
        
        output.export_to_java(env)
    }).resolve::<jni::errors::ThrowRuntimeExAndDefault>()
}

#[jni_mangle("Ebi")]
pub fn visualise_graph<'caller>(mut unowned_env: EnvUnowned<'caller>, _class: JClass<'caller>, arg0: JString<'caller>) -> JString<'caller> {
    unowned_env.with_env(|env| -> Result<_, jni::errors::Error> {
        ebi_objects::ebi_arithmetic::exact::set_exact_globally(true);
        let command: &&EbiCommand = &&crate::ebi_commands::ebi_command_visualise::EBI_VISUALISE_GRAPH;
        let input_types = match **command {
            EbiCommand::Command { input_types, .. } => input_types,
            _ => {
                env.throw("Expected a command.")?;
                return Ok(JString::null())
            },
        };

        let input0 = match import_or_load(arg0, input_types[0], 0).with_context(|| anyhow!("Could not import argument 0")) {
                    Ok(x) => x,
                    Err(err) => {
                        env.throw(err.to_string().as_str())?;
                        return Ok(JString::null())
                    }
                };
    

        let inputs = vec![input0];

        // Execute the command.
        let output = match command.execute_with_inputs(inputs) {
            Ok(result) => result,
            Err(err) => {
                env.throw(format!("Command error: {}", err.to_string().as_str()))?;
                return Ok(JString::null());
            }
        };
        
        output.export_to_java(env)
    }).resolve::<jni::errors::ThrowRuntimeExAndDefault>()
}

#[jni_mangle("Ebi")]
pub fn visualise_text<'caller>(mut unowned_env: EnvUnowned<'caller>, _class: JClass<'caller>, arg0: JString<'caller>) -> JString<'caller> {
    unowned_env.with_env(|env| -> Result<_, jni::errors::Error> {
        ebi_objects::ebi_arithmetic::exact::set_exact_globally(false);
        let command: &&EbiCommand = &&crate::ebi_commands::ebi_command_visualise::EBI_VISUALISE_TEXT;
        let input_types = match **command {
            EbiCommand::Command { input_types, .. } => input_types,
            _ => {
                env.throw("Expected a command.")?;
                return Ok(JString::null())
            },
        };

        let input0 = match import_or_load(arg0, input_types[0], 0).with_context(|| anyhow!("Could not import argument 0")) {
                    Ok(x) => x,
                    Err(err) => {
                        env.throw(err.to_string().as_str())?;
                        return Ok(JString::null())
                    }
                };
    

        let inputs = vec![input0];

        // Execute the command.
        let output = match command.execute_with_inputs(inputs) {
            Ok(result) => result,
            Err(err) => {
                env.throw(format!("Command error: {}", err.to_string().as_str()))?;
                return Ok(JString::null());
            }
        };
        
        output.export_to_java(env)
    }).resolve::<jni::errors::ThrowRuntimeExAndDefault>()
}


