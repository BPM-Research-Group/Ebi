use std::{collections::HashMap, io::Write};
use clap::Command;
use anyhow::Result;
use ebi_objects::{ebi_objects::scalable_vector_graphics::ToSVGMut, EbiObject, EbiObjectType};
use inflector::Inflector;
use layout::{core::{base::Orientation, color::Color, geometry::Point, style::StyleAttr}, std_shapes::{render::get_shape_size, shapes::{Arrow, Element, ShapeKind}}, topo::layout::VisualGraph};
use strum::IntoEnumIterator;

use crate::{ebi_framework::{ebi_command::{get_applicable_commands, EbiCommand, EBI_COMMANDS}, ebi_file_handler::{get_file_handlers, EBI_FILE_HANDLERS}, ebi_input::{self, EbiInputType}, ebi_output::{EbiExporter, EbiOutput, EbiOutputType}, ebi_trait::EbiTrait, prom_link::{self, get_java_object_handlers_that_can_export, get_java_object_handlers_that_can_import}}, pm4py::pm4py_module_generator::{generate_pm4py_module, pm4py_function_name, PM4PY_PACKAGE}, text::{Joiner, LatexEscaper}};

pub const LOGO: &str = r"□ □ □ □ □ □ □ □ □ □ □ □ □ □ □
 □ □ □ □ □ □ □ □ □ □ □ □ □ □ 
□ □ □ □ □◦◦◦◦ □-◦◦◦◦□ □ □ □ □
 □ □ □ □ ◦◦◦◦◦ ◦◦◦◦◦ □ □ □ □ 
□ □ □ ◦◦□ ◦◦◦ □ ◦◦◦ □◦◦ □ □ □
 □ □ ◦◦◦◦# □ ◦◦◦◦□ =◦◦◦◦ □ □ 
□ □ □ ◦◦◦+□◦◦◦◦◦◦◦◦-◦◦◦ □ □ □
 □ □ □ □ ◦◦◦◦◦◦◦◦◦◦◦ □ □ □ □ 
□ □ □ □ =◦◦◦◦◦◦◦◦◦◦◦◦ □ □ □ □
 □ □ □ □ ◦◦◦◦◦◦◦◦◦◦◦◦□ □ □ □ 
□ □ □ □ □ #=□ □ □ □ □ □ □ □ □
 □ □ □   ___| |__ (_)  □ □ □ 
□ □ □ □ / _ \ '_ \| | □ □ □ □
 □ □ □ |  __/ |_) | |  □ □ □ 
□ □ □ □ \___|_.__/|_| □ □ □ □
 □ □ □ □ □ □ □ □ □ □ □ □ □ □ ";

pub const EBI_ITSELF: EbiCommand = EbiCommand::Group { 
    name_short: "it", 
    name_long: Some("itself"), 
    explanation_short: "Print things on Ebi.",
    explanation_long: None,
    children: &[ 
        &EBI_ITSELF_GRAPH,
        &EBI_ITSELF_HTML,
        &EBI_ITSELF_JAVA,
        &EBI_ITSELF_LOGO,
        &EBI_ITSELF_MANUAL,
        &EBI_ITSELF_PM4PY,
     ]
};

pub const EBI_ITSELF_LOGO: EbiCommand = EbiCommand::Command {
    name_short: "log", 
    name_long: Some("logo"), 
    library_name: "ebi_commands::ebi_command_itself::EBI_ITSELF_LOGO",
    explanation_short: "Print the logo of Ebi.", 
    explanation_long: None, 
    cli_command: None, 
    latex_link: None, 
    exact_arithmetic: false, 
    input_types: &[], 
    input_names: &[],
    input_helps: &[],
    execute: |_, _| Ok(EbiOutput::String(LOGO.to_string())), 
    output_type: &EbiOutputType::String
};

pub const EBI_ITSELF_MANUAL: EbiCommand = EbiCommand::Command { 
    name_short: "man", 
    name_long: Some("manual"), 
    library_name: "ebi_commands::ebi_command_itself::EBI_ITSELF_MANUAL",
    explanation_short: "Print the automatically generated parts of the manual of Ebi in Latex format.", 
    explanation_long: None, 
    cli_command: None, 
    latex_link: None, 
    exact_arithmetic: false, 
    input_types: &[], 
    input_names: &[],
    input_helps: &[],
    execute: |_, _| Ok(manual()?), 
    output_type: &EbiOutputType::String
};

pub const EBI_ITSELF_GRAPH: EbiCommand = EbiCommand::Command { 
    name_short: "graph", 
    name_long: None, 
    library_name: "ebi_commands::ebi_command_itself::EBI_ITSELF_GRAPH",
    explanation_short: "Print the graph of Ebi.", 
    explanation_long: None, 
    cli_command: None, 
    latex_link: None, 
    exact_arithmetic: false, 
    input_types: &[], 
    input_names: &[],
    input_helps: &[],
    execute: |_, _| {
        let svg = graph()?.to_svg_mut()?;
        Ok(EbiOutput::Object(EbiObject::ScalableVectorGraphics(svg)))
    }, 
    output_type: &EbiOutputType::ObjectType(EbiObjectType::ScalableVectorGraphics)
};

pub const EBI_ITSELF_JAVA: EbiCommand = EbiCommand::Command { 
    name_short: "java", 
    name_long: None, 
    library_name: "ebi_commands::ebi_command_itself::EBI_ITSELF_JAVA",
    explanation_short: "Print the classes for Java.", 
    explanation_long: None, 
    cli_command: None, 
    latex_link: None, 
    exact_arithmetic: false, 
    input_types: &[], 
    input_names: &[],
    input_helps: &[],
    execute: |_, _| Ok(prom_link::print_java_plugins()?), 
    output_type: &EbiOutputType::String
};
pub const EBI_ITSELF_HTML: EbiCommand = EbiCommand::Command { 
    name_short: "html", 
    name_long: None, 
    library_name: "ebi_commands::ebi_command_itself::EBI_ITSELF_HTML",
    explanation_short: "Print parts of the website.", 
    explanation_long: None, 
    cli_command: None, 
    latex_link: None, 
    exact_arithmetic: false, 
    input_types: &[], 
    input_names: &[],
    input_helps: &[],
    execute: |_, _| Ok(EbiOutput::String(html())), 
    output_type: &EbiOutputType::String
};

pub const EBI_ITSELF_PM4PY: EbiCommand = EbiCommand::Command { 
    name_short: "pm4py", 
    name_long: None, 
    library_name: "ebi_commands::ebi_command_itself::EBI_ITSELF_PM4PY",
    explanation_short: "Generate the module exposed to PM4Py with all functions.", 
    explanation_long: None, 
    cli_command: None, 
    latex_link: None, 
    exact_arithmetic: false, 
    input_types: &[], 
    input_names: &[],
    input_helps: &[],
    execute: |_, _| Ok(generate_pm4py_module()?), 
    output_type: &EbiOutputType::String
};


fn manual() -> Result<EbiOutput> {
    let mut f = vec![];

    //version
    writeln!(f, "\\def\\version{{{}}}", env!("CARGO_PKG_VERSION"))?;

    //statistics
    writeln!(f, "\\def\\numberofcommands{{{}}}", EBI_COMMANDS.get_command_paths().len())?;
    writeln!(f, "\\def\\numberoftraits{{{}}}", EbiTrait::iter().len())?;
    writeln!(f, "\\def\\numberoffilehandlers{{{}}}", EBI_FILE_HANDLERS.len())?;

    //command list
    writeln!(f, "\\def\\ebicommandlist{{\\begin{{itemize}}")?;
    for path in EBI_COMMANDS.get_command_paths() {
        writeln!(f, "\\item\\texttt{{{}}} or \\texttt{{{}}} (Section~\\ref{{command:{}}})", EbiCommand::path_to_string(&path), EbiCommand::path_to_short_string(&path), EbiCommand::path_to_string(&path))?;
    }
    writeln!(f, "\\end{{itemize}}}}")?;

    //commands
    writeln!(f, "\\def\\ebicommands{{")?;
    for path in EBI_COMMANDS.get_command_paths() {
        writeln!(f, "\\subsection{{\\texttt{{{}}}}}", EbiCommand::path_to_string(&path))?;
        writeln!(f, "\\label{{command:{}}}", EbiCommand::path_to_string(&path))?;

        if let EbiCommand::Command { name_long, explanation_short, explanation_long, latex_link, cli_command, exact_arithmetic, input_types: input_typess, input_names, input_helps, output_type, .. } = path[path.len()-1] {

            //alias
            if let Some(_) = name_long {
                writeln!(f, "Alias: \\texttt{{{}}}.\\\\", EbiCommand::path_to_short_string(&path))?;
            }

            //explanation
            if let Some(l) = explanation_long {
                writeln!(f, "{}\\\\", l)?;
            } else {
                writeln!(f, "{}\\\\", explanation_short)?;
            }

            //latex link
            if let Some(link) = latex_link {
                writeln!(f, "More information: {}.\\\\", link)?;
            }

            //parameters table
            writeln!(f, "\\begin{{tabularx}}{{\\linewidth}}{{lX}}")?;
            writeln!(f, "\\toprule")?;
            writeln!(f, "Parameter \\\\\\midrule")?;

            //standard parameters
            for (input_name, (input_types, input_help)) in input_names.iter().zip(input_typess.iter().zip(input_helps.iter())) {
                write!(f, "<\\texttt{{{}}}>", input_name.escape_latex())?;    
                writeln!(f, "&{}\\\\", input_help)?;

                //mandatoryness
                if let Some(default) = ebi_input::default(input_types) {
                    writeln!(f, "&\\textit{{Mandatory:}} \\quad no: if no value is provided, a default of {} will be used. It can also be provided on STDIN by giving a `-' on the command line.\\\\", default)?;
                } else {
                    writeln!(f, "&\\textit{{Mandatory:}} \\quad yes, though it can be given on STDIN by giving a `-' on the command line.\\\\")?;
                }

                //accepted values
                writeln!(f, "&\\textit{{Accepted values:}}\\quad {}.\\\\", EbiInputType::get_possible_inputs_with_latex(input_types).join_with(", ", " and "))?;
            }

            //custom parameters
            if let Some(fu) = cli_command {
                for arg in (fu)(Command::new("")).get_arguments() {
                    if let Some(short) = arg.get_short() {
                        if let Some(long) = arg.get_long() {
                            writeln!(f, "-\\texttt{{{}}} or --\\texttt{{{}}}", short, long)?;
                        } else {
                            writeln!(f, "-\\texttt{{{}}}", short)?;
                        }
                    } else {
                        if let Some(long) = arg.get_long() {
                            writeln!(f, "--\\texttt{{{}}}", long)?;
                        } else {
                            writeln!(f, "<\\texttt{{{}}}>", arg.get_value_names().unwrap()[0])?;
                        }
                    }
                    writeln!(f, "&{}\\\\", if let Some(h) = arg.get_long_help() {h.to_string()} else {arg.get_help().unwrap().to_string()})?;

                    if arg.get_short().is_none() && arg.get_long().is_none() {
                        //custom argument
                        if let Some(default) = arg.get_default_values().iter().next() {
                            writeln!(f, "&\\textit{{Mandatory:}}\\quad {}\\\\", if arg.is_required_set() {"yes".to_owned()} else {format!("no; if not provided, a default value of {} will be used", default.to_string_lossy())} )?;
                        } else {
                            writeln!(f, "&\\textit{{Mandatory:}}\\quad {}\\\\", if arg.is_required_set() {"yes"} else {"no"} )?;
                        }
                    } else {
                        writeln!(f, "&\\textit{{Mandatory:}}\\quad no\\\\")?;
                    }
                }
            }

            //output
            writeln!(f, "\\texttt{{-o}} or \\texttt{{--output}} <\\texttt{{FILE}}> &")?;
            if output_type.get_exporters().len() == 1 {
                let exporter = output_type.get_exporters().remove(0);
                writeln!(f, "The {} file to which the result must be written. If the parameter is not given, the results will be written to STDOUT.\\\\", exporter)?;
            } else {
                writeln!(f, "The file to which the results must be written. Based on the file extension, Ebi will output either {}.", output_types(output_type))?;
                writeln!(f, "If the parameter is not given, the results will be written to STDOUT as {} {}.\\\\", output_type.get_default_exporter().get_article(), output_type.get_default_exporter())?;
            }
            writeln!(f, "&\\textit{{Mandatory:}} \\quad no\\\\")?;
            
            if *exact_arithmetic {
                writeln!(f, "\\texttt{{-a}} or \\texttt{{--approximate}} & Use approximate arithmetic instead of exact arithmetic.\\\\")?;
                writeln!(f, "&\\textit{{Mandatory:}}\\quad no\\\\")?;
            }

            writeln!(f, "\\bottomrule")?;
            writeln!(f, "\\end{{tabularx}}")?;

            //output
            writeln!(f, "\\noindent Output: {}, which can be written as {}.", output_type, output_types(output_type))?;

            //ProM
            if path.last().unwrap().is_in_java() {
                writeln!(f, "\\\\This command is available in Java and ProM.")?;
            } else {
                writeln!(f, "\\\\This command is not available in Java and ProM.")?;
            }

            //pm4py
            writeln!(f, "\\\\This command is available in the {} Python package using the function {}", PM4PY_PACKAGE, pm4py_function_name(&path).escape_latex())?;
        }
    }
    writeln!(f, "}}")?;

    //file handlers
    writeln!(f, "\\long\\def\\ebifilehandlers{{")?;
    for (i, file_handler) in EBI_FILE_HANDLERS.iter().enumerate() {
        if i != 0 {
            writeln!(f, "\\clearpage")?;
        }
        writeln!(f, "\\subsection{{{} (.{})}}", file_handler.name.to_sentence_case(), file_handler.file_extension)?;
        writeln!(f, "\\label{{filehandler:{}}}", file_handler.name)?;
        writeln!(f, "Import as objects: {}.", or_none(&file_handler.object_importers.iter().map(|importer| importer.to_string()).collect::<Vec<_>>().join(", ")))?;
        writeln!(f, "\\\\Import as traits: {}.", or_none(&file_handler.trait_importers.iter().map(|importer| importer.to_string()).collect::<Vec<_>>().join(", ")))?;
        writeln!(f, "\\\\Input to commands: {}.", or_none(&file_handler.get_applicable_commands().iter().map(
            |path| format!("\\\\\\null\\qquad\\hyperref[command:{}]{{\\texttt{{{}}}}} (Section~\\ref{{command:{}}})", EbiCommand::path_to_string(path), EbiCommand::path_to_string(path), EbiCommand::path_to_string(path)))
            .collect::<Vec<_>>().join("")))?;
        writeln!(f, "\\\\Output of commands: {}.", or_none(&file_handler.get_producing_commands().iter().map(
            |path| format!("\\\\\\null\\qquad\\hyperref[command:{}]{{\\texttt{{{}}}}} (Section~\\ref{{command:{}}})", EbiCommand::path_to_string(path), EbiCommand::path_to_string(path), EbiCommand::path_to_string(path)))
            .collect::<Vec<_>>().join("")))?;

        {
            let mut to_java = false;
            let mut from_java = false;
            for java in file_handler.java_object_handlers {
                if java.translator_ebi_to_java.is_some() {
                    to_java = true;
                }
                if java.translator_java_to_ebi.is_some() {
                    from_java = true;
                }
            }
            if to_java && from_java {
                writeln!(f, "\\\\{} {} can be imported and exported between Ebi, and ProM and Java.", file_handler.article.to_string().to_sentence_case(), file_handler)?;
            } else if to_java {
                writeln!(f, "\\\\{} {} can be exported from Ebi to ProM and Java.", file_handler.article.to_string().to_sentence_case(), file_handler)?;
            } else if from_java {
                writeln!(f, "\\\\{} {} can be imported from ProM and Java to Ebi.", file_handler.article.to_string().to_sentence_case(), file_handler)?;
            } else {
                writeln!(f, "\\\\{} {} can be neither imported nor exported between Ebi, and ProM and Java.", file_handler.article.to_string().to_sentence_case(), file_handler)?;
            }
        }

        writeln!(f, "\\\\File format specification:\n{}", file_handler.format_specification)?;
    }
    writeln!(f, "}}")?;

    //file handlers list
    writeln!(f, "\\def\\ebifilehandlerlist{{\\begin{{itemize}}")?;
    for file_handler in EBI_FILE_HANDLERS {
        writeln!(f, "\\item {} (.{}) (Section~\\ref{{filehandler:{}}})", file_handler.name, file_handler.file_extension, file_handler.name)?;
    }
    writeln!(f, "\\end{{itemize}}}}")?;

    //trait list
    writeln!(f, "\\def\\ebitraitlist{{\\begin{{itemize}}")?;
    for etrait in EbiTrait::iter() {
        writeln!(f, "\\item {}.", etrait.to_string().to_sentence_case())?;
        writeln!(f, "\\\\{}", etrait.get_explanation())?;

        writeln!(f, "\\\\File types that can be imported as {} {}: {}.", 
            etrait.get_article(), 
            etrait, 
            or_none(
                    &etrait.get_file_handlers().iter().map(|file_handler| 
                        format!("\\\\\\null\\qquad\\hyperref[filehandler:{}]{{{}}} (.{} -- Section~\\ref{{filehandler:{}}})", file_handler.name, file_handler.name, file_handler.file_extension, file_handler.name)
                    ).collect::<Vec<_>>().join(", "))
            )?;

        writeln!(f, "\\\\Commands that accept {} {} as input: {}", etrait.get_article(), etrait, or_none(&etrait.get_applicable_commands().iter().map(
            |path| format!("\\\\\\null\\qquad\\hyperref[command:{}]{{\\texttt{{{}}}}} (Section~\\ref{{command:{}}})", EbiCommand::path_to_string(path), EbiCommand::path_to_string(path), EbiCommand::path_to_string(path)))
            .collect::<Vec<_>>().join("")))?;
    }
    writeln!(f, "\\end{{itemize}}}}")?;

    //object type list
    writeln!(f, "\\def\\ebiobjecttypelist{{\\begin{{itemize}}")?;
    for object_type in EbiObjectType::iter() {
        writeln!(f, "\\item {}", object_type)?;
    }
    writeln!(f, "\\end{{itemize}}}}")?;

    //prom commands
    writeln!(f, "\\def\\promcommands{{{}}}", 
        EBI_COMMANDS.get_command_paths().iter().filter_map(
            |path| 
            {
                if path.last().unwrap().is_in_java() {
                    Some(format!("\\\\\\null\\qquad\\hyperref[command:{}]{{\\texttt{{{}}}}} (Section~\\ref{{command:{}}})", EbiCommand::path_to_string(path), EbiCommand::path_to_string(path), EbiCommand::path_to_string(path)))
                } else {
                    None
                }
            }).collect::<Vec<_>>().join("")
        )?;

    //prom input list
    writeln!(f, "\\def\\ebiprominput{{\\begin{{itemize}}")?;
    for r#trait in EbiTrait::iter() {
        let java_object_handlers = r#trait.get_java_object_handlers_that_can_import();
        if !java_object_handlers.is_empty() {
            writeln!(f, "\\item {} (trait).\\\\Java class: {}.", 
                r#trait.to_string().to_sentence_case(), 
                java_object_handlers.into_iter().map(|java_object_handler| java_object_handler.java_class).collect::<Vec<_>>().join(", ")
            )?;
        }
    }
    for object_type in EbiObjectType::iter() {
        let java_object_handlers = get_java_object_handlers_that_can_import(&object_type);
        if !java_object_handlers.is_empty() {
            writeln!(f, "\\item {}.\\\\Java class: {}.", 
                object_type.to_string().to_sentence_case(), 
                java_object_handlers.into_iter().map(|java_object_handler| java_object_handler.java_class).collect::<Vec<_>>().join(", ")
            )?;
        }
    }
    for output_type in EbiInputType::iter() {
        let java_object_handlers = output_type.get_java_object_handlers_that_can_import();
        if !java_object_handlers.is_empty() {
            writeln!(f, "\\item {}.\\\\Java class: {}.", 
                output_type.to_string().to_sentence_case(), 
                java_object_handlers.into_iter().map(|java_object_handler| java_object_handler.java_class).collect::<Vec<_>>().join(", ")
            )?;
        }
    }
    writeln!(f, "\\end{{itemize}}}}")?;

    //prom output list
    writeln!(f, "\\def\\ebipromoutput{{\\begin{{itemize}}")?;
    for object_type in EbiObjectType::iter() {
        let java_object_handlers = get_java_object_handlers_that_can_export(&object_type);
        if !java_object_handlers.is_empty() {
            writeln!(f, "\\item {}.\\\\Java class: {}.", 
                object_type.to_string().to_sentence_case(), 
                java_object_handlers.into_iter().map(|java_object_handler| java_object_handler.java_class).collect::<Vec<_>>().join(", ")
            )?;
        }
    }
    for output_type in EbiOutputType::iter() {
        let java_object_handlers = output_type.get_java_object_handlers_that_can_export();
        if !java_object_handlers.is_empty() {
            writeln!(f, "\\item {}.\\\\Java class: {}.", 
                output_type.to_string().to_sentence_case(), 
                java_object_handlers.into_iter().map(|java_object_handler| java_object_handler.java_class).collect::<Vec<_>>().join(", ")
            )?;
        }
    }
    writeln!(f, "\\end{{itemize}}}}")?;

    Ok(EbiOutput::String(String::from_utf8(f).unwrap()))
}

pub fn or_none(string: &str) -> &str {
    if string.is_empty() {
        return "none";
    }
    string
}

pub fn output_types(output_type: &EbiOutputType) -> String {
    let mut list = output_type.get_exporters().into_iter().map(
        |exp| exp.get_article().to_string() 
        + " " 
        + match exp {
            EbiExporter::Object(_, file_handler) => format!("{} (.{} -- Section~\\ref{{filehandler:{}}})", file_handler.name, file_handler.file_extension, file_handler.name),
            _ => exp.to_string()
        }.as_str()
    ).collect::<Vec<_>>();
    if list.len() == 1 {
        return list.remove(0)
    }
    let (last, list) = list.split_last().unwrap();
    format!("{} {} {}", list.join(", "), "or", last)
}

pub fn graph() -> Result<VisualGraph> {
    let mut graph = VisualGraph::new(Orientation::LeftToRight);

    //traits
    let mut traits = HashMap::new();
    for etrait in EbiTrait::iter() {
        let shape = ShapeKind::new_box(&("trait ".to_owned() + etrait.to_string().as_str()));
        let look = StyleAttr::simple();
        let orientation = Orientation::LeftToRight;
        let mut size = get_shape_size(orientation, &shape, look.font_size, false);
        scale(&mut size);
        let node = Element::create(shape, look, orientation, size);
        let nodeh = graph.add_node(node);

        traits.insert(etrait, nodeh);
    }

    //object types
    let mut object_types = HashMap::new();
    for object_type in EbiObjectType::iter() {
        let shape = ShapeKind::new_box(&("type ".to_owned() + object_type.to_string().as_str()));
        let mut look = StyleAttr::simple();
        look.fill_color = Color::from_name("lightgray");
        let orientation = Orientation::LeftToRight;
        let mut size = get_shape_size(orientation, &shape, look.font_size, false);
        scale(&mut size);
        let node = Element::create(shape, look, orientation, size);
        let nodeh = graph.add_node(node);

        object_types.insert(object_type, nodeh);
    }

    //file handlers
    let mut file_handlers = HashMap::new();
    for file_handler in EBI_FILE_HANDLERS {
        let shape = ShapeKind::new_circle(&(".".to_owned() + file_handler.file_extension.to_string().as_str()));
        let look = StyleAttr::simple();
        let orientation = Orientation::LeftToRight;
        let mut size = get_shape_size(orientation, &shape, look.font_size-5, false);
        scale(&mut size);
        let node = Element::create(shape, look, orientation, size);
        let nodeh = graph.add_node(node);

        file_handlers.insert(file_handler, nodeh);
    }

    //files to traits
    for etrait in EbiTrait::iter() {
        let to = traits.get(&etrait).unwrap();
        for file_handler in etrait.get_file_handlers() {
            let from = file_handlers.get(&file_handler).unwrap();
            let arrow = Arrow::simple("");
            graph.add_edge(arrow, *from, *to);
        }
    }

    //commands
    let mut commands = HashMap::new();
    for command in EBI_COMMANDS.get_command_paths() {
        let shape = ShapeKind::new_double_circle(&EbiCommand::path_to_short_string(&command));
        let look = StyleAttr::simple();
        let orientation = Orientation::LeftToRight;
        let size = get_shape_size(orientation, &shape, look.font_size, false);
        let node = Element::create(shape, look, orientation, size);
        let nodeh = graph.add_node(node);

        commands.insert(command, nodeh);
    }

    //command trait inputs
    for etrait in EbiTrait::iter() {
        let source = traits.get(&etrait).unwrap();
        for command in etrait.get_applicable_commands() {
            let target = commands.get(&command).unwrap();
            let arrow = Arrow::simple("");
            graph.add_edge(arrow, *source, *target);
        }
    }

    //object types to commands
    for object_type in EbiObjectType::iter() {
        let source = object_types.get(&object_type).unwrap();
        for command in get_applicable_commands(&object_type) {
            let target = commands.get(&command).unwrap();
            let arrow = Arrow::simple("");
            graph.add_edge(arrow, *source, *target);
        }
    }

    //files to object types
    for object_type in EbiObjectType::iter() {
        let to = object_types.get(&object_type).unwrap();
        for file_handler in get_file_handlers(&object_type) {
            let from = file_handlers.get(&file_handler).unwrap();
            let arrow = Arrow::simple("");
            graph.add_edge(arrow, *from, *to);
        }
    }

    return Ok(graph)
}

pub fn scale(point: &mut Point) {
    point.x *= 0.5;
}

pub fn html() -> String {
    let commands_html = EBI_COMMANDS
        .get_command_paths()
        .iter()
        .filter_map(|path| {
            if path[1].long_name() != EBI_ITSELF.long_name() {
                Some(format!(
                    "<i>{}</i>. {}",
                    EbiCommand::path_to_string(&path[1..]).to_sentence_case(),
                    path.last().unwrap().explanation_short()
                ))
            } else {
                None
            }
        })
        .collect::<Vec<_>>()
        .join("</li><li>");

    let file_formats_html = EBI_FILE_HANDLERS
        .iter()
        .map(|file_handler| {
            format!(
                "{} (.{})",
                file_handler.name.to_sentence_case(),
                file_handler.file_extension
            )
        })
        .collect::<Vec<_>>()
        .join("</li><li>");

    format!(
        "<h2>Commands</h2>\
        Ebi offers the following comands and techniques. \
        Please refer to the <a href=\"https://git.rwth-aachen.de/rwth-bpm/rustlibrary/-/raw/main/build/nightly/manual.pdf?ref_type=heads&inline=true\">manual</a> for more information. \
        <ul><li>{}</li></ul>\
        <h2>Supported file formats</h2>\
        <ul><li>{}</li></ul>",
        commands_html, file_formats_html
    )
}