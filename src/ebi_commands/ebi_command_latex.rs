use std::{io::Write, collections::HashMap};
use clap::Command;
use anyhow::Result;
use layout::{backends::svg::SVGWriter, core::{base::Orientation, color::Color, geometry::Point, style::StyleAttr}, std_shapes::{render::get_shape_size, shapes::{Arrow, Element, ShapeKind}}, topo::layout::VisualGraph};
use strum::IntoEnumIterator;

use crate::{ebi_framework::{ebi_command::{EbiCommand, EBI_COMMANDS}, ebi_file_handler::EBI_FILE_HANDLERS, ebi_input::EbiInputType, ebi_object::EbiObjectType, ebi_output::{EbiOutput, EbiOutputType}, ebi_trait::EbiTrait}, text::Joiner};


pub const EBI_LATEX: EbiCommand = EbiCommand::Group { 
    name_short: "latex", 
    name_long: Some("latex-help"), 
    explanation_short: "Print the help of Ebi in Latex format.",
    explanation_long: None,
    children: &[ 
        &EBI_LATEX_GRAPH,
        &EBI_LATEX_MANUAL,
     ]
};

pub const EBI_LATEX_MANUAL: EbiCommand = EbiCommand::Command { 
    name_short: "man", 
    name_long: Some("manual"), 
    explanation_short: "Print the automatically generated parts of the manual of Ebi in Latex format.", 
    explanation_long: None, 
    cli_command: None, 
    latex_link: None, 
    exact_arithmetic: false, 
    input_types: &[], 
    input_names: &[],
    input_helps: &[],
    execute: |_, _| Ok(manual()?), 
    output: &EbiOutputType::String
};

pub const EBI_LATEX_GRAPH: EbiCommand = EbiCommand::Command { 
    name_short: "graph", 
    name_long: None, 
    explanation_short: "Print the graph of Ebi.", 
    explanation_long: None, 
    cli_command: None, 
    latex_link: None, 
    exact_arithmetic: false, 
    input_types: &[], 
    input_names: &[],
    input_helps: &[],
    execute: |_, _| Ok(graph()?), 
    output: &EbiOutputType::String
};

fn manual() -> Result<EbiOutput> {
    let mut f = vec![];

    //version
    writeln!(f, "\\def\\version{{{}}}", env!("CARGO_PKG_VERSION"))?;

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

        if let EbiCommand::Command { name_long, explanation_short, explanation_long, latex_link, cli_command, exact_arithmetic, input_types: input_typess, input_names, input_helps, output: output_type, .. } = path[path.len()-1] {

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
                write!(f, "<\\texttt{{{}}}>", input_name.replace("_", "\\_"))?;    
                writeln!(f, "&{}\\\\", input_help)?;

                //mandatoryness
                writeln!(f, "&\\textit{{Mandatory:}} \\quad yes, though it can be given on STDIN by giving a `-' on the command line.\\\\")?;

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

                        writeln!(f, "&\\textit{{Mandatory:}}\\quad {}\\\\", if arg.is_required_set() {"yes"} else {"no"} )?;
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
                writeln!(f, "The file to which the results must be written. Based on the file extension, Ebi will output either {}.", output_type.exporters_as_strings_with_articles("or"))?;
                writeln!(f, "If the parameter is not given, the results will be written to STDOUT as {} {}.\\\\", output_type.get_exporters()[0].get_article(), output_type.get_exporters()[0])?;
            }
            writeln!(f, "&\\textit{{Mandatory:}} \\quad no\\\\")?;
            
            if *exact_arithmetic {
                writeln!(f, "\\texttt{{-a}} or \\texttt{{--approximate}} & Use approximate arithmetic instead of exact arithmetic.\\\\")?;
                writeln!(f, "&\\textit{{Mandatory:}}\\quad no\\\\")?;
            }

            writeln!(f, "\\bottomrule")?;
            writeln!(f, "\\end{{tabularx}}")?;

            //output
            writeln!(f, "Output: {}.", output_type)?;
        }
    }
    writeln!(f, "}}")?;

    //file handlers
    writeln!(f, "\\def\\ebifilehandlers{{")?;
    for file_handler in EBI_FILE_HANDLERS {
        writeln!(f, "\\subsection{{{} (.{})}}", file_handler.name, file_handler.file_extension)?;
        writeln!(f, "\\label{{filehandler:{}}}", file_handler.name)?;
        writeln!(f, "Import as objects: {}.", file_handler.object_importers.iter().map(|importer| importer.to_string()).collect::<Vec<_>>().join(", "))?;
        writeln!(f, "\\\\Import as traits: {}.", file_handler.trait_importers.iter().map(|importer| importer.to_string()).collect::<Vec<_>>().join(", "))?;
        writeln!(f, "\\\\Input to commands:{}.\n", file_handler.get_applicable_commands().iter().map(
            |path| format!("\\\\\\null\\qquad\\hyperref[command:{}]{{\\texttt{{{}}}}} (Section~\\ref{{command:{}}})", EbiCommand::path_to_string(path), EbiCommand::path_to_string(path), EbiCommand::path_to_string(path)))
            .collect::<Vec<_>>().join(""))?;
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
        writeln!(f, "\\item {}", etrait)?;
    }
    writeln!(f, "\\end{{itemize}}}}")?;

    //object type list
    writeln!(f, "\\def\\ebiobjecttypelist{{\\begin{{itemize}}")?;
    for object_type in EbiObjectType::iter() {
        writeln!(f, "\\item {}", object_type)?;
    }
    writeln!(f, "\\end{{itemize}}}}")?;

    Ok(EbiOutput::String(String::from_utf8(f).unwrap()))
}

pub fn graph() -> Result<EbiOutput> {
    let mut graph = VisualGraph::new(Orientation::LeftToRight);

    //traits
    let mut traits = HashMap::new();
    for etrait in EbiTrait::iter() {
        let shape = ShapeKind::new_box(&("trait ".to_owned() + &etrait.to_string()));
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
        let shape = ShapeKind::new_box(&("type ".to_owned() + &object_type.to_string()));
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
        let shape = ShapeKind::new_circle(&(".".to_owned() + &file_handler.file_extension.to_string()));
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
        for command in object_type.get_applicable_commands() {
            let target = commands.get(&command).unwrap();
            let arrow = Arrow::simple("");
            graph.add_edge(arrow, *source, *target);
        }
    }

    //files to object types
    for object_type in EbiObjectType::iter() {
        let to = object_types.get(&object_type).unwrap();
        for file_handler in object_type.get_file_handlers() {
            let from = file_handlers.get(&file_handler).unwrap();
            let arrow = Arrow::simple("");
            graph.add_edge(arrow, *from, *to);
        }
    }

    let mut svg = SVGWriter::new();
    graph.do_it(false, false, false, &mut svg);
    return Ok(EbiOutput::String(svg.finalize()));
}

pub fn scale(point: &mut Point) {
    point.x *= 0.5;
}