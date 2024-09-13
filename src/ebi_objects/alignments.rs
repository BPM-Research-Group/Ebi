use std::{fmt::Display, str::FromStr};
use anyhow::{anyhow, Context, Error, Result};

use crate::{activity_key::{Activity, ActivityKey}, ebi_commands::ebi_command_info::Infoable, ebi_traits::ebi_trait_stochastic_semantics::TransitionIndex, export::{EbiOutput, Exportable}, import::Importable, line_reader::LineReader};

use super::ebi_object::EbiObject;

pub const HEADER: &str = "alignments";

pub struct Alignments {
    activity_key: ActivityKey,
    alignments: Vec<Vec<(Option<Activity>, Option<(Option<Activity>, TransitionIndex)>)>>// (log move, model move)
}

impl Alignments {
    pub fn get_activity_key(&self) -> &ActivityKey {
        &self.activity_key
    }

    pub fn get_activity_key_mut(&mut self) -> &mut ActivityKey {
        &mut self.activity_key
    }
}

impl Exportable for Alignments {
    fn export_from_object(object: EbiOutput, f: &mut dyn std::io::Write) -> Result<()> {
        match object {
            EbiOutput::Object(EbiObject::Alignments(alignments)) => alignments.export(f),
            _ => unreachable!()
        }
    }

    fn export(&self, f: &mut dyn std::io::Write) -> Result<()> {
        Ok(write!(f, "{}", self)?)
    }
}

impl Display for Alignments {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        writeln!(f, "{}", HEADER)?;
        
        writeln!(f, "# number of alignments\n{}", self.alignments.len())?;

        for (i, moves) in self.alignments.iter().enumerate() {
            writeln!(f, "# alignment {}", i)?;
            writeln!(f, "# number of moves\n{}", moves.len())?;

            for (j, (log, model)) in moves.iter().enumerate() {
                writeln!(f, "# move {}", j)?;

                match (log, model) {
                    (None, None) => {
                        writeln!(f, "error: empty move encountered"); //Rust defines that string building should never fail
                    },
                    (None, Some((Some(activity), transition))) => {
                        //model move
                        writeln!(f, "model move")?;
                        writeln!(f, "label {}", self.activity_key.get_activity_label(activity))?;
                        writeln!(f, "{}", transition)?;
                    },
                    (None, Some((None, transition))) => {
                        //model move on silent transition
                        writeln!(f, "model move")?;
                        writeln!(f, "silent")?;
                        writeln!(f, "{}", transition)?;
                    },
                    (Some(activity), None) => { 
                        //log move
                        writeln!(f, "log move")?;
                        writeln!(f, "label {}", self.activity_key.get_activity_label(activity))?;
                    },
                    (Some(activity), Some((_, transition))) => {
                        //synchronous move
                        writeln!(f, "synchronous move")?;
                        writeln!(f, "label {}", self.activity_key.get_activity_label(activity))?;
                        writeln!(f, "{}", transition)?;
                    },
                }
            }
        }

        write!(f, "")
    }
}

impl Importable for Alignments {
    fn import_as_object(reader: &mut dyn std::io::BufRead) -> anyhow::Result<super::ebi_object::EbiObject> {
        Ok(EbiObject::Alignments(Self::import(reader)?))
    }

    fn import(reader: &mut dyn std::io::BufRead) -> anyhow::Result<Self> where Self: Sized {
        let mut lreader = LineReader::new(reader);
        let mut activity_key = ActivityKey::new();

        let head = lreader.next_line_string().with_context(|| format!("failed to read header, which should be {}", HEADER))?;
        if head != HEADER {
            return Err(anyhow!("first line should be exactly `{}`, but found `{}`", HEADER, lreader.get_last_line()));
        }

        let number_of_alignments = lreader.next_line_index().context("failed to read number of alignments")?;

        let mut alignments = Vec::new();
        for alignment_number in 0 .. number_of_alignments {

            let mut moves = vec![];

            let number_of_moves = lreader.next_line_index().with_context(|| format!("failed to read number of moves in alignemnt {}", alignment_number))?;

            for move_number in 0 .. number_of_moves {
            
                //read type of move
                let move_type_line = lreader.next_line_string().with_context(|| format!("failed to read type of move {} of alignment {}", move_number, alignment_number))?;
                if move_type_line.trim_start().starts_with("log move") {

                    //read a log move
                    let label_line = lreader.next_line_string().with_context(|| format!("failed to read label of log move {} of alignment {}", move_number, alignment_number))?;
                    if label_line.trim_start().starts_with("label ") {
                        let label = label_line[6..].to_string();
                        let activity = activity_key.process_activity(&label);
                        moves.push((Some(activity), None));
                    } else {
                        return Err(anyhow!("Line must have a label"));
                    }
                    

                } else if move_type_line.starts_with("model move") {

                    //read the label
                    let label_line = lreader.next_line_string().with_context(|| format!("failed to read label of model move {} of alignment {}", move_number, alignment_number))?;
                    let activity = if label_line.trim_start().starts_with("label ") {
                        let label = label_line.trim_start()[6..].to_string();
                        let activity = activity_key.process_activity(&label);
                        Some(activity)
                    } else {
                        None
                    };

                    //read the transition
                    let transition = lreader.next_line_index().with_context(|| format!("failed to read transition of move {} in alignemnt {}", move_number, alignment_number))?;

                    moves.push((None, Some((activity, transition))));

                } else if move_type_line.starts_with("synchronous move") {

                    //read the label
                    let label_line = lreader.next_line_string().with_context(|| format!("failed to read label of synchronous move {} of alignment {}", move_number, alignment_number))?;
                    if label_line.trim_start().starts_with("label ") {
                        let label = label_line.trim_start()[6..].to_string();
                        let activity = activity_key.process_activity(&label);

                        //read the transition
                        let transition = lreader.next_line_index().with_context(|| format!("failed to read transition of move {} in alignemnt {}", move_number, alignment_number))?;

                        moves.push((Some(activity), Some((Some(activity), transition))));

                    } else {
                        return Err(anyhow!("Line must have a label"));
                    }

                } else {
                    return Err(anyhow!("Type of log move {} of alignment {} is not recognised.", move_number, alignment_number));
                }
            }

            alignments.push(moves);
        }


        Ok(Self {
            activity_key: activity_key,
            alignments: alignments
        })
    }
}

impl FromStr for Alignments {
    type Err = Error;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let mut reader = std::io::Cursor::new(s);
        Self::import(&mut reader)
    }
}

impl Infoable for Alignments {
    fn info(&self, f: &mut impl std::io::Write) -> Result<()> {
        writeln!(f, "Number of alignments\t\t{}", self.alignments.len())?;
        Ok(write!(f, "")?)
    }
}