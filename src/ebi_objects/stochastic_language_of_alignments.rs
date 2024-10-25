use std::{fmt::Display, str::FromStr};
use anyhow::{anyhow, Context, Error, Result};

use crate::{ebi_framework::{activity_key::ActivityKey, ebi_file_handler::EbiFileHandler, ebi_input::{self, EbiInput, EbiObjectImporter}, ebi_object::EbiObject, ebi_output::{EbiObjectExporter, EbiOutput}, ebi_trait::FromEbiTraitObject, exportable::Exportable, importable::Importable, infoable::Infoable}, line_reader::LineReader, math::fraction::Fraction};

use super::alignments::Move;

pub const HEADER: &str = "stochastic alignments";

pub const EBI_ALIGNMENTS: EbiFileHandler = EbiFileHandler {
    name: "stochastic alignments",
    article: "",
    file_extension: "sali",
    validator: ebi_input::validate::<StochasticLanguageOfAlignments>,
    trait_importers: &[
        
    ],
    object_importers: &[
        EbiObjectImporter::StochasticLanguageOfAlignments(StochasticLanguageOfAlignments::import_as_object)
    ],
    object_exporters: &[ 
        EbiObjectExporter::StochasticLanguageOfAlignments(StochasticLanguageOfAlignments::export_from_object)
    ],
    java_object_handlers: &[]
};

pub struct StochasticLanguageOfAlignments {
    activity_key: ActivityKey,
    alignments: Vec<Vec<Move>>,
    weights: Vec<Fraction>
}

impl StochasticLanguageOfAlignments {
    pub fn new(activity_key: ActivityKey) -> Self {
        Self {
            activity_key: activity_key,
            alignments: vec![],
            weights: vec![],
        }
    }

    pub fn push(&mut self, alignment: Vec<Move>, weight: Fraction) {
        self.alignments.push(alignment);
        self.weights.push(weight);
    }

    pub fn len(&self) -> usize {
        self.alignments.len()
    }

    pub fn get(&self, index: usize) -> Option<&Vec<Move>> {
        self.alignments.get(index)
    }

    pub fn get_weight(&self, index: usize) -> Option<&Fraction> {
        self.weights.get(index)
    }

    pub fn get_activity_key(&self) -> &ActivityKey {
        &self.activity_key
    }

    pub fn get_activity_key_mut(&mut self) -> &mut ActivityKey {
        &mut self.activity_key
    }
}

impl Exportable for StochasticLanguageOfAlignments {
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

impl Display for StochasticLanguageOfAlignments {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        writeln!(f, "{}", HEADER)?;
        
        writeln!(f, "# number of alignments\n{}", self.alignments.len())?;

        for (i, moves) in self.alignments.iter().enumerate() {
            writeln!(f, "# alignment {}", i)?;
            writeln!(f, "# weight\n{}", self.weights[i])?;
            writeln!(f, "# number of moves\n{}", moves.len())?;

            for (j, movee) in moves.iter().enumerate() {
                writeln!(f, "# move {}", j)?;

                match movee {
                    Move::LogMove(activity) => {
                        writeln!(f, "log move")?;
                        writeln!(f, "label {}", self.activity_key.get_activity_label(activity))?;
                    },
                    Move::ModelMove(activity, transition) => {
                        writeln!(f, "model move")?;
                        writeln!(f, "label {}", self.activity_key.get_activity_label(activity))?;
                        writeln!(f, "{}", transition)?;
                    },
                    Move::SynchronousMove(activity, transition) => {
                        writeln!(f, "synchronous move")?;
                        writeln!(f, "label {}", self.activity_key.get_activity_label(activity))?;
                        writeln!(f, "{}", transition)?;
                    },
                    Move::SilentMove(transition) => {
                        writeln!(f, "silent move")?;
                        writeln!(f, "{}", transition)?;
                    },
                };
            }
        }

        write!(f, "")
    }
}

impl Importable for StochasticLanguageOfAlignments {
    fn import_as_object(reader: &mut dyn std::io::BufRead) -> Result<EbiObject> {
        Ok(EbiObject::StochasticLanguageOfAlignments(Self::import(reader)?))
    }

    fn import(reader: &mut dyn std::io::BufRead) -> anyhow::Result<Self> where Self: Sized {
        let mut lreader = LineReader::new(reader);
        let mut activity_key = ActivityKey::new();

        let head = lreader.next_line_string().with_context(|| format!("failed to read header, which should be {}", HEADER))?;
        if head != HEADER {
            return Err(anyhow!("first line should be exactly `{}`, but found `{}`", HEADER, lreader.get_last_line()));
        }

        let number_of_alignments = lreader.next_line_index().context("failed to read number of alignments")?;

        let mut alignments = vec![];
        let mut weights = vec![];
        for alignment_number in 0 .. number_of_alignments {

            let mut moves = vec![];

            let weight = lreader.next_line_weight().with_context(|| format!("failed to read weight of alignment {}", alignment_number))?;
            weights.push(weight);

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
                        moves.push(Move::LogMove(activity));
                    } else {
                        return Err(anyhow!("Line must have a label"));
                    }

                } else if move_type_line.trim_start().starts_with("model move") {

                    //read the label
                    let label_line = lreader.next_line_string().with_context(|| format!("failed to read label of model move {} of alignment {}", move_number, alignment_number))?;
                    let activity = if label_line.trim_start().starts_with("label ") {
                        let label = label_line.trim_start()[6..].to_string();
                        let activity = activity_key.process_activity(&label);
                        activity
                    } else {
                        return Err(anyhow!("Line must have a label"));
                    };

                    //read the transition
                    let transition = lreader.next_line_index().with_context(|| format!("failed to read transition of move {} in alignemnt {}", move_number, alignment_number))?;

                    moves.push(Move::ModelMove(activity, transition));

                } else if move_type_line.trim_start().starts_with("synchronous move") {

                    //read the label
                    let label_line = lreader.next_line_string().with_context(|| format!("failed to read label of synchronous move {} of alignment {}", move_number, alignment_number))?;
                    if label_line.trim_start().starts_with("label ") {
                        let label = label_line.trim_start()[6..].to_string();
                        let activity = activity_key.process_activity(&label);

                        //read the transition
                        let transition = lreader.next_line_index().with_context(|| format!("failed to read transition of move {} in alignemnt {}", move_number, alignment_number))?;

                        moves.push(Move::SynchronousMove(activity, transition));

                    } else {
                        return Err(anyhow!("Line must have a label"));
                    }

                } else if move_type_line.trim_start().starts_with("silent move") {

                    //read the transition
                    let transition = lreader.next_line_index().with_context(|| format!("failed to read transition of move {} in alignemnt {}", move_number, alignment_number))?;

                    moves.push(Move::SilentMove(transition));

                } else {
                    return Err(anyhow!("Type of log move {} of alignment {} is not recognised.", move_number, alignment_number));
                }
            }

            alignments.push(moves);
        }


        Ok(Self {
            activity_key: activity_key,
            alignments: alignments,
            weights: weights
        })
    }
}

impl FromStr for StochasticLanguageOfAlignments {
    type Err = Error;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let mut reader = std::io::Cursor::new(s);
        Self::import(&mut reader)
    }
}

impl Infoable for StochasticLanguageOfAlignments {
    fn info(&self, f: &mut impl std::io::Write) -> Result<()> {
        writeln!(f, "Number of alignments\t\t{}", self.alignments.len())?;
        Ok(write!(f, "")?)
    }
}

impl FromEbiTraitObject for StochasticLanguageOfAlignments {
    fn from_trait_object(object: EbiInput) -> Result<Box<Self>> {
        match object {
            EbiInput::Object(EbiObject::StochasticLanguageOfAlignments(e), _) => Ok(Box::new(e)),
            _ => Err(anyhow!("cannot read {} {} as a finite language", object.get_type().get_article(), object.get_type()))
        }
    }
}