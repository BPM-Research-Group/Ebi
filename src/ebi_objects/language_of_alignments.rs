use anyhow::{Context, Error, Result, anyhow};
use std::{fmt::Display, str::FromStr};

use crate::{
    ebi_framework::{
        activity_key::{Activity, ActivityKey, ActivityKeyTranslator, TranslateActivityKey},
        ebi_file_handler::EbiFileHandler,
        ebi_input::{self, EbiObjectImporter},
        ebi_object::EbiObject,
        ebi_output::{EbiObjectExporter, EbiOutput},
        exportable::Exportable,
        importable::Importable,
        infoable::Infoable,
    },
    ebi_traits::ebi_trait_stochastic_semantics::TransitionIndex,
    line_reader::LineReader,
};

pub const HEADER: &str = "language of alignments";

pub const FORMAT_SPECIFICATION: &str = "A language of alignments is a line-based structure. Lines starting with a \\# are ignored.
    This first line is exactly `language of alignments'.
    The second line is the number of alignments in the language.
    For each alignment, the first line contains the number of moves in the alignment.
    Then, each move is given as either 
    \\begin{itemize}
        \\item `synchronous move', followed by a line with the word `label' followed by a space and the activity label, which is followed with a line with the index of the involved transition.
        \\item `silent move', followed by a line with the index of the silent transition.
        \\item `log move', followed by a line with the word `label', then a space, and then the activity label.
        \\item `model move', followed by a line with the word `label' followed by a space and the activity label, which is followed with a line with the index of the involved ransition.
    \\end{itemize}
    Note that the Semantics trait of Ebi, which is what most alignment computations use, requires that every final marking is a deadlock.
    Consequently, an implicit silent transition may be added by the Semantics trait that is not in the model.
    
    For instance:
    \\lstinputlisting[language=ebilines, style=boxed]{../testfiles/aa-ab-ba.ali}";

pub const EBI_LANGUAGE_OF_ALIGNMENTS: EbiFileHandler = EbiFileHandler {
    name: "language of alignments",
    article: "a",
    file_extension: "ali",
    format_specification: &FORMAT_SPECIFICATION,
    validator: ebi_input::validate::<LanguageOfAlignments>,
    trait_importers: &[],
    object_importers: &[EbiObjectImporter::LanguageOfAlignments(
        LanguageOfAlignments::import_as_object,
    )],
    object_exporters: &[
        EbiObjectExporter::LanguageOfAlignments(LanguageOfAlignments::export_from_object),
        EbiObjectExporter::StochasticLanguageOfAlignments(LanguageOfAlignments::export_from_object),
    ],
    java_object_handlers: &[],
};

#[derive(ActivityKey, Clone)]
pub struct LanguageOfAlignments {
    pub(crate) activity_key: ActivityKey,
    pub(crate) alignments: Vec<Vec<Move>>,
}

impl LanguageOfAlignments {
    pub fn new(activity_key: ActivityKey) -> Self {
        Self {
            activity_key: activity_key,
            alignments: vec![],
        }
    }

    pub fn push(&mut self, alignment: Vec<Move>) {
        self.alignments.push(alignment);
    }

    pub fn append(&mut self, alignments: &mut Vec<Vec<Move>>) {
        self.alignments.append(alignments);
    }

    pub fn get(&self, index: usize) -> Option<&Vec<Move>> {
        self.alignments.get(index)
    }

    pub fn get_activity_key(&self) -> &ActivityKey {
        &self.activity_key
    }

    pub fn get_activity_key_mut(&mut self) -> &mut ActivityKey {
        &mut self.activity_key
    }

    pub fn sort(&mut self) {
        self.alignments.sort();
    }
}

impl TranslateActivityKey for LanguageOfAlignments {
    fn translate_using_activity_key(&mut self, to_activity_key: &mut ActivityKey) {
        let translator = ActivityKeyTranslator::new(&self.activity_key, to_activity_key);
        self.alignments.iter_mut().for_each(|alignment| {
            alignment.iter_mut().for_each(|activity| match activity {
                Move::SynchronousMove(activity, _)
                | Move::LogMove(activity)
                | Move::ModelMove(activity, _) => {
                    *activity = translator.translate_activity(&activity)
                }
                _ => {}
            })
        });
        self.activity_key = to_activity_key.clone();
    }
}

impl Exportable for LanguageOfAlignments {
    fn export_from_object(object: EbiOutput, f: &mut dyn std::io::Write) -> Result<()> {
        match object {
            EbiOutput::Object(EbiObject::LanguageOfAlignments(alignments)) => alignments.export(f),
            EbiOutput::Object(EbiObject::StochasticLanguageOfAlignments(sali)) => {
                Into::<Self>::into(sali).export(f)
            }
            _ => Err(anyhow!("Cannot export as language of alignments.")),
        }
    }

    fn export(&self, f: &mut dyn std::io::Write) -> Result<()> {
        Ok(write!(f, "{}", self)?)
    }
}

impl Display for LanguageOfAlignments {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        writeln!(f, "{}", HEADER)?;

        writeln!(f, "# number of alignments\n{}", self.alignments.len())?;

        for (i, moves) in self.alignments.iter().enumerate() {
            writeln!(f, "# alignment {}", i)?;
            writeln!(f, "# number of moves\n{}", moves.len())?;

            for (j, movee) in moves.iter().enumerate() {
                writeln!(f, "# move {}", j)?;

                match movee {
                    Move::LogMove(activity) => {
                        writeln!(f, "log move")?;
                        writeln!(
                            f,
                            "label {}",
                            self.activity_key.get_activity_label(activity)
                        )?;
                    }
                    Move::ModelMove(activity, transition) => {
                        writeln!(f, "model move")?;
                        writeln!(
                            f,
                            "label {}",
                            self.activity_key.get_activity_label(activity)
                        )?;
                        writeln!(f, "{}", transition)?;
                    }
                    Move::SynchronousMove(activity, transition) => {
                        writeln!(f, "synchronous move")?;
                        writeln!(
                            f,
                            "label {}",
                            self.activity_key.get_activity_label(activity)
                        )?;
                        writeln!(f, "{}", transition)?;
                    }
                    Move::SilentMove(transition) => {
                        writeln!(f, "silent move")?;
                        writeln!(f, "{}", transition)?;
                    }
                };
            }
        }

        write!(f, "")
    }
}

impl Importable for LanguageOfAlignments {
    fn import_as_object(reader: &mut dyn std::io::BufRead) -> Result<EbiObject> {
        Ok(EbiObject::LanguageOfAlignments(Self::import(reader)?))
    }

    fn import(reader: &mut dyn std::io::BufRead) -> anyhow::Result<Self>
    where
        Self: Sized,
    {
        let mut lreader = LineReader::new(reader);
        let mut activity_key = ActivityKey::new();

        let head = lreader
            .next_line_string()
            .with_context(|| format!("failed to read header, which should be {}", HEADER))?;
        if head != HEADER {
            return Err(anyhow!(
                "first line should be exactly `{}`, but found `{}`",
                HEADER,
                lreader.get_last_line()
            ));
        }

        let number_of_alignments = lreader
            .next_line_index()
            .context("failed to read number of alignments")?;

        let mut alignments = Vec::new();
        for alignment_number in 0..number_of_alignments {
            let mut moves = vec![];

            let number_of_moves = lreader.next_line_index().with_context(|| {
                format!(
                    "failed to read number of moves in alignemnt {}",
                    alignment_number
                )
            })?;

            for move_number in 0..number_of_moves {
                //read type of move
                let move_type_line = lreader.next_line_string().with_context(|| {
                    format!(
                        "failed to read type of move {} of alignment {}",
                        move_number, alignment_number
                    )
                })?;
                if move_type_line.trim_start().starts_with("log move") {
                    //read a log move
                    let label_line = lreader.next_line_string().with_context(|| {
                        format!(
                            "failed to read label of log move {} of alignment {}",
                            move_number, alignment_number
                        )
                    })?;
                    if label_line.trim_start().starts_with("label ") {
                        let label = label_line[6..].to_string();
                        let activity = activity_key.process_activity(&label);
                        moves.push(Move::LogMove(activity));
                    } else {
                        return Err(anyhow!("Line must have a label"));
                    }
                } else if move_type_line.trim_start().starts_with("model move") {
                    //read the label
                    let label_line = lreader.next_line_string().with_context(|| {
                        format!(
                            "failed to read label of model move {} of alignment {}",
                            move_number, alignment_number
                        )
                    })?;
                    let activity = if label_line.trim_start().starts_with("label ") {
                        let label = label_line.trim_start()[6..].to_string();
                        let activity = activity_key.process_activity(&label);
                        activity
                    } else {
                        return Err(anyhow!("Line must have a label"));
                    };

                    //read the transition
                    let transition = lreader.next_line_index().with_context(|| {
                        format!(
                            "failed to read transition of move {} in alignemnt {}",
                            move_number, alignment_number
                        )
                    })?;

                    moves.push(Move::ModelMove(activity, transition));
                } else if move_type_line.trim_start().starts_with("synchronous move") {
                    //read the label
                    let label_line = lreader.next_line_string().with_context(|| {
                        format!(
                            "failed to read label of synchronous move {} of alignment {}",
                            move_number, alignment_number
                        )
                    })?;
                    if label_line.trim_start().starts_with("label ") {
                        let label = label_line.trim_start()[6..].to_string();
                        let activity = activity_key.process_activity(&label);

                        //read the transition
                        let transition = lreader.next_line_index().with_context(|| {
                            format!(
                                "failed to read transition of move {} in alignemnt {}",
                                move_number, alignment_number
                            )
                        })?;

                        moves.push(Move::SynchronousMove(activity, transition));
                    } else {
                        return Err(anyhow!("Line must have a label"));
                    }
                } else if move_type_line.trim_start().starts_with("silent move") {
                    //read the transition
                    let transition = lreader.next_line_index().with_context(|| {
                        format!(
                            "failed to read transition of move {} in alignemnt {}",
                            move_number, alignment_number
                        )
                    })?;

                    moves.push(Move::SilentMove(transition));
                } else {
                    return Err(anyhow!(
                        "Type of log move {} of alignment {} is not recognised.",
                        move_number,
                        alignment_number
                    ));
                }
            }

            alignments.push(moves);
        }

        Ok(Self {
            activity_key: activity_key,
            alignments: alignments,
        })
    }
}

impl FromStr for LanguageOfAlignments {
    type Err = Error;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let mut reader = std::io::Cursor::new(s);
        Self::import(&mut reader)
    }
}

impl Infoable for LanguageOfAlignments {
    fn info(&self, f: &mut impl std::io::Write) -> Result<()> {
        writeln!(f, "Number of alignments\t\t{}", self.alignments.len())?;
        Ok(write!(f, "")?)
    }
}

#[derive(Debug, PartialEq, Eq, Ord, PartialOrd, Clone)]
pub enum Move {
    LogMove(Activity),
    ModelMove(Activity, TransitionIndex),
    SynchronousMove(Activity, TransitionIndex),
    SilentMove(TransitionIndex),
}

impl Move {
    pub fn get_transition(&self) -> Option<TransitionIndex> {
        match self {
            Move::LogMove(_) => None,
            Move::ModelMove(_, transition)
            | Move::SilentMove(transition)
            | Move::SynchronousMove(_, transition) => Some(*transition),
        }
    }
}
