use crate::ebi_objects::{language_of_alignments::LanguageOfAlignments, stochastic_language_of_alignments::StochasticLanguageOfAlignments};

impl From<StochasticLanguageOfAlignments> for LanguageOfAlignments {
    fn from(value: StochasticLanguageOfAlignments) -> Self {
        log::info!("convert stochastic language of alignments into language of alignments");
        Self {
            activity_key: value.activity_key,
            alignments: value.alignments,
        }
    }
}