use crate::{
    ebi_framework::{
        ebi_input::EbiInput, ebi_trait::FromEbiTraitObject, ebi_trait_object::EbiTraitObject,
    },
    ebi_traits::{EbiTraitFiniteLanguage, EbiTraitFiniteStochasticLanguage},
    techniques::align_process_tree::ProcessTreeAligner,
};
use ebi_derive::EbiInputEnum;
use ebi_objects::{
    anyhow::{anyhow, Context, Result},
    ebi_objects::language_of_alignments::Move,
    Activity, EbiObject, LanguageOfAlignments, ProcessTree, StochasticLanguageOfAlignments,
};

pub trait Align {
    fn align_language(
        &mut self,
        log: Box<dyn EbiTraitFiniteLanguage>,
    ) -> Result<LanguageOfAlignments>;

    fn align_stochastic_language(
        &mut self,
        log: Box<dyn EbiTraitFiniteStochasticLanguage>,
    ) -> Result<StochasticLanguageOfAlignments>;

    /**
     * Please note to ensure the trace and the semantics use the same ActivityKey, or they have been translated
     */
    fn align_trace(&self, trace: &[Activity]) -> Result<(Vec<Move>, usize)>;
}

#[derive(EbiInputEnum)]
pub enum AlignmentAlgorithm {
    Auto, //attempt tree, if not applicable apply astar
    Astar,
    Tree,
}

impl AlignmentAlgorithm {
    /**
     * Execute the alignment algorithm. Input must be either a process tree or a semantics trait.
     * Still, algorithms may be more strict.
     */
    pub fn execute(
        &self,
        log: Box<dyn EbiTraitFiniteStochasticLanguage>,
        model: EbiInput,
    ) -> Result<StochasticLanguageOfAlignments> {
        match self {
            AlignmentAlgorithm::Auto => {
                // Attempt to create a ProcessTreeAligner. If that succeeds, we can align using it.
                if let EbiInput::Object(EbiObject::ProcessTree(tree), _) = &model
                    && let Ok(mut tree_aligner) = ProcessTreeAligner::new(tree)
                {
                    log::info!("Align a process tree without duplicated activity labels");
                    return tree_aligner.align_stochastic_language(log);
                }

                AlignmentAlgorithm::Astar.execute(log, model)
            }
            AlignmentAlgorithm::Astar => match model {
                EbiInput::Trait(EbiTraitObject::Semantics(mut sem), _) => {
                    sem.align_stochastic_language(log)
                }
                EbiInput::Object(EbiObject::ProcessTree(mut tree), _) => {
                    tree.align_stochastic_language(log)
                }
                _ => Err(anyhow!("Input type not supported.")),
            },
            AlignmentAlgorithm::Tree => {
                let tree = model.to_type::<ProcessTree>().with_context(|| {
                    anyhow!("The tree alignment algorithm only accepts process trees.")
                })?;
                let mut tree_aligner = ProcessTreeAligner::new(&tree).with_context(|| anyhow!("The tree alignment algorithm can only be applied to trees without duplicated activities. Try the `auto` algorithm."))?;
                tree_aligner.align_stochastic_language(log)
            }
        }
    }
}
