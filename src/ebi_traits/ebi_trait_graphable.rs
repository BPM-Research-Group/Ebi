use anyhow::{Result, anyhow};
use ebi_objects::{
    Graphable, Importable, ScalableVectorGraphics, ebi_objects::scalable_vector_graphics::ToSVG,
};
use layout::backends::svg::SVGWriter;
use std::io::BufRead;

use crate::ebi_framework::{
    ebi_input::EbiInput, ebi_trait::FromEbiTraitObject, ebi_trait_object::EbiTraitObject,
};

pub trait EbiTraitGraphable: Graphable {}

impl<X> EbiTraitGraphable for X where X: Graphable {} //blanket implementation

impl FromEbiTraitObject for dyn EbiTraitGraphable {
    fn from_trait_object(object: EbiInput) -> Result<Box<Self>> {
        match object {
            EbiInput::Trait(EbiTraitObject::Graphable(e), _) => Ok(e),
            _ => Err(anyhow!(
                "cannot read {} {} as a finite stochastic language",
                object.get_type().get_article(),
                object.get_type()
            )),
        }
    }
}

impl ToSVG for dyn EbiTraitGraphable {
    fn to_svg(&self) -> Result<ScalableVectorGraphics> {
        let mut svg = SVGWriter::new();
        let mut graph = self.to_dot()?;
        Ok(if graph.num_nodes() == 0 {
            ScalableVectorGraphics::empty()
        } else {
            graph.do_it(false, false, false, &mut svg);
            ScalableVectorGraphics(svg.finalize())
        })
    }
}

pub trait ToGraphable: Importable {
    fn to_graphable(self) -> Box<dyn EbiTraitGraphable>;

    fn import_as_graphable(reader: &mut dyn BufRead) -> Result<Box<dyn EbiTraitGraphable>>
    where
        Self: Sized,
    {
        Ok(Self::import(reader)?.to_graphable())
    }
}

impl<T> ToGraphable for T
where
    T: EbiTraitGraphable + Importable + 'static,
{
    fn to_graphable(self) -> Box<dyn EbiTraitGraphable> {
        Box::new(self)
    }
}

#[cfg(test)]
pub mod tests {

    use crate::ebi_framework::{ebi_input::EbiInput, ebi_trait_object::EbiTraitObject};

    #[test]
    fn all_graphable() {
        for (input, _, _, _) in crate::tests::get_all_test_files() {
            if let EbiInput::Trait(object, _) = input {
                if let EbiTraitObject::Graphable(object) = object {
                    assert!(object.to_dot().is_ok());
                }
            }
        }
    }
}
