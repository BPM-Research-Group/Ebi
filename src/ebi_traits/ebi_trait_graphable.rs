use anyhow::{Result, anyhow};
use std::io::BufRead;

use layout::{adt::dag::NodeHandle, core::{base::Orientation, color::Color, geometry::Point, style::StyleAttr}, std_shapes::{render::get_shape_size, shapes::{Arrow, Element}}, topo::layout::VisualGraph};

use crate::ebi_framework::{ebi_input::EbiInput, ebi_object::EbiTraitObject, ebi_trait::FromEbiTraitObject, importable::Importable};

pub trait EbiTraitGraphable {
    fn to_dot(&self) -> Result<VisualGraph>;
}

impl dyn EbiTraitGraphable {

    pub fn create_place(graph: &mut VisualGraph, label: &str) -> NodeHandle {
        let shape = layout::std_shapes::shapes::ShapeKind::Circle(label.to_string());
        let look = StyleAttr::simple();
        let orientation = Orientation::LeftToRight;
        let size = get_shape_size(orientation, &shape, look.font_size, true);
        let node = Element::create(shape, look, orientation, size);
        return graph.add_node(node);
    }

    pub fn create_transition(graph: &mut VisualGraph, label: &str, xlabel: &str) -> NodeHandle {
        let shape = layout::std_shapes::shapes::ShapeKind::Box(label.to_string() + "\n" + xlabel);
        let look = StyleAttr::simple();
        let orientation = Orientation::LeftToRight;
        let size = get_shape_size(orientation, &shape, look.font_size, false);
        let node = Element::create(shape, look, orientation, size);
        return graph.add_node(node);
    }

    pub fn create_silent_transition(graph: &mut VisualGraph, xlabel: &str) -> NodeHandle {
        let shape = layout::std_shapes::shapes::ShapeKind::Box(xlabel.to_string());
        let mut look = StyleAttr::simple();
        look.fill_color = Some(Color::fast("grey"));
        let orientation = Orientation::LeftToRight;
        let size = Point::new(20., 30.);
        let node = Element::create(shape, look, orientation, size);
        return graph.add_node(node);
    }

    pub fn create_gateway(graph: &mut VisualGraph, label: &str) -> NodeHandle {
        let shape = layout::std_shapes::shapes::ShapeKind::Box(label.to_string());
        let mut look = StyleAttr::simple();
        look.fill_color = Some(Color::new(0xa1cff3));
        let orientation = Orientation::LeftToRight;
        let size = Point::new(20., 20.);
        let node = Element::create(shape, look, orientation, size);
        return graph.add_node(node);
    }

    pub fn create_dot(graph: &mut VisualGraph) -> NodeHandle {
        let shape = layout::std_shapes::shapes::ShapeKind::Circle("".to_string());
        let look = StyleAttr::simple();
        let orientation = Orientation::LeftToRight;
        let size = Point::new(5., 5.);
        let node = Element::create(shape, look, orientation, size);
        return graph.add_node(node);
    }

    pub fn create_edge(graph: &mut VisualGraph, from: &NodeHandle, to: &NodeHandle, label: &str) {
        let arrow = Arrow::simple(label);
        return graph.add_edge(arrow, *from, *to);
    }
}

impl FromEbiTraitObject for dyn EbiTraitGraphable {
    fn from_trait_object(object: EbiInput) -> Result<Box<Self>> {
        match object {
            EbiInput::Trait(EbiTraitObject::Graphable(e), _) => Ok(e),
            _ => Err(anyhow!("cannot read {} {} as a finite stochastic language", object.get_type().get_article(), object.get_type()))
        }
    }
}

/**
 * Convenience function to import any object as a Graphable trait object.
 */
pub fn import<X: 'static + Importable + EbiTraitGraphable> (reader: &mut dyn BufRead) -> Result<Box<dyn EbiTraitGraphable>> {
    match X::import(reader) {
        Ok(x) => Ok(Box::new(x)),
        Err(x) => Err(x),
    }
}

#[cfg(test)]
pub mod tests {

    use crate::ebi_framework::{ebi_input::EbiInput, ebi_object::EbiTraitObject};

    #[test]
    fn all_graphable() {
        for (input, _, _) in crate::tests::get_all_test_files() {
            if let EbiInput::Trait(object, _) = input {
                if let EbiTraitObject::Graphable(object) = object {
                    assert!(object.to_dot().is_ok());
                }
            }
        }
    }
}