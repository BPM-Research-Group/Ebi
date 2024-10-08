
use layout::{adt::dag::NodeHandle, core::{base::Orientation, color::Color, geometry::Point, style::StyleAttr}, std_shapes::{render::get_shape_size, shapes::{Arrow, Element}}, topo::layout::VisualGraph};

pub trait Dottable {
    fn to_dot(&self) -> VisualGraph;
}

impl dyn Dottable {

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

    pub fn create_edge(graph: &mut VisualGraph, from: &NodeHandle, to: &NodeHandle, label: &str) {
        let arrow = Arrow::simple(label);
        return graph.add_edge(arrow, *from, *to);
    }
}