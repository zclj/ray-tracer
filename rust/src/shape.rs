use crate::matrices::M4x4;

pub enum Shape {
    Sphere { id: u32, transform: M4x4 },
}

// impl Shape {
//     fn new(&self,
// }

#[cfg(test)]
mod test {
    use super::*;
    use crate::context::Context;
    use crate::transformations::translation;

    // Scenario: A sphere's default transformation
    //   Given s ← sphere()
    //   Then s.transform = identity_matrix
    #[test]
    fn a_spheres_default_transformation() {
        let mut ctx = Context::new();
        let s_id = ctx.push_sphere(None);

        let s_transform = match ctx.get_shape(0) {
            Shape::Sphere { transform, .. } => transform,
            _ => panic!(),
        };

        assert_eq!(*s_transform, M4x4::IDENTITY)
    }

    // Scenario: Changing a sphere's transformation
    //   Given s ← sphere()
    //     And t ← translation(2, 3, 4)
    //   When set_transform(s, t)
    //   Then s.transform = t
    #[test]
    fn changing_a_spheres_transformation() {
        let mut ctx = Context::new();
        let s_id = ctx.push_sphere(Some(translation(2.0, 3.0, 4.0)));

        let s_transform = match ctx.get_shape(0) {
            Shape::Sphere { transform, .. } => transform,
            _ => panic!(),
        };

        assert_eq!(*s_transform, translation(2.0, 3.0, 4.0))
    }
}
