use crate::materials::Material;
use crate::matrices::M4x4;
use crate::vector::{Point, Vector};

#[derive(PartialEq, Debug)]
pub enum Shape {
    Sphere {
        id: u32,
        transform: M4x4,
        material: Material,
    },
}

impl Shape {
    #[must_use]
    pub fn normal_at(&self, world_point: &Point) -> Vector {
        match self {
            Shape::Sphere { transform, .. } => {
                let inversed = transform.inverse();
                let object_point = &inversed * world_point;
                let object_normal = object_point - Point::new(0., 0., 0.);
                let world_normal = &inversed.transpose() * &object_normal;

                world_normal.norm()
            }
        }
    }

    #[must_use]
    pub fn material(&self) -> &Material {
        match self {
            Shape::Sphere { material, .. } => material,
        }
    }
}

#[cfg(test)]
mod test {
    use super::*;
    use crate::materials::Material;
    use crate::transformations::{rotation_z, scaling, translation};
    use crate::world::World;
    use core::f32::consts::PI;

    // Scenario: A sphere's default transformation
    //   Given s ← sphere()
    //   Then s.transform = identity_matrix
    #[test]
    fn a_spheres_default_transformation() {
        let mut world = World::new();
        let s_id = world.push_sphere(None, None);

        let s_transform = match world.get_shape(0) {
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
        let mut world = World::new();
        let s_id = world.push_sphere(Some(translation(2.0, 3.0, 4.0)), None);

        let s_transform = match world.get_shape(0) {
            Shape::Sphere { transform, .. } => transform,
            _ => panic!(),
        };

        assert_eq!(*s_transform, translation(2.0, 3.0, 4.0))
    }

    // Scenario: The normal on a sphere at a point on the x axis
    //   Given s ← sphere()
    //   When n ← normal_at(s, point(1, 0, 0))
    //   Then n = vector(1, 0, 0)
    #[test]
    fn the_normal_on_a_sphere_at_a_point_on_the_x_axis() {
        let mut world = World::new();
        let s_id = world.push_sphere(None, None);
        let s = world.get_shape(s_id);

        let n = s.normal_at(&Point::new(1.0, 0.0, 0.0));

        assert_eq!(n, Vector::new(1.0, 0.0, 0.0))
    }

    // Scenario: The normal on a sphere at a point on the y axis
    //   Given s ← sphere()
    //   When n ← normal_at(s, point(0, 1, 0))
    //   Then n = vector(0, 1, 0)
    #[test]
    fn the_normal_on_a_sphere_at_a_point_on_the_y_axis() {
        let mut world = World::new();
        let s_id = world.push_sphere(None, None);
        let s = world.get_shape(s_id);

        let n = s.normal_at(&Point::new(0.0, 1.0, 0.0));

        assert_eq!(n, Vector::new(0.0, 1.0, 0.0))
    }

    // Scenario: The normal on a sphere at a point on the z axis
    //   Given s ← sphere()
    //   When n ← normal_at(s, point(0, 0, 1))
    //   Then n = vector(0, 0, 1)
    #[test]
    fn the_normal_on_a_sphere_at_a_point_on_the_z_axis() {
        let mut world = World::new();
        let s_id = world.push_sphere(None, None);
        let s = world.get_shape(s_id);

        let n = s.normal_at(&Point::new(0.0, 0.0, 1.0));

        assert_eq!(n, Vector::new(0.0, 0.0, 1.0))
    }

    // Scenario: The normal on a sphere at a nonaxial point
    //   Given s ← sphere()
    //   When n ← normal_at(s, point(√3/3, √3/3, √3/3))
    //   Then n = vector(√3/3, √3/3, √3/3)
    #[test]
    fn the_normal_on_a_sphere_at_a_nonaxial_point() {
        let mut world = World::new();
        let s_id = world.push_sphere(None, None);
        let s = world.get_shape(s_id);

        let n = s.normal_at(&Point::new(
            f32::sqrt(3.0) / 3.0,
            f32::sqrt(3.0) / 3.0,
            f32::sqrt(3.0) / 3.0,
        ));

        assert_eq!(
            n,
            Vector::new(
                f32::sqrt(3.0) / 3.0,
                f32::sqrt(3.0) / 3.0,
                f32::sqrt(3.0) / 3.0
            )
        )
    }

    // Scenario: The normal is a normalized vector
    //   Given s ← sphere()
    //   When n ← normal_at(s, point(√3/3, √3/3, √3/3))
    //   Then n = normalize(n)
    #[test]
    fn the_normal_is_a_normalized_vector() {
        let mut world = World::new();
        let s_id = world.push_sphere(None, None);
        let s = world.get_shape(s_id);

        let n = s.normal_at(&Point::new(
            f32::sqrt(3.0) / 3.0,
            f32::sqrt(3.0) / 3.0,
            f32::sqrt(3.0) / 3.0,
        ));

        assert_eq!(n, n.norm())
    }

    // Scenario: Computing the normal on a translated sphere
    //   Given s ← sphere()
    //     And set_transform(s, translation(0, 1, 0))
    //   When n ← normal_at(s, point(0, 1.70711, -0.70711))
    //   Then n = vector(0, 0.70711, -0.70711)
    #[test]
    fn computing_the_normal_on_a_translated_sphere() {
        let mut world = World::new();
        let s_id = world.push_sphere(Some(translation(0.0, 1.0, 0.0)), None);
        let s = world.get_shape(s_id);

        let n = s.normal_at(&Point::new(0.0, 1.70711, -0.70711));

        assert_eq!(n, Vector::new(0.0, 0.70711, -0.70711))
    }

    // Scenario: Computing the normal on a transformed sphere
    //   Given s ← sphere()
    //     And m ← scaling(1, 0.5, 1) * rotation_z(π/5)
    //     And set_transform(s, m)
    //   When n ← normal_at(s, point(0, √2/2, -√2/2))
    //   Then n = vector(0, 0.97014, -0.24254)
    #[test]
    fn computing_the_normal_on_a_transformed_sphere() {
        let mut world = World::new();
        let s_id = world.push_sphere(Some(&scaling(1.0, 0.5, 1.0) * &rotation_z(PI / 5.0)), None);
        let s = world.get_shape(s_id);

        let n = s.normal_at(&Point::new(
            0.0,
            f32::sqrt(2.0) / 2.0,
            -f32::sqrt(2.0) / 2.0,
        ));

        assert_eq!(n, Vector::new(0.0, 0.97014, -0.24254))
    }

    // Scenario: A sphere has a default material
    //   Given s ← sphere()
    //   When m ← s.material
    //   Then m = material()
    #[test]
    fn a_sphere_has_a_default_material() {
        let mut world = World::new();
        let s_id = world.push_sphere(Some(&scaling(1.0, 0.5, 1.0) * &rotation_z(PI / 5.0)), None);
        let s = world.get_shape(s_id);

        let s_material = match world.get_shape(0) {
            Shape::Sphere { material, .. } => material,
        };

        let material: Material = Default::default();
        assert_eq!(*s_material, material)
    }
    // Scenario: A sphere may be assigned a material
    //   Given s ← sphere()
    //     And m ← material()
    //     And m.ambient ← 1
    //   When s.material ← m
    //   Then s.material = m
    #[test]
    fn a_sphere_may_be_assigned_a_material() {
        let mut world = World::new();
        let s_id = world.push_sphere(
            None,
            Some(Material {
                ambient: 1.0,
                ..Default::default()
            }),
        );
        let s = world.get_shape(s_id);

        let s_material = match world.get_shape(0) {
            Shape::Sphere { material, .. } => material,
        };

        let material = Material {
            ambient: 1.0,
            ..Default::default()
        };

        assert_eq!(*s_material, material)
    }
}
