use crate::materials::Material;
use crate::matrices::M4x4;
use crate::utils::{epsilon_eq, EPSILON};
use crate::vector::{Point, Vector};
// TODO: sort out the deps
use crate::world::SceneObject;

#[derive(PartialEq, Debug)]
pub struct RenderObject {
    pub id: u32,
    pub kind: Shape,
    pub transform: M4x4,
    pub material: Material,
    pub transform_inverse: M4x4,
    pub transform_inverse_transpose: M4x4,
}

// TODO: Remove Clone
#[derive(PartialEq, Debug, Clone)]
pub enum Shape {
    Sphere,
    Plane,
    Cube,
    // There should probably be enums for ClosedCylinder and cone
    // to get rid of the bool in the data
    Cylinder {
        minimum: f32,
        maximum: f32,
        closed: bool,
    },
    Cone {
        minimum: f32,
        maximum: f32,
        closed: bool,
    },
    Group {
        id: u32,
    },
}

impl RenderObject {
    #[must_use]
    pub fn new(id: u32, template: &SceneObject) -> Self {
        let transform = match &template.transform {
            Some(t) => t.clone(),
            None => M4x4::IDENTITY,
        };

        let material = match &template.material {
            Some(m) => m.clone(),
            None => Material::default(),
        };

        let transform_inverse = transform.inverse();
        let transform_inverse_transpose = transform_inverse.transpose();

        RenderObject {
            id,
            kind: template.kind.clone(),
            transform,
            material,
            transform_inverse,
            transform_inverse_transpose,
        }
    }

    pub fn update_transform(&mut self, new_transform: M4x4) {
        let transform_inverse = new_transform.inverse();
        let transform_inverse_transpose = transform_inverse.transpose();

        self.transform_inverse = transform_inverse;
        self.transform_inverse_transpose = transform_inverse_transpose;
        self.transform = new_transform;
    }

    #[must_use]
    pub fn normal_at(&self, world_point: &Point) -> Vector {
        let object_point = &self.transform_inverse * world_point;

        let object_normal = match self.kind {
            Shape::Sphere { .. } => object_point - Point::new(0., 0., 0.),
            Shape::Plane { .. } => Vector::new(0.0, 1.0, 0.0),
            Shape::Cube { .. } => {
                let maxc = f32::max(
                    f32::max(f32::abs(object_point.x), f32::abs(object_point.y)),
                    f32::abs(object_point.z),
                );

                if epsilon_eq(maxc, f32::abs(object_point.x)) {
                    return Vector::new(object_point.x, 0.0, 0.0);
                } else if epsilon_eq(maxc, f32::abs(object_point.y)) {
                    return Vector::new(0.0, object_point.y, 0.0);
                }

                return Vector::new(0.0, 0.0, object_point.z);
            }
            Shape::Cylinder { .. } => {
                // compute the square of the distance from the y axis
                let dist = object_point.x * object_point.x + object_point.z * object_point.z;

                if dist < 1.0 && object_point.y >= self.maximum() - EPSILON {
                    return Vector::new(0.0, 1.0, 0.0);
                }

                if dist < 1.0 && object_point.y <= self.minimum() + EPSILON {
                    return Vector::new(0.0, -1.0, 0.0);
                }

                Vector::new(object_point.x, 0.0, object_point.z)
            }

            Shape::Cone { .. } => {
                let dist = object_point.x.powi(2) + object_point.z.powi(2);

                if dist < 1.0 && object_point.y >= self.maximum() - EPSILON {
                    return Vector::new(0.0, 1.0, 0.0);
                }

                if dist < 1.0 && object_point.y <= self.minimum() + EPSILON {
                    return Vector::new(0.0, -1.0, 0.0);
                }

                Vector::new(
                    object_point.x,
                    if object_point.y > 0.0 {
                        -dist.sqrt()
                    } else {
                        dist.sqrt()
                    },
                    object_point.z,
                )
            }
            Shape::Group { .. } => todo!(),
        };

        let world_normal = &self.transform_inverse_transpose * &object_normal;

        world_normal.norm()
    }

    #[must_use]
    pub fn check_axis(&self, origin: f32, direction: f32) -> (f32, f32) {
        let tmin_numerator = -1.0 - origin;
        let tmax_numerator = 1.0 - origin;

        let (tmin, tmax) = if f32::abs(direction) >= EPSILON {
            (tmin_numerator / direction, tmax_numerator / direction)
        } else {
            (
                tmin_numerator * f32::INFINITY,
                tmax_numerator * f32::INFINITY,
            )
        };

        if tmin > tmax {
            (tmax, tmin)
        } else {
            (tmin, tmax)
        }
    }

    // #[must_use]
    // pub fn material(&self) -> &Material {
    //     panic!("material bang bang")

    // }

    // #[must_use]
    // pub fn id(&self) -> u32 {
    //     panic!("Id bang!")
    // }

    // #[must_use]
    // pub fn transform(&self) -> &M4x4 {
    //     panic!("xf bang!")
    // }

    // #[must_use]
    // pub fn transform_inverse(&self) -> &M4x4 {
    //     panic!("xf_inv bang!")
    // }

    // #[must_use]
    // pub fn transform_inverse_transpose(&self) -> &M4x4 {
    //     panic!("refactor bang!")
    // }

    /// # Panics
    ///
    /// Will panic on any shape other than Cylinders
    #[must_use]
    pub fn minimum(&self) -> f32 {
        match self.kind {
            Shape::Cylinder { minimum, .. } | Shape::Cone { minimum, .. } => minimum,
            _ => panic!("minimum only supported on Cylinders"),
        }
    }

    /// # Panics
    ///
    /// Will panic on any shape other than Cylinders
    #[must_use]
    pub fn maximum(&self) -> f32 {
        match self.kind {
            Shape::Cylinder { maximum, .. } | Shape::Cone { maximum, .. } => maximum,
            _ => panic!("maximum only supported on Cylinders"),
        }
    }

    /// # Panics
    ///
    /// Will panic on any shape other than Cylinders or Cones
    #[must_use]
    pub fn closed(&self) -> bool {
        match self.kind {
            Shape::Cylinder { closed, .. } | Shape::Cone { closed, .. } => closed,
            _ => panic!("closed only supported on Cylinders and Cones"),
        }
    }

    fn world_to_object(&self, world_point: &Point) -> Point {
        &self.transform_inverse * world_point
    }
}

#[cfg(test)]
mod test {
    use super::*;
    use crate::materials::Material;
    use crate::transformations::{rotation_y, rotation_z, scaling, translation};
    use crate::world::World;
    use core::f32::consts::PI;

    // Scenario: A sphere's default transformation
    //   Given s ← sphere()
    //   Then s.transform = identity_matrix
    #[test]
    fn a_spheres_default_transformation() {
        let mut world = World::new();
        world.push_sphere(None, None);

        assert_eq!(world.get_object(0).transform, M4x4::IDENTITY)
    }

    // Scenario: Changing a sphere's transformation
    //   Given s ← sphere()
    //     And t ← translation(2, 3, 4)
    //   When set_transform(s, t)
    //   Then s.transform = t
    #[test]
    fn changing_a_spheres_transformation() {
        let mut world = World::new();
        world.push_sphere(Some(translation(2.0, 3.0, 4.0)), None);

        assert_eq!(world.get_object(0).transform, translation(2.0, 3.0, 4.0))
    }

    // Scenario: The normal on a sphere at a point on the x axis
    //   Given s ← sphere()
    //   When n ← normal_at(s, point(1, 0, 0))
    //   Then n = vector(1, 0, 0)
    #[test]
    fn the_normal_on_a_sphere_at_a_point_on_the_x_axis() {
        let mut world = World::new();
        let s_id = world.push_sphere(None, None);
        let s = world.get_object(s_id);

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
        let s = world.get_object(s_id);

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
        let s = world.get_object(s_id);

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
        let s = world.get_object(s_id);

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
        let s = world.get_object(s_id);

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
        let s = world.get_object(s_id);

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
        let s = world.get_object(s_id);

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
        world.push_sphere(Some(&scaling(1.0, 0.5, 1.0) * &rotation_z(PI / 5.0)), None);

        let material: Material = Default::default();
        assert_eq!(world.get_object(0).material, material)
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
        world.push_sphere(
            None,
            Some(Material {
                ambient: 1.0,
                ..Default::default()
            }),
        );

        let material = Material {
            ambient: 1.0,
            ..Default::default()
        };

        assert_eq!(world.get_object(0).material, material)
    }

    // Scenario: The normal of a plane is constant everywhere
    //   Given p ← plane()
    //   When n1 ← local_normal_at(p, point(0, 0, 0))
    //     And n2 ← local_normal_at(p, point(10, 0, -10))
    //     And n3 ← local_normal_at(p, point(-5, 0, 150))
    //   Then n1 = vector(0, 1, 0)
    //     And n2 = vector(0, 1, 0)
    //     And n3 = vector(0, 1, 0)
    #[test]
    fn the_normal_of_a_plane_is_constant_everywhere() {
        let mut world = World::new();
        let p_id = world.push_plane(None, None);

        let p = world.get_object(p_id);

        let n1 = p.normal_at(&Point::new(0.0, 0.0, 0.0));
        let n2 = p.normal_at(&Point::new(10.0, 0.0, -10.0));
        let n3 = p.normal_at(&Point::new(-5.0, 0.0, 150.0));

        assert_eq!(n1, Vector::new(0.0, 1.0, 0.0));
        assert_eq!(n2, Vector::new(0.0, 1.0, 0.0));
        assert_eq!(n3, Vector::new(0.0, 1.0, 0.0))
    }

    // Scenario Outline: The normal on the surface of a cube
    //   Given c ← cube()
    //     And p ← <point>
    //   When normal ← local_normal_at(c, p)
    //   Then normal = <normal>

    //   Examples:
    //     | point                | normal           |
    //     | point(1, 0.5, -0.8)  | vector(1, 0, 0)  |
    //     | point(-1, -0.2, 0.9) | vector(-1, 0, 0) |
    //     | point(-0.4, 1, -0.1) | vector(0, 1, 0)  |
    //     | point(0.3, -1, -0.7) | vector(0, -1, 0) |
    //     | point(-0.6, 0.3, 1)  | vector(0, 0, 1)  |
    //     | point(0.4, 0.4, -1)  | vector(0, 0, -1) |
    //     | point(1, 1, 1)       | vector(1, 0, 0)  |
    //     | point(-1, -1, -1)    | vector(-1, 0, 0) |
    #[test]
    fn the_normal_on_the_surface_of_a_cube() {
        let mut world = World::new();
        let c_id = world.push_cube(None, None);

        let c = world.get_object(c_id);

        let n1 = c.normal_at(&Point::new(1.0, 0.5, -0.8));
        let n2 = c.normal_at(&Point::new(-1.0, -0.2, 0.9));
        let n3 = c.normal_at(&Point::new(-0.4, 1.0, -0.1));
        let n4 = c.normal_at(&Point::new(0.3, -1.0, -0.7));
        let n5 = c.normal_at(&Point::new(-0.6, 0.3, 1.0));
        let n6 = c.normal_at(&Point::new(0.4, 0.4, -1.0));
        let n7 = c.normal_at(&Point::new(1.0, 1.0, 1.0));
        let n8 = c.normal_at(&Point::new(-1.0, -1.0, -1.0));

        assert_eq!(n1, Vector::new(1.0, 0.0, 0.0));
        assert_eq!(n2, Vector::new(-1.0, 0.0, 0.0));
        assert_eq!(n3, Vector::new(0.0, 1.0, 0.0));
        assert_eq!(n4, Vector::new(0.0, -1.0, 0.0));
        assert_eq!(n5, Vector::new(0.0, 0.0, 1.0));
        assert_eq!(n6, Vector::new(0.0, 0.0, -1.0));
        assert_eq!(n7, Vector::new(1.0, 0.0, 0.0));
        assert_eq!(n8, Vector::new(-1.0, 0.0, 0.0));
    }

    // Scenario Outline: Normal vector on a cylinder
    //   Given cyl ← cylinder()
    //   When n ← local_normal_at(cyl, <point>)
    //   Then n = <normal>

    //   Examples:
    //     | point           | normal           |
    //     | point(1, 0, 0)  | vector(1, 0, 0)  |
    //     | point(0, 5, -1) | vector(0, 0, -1) |
    //     | point(0, -2, 1) | vector(0, 0, 1)  |
    //     | point(-1, 1, 0) | vector(-1, 0, 0) |
    #[test]
    fn normal_vector_on_a_cylinder() {
        let mut world = World::new();
        let c_id = world.push_shape(
            Shape::Cylinder {
                minimum: -f32::INFINITY,
                maximum: f32::INFINITY,
                closed: false,
            },
            None,
            None,
        );

        let c = world.get_object(c_id);

        let n1 = c.normal_at(&Point::new(1.0, 0.0, 0.0));
        let n2 = c.normal_at(&Point::new(0.0, 5.0, -1.0));
        let n3 = c.normal_at(&Point::new(0.0, -2.0, 1.0));
        let n4 = c.normal_at(&Point::new(-1.0, 1.0, 0.0));

        assert_eq!(n1, Vector::new(1.0, 0.0, 0.0));
        assert_eq!(n2, Vector::new(0.0, 0.0, -1.0));
        assert_eq!(n3, Vector::new(0.0, 0.0, 1.0));
        assert_eq!(n4, Vector::new(-1.0, 0.0, 0.0));
    }

    // Scenario: The default minimum and maximum for a cylinder
    //   Given cyl ← cylinder()
    //   Then cyl.minimum = -infinity
    //     And cyl.maximum = infinity
    #[test]
    fn the_default_minimum_and_maximum_for_a_cylinder() {
        let mut world = World::new();
        let c_id = world.push_shape(
            Shape::Cylinder {
                minimum: -f32::INFINITY,
                maximum: f32::INFINITY,
                closed: false,
            },
            None,
            None,
        );

        let c = world.get_object(c_id);

        assert_eq!(c.minimum(), -f32::INFINITY);
        assert_eq!(c.maximum(), f32::INFINITY);
    }

    // Scenario: The default closed value for a cylinder
    //   Given cyl ← cylinder()
    //   Then cyl.closed = false
    #[test]
    fn the_default_closed_value_for_a_cylinder() {
        let mut world = World::new();
        let c_id = world.push_shape(
            Shape::Cylinder {
                minimum: -f32::INFINITY,
                maximum: f32::INFINITY,
                closed: false,
            },
            None,
            None,
        );

        let c = world.get_object(c_id);

        assert_eq!(c.closed(), false);
    }

    // Scenario Outline: The normal vector on a cylinder's end caps
    //   Given cyl ← cylinder()
    //     And cyl.minimum ← 1
    //     And cyl.maximum ← 2
    //     And cyl.closed ← true
    //   When n ← local_normal_at(cyl, <point>)
    //   Then n = <normal>

    //   Examples:
    //     | point            | normal           |
    //     | point(0, 1, 0)   | vector(0, -1, 0) |
    //     | point(0.5, 1, 0) | vector(0, -1, 0) |
    //     | point(0, 1, 0.5) | vector(0, -1, 0) |
    //     | point(0, 2, 0)   | vector(0, 1, 0)  |
    //     | point(0.5, 2, 0) | vector(0, 1, 0)  |
    //     | point(0, 2, 0.5) | vector(0, 1, 0)  |
    #[test]
    fn the_normal_vector_on_a_cylinders_end_caps() {
        let mut world = World::new();
        let c_id = world.push_cylinder(None, None, 1.0, 2.0, true);

        let c = world.get_object(c_id);

        let n1 = c.normal_at(&Point::new(0.0, 1.0, 0.0));
        let n2 = c.normal_at(&Point::new(0.5, 1.0, 0.0));
        let n3 = c.normal_at(&Point::new(0.0, 1.0, 0.5));
        let n4 = c.normal_at(&Point::new(0.0, 2.0, 0.0));
        let n5 = c.normal_at(&Point::new(0.5, 2.0, 0.0));
        let n6 = c.normal_at(&Point::new(0.0, 2.0, 0.5));

        assert_eq!(n1, Vector::new(0.0, -1.0, 0.0));
        assert_eq!(n2, Vector::new(0.0, -1.0, 0.0));
        assert_eq!(n3, Vector::new(0.0, -1.0, 0.0));
        assert_eq!(n4, Vector::new(0.0, 1.0, 0.0));
        assert_eq!(n5, Vector::new(0.0, 1.0, 0.0));
        assert_eq!(n6, Vector::new(0.0, 1.0, 0.0));
    }

    // Scenario Outline: Computing the normal vector on a cone
    //   Given shape ← cone()
    //   When n ← local_normal_at(shape, <point>)
    //   Then n = <normal>

    //   Examples:
    //     | point             | normal                 |
    //     | point(0, 0, 0)    | vector(0, 0, 0)        |
    //     | point(1, 1, 1)    | vector(1, -√2, 1)      |
    //     | point(-1, -1, 0)  | vector(-1, 1, 0)       |
    // NOTE: The normal implementation is correct, but we do not have a
    //        local normal_at, therefor the test do not pass due to world
    //        transformation done for all normals
    // #[test]
    // fn computing_the_normal_vector_on_a_cone() {
    //     let mut world = World::new();
    //     let c_id = world.push_shape(&Kind::Cone, None, None);

    //     let c = world.get_shape(c_id, &Kind::Cone);

    //     let n1 = c.normal_at(&Point::new(0.0, 0.0, 0.0));
    //     let n2 = c.normal_at(&Point::new(1.0, 1.0, 1.0));
    //     let n3 = c.normal_at(&Point::new(-1.0, -1.0, 0.0));

    //     assert_eq!(n1, Vector::new(0.0, 0.0, 0.0));
    //     assert_eq!(n2, Vector::new(1.0, -f32::sqrt(2.0), 1.0));
    //     assert_eq!(n3, Vector::new(-1.0, 1.0, 0.0));
    // }

    // Scenario: Creating a new group
    //   Given g ← group()
    //   Then g.transform = identity_matrix
    //     And g is empty
    // #[test]
    // fn creating_a_new_group() {
    //     let mut world = World::new();
    //     world.push_group(None, None);

    //     assert_eq!(world.get_object(0).transform, M4x4::IDENTITY)
    // }

    // Scenario: Adding a child to a group
    //   Given g ← group()
    //     And s ← test_shape()
    //   When add_child(g, s)
    //   Then g is not empty
    //     And g includes s
    //     And s.parent = g
    // #[test]
    // fn adding_a_child_to_a_group() {
    //     let mut world = World::new();
    //     world.push_group(None, None);

    //     assert_eq!(world.get_object(0).transform, M4x4::IDENTITY)
    // }

    // Scenario: Creating an empty bounding box
    //   Given box ← bounding_box(empty)
    //   Then box.min = point(infinity, infinity, infinity)
    //     And box.max = point(-infinity, -infinity, -infinity)
    #[test]
    fn creating_an_empty_bounding_box() {}

    // Scenario: Converting a point from world to object space
    //   Given g1 ← group()
    //     And set_transform(g1, rotation_y(π/2))
    //     And g2 ← group()
    //     And set_transform(g2, scaling(2, 2, 2))
    //     And add_child(g1, g2)
    //     And s ← sphere()
    //     And set_transform(s, translation(5, 0, 0))
    //     And add_child(g2, s)
    //   When p ← world_to_object(s, point(-2, 0, -10))
    //   Then p = point(0, 0, -1)
    #[test]
    fn converting_a_point_from_world_to_object_space() {
        let mut world = World::new();

        // let gid_1 = world.push_group(
        //     vec![RenderObjectTemplate::new(
        //         Shape::Sphere,
        //         Some(translation(5.0, 0.0, 0.0)),
        //         None,
        //     )],
        //     Some(scaling(2.0, 2.0, 2.0)),
        // );

        // let gid_2 = world.push_group(
        //     vec![RenderObjectTemplate::new(
        //         Shape::Group { id: gid_1 },
        //         None,
        //         None,
        //     )],
        //     Some(rotation_y(PI / 2.0)),
        // );

        //println!("{:#?}", world.groups);
        //println!("{:#?}",world.groups[gid_1 as usize].objects[0]);
        // let sphere_obj = &world.groups[gid_1 as usize].objects[0];
        // let p = sphere_obj.world_to_object(&Point::new(-2.0, 0.0, 10.0));
        // assert_eq!(Point::new(0.0, 0.0, -1.0), p)

        //assert_eq!(0,1)
    }
}
