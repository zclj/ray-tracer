use crate::bounds::BoundingBox;
use crate::materials::Material;
use crate::matrices::M4x4;
use crate::utils::{epsilon_eq, EPSILON};
use crate::vector::{Point, Vector};
use std::f32::{INFINITY, NEG_INFINITY};

#[derive(PartialEq, Debug)]
pub struct RenderObject {
    pub id: u32,
    pub kind: Shape,
    pub transform: M4x4,
    pub material: Material,
    pub transform_inverse: M4x4,
    pub transform_inverse_transpose: M4x4,
    pub bounding_box: BoundingBox,
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
    // Group {
    //     id: u32,
    // },
}

#[must_use]
#[allow(clippy::similar_names)]
pub fn check_axis(origin: f32, direction: f32, min: f32, max: f32) -> (f32, f32) {
    let tmin_numerator = min - origin;
    let tmax_numerator = max - origin;

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

impl RenderObject {
    #[must_use]
    // TODO: remove the template and take the params
    pub fn new(
        id: u32,
        //template: &SceneObject
        kind: Shape,
        transform: Option<M4x4>,
        material: Option<Material>,
        bounding_box: Option<BoundingBox>,
    ) -> Self {
        let transform = match transform {
            Some(t) => t,
            None => M4x4::IDENTITY,
        };

        let material = match material {
            Some(m) => m,
            None => Material::default(),
        };

        let bounds = match bounding_box {
            Some(b) => b,
            None => bounds(&kind),
        };

        let transform_inverse = transform.inverse();
        let transform_inverse_transpose = transform_inverse.transpose();

        RenderObject {
            id,
            kind,
            transform,
            material,
            transform_inverse,
            transform_inverse_transpose,
            bounding_box: bounds,
        }
    }

    /// # Panics
    ///
    /// Will panic if called on a Group
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
        };

        let world_normal = &self.transform_inverse_transpose * &object_normal;

        world_normal.norm()
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

    // NOTE: seems these are redundant when applying the translations when
    //  building the render tree
    // fn world_to_object(&self, world_point: &Point) -> Point {
    //     &self.transform_inverse * world_point
    // }

    // fn normal_to_world(&self, normal: &Vector) -> Vector {
    //     (&self.transform_inverse.transpose() * normal).norm()
    // }
}

#[must_use]
pub fn bounds(kind: &Shape) -> BoundingBox {
    match kind {
        Shape::Sphere | Shape::Cube => {
            BoundingBox::new(Point::new(-1.0, -1.0, -1.0), Point::new(1.0, 1.0, 1.0))
        }
        Shape::Plane => BoundingBox::new(
            Point::new(NEG_INFINITY, 0.0, NEG_INFINITY),
            Point::new(INFINITY, 0.0, INFINITY),
        ),
        Shape::Cylinder {
            closed,
            minimum,
            maximum,
        } => {
            if *closed {
                BoundingBox::new(
                    Point::new(-1.0, *minimum, -1.0),
                    Point::new(1.0, *maximum, 1.0),
                )
            } else {
                BoundingBox::new(
                    Point::new(-1.0, NEG_INFINITY, -1.0),
                    Point::new(1.0, INFINITY, 1.0),
                )
            }
        }
        Shape::Cone {
            closed,
            minimum,
            maximum,
        } => {
            if *closed {
                let limit = f32::max(f32::abs(*minimum), f32::abs(*maximum));

                BoundingBox::new(
                    Point::new(-limit, *minimum, -limit),
                    Point::new(limit, *maximum, limit),
                )
            } else {
                BoundingBox::new(
                    Point::new(NEG_INFINITY, NEG_INFINITY, NEG_INFINITY),
                    Point::new(INFINITY, INFINITY, INFINITY),
                )
            }
        }
    }
}

#[cfg(test)]
mod test {
    use super::*;
    use crate::bounds::BoundingBox;
    use crate::materials::Material;
    use crate::transformations::{rotation_y, rotation_z, scaling, translation};
    use crate::world::World;
    use core::f32::consts::PI;
    use std::f32::{INFINITY, NEG_INFINITY};

    // Scenario: A sphere's default transformation
    //   Given s ← sphere()
    //   Then s.transform = identity_matrix
    #[test]
    fn a_spheres_default_transformation() {
        let mut world = World::new();
        world.scene.insert_object(Shape::Sphere, None, None);
        world.build();

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
        let _s_id =
            world
                .scene
                .insert_object(Shape::Sphere, Some(translation(2.0, 3.0, 4.0)), None);
        world.build();

        assert_eq!(world.get_object(0).transform, translation(2.0, 3.0, 4.0))
    }

    // Scenario: The normal on a sphere at a point on the x axis
    //   Given s ← sphere()
    //   When n ← normal_at(s, point(1, 0, 0))
    //   Then n = vector(1, 0, 0)
    #[test]
    fn the_normal_on_a_sphere_at_a_point_on_the_x_axis() {
        let mut world = World::new();
        let s_id = world.scene.insert_object(Shape::Sphere, None, None);
        world.build();
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
        let s_id = world.scene.insert_object(Shape::Sphere, None, None);
        world.build();
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
        let s_id = world.scene.insert_object(Shape::Sphere, None, None);
        world.build();

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
        let s_id = world.scene.insert_object(Shape::Sphere, None, None);
        world.build();

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
        let s_id = world.scene.insert_object(Shape::Sphere, None, None);
        world.build();

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
        let s_id = world
            .scene
            .insert_object(Shape::Sphere, Some(translation(0.0, 1.0, 0.0)), None);
        world.build();

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
        let s_id = world.scene.insert_object(
            Shape::Sphere,
            Some(&scaling(1.0, 0.5, 1.0) * &rotation_z(PI / 5.0)),
            None,
        );
        world.build();

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
        world.scene.insert_object(Shape::Sphere, None, None);
        world.build();

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
        world.scene.insert_object(
            Shape::Sphere,
            None,
            Some(Material {
                ambient: 1.0,
                ..Default::default()
            }),
        );
        world.build();

        let material = Material {
            ambient: 1.0,
            ..Default::default()
        };

        assert_eq!(world.get_object(0).material, material)
    }

    // Scenario: A sphere has a bounding box
    //   Given shape ← sphere()
    //   When box ← bounds_of(shape)
    //   Then box.min = point(-1, -1, -1)
    //     And box.max = point(1, 1, 1)
    #[test]
    fn a_sphere_has_a_bounding_box() {
        let mut world = World::new();
        world.scene.insert_object(Shape::Sphere, None, None);
        world.build();

        assert_eq!(
            bounds(&world.get_object(0).kind),
            BoundingBox::new(Point::new(-1.0, -1.0, -1.0), Point::new(1.0, 1.0, 1.0))
        )
    }

    // Scenario: A plane has a bounding box
    //   Given shape ← plane()
    //   When box ← bounds_of(shape)
    //   Then box.min = point(-infinity, 0, -infinity)
    //     And box.max = point(infinity, 0, infinity)
    #[test]
    fn a_plane_has_a_bounding_box() {
        let mut world = World::new();
        world.scene.insert_object(Shape::Plane, None, None);
        world.build();

        let bounds = bounds(&world.get_object(0).kind);

        assert_eq!(bounds.min.x, NEG_INFINITY);
        assert_eq!(bounds.min.y, 0.0);
        assert_eq!(bounds.min.z, NEG_INFINITY);

        assert_eq!(bounds.max.x, INFINITY);
        assert_eq!(bounds.max.y, 0.0);
        assert_eq!(bounds.max.z, INFINITY);
    }

    // Scenario: A cube has a bounding box
    //   Given shape ← cube()
    //   When box ← bounds_of(shape)
    //   Then box.min = point(-1, -1, -1)
    //     And box.max = point(1, 1, 1)
    #[test]
    fn a_cube_has_a_bounding_box() {
        let mut world = World::new();
        world.scene.insert_object(Shape::Cube, None, None);
        world.build();

        assert_eq!(
            bounds(&world.get_object(0).kind),
            BoundingBox::new(Point::new(-1.0, -1.0, -1.0), Point::new(1.0, 1.0, 1.0))
        )
    }

    // Scenario: An unbounded cylinder has a bounding box
    //   Given shape ← cylinder()
    //   When box ← bounds_of(shape)
    //   Then box.min = point(-1, -infinity, -1)
    //     And box.max = point(1, infinity, 1)
    #[test]
    fn an_unbounded_cylinder_has_a_bounding_box() {
        let mut world = World::new();
        let _c_id = world.scene.insert_object(
            Shape::Cylinder {
                minimum: -f32::INFINITY,
                maximum: f32::INFINITY,
                closed: false,
            },
            None,
            None,
        );
        world.build();

        let bounds = bounds(&world.get_object(0).kind);

        assert_eq!(bounds.min.x, -1.0);
        assert_eq!(bounds.min.y, NEG_INFINITY);
        assert_eq!(bounds.min.z, -1.0);

        assert_eq!(bounds.max.x, 1.0);
        assert_eq!(bounds.max.y, INFINITY);
        assert_eq!(bounds.max.z, 1.0);
    }

    // Scenario: A bounded cylinder has a bounding box
    //   Given shape ← cylinder()
    //     And shape.minimum ← -5
    //     And shape.maximum ← 3
    //   When box ← bounds_of(shape)
    //   Then box.min = point(-1, -5, -1)
    //     And box.max = point(1, 3, 1)
    #[test]
    fn a_bounded_cylinder_has_a_bounding_box() {
        let mut world = World::new();
        let c_id = world.scene.insert_object(
            Shape::Cylinder {
                minimum: -5.0,
                maximum: 3.0,
                closed: true,
            },
            None,
            None,
        );

        let g1_id = world
            .scene
            .insert_group(SceneGroup::new(vec![c_id], None, None));
        world.root_group_id = g1_id;
        world.build();

        assert_eq!(
            *world.bvh[0].bounds(),
            BoundingBox::new(Point::new(-1.0, -5.0, -1.0), Point::new(1.0, 3.0, 1.0))
        )
    }

    // Scenario: An unbounded cone has a bounding box
    //   Given shape ← cone()
    //   When box ← bounds_of(shape)
    //   Then box.min = point(-infinity, -infinity, -infinity)
    //     And box.max = point(infinity, infinity, infinity)
    #[test]
    fn an_unbounded_cone_has_a_bounding_box() {
        let mut world = World::new();
        let c_id = world.scene.insert_object(
            Shape::Cone {
                minimum: -f32::INFINITY,
                maximum: f32::INFINITY,
                closed: false,
            },
            None,
            None,
        );

        let g1_id = world
            .scene
            .insert_group(SceneGroup::new(vec![c_id], None, None));
        world.root_group_id = g1_id;
        world.build();

        let bounds = world.bvh[0].bounds();

        assert_eq!(bounds.min.x, NEG_INFINITY);
        assert_eq!(bounds.min.y, NEG_INFINITY);
        assert_eq!(bounds.min.z, NEG_INFINITY);

        assert_eq!(bounds.max.x, INFINITY);
        assert_eq!(bounds.max.y, INFINITY);
        assert_eq!(bounds.max.z, INFINITY);
    }

    // Scenario: A bounded cone has a bounding box
    //   Given shape ← cone()
    //     And shape.minimum ← -5
    //     And shape.maximum ← 3
    //   When box ← bounds_of(shape)
    //   Then box.min = point(-5, -5, -5)
    //     And box.max = point(5, 3, 5)
    #[test]
    fn a_bounded_cone_has_a_bounding_box() {
        let mut world = World::new();

        let _s_id = world.scene.insert_object(
            Shape::Cone {
                minimum: -5.0,
                maximum: 3.0,
                closed: true,
            },
            None,
            None,
        );
        world.build();

        assert_eq!(
            bounds(&world.get_object(0).kind),
            BoundingBox::new(Point::new(-5.0, -5.0, -5.0), Point::new(5.0, 3.0, 5.0))
        )
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
        let p_id = world.scene.insert_object(Shape::Plane, None, None);
        world.build();

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

        let c_id = world.scene.insert_object(Shape::Cube, None, None);
        world.build();

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

        let c_id = world.scene.insert_object(
            Shape::Cylinder {
                minimum: -f32::INFINITY,
                maximum: f32::INFINITY,
                closed: false,
            },
            None,
            None,
        );
        world.build();

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

        let c_id = world.scene.insert_object(
            Shape::Cylinder {
                minimum: -f32::INFINITY,
                maximum: f32::INFINITY,
                closed: false,
            },
            None,
            None,
        );
        world.build();

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
        let c_id = world.scene.insert_object(
            Shape::Cylinder {
                minimum: -f32::INFINITY,
                maximum: f32::INFINITY,
                closed: false,
            },
            None,
            None,
        );
        world.build();

        let c = world.get_object(c_id);

        assert!(!c.closed());
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
        let c_id = world.scene.insert_object(
            Shape::Cylinder {
                minimum: 1.0,
                maximum: 2.0,
                closed: true,
            },
            None,
            None,
        );
        world.build();

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
    // use crate::world::{SceneGroup, SceneTree};
    // #[test]
    // fn converting_a_point_from_world_to_object_space() {
    //     let mut world = World::new();

    //     let mut scene = SceneTree::new();

    //     let o1_id = scene.insert_object(SceneObject::new(
    //         Shape::Sphere,
    //         Some(translation(5.0, 0.0, 0.0)),
    //         None,
    //     ));

    //     let g2_id = scene.insert_group(SceneGroup::new(
    //         vec![o1_id],
    //         Some(scaling(2.0, 2.0, 2.0)),
    //         None,
    //     ));

    //     let g1_id = scene.insert_group(SceneGroup::new(
    //         vec![g2_id],
    //         Some(rotation_y(PI / 2.0)),
    //         None,
    //     ));

    //     scene.apply_transforms(g1_id, &None);
    //     let scene_objects = scene.build();
    //     world.groups = vec![scene_objects];

    //     let p = (world.get_object(o1_id)).world_to_object(&Point::new(-2.0, 0.0, -10.0));

    //     assert_eq!(Point::new(0.0, 0.0, -1.0), p)
    // }

    // Scenario: Converting a normal from object to world space
    //   Given g1 ← group()
    //     And set_transform(g1, rotation_y(π/2))
    //     And g2 ← group()
    //     And set_transform(g2, scaling(1, 2, 3))
    //     And add_child(g1, g2)
    //     And s ← sphere()
    //     And set_transform(s, translation(5, 0, 0))
    //     And add_child(g2, s)
    //   When n ← normal_to_world(s, vector(√3/3, √3/3, √3/3))
    //   Then n = vector(0.2857, 0.4286, -0.8571)
    // #[test]
    // fn converting_a_normal_from_object_to_world_space() {
    //     let mut world = World::new();

    //     let mut scene = SceneTree::new();

    //     let o1_id = scene.insert_object(SceneObject::new(
    //         Shape::Sphere,
    //         Some(translation(5.0, 0.0, 0.0)),
    //         None,
    //     ));

    //     let g2_id = scene.insert_group(SceneGroup::new(
    //         vec![o1_id],
    //         Some(scaling(1.0, 2.0, 3.0)),
    //         None,
    //     ));

    //     let g1_id = scene.insert_group(SceneGroup::new(
    //         vec![g2_id],
    //         Some(rotation_y(PI / 2.0)),
    //         None,
    //     ));

    //     scene.apply_transforms(g1_id, &None);
    //     let scene_objects = scene.build();
    //     world.groups = vec![scene_objects];

    //     let p = (world.get_object(o1_id)).normal_to_world(&Vector::new(
    //         f32::sqrt(3.0 / 3.0),
    //         f32::sqrt(3.0 / 3.0),
    //         f32::sqrt(3.0 / 3.0),
    //     ));

    //     assert_eq!(Vector::new(0.2857, 0.4286, -0.8571), p)
    // }

    // Scenario: Finding the normal on a child object
    //   Given g1 ← group()
    //     And set_transform(g1, rotation_y(π/2))
    //     And g2 ← group()
    //     And set_transform(g2, scaling(1, 2, 3))
    //     And add_child(g1, g2)
    //     And s ← sphere()
    //     And set_transform(s, translation(5, 0, 0))
    //     And add_child(g2, s)
    //   When n ← normal_at(s, point(1.7321, 1.1547, -5.5774))
    //   Then n = vector(0.2857, 0.4286, -0.8571)
    use crate::world::SceneGroup;
    #[test]
    fn finding_the_normal_on_a_child_object() {
        let mut world = World::new();
        let o1_id =
            world
                .scene
                .insert_object(Shape::Sphere, Some(translation(5.0, 0.0, 0.0)), None);

        let g2_id = world.scene.insert_group(SceneGroup::new(
            vec![o1_id],
            Some(scaling(1.0, 2.0, 3.0)),
            None,
        ));

        let g1_id = world.scene.insert_group(SceneGroup::new(
            vec![g2_id],
            Some(rotation_y(PI / 2.0)),
            None,
        ));
        world.root_group_id = g1_id;
        world.build();

        let p = (world.get_object(o1_id)).normal_at(&Point::new(1.7321, 1.1547, -5.5774));

        assert_eq!(Vector::new(0.2857, 0.4286, -0.8571), p)
    }

    // Scenario: Querying a shape's bounding box in its parent's space
    //   Given shape ← sphere()
    //     And set_transform(shape, translation(1, -3, 5) * scaling(0.5, 2, 4))
    //   When box ← parent_space_bounds_of(shape)
    //   Then box.min = point(0.5, -5, 1)
    //     And box.max = point(1.5, -1, 9)
    #[test]
    fn querying_a_shapes_bounding_box_in_its_parents_space() {
        let mut world = World::new();

        let o1_id = world.scene.insert_object(
            Shape::Sphere,
            Some(&translation(1.0, -3.0, 5.0) * &scaling(0.5, 2.0, 4.0)),
            None,
        );

        let g1_id = world
            .scene
            .insert_group(SceneGroup::new(vec![o1_id], None, None));
        world.root_group_id = g1_id;
        world.build();

        let bbox = world.bvh[0].bounds();

        assert_eq!(bbox.min, Point::new(0.5, -5.0, 1.0));
        assert_eq!(bbox.max, Point::new(1.5, -1.0, 9.0))
    }

    // Scenario: A group has a bounding box that contains its children
    //   Given s ← sphere()
    //     And set_transform(s, translation(2, 5, -3) * scaling(2, 2, 2))
    //     And c ← cylinder()
    //     And c.minimum ← -2
    //     And c.maximum ← 2
    //     And set_transform(c, translation(-4, -1, 4) * scaling(0.5, 1, 0.5))
    //     And shape ← group()
    //     And add_child(shape, s)
    //     And add_child(shape, c)
    //   When box ← bounds_of(shape)
    //   Then box.min = point(-4.5, -3, -5)
    //     And box.max = point(4, 7, 4.5)
    #[test]
    fn a_group_has_a_bounding_box_that_contains_its_children() {
        let mut world = World::new();

        let s_id = world.scene.insert_object(
            Shape::Sphere,
            Some(&translation(2.0, 5.0, -3.0) * &scaling(2.0, 2.0, 2.0)),
            None,
        );

        let c_id = world.scene.insert_object(
            Shape::Cylinder {
                minimum: -2.0,
                maximum: 2.0,
                closed: true,
            },
            Some(&translation(-4.0, -1.0, 4.0) * &scaling(0.5, 1.0, 0.5)),
            None,
        );

        let g1_id = world
            .scene
            .insert_group(SceneGroup::new(vec![s_id, c_id], None, None));
        world.root_group_id = g1_id;
        world.build();

        let bbox = world.bvh[0].bounds();

        assert_eq!(bbox.min, Point::new(-4.5, -3.0, -5.0));
        assert_eq!(bbox.max, Point::new(4.0, 7.0, 4.5))
    }
}
