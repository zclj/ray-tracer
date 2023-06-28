use crate::rays::Ray;
use crate::vector::{Point, Vector};
use crate::world::World;

#[derive(Debug, PartialEq, Clone)]
pub struct Intersection {
    pub t: f32,
    pub object: u32,
}

#[derive(Debug)]
pub struct ComputedIntersection {
    pub t: f32,
    pub object: u32,
    pub point: Point,
    pub eyev: Vector,
    pub normalv: Vector,
    pub over_point: Point,
    pub under_point: Point,
    pub reflectv: Vector,
    pub n1: f32,
    pub n2: f32,
    pub cos_i: f32,
}

impl Intersection {
    #[must_use]
    pub fn new(t: f32, shape_id: u32) -> Self {
        Intersection {
            t,
            object: shape_id,
        }
    }

    #[must_use]
    /// # Panics
    ///
    /// Will panic if shape do not exist
    pub fn compute(
        &self,
        world: &World,
        ray: &Ray,
        intersections: &[Intersection],
        shadow_bias: f32,
        containers: &mut Vec<u32>,
    ) -> ComputedIntersection {
        let mut normalv = world
            .get_object(self.object)
            .normal_at(&ray.position(self.t));

        let eyev = -&ray.direction;

        if normalv.dot(&eyev) < 0.0 {
            normalv = -normalv;
        }

        let cpoint = ray.position(self.t);

        // compute refractive indices on both sides (n1, n2)
        // @TODO - there might be a nicer way to do this
        let mut n1 = 1.0;
        let mut n2 = 1.0;
        containers.clear();
        {
            for i in intersections {
                if i == self {
                    if containers.is_empty() {
                        n1 = 1.0;
                    } else {
                        let obj = containers.last().unwrap();
                        n1 = world.get_object(*obj).material.refractive_index;
                    }
                }

                if containers.contains(&i.object) {
                    containers.remove(containers.iter().position(|x| *x == i.object).unwrap());
                } else {
                    containers.push(i.object);
                }

                if i == self {
                    if containers.is_empty() {
                        n2 = 1.0;
                    } else {
                        let obj = containers.last().unwrap();
                        n2 = world.get_object(*obj).material.refractive_index;
                    }

                    break;
                }
            }
        };

        let shadow_biased_normal = &normalv * shadow_bias;
        ComputedIntersection {
            // normal might have changed direction
            cos_i: normalv.dot(&eyev),
            t: self.t,
            object: self.object,
            over_point: &cpoint + &shadow_biased_normal,
            under_point: &cpoint - &shadow_biased_normal,
            point: cpoint,
            eyev,
            reflectv: ray.direction.reflect(&normalv),
            normalv,
            n1,
            n2,
        }
    }
}

impl ComputedIntersection {
    #[must_use]
    pub fn schlick(&self) -> f32 {
        let mut cos = self.cos_i;

        if self.n1 > self.n2 {
            let n = self.n1 / self.n2;
            let sin2_t = n.powf(2.0) * (1.0 - cos.powf(2.0));
            if sin2_t > 1.0 {
                return 1.0;
            }

            cos = f32::sqrt(1.0 - sin2_t);
        }

        let r0 = ((self.n1 - self.n2) / (self.n1 + self.n2)).powf(2.0);
        r0 + (1.0 - r0) * (1.0 - cos).powf(5.0)
    }
}

/// # Panics
///
/// Will panic if intersection t value is NaN
pub fn sort_by_t(xs: &mut [Intersection]) {
    xs.sort_unstable_by(|a, b| a.t.partial_cmp(&b.t).unwrap());
}

/// # Panics
///
/// Will panic if intersection t value is NaN
// #[inline]
// #[must_use]
// pub fn hit(xs: &[Intersection]) -> Option<&Intersection> {
//     // intersections are already sorted
//     xs.iter().find(|x| x.t >= 0.0)
// }

#[cfg(test)]
mod test {
    use super::*;
    use crate::bounds::BoundingBox;
    use crate::shape::{RenderObject, Shape};
    use crate::transformations::{scaling, translation};
    use crate::utils::EPSILON;
    use crate::vector::Vector;
    use crate::world::World;

    // Scenario: An intersection encapsulates t and object
    //   Given s ← sphere()
    //   When i ← intersection(3.5, s)
    //   Then i.t = 3.5
    //     And i.object = s
    #[test]
    fn an_intersection_encapsulates_t_and_object() {
        let s_id = 1;
        let i = Intersection::new(3.5, s_id);

        assert_eq!(i.t, 3.5);
        assert_eq!(i.object, 1);
    }

    // Scenario: Aggregating intersections
    //   Given s ← sphere()
    //     And i1 ← intersection(1, s)
    //     And i2 ← intersection(2, s)
    //   When xs ← intersections(i1, i2)
    //   Then xs.count = 2
    //     And xs[0].t = 1
    //     And xs[1].t = 2
    #[test]
    fn aggregating_intersections() {
        let s1_id = 1;
        let s2_id = 2;
        let i1 = Intersection::new(1.0, s1_id);
        let i2 = Intersection::new(2.0, s2_id);

        let xs = [i1, i2];

        assert_eq!(xs.len(), 2);
        assert_eq!(xs[0].t, 1.0);
        assert_eq!(xs[1].t, 2.0);
    }

    // Scenario: A ray intersects a sphere at two points
    //   Given r ← ray(point(0, 0, -5), vector(0, 0, 1))
    //     And s ← sphere()
    //   When xs ← intersect(s, r)
    //   Then xs.count = 2
    //     And xs[0] = 4.0
    //     And xs[1] = 6.0
    #[test]
    fn a_ray_intersects_a_sphere_at_two_points() {
        let r = Ray::new(Point::new(0.0, 0.0, -5.0), Vector::new(0.0, 0.0, 1.0));
        let world = World::new();

        let s = RenderObject::new(0, Shape::Sphere, None, None, None);
        let mut xs = vec![];
        world.intersect_primitive(&r, &mut xs, &s);

        assert_eq!(xs.len(), 2);
        assert_eq!(xs[0].t, 4.0);
        assert_eq!(xs[1].t, 6.0);
    }

    // Scenario: A ray intersects a sphere at a tangent
    //   Given r ← ray(point(0, 1, -5), vector(0, 0, 1))
    //     And s ← sphere()
    //   When xs ← intersect(s, r)
    //   Then xs.count = 2
    //     And xs[0] = 5.0
    //     And xs[1] = 5.0
    #[test]
    fn a_ray_intersects_a_sphere_at_a_tangent() {
        let r = Ray::new(Point::new(0.0, 1.0, -5.0), Vector::new(0.0, 0.0, 1.0));
        let world = World::new();

        let s = RenderObject::new(0, Shape::Sphere, None, None, None);

        let mut xs = vec![];
        world.intersect_primitive(&r, &mut xs, &s);

        assert_eq!(xs.len(), 2);
        assert_eq!(xs[0].t, 5.0);
        assert_eq!(xs[1].t, 5.0);
    }

    // Scenario: A ray misses a sphere
    //   Given r ← ray(point(0, 2, -5), vector(0, 0, 1))
    //     And s ← sphere()
    //   When xs ← intersect(s, r)
    //   Then xs.count = 0
    #[test]
    fn a_ray_misses_a_sphere() {
        let r = Ray::new(Point::new(0.0, 2.0, -5.0), Vector::new(0.0, 0.0, 1.0));
        let world = World::new();

        let s = RenderObject::new(0, Shape::Sphere, None, None, None);

        let mut xs = vec![];
        world.intersect_primitive(&r, &mut xs, &s);

        assert_eq!(xs.len(), 0);
    }

    // Scenario: A ray originates inside a sphere
    //   Given r ← ray(point(0, 0, 0), vector(0, 0, 1))
    //     And s ← sphere()
    //   When xs ← intersect(s, r)
    //   Then xs.count = 2
    //     And xs[0] = -1.0
    //     And xs[1] = 1.0
    #[test]
    fn a_ray_originates_inside_a_sphere() {
        let r = Ray::new(Point::new(0.0, 0.0, 0.0), Vector::new(0.0, 0.0, 1.0));
        let world = World::new();

        let s = RenderObject::new(0, Shape::Sphere, None, None, None);
        let mut xs = vec![];
        world.intersect_primitive(&r, &mut xs, &s);

        assert_eq!(xs.len(), 2);
        assert_eq!(xs[0].t, -1.0);
        assert_eq!(xs[1].t, 1.0);
    }

    // Scenario: A sphere is behind a ray
    //   Given r ← ray(point(0, 0, 5), vector(0, 0, 1))
    //     And s ← sphere()
    //   When xs ← intersect(s, r)
    //   Then xs.count = 2
    //     And xs[0] = -6.0
    //     And xs[1] = -4.0
    #[test]
    fn a_sphere_is_behind_a_ray() {
        let r = Ray::new(Point::new(0.0, 0.0, 5.0), Vector::new(0.0, 0.0, 1.0));
        let world = World::new();

        let s = RenderObject::new(0, Shape::Sphere, None, None, None);
        let mut xs = vec![];
        world.intersect_primitive(&r, &mut xs, &s);

        assert_eq!(xs.len(), 2);
        assert_eq!(xs[0].t, -6.0);
        assert_eq!(xs[1].t, -4.0);
    }

    // Scenario: Intersect sets the object on the intersection
    //   Given r ← ray(point(0, 0, -5), vector(0, 0, 1))
    //     And s ← sphere()
    //   When xs ← intersect(s, r)
    //   Then xs.count = 2
    //     And xs[0].object = s
    //     And xs[1].object = s
    #[test]
    fn intersect_sets_the_object_on_the_intersection() {
        let r = Ray::new(Point::new(0.0, 0.0, -5.0), Vector::new(0.0, 0.0, 1.0));
        let world = World::new();

        let s_id = 10;
        let s = RenderObject::new(s_id, Shape::Sphere, None, None, None);
        let mut xs = vec![];
        world.intersect_primitive(&r, &mut xs, &s);

        assert_eq!(xs.len(), 2);
        assert_eq!(xs[0].object, s_id);
        assert_eq!(xs[1].object, s_id);
    }

    // Scenario: The hit, when all intersections have positive t
    //   Given s ← sphere()
    //     And i1 ← intersection(1, s)
    //     And i2 ← intersection(2, s)
    //     And xs ← intersections(i2, i1)
    //   When i ← hit(xs)
    //   Then i = i1
    // #[test]
    // fn the_hit_when_all_intersections_have_positive_t() {
    //     let mut world = World::new();
    //     let s_id = world.push_sphere(None, None);
    //     let i1 = Intersection::new(1.0, s_id, Kind::Sphere);
    //     let i2 = Intersection::new(2.0, s_id, Kind::Sphere);

    //     let mut is = [i1, i2];
    //     let i = hit(&mut is);

    //     assert_eq!(i, Some(&Intersection::new(1.0, s_id, Kind::Sphere)));
    // }

    // Scenario: The hit, when some intersections have negative t
    //   Given s ← sphere()
    //     And i1 ← intersection(-1, s)
    //     And i2 ← intersection(1, s)
    //     And xs ← intersections(i2, i1)
    //   When i ← hit(xs)
    //   Then i = i2
    // #[test]
    // fn the_hit_when_some_intersections_have_negative_t() {
    //     let mut world = World::new();
    //     let s_id = world.push_sphere(None, None);
    //     let i1 = Intersection::new(-1.0, s_id, Kind::Sphere);
    //     let i2 = Intersection::new(1.0, s_id, Kind::Sphere);

    //     let mut is = [i2, i1];
    //     let i = hit(&mut is);

    //     assert_eq!(i, Some(&Intersection::new(1.0, s_id, Kind::Sphere)));
    // }

    // Scenario: The hit, when all intersections have negative t
    //   Given s ← sphere()
    //     And i1 ← intersection(-2, s)
    //     And i2 ← intersection(-1, s)
    //     And xs ← intersections(i2, i1)
    //   When i ← hit(xs)
    //   Then i is nothing
    // #[test]
    // fn the_hit_when_all_intersections_have_negative_t() {
    //     let mut world = World::new();
    //     let s_id = world.push_sphere(None, None);
    //     let i1 = Intersection::new(-2.0, s_id, Kind::Sphere);
    //     let i2 = Intersection::new(-1.0, s_id, Kind::Sphere);

    //     let mut is = [i2, i1];
    //     let i = hit(&mut is);

    //     assert_eq!(i, None);
    // }

    // Scenario: The hit is always the lowest nonnegative intersection
    //   Given s ← sphere()
    //   And i1 ← intersection(5, s)
    //   And i2 ← intersection(7, s)
    //   And i3 ← intersection(-3, s)
    //   And i4 ← intersection(2, s)
    //   And xs ← intersections(i1, i2, i3, i4)
    // When i ← hit(xs)
    // Then i = i4
    // #[test]
    // fn the_hit_is_always_the_lowest_nonnegative_intersection() {
    //     let mut world = World::new();
    //     let s_id = world.push_sphere(None, None);
    //     let i1 = Intersection::new(5.0, s_id, Kind::Sphere);
    //     let i2 = Intersection::new(7.0, s_id, Kind::Sphere);
    //     let i3 = Intersection::new(-3.0, s_id, Kind::Sphere);
    //     let i4 = Intersection::new(2.0, s_id, Kind::Sphere);

    //     let mut is = [i1, i2, i3, i4];
    //     // to not sort redundantly, the sort is outside of the hit fn
    //     sort_by_t(&mut is);
    //     let i = hit(&mut is);

    //     assert_eq!(i, Some(&Intersection::new(2.0, s_id, Kind::Sphere)));
    // }

    // Scenario: Intersecting a scaled sphere with a ray
    //   Given r ← ray(point(0, 0, -5), vector(0, 0, 1))
    //     And s ← sphere()
    //   When set_transform(s, scaling(2, 2, 2))
    //     And xs ← intersect(s, r)
    //   Then xs.count = 2
    //     And xs[0].t = 3
    //     And xs[1].t = 7
    #[test]
    fn intersecting_a_scaled_sphere_with_a_ray() {
        let r = Ray::new(Point::new(0.0, 0.0, -5.0), Vector::new(0.0, 0.0, 1.0));
        let world = World::new();

        let s = RenderObject::new(0, Shape::Sphere, Some(scaling(2.0, 2.0, 2.0)), None, None);
        let mut xs = vec![];
        world.intersect_primitive(&r, &mut xs, &s);

        assert_eq!(xs.len(), 2);
        assert_eq!(xs[0].t, 3.0);
        assert_eq!(xs[1].t, 7.0);
    }

    // Scenario: Intersecting a translated sphere with a ray
    //   Given r ← ray(point(0, 0, -5), vector(0, 0, 1))
    //     And s ← sphere()
    //   When set_transform(s, translation(5, 0, 0))
    //     And xs ← intersect(s, r)
    //   Then xs.count = 0
    #[test]
    fn intersecting_a_translated_sphere_with_a_ray() {
        let r = Ray::new(Point::new(0.0, 0.0, -5.0), Vector::new(0.0, 0.0, 1.0));
        let world = World::new();
        let s = RenderObject::new(
            0,
            Shape::Sphere,
            Some(translation(5.0, 0.0, 0.0)),
            None,
            None,
        );
        let mut xs = vec![];
        world.intersect_primitive(&r, &mut xs, &s);

        assert_eq!(xs.len(), 0);
    }

    // Scenario: Precomputing the state of an intersection
    //   Given r ← ray(point(0, 0, -5), vector(0, 0, 1))
    //     And shape ← sphere()
    //     And i ← intersection(4, shape)
    //   When comps ← prepare_computations(i, r)
    //   Then comps.t = i.t
    //     And comps.object = i.object
    //     And comps.point = point(0, 0, -1)
    //     And comps.eyev = vector(0, 0, -1)
    //     And comps.normalv = vector(0, 0, -1)
    #[test]
    fn precomputing_the_state_of_an_intersection() {
        let r = Ray::new(Point::new(0.0, 0.0, -5.0), Vector::new(0.0, 0.0, 1.0));
        let mut world = World::new();

        let s_id = world.scene.insert_object(Shape::Sphere, None, None);
        world.build();

        let i = Intersection::new(4.0, s_id);
        let mut containers = vec![];
        let comps = i.compute(&world, &r, &[i.clone()], EPSILON, &mut containers);

        assert_eq!(comps.t, i.t);
        assert_eq!(comps.object, i.object);
        assert_eq!(comps.point, Point::new(0.0, 0.0, -1.0));
        assert_eq!(comps.eyev, Vector::new(0.0, 0.0, -1.0));
        assert_eq!(comps.normalv, Vector::new(0.0, 0.0, -1.0));
    }

    // Scenario: The hit, when an intersection occurs on the outside
    //   Given r ← ray(point(0, 0, -5), vector(0, 0, 1))
    //     And shape ← sphere()
    //     And i ← intersection(4, shape)
    //   When comps ← prepare_computations(i, r)
    //   Then comps.inside = false
    // #[test]
    // fn the_hit_when_an_intersection_occurs_on_the_outside() {
    //     let r = Ray::new(Point::new(0.0, 0.0, -5.0), Vector::new(0.0, 0.0, 1.0));
    //     let mut world = World::new();
    //     let s_id = world.push_sphere(None, None);

    //     let i = Intersection::new(4.0, s_id);

    //     let comps = i.compute(&world, &r, &[i.clone()], EPSILON);

    //     assert_eq!(comps.inside, false);
    // }

    // Scenario: The hit, when an intersection occurs on the inside
    //   Given r ← ray(point(0, 0, 0), vector(0, 0, 1))
    //     And shape ← sphere()
    //     And i ← intersection(1, shape)
    //   When comps ← prepare_computations(i, r)
    //   Then comps.point = point(0, 0, 1)
    //     And comps.eyev = vector(0, 0, -1)
    //     And comps.inside = true
    //       # normal would have been (0, 0, 1), but is inverted!
    //     And comps.normalv = vector(0, 0, -1)
    // #[test]
    // fn the_hit_when_an_intersection_occurs_on_the_inside() {
    //     let r = Ray::new(Point::new(0.0, 0.0, 0.0), Vector::new(0.0, 0.0, 1.0));
    //     let mut world = World::new();
    //     let s_id = world.push_sphere(None, None);

    //     let i = Intersection::new(1.0, s_id);

    //     let comps = i.compute(&world, &r, &[i.clone()], EPSILON);

    //     assert_eq!(comps.point, Point::new(0.0, 0.0, 1.0));
    //     assert_eq!(comps.eyev, Vector::new(0.0, 0.0, -1.0));
    //     assert_eq!(comps.normalv, Vector::new(0.0, 0.0, -1.0));
    //     assert_eq!(comps.inside, true);
    // }

    // Scenario: The hit should offset the point
    //   Given r ← ray(point(0, 0, -5), vector(0, 0, 1))
    //     And shape ← sphere() with:
    //       | transform | translation(0, 0, 1) |
    //     And i ← intersection(5, shape)
    //   When comps ← prepare_computations(i, r)
    //   Then comps.over_point.z < -EPSILON/2
    //     And comps.point.z > comps.over_point.z
    #[test]
    fn the_hit_should_offset_the_point() {
        let r = Ray::new(Point::new(0.0, 0.0, -5.0), Vector::new(0.0, 0.0, 1.0));
        let mut world = World::new();
        world
            .scene
            .insert_object(Shape::Sphere, Some(translation(0.0, 0.0, 1.0)), None);
        let s_id = 0;
        world.build();

        let i = Intersection::new(5.0, s_id);
        let mut containers = vec![];
        let comps = i.compute(&world, &r, &[i.clone()], EPSILON, &mut containers);
        assert!(comps.over_point.z < -EPSILON / 2.0);
        assert!(comps.point.z > comps.over_point.z);
    }

    // Scenario: Intersect with a ray parallel to the plane
    //   Given p ← plane()
    //     And r ← ray(point(0, 10, 0), vector(0, 0, 1))
    //   When xs ← local_intersect(p, r)
    //   Then xs is empty
    #[test]
    fn intersect_with_a_ray_parallel_to_the_plane() {
        let mut world = World::new();
        let p_id = 0;
        world.scene.insert_object(Shape::Plane, None, None);

        world.build();
        let p = world.get_object(p_id);

        let r = Ray::new(Point::new(0.0, 10.0, 0.0), Vector::new(0.0, 0.0, 1.0));

        let mut xs = vec![];
        world.intersect_primitive(&r, &mut xs, p);

        assert_eq!(xs.len(), 0)
    }

    // Scenario: Intersect with a coplanar ray
    //   Given p ← plane()
    //     And r ← ray(point(0, 0, 0), vector(0, 0, 1))
    //   When xs ← local_intersect(p, r)
    //   Then xs is empty
    #[test]
    fn intersect_with_a_coplanar_ray() {
        let mut world = World::new();
        let p_id = 0;
        world.scene.insert_object(Shape::Plane, None, None);

        world.build();
        let p = world.get_object(p_id);

        let r = Ray::new(Point::new(0.0, 0.0, 0.0), Vector::new(0.0, 0.0, 1.0));

        let mut xs = vec![];
        world.intersect_primitive(&r, &mut xs, p);

        assert_eq!(xs.len(), 0)
    }

    // Scenario: A ray intersecting a plane from above
    //   Given p ← plane()
    //     And r ← ray(point(0, 1, 0), vector(0, -1, 0))
    //   When xs ← local_intersect(p, r)
    //   Then xs.count = 1
    //     And xs[0].t = 1
    //     And xs[0].object = p
    #[test]
    fn a_ray_intersecting_a_plane_from_above() {
        let mut world = World::new();

        let p_id = 0;
        world.scene.insert_object(Shape::Plane, None, None);

        world.build();
        let p = world.get_object(p_id);

        let r = Ray::new(Point::new(0.0, 1.0, 0.0), Vector::new(0.0, -1.0, 0.0));

        let mut xs = vec![];
        world.intersect_primitive(&r, &mut xs, p);

        assert_eq!(xs.len(), 1);
        assert_eq!(xs[0].t, 1.0);
        assert_eq!(xs[0].object, p_id)
    }

    // Scenario: A ray intersecting a plane from below
    //   Given p ← plane()
    //     And r ← ray(point(0, -1, 0), vector(0, 1, 0))
    //   When xs ← local_intersect(p, r)
    //   Then xs.count = 1
    //     And xs[0].t = 1
    //     And xs[0].object = p
    #[test]
    fn a_ray_intersecting_a_plane_from_below() {
        let mut world = World::new();

        let p_id = 0;
        world.scene.insert_object(Shape::Plane, None, None);

        world.build();
        let p = world.get_object(p_id);

        let r = Ray::new(Point::new(0.0, -1.0, 0.0), Vector::new(0.0, 1.0, 0.0));

        let mut xs = vec![];
        world.intersect_primitive(&r, &mut xs, p);

        assert_eq!(xs.len(), 1);
        assert_eq!(xs[0].t, 1.0);
        assert_eq!(xs[0].object, p_id)
    }

    // Scenario Outline: A ray intersects a cube
    //   Given c ← cube()
    //     And r ← ray(<origin>, <direction>)
    //   When xs ← local_intersect(c, r)
    //   Then xs.count = 2
    //     And xs[0].t = <t1>
    //     And xs[1].t = <t2>

    //   Examples:
    //     |        | origin            | direction        | t1 | t2 |
    //     | +x     | point(5, 0.5, 0)  | vector(-1, 0, 0) |  4 |  6 |
    //     | -x     | point(-5, 0.5, 0) | vector(1, 0, 0)  |  4 |  6 |
    //     | +y     | point(0.5, 5, 0)  | vector(0, -1, 0) |  4 |  6 |
    //     | -y     | point(0.5, -5, 0) | vector(0, 1, 0)  |  4 |  6 |
    //     | +z     | point(0.5, 0, 5)  | vector(0, 0, -1) |  4 |  6 |
    //     | -z     | point(0.5, 0, -5) | vector(0, 0, 1)  |  4 |  6 |
    //     | inside | point(0, 0.5, 0)  | vector(0, 0, 1)  | -1 |  1 |
    #[test]
    fn a_ray_intersects_a_cube() {
        let mut world = World::new();

        world.scene.insert_object(Shape::Cube, None, None);

        world.build();

        let rays = [
            Ray::new(Point::new(5.0, 0.5, 0.0), Vector::new(-1.0, 0.0, 0.0)),
            Ray::new(Point::new(-5.0, 0.5, 0.0), Vector::new(1.0, 0.0, 0.0)),
            Ray::new(Point::new(0.5, 5.0, 0.0), Vector::new(0.0, -1.0, 0.0)),
            Ray::new(Point::new(0.5, -5.0, 0.0), Vector::new(0.0, 1.0, 0.0)),
            Ray::new(Point::new(0.5, 0.0, 5.0), Vector::new(0.0, 0.0, -1.0)),
            Ray::new(Point::new(0.5, 0.0, -5.0), Vector::new(0.0, 0.0, 1.0)),
            Ray::new(Point::new(0.0, 0.5, 0.0), Vector::new(0.0, 0.0, 1.0)),
        ];

        let c = world.get_object(0);

        let xss = rays
            .iter()
            .map(|r| {
                let mut xs = vec![];
                world.intersect_primitive(r, &mut xs, c);
                xs.clone()
            })
            .collect::<Vec<Vec<Intersection>>>();

        assert_eq!(xss.len(), 7);
        for i in 0..6 {
            assert_eq!(xss[i][0].t, 4.0);
            assert_eq!(xss[i][1].t, 6.0);
        }

        assert_eq!(xss[6][0].t, -1.0);
        assert_eq!(xss[6][1].t, 1.0);
    }

    // Scenario Outline: A ray misses a cube
    //   Given c ← cube()
    //     And r ← ray(<origin>, <direction>)
    //   When xs ← local_intersect(c, r)
    //   Then xs.count = 0

    //   Examples:
    //     | origin           | direction                      |
    //     | point(-2, 0, 0)  | vector(0.2673, 0.5345, 0.8018) |
    //     | point(0, -2, 0)  | vector(0.8018, 0.2673, 0.5345) |
    //     | point(0, 0, -2)  | vector(0.5345, 0.8018, 0.2673) |
    //     | point(2, 0, 2)   | vector(0, 0, -1)               |
    //     | point(0, 2, 2)   | vector(0, -1, 0)               |
    //     | point(2, 2, 0)   | vector(-1, 0, 0)               |
    #[test]
    fn a_ray_misses_a_cube() {
        let mut world = World::new();

        world.scene.insert_object(Shape::Cube, None, None);

        world.build();

        let rays = [
            Ray::new(
                Point::new(-2.0, 0.0, 0.0),
                Vector::new(0.2673, 0.5345, 0.8018),
            ),
            Ray::new(
                Point::new(0.0, -2.0, 0.0),
                Vector::new(0.8018, 0.2673, 0.5345),
            ),
            Ray::new(
                Point::new(0.0, 0.0, -2.0),
                Vector::new(0.5345, 0.8018, 0.2673),
            ),
            Ray::new(Point::new(2.0, 0.0, 2.0), Vector::new(0.0, 0.0, -1.0)),
            Ray::new(Point::new(0.0, 2.0, 2.0), Vector::new(0.0, -1.0, 0.0)),
            Ray::new(Point::new(2.0, 2.0, 0.0), Vector::new(-1.0, 0.0, 0.0)),
        ];

        let c = world.get_object(0);

        let xss = rays
            .iter()
            .map(|r| {
                let mut xs = vec![];
                world.intersect_primitive(r, &mut xs, c);
                xs.clone()
            })
            .collect::<Vec<Vec<Intersection>>>();

        assert_eq!(xss.len(), 6);
        for i in 0..6 {
            assert_eq!(xss[i].len(), 0);
        }
    }

    // Scenario Outline: A ray misses a cylinder
    //   Given cyl ← cylinder()
    //     And direction ← normalize(<direction>)
    //     And r ← ray(<origin>, direction)
    //   When xs ← local_intersect(cyl, r)
    //   Then xs.count = 0

    //   Examples:
    //     | origin          | direction       |
    //     | point(1, 0, 0)  | vector(0, 1, 0) |
    //     | point(0, 0, 0)  | vector(0, 1, 0) |
    //     | point(0, 0, -5) | vector(1, 1, 1) |
    #[test]
    fn a_ray_misses_a_cylinder() {
        let mut world = World::new();
        world.scene.insert_object(
            Shape::Cylinder {
                minimum: f32::MIN,
                maximum: f32::MAX,
                closed: false,
            },
            None,
            None,
        );

        world.build();

        let rays = [
            Ray::new(Point::new(1.0, 0.0, 0.0), Vector::new(0.0, 1.0, 0.0).norm()),
            Ray::new(Point::new(0.0, 0.0, 0.0), Vector::new(0.0, 1.0, 0.0).norm()),
            Ray::new(
                Point::new(0.0, 0.0, -5.0),
                Vector::new(1.0, 1.0, 1.0).norm(),
            ),
        ];

        let c = world.get_object(0);

        let xss = rays
            .iter()
            .map(|r| {
                let mut xs = vec![];
                world.intersect_primitive(r, &mut xs, c);
                xs.clone()
            })
            .collect::<Vec<Vec<Intersection>>>();

        assert_eq!(xss.len(), 3);
        for i in 0..3 {
            assert_eq!(xss[i].len(), 0);
        }
    }

    // Scenario Outline: A ray strikes a cylinder
    //   Given cyl ← cylinder()
    //     And direction ← normalize(<direction>)
    //     And r ← ray(<origin>, direction)
    //   When xs ← local_intersect(cyl, r)
    //   Then xs.count = 2
    //     And xs[0].t = <t0>
    //     And xs[1].t = <t1>

    //   Examples:
    //     | origin            | direction         | t0      | t1      |
    //     | point(1, 0, -5)   | vector(0, 0, 1)   | 5       | 5       |
    //     | point(0, 0, -5)   | vector(0, 0, 1)   | 4       | 6       |
    //     | point(0.5, 0, -5) | vector(0.1, 1, 1) | 6.80798 | 7.08872 |
    #[test]
    fn a_ray_strikes_a_cylinder() {
        let mut world = World::new();
        world.scene.insert_object(
            Shape::Cylinder {
                minimum: f32::MIN,
                maximum: f32::MAX,
                closed: false,
            },
            None,
            None,
        );

        world.build();

        let rays = [
            Ray::new(
                Point::new(1.0, 0.0, -5.0),
                Vector::new(0.0, 0.0, 1.0).norm(),
            ),
            Ray::new(
                Point::new(0.0, 0.0, -5.0),
                Vector::new(0.0, 0.0, 1.0).norm(),
            ),
            Ray::new(
                Point::new(0.5, 0.0, -5.0),
                Vector::new(0.1, 1.0, 1.0).norm(),
            ),
        ];

        let c = world.get_object(0);

        let xss = rays
            .iter()
            .map(|r| {
                let mut xs = vec![];
                world.intersect_primitive(r, &mut xs, c);
                xs.clone()
            })
            .collect::<Vec<Vec<Intersection>>>();

        assert_eq!(xss[0].len(), 2);
        assert_eq!(xss[0][0].t, 5.0);
        assert_eq!(xss[0][1].t, 5.0);

        assert_eq!(xss[1].len(), 2);
        assert_eq!(xss[1][0].t, 4.0);
        assert_eq!(xss[1][1].t, 6.0);

        assert_eq!(xss[2].len(), 2);
        assert!(epsilon_eq(xss[2][0].t, 6.80798));
        assert!(epsilon_eq(xss[2][1].t, 7.08872));
    }

    // Scenario Outline: Intersecting a constrained cylinder
    //   Given cyl ← cylinder()
    //     And cyl.minimum ← 1
    //     And cyl.maximum ← 2
    //     And direction ← normalize(<direction>)
    //     And r ← ray(<point>, direction)
    //   When xs ← local_intersect(cyl, r)
    //   Then xs.count = <count>

    //   Examples:
    //     |   | point             | direction         | count |
    //     | 1 | point(0, 1.5, 0)  | vector(0.1, 1, 0) | 0     |
    //     | 2 | point(0, 3, -5)   | vector(0, 0, 1)   | 0     |
    //     | 3 | point(0, 0, -5)   | vector(0, 0, 1)   | 0     |
    //     | 4 | point(0, 2, -5)   | vector(0, 0, 1)   | 0     |
    //     | 5 | point(0, 1, -5)   | vector(0, 0, 1)   | 0     |
    //     | 6 | point(0, 1.5, -2) | vector(0, 0, 1)   | 2     |
    #[test]
    fn intersecting_a_constrained_cylinder() {
        let mut world = World::new();

        world.scene.insert_object(
            Shape::Cylinder {
                minimum: 1.0,
                maximum: 2.0,
                closed: false,
            },
            None,
            None,
        );

        world.build();

        let rays = [
            Ray::new(Point::new(0.0, 1.5, 0.0), Vector::new(0.1, 1.0, 0.0).norm()),
            Ray::new(
                Point::new(0.0, 3.0, -5.0),
                Vector::new(0.0, 0.0, 1.0).norm(),
            ),
            Ray::new(
                Point::new(0.0, 0.0, -5.0),
                Vector::new(0.0, 0.0, 1.0).norm(),
            ),
            Ray::new(
                Point::new(0.0, 2.0, -5.0),
                Vector::new(0.0, 0.0, 1.0).norm(),
            ),
            Ray::new(
                Point::new(0.0, 1.0, -5.0),
                Vector::new(0.0, 0.0, 1.0).norm(),
            ),
            Ray::new(
                Point::new(0.0, 1.5, -2.0),
                Vector::new(0.0, 0.0, 1.0).norm(),
            ),
        ];

        let c = world.get_object(0);
        let xss = rays
            .iter()
            .map(|r| {
                let mut xs = vec![];
                world.intersect_primitive(r, &mut xs, c);
                xs.clone()
            })
            .collect::<Vec<Vec<Intersection>>>();

        for i in 0..5 {
            assert_eq!(xss[i].len(), 0);
        }

        assert_eq!(xss[5].len(), 2);
    }

    // Scenario Outline: Intersecting the caps of a closed cylinder
    //   Given cyl ← cylinder()
    //     And cyl.minimum ← 1
    //     And cyl.maximum ← 2
    //     And cyl.closed ← true
    //     And direction ← normalize(<direction>)
    //     And r ← ray(<point>, direction)
    //   When xs ← local_intersect(cyl, r)
    //   Then xs.count = <count>

    //   Examples:
    //     |   | point            | direction        | count |
    //     | 1 | point(0, 3, 0)   | vector(0, -1, 0) | 2     |
    //     | 2 | point(0, 3, -2)  | vector(0, -1, 2) | 2     |
    //     | 3 | point(0, 4, -2)  | vector(0, -1, 1) | 2     | # corner case
    //     | 4 | point(0, 0, -2)  | vector(0, 1, 2)  | 2     |
    //     | 5 | point(0, -1, -2) | vector(0, 1, 1)  | 2     | # corner case
    #[test]
    fn intersecting_the_caps_of_a_closed_cylinder() {
        let mut world = World::new();

        world.scene.insert_object(
            Shape::Cylinder {
                minimum: 1.0,
                maximum: 2.0,
                closed: true,
            },
            None,
            None,
        );

        world.build();

        let rays = [
            Ray::new(
                Point::new(0.0, 3.0, 0.0),
                Vector::new(0.0, -1.0, 0.0).norm(),
            ),
            Ray::new(
                Point::new(0.0, 3.0, -2.0),
                Vector::new(0.0, -1.0, 2.0).norm(),
            ),
            Ray::new(
                Point::new(0.0, 4.0, -2.0),
                Vector::new(0.0, -1.0, 1.0).norm(),
            ),
            Ray::new(
                Point::new(0.0, 0.0, -2.0),
                Vector::new(0.0, 1.0, 2.0).norm(),
            ),
            Ray::new(
                Point::new(0.0, -1.0, -2.0),
                Vector::new(0.0, 1.0, 1.0).norm(),
            ),
        ];

        let c = world.get_object(0);
        let xss = rays
            .iter()
            .map(|r| {
                let mut xs = vec![];
                world.intersect_primitive(r, &mut xs, c);
                xs.clone()
            })
            .collect::<Vec<Vec<Intersection>>>();

        for i in 0..5 {
            println!("i: {i:?}");
            assert_eq!(xss[i].len(), 2);
        }
    }

    // Scenario Outline: Intersecting a cone with a ray
    //   Given shape ← cone()
    //     And direction ← normalize(<direction>)
    //     And r ← ray(<origin>, direction)
    //   When xs ← local_intersect(shape, r)
    //   Then xs.count = 2
    //     And xs[0].t = <t0>
    //     And xs[1].t = <t1>

    //   Examples:
    //     | origin          | direction           | t0      | t1       |
    //     | point(0, 0, -5) | vector(0, 0, 1)     | 5       |  5       |
    //     | point(0, 0, -5) | vector(1, 1, 1)     | 8.66025 |  8.66025 |
    //     | point(1, 1, -5) | vector(-0.5, -1, 1) | 4.55006 | 49.44994 |
    #[test]
    fn intersecting_a_cone_with_a_ray() {
        let mut world = World::new();

        world.scene.insert_object(
            Shape::Cone {
                minimum: f32::MIN,
                maximum: f32::MAX,
                closed: false,
            },
            None,
            None,
        );

        world.build();

        let rays = [
            Ray::new(
                Point::new(0.0, 0.0, -5.0),
                Vector::new(0.0, 0.0, 1.0).norm(),
            ),
            Ray::new(
                Point::new(0.0, 0.0, -5.0),
                Vector::new(1.0, 1.0, 1.0).norm(),
            ),
            Ray::new(
                Point::new(1.0, 1.0, -5.0),
                Vector::new(-0.5, -1.0, 1.0).norm(),
            ),
        ];

        let c = world.get_object(0);
        let xss = rays
            .iter()
            .map(|r| {
                let mut xs = vec![];
                world.intersect_primitive(r, &mut xs, c);
                xs.clone()
            })
            .collect::<Vec<Vec<Intersection>>>();

        assert_eq!(xss[0].len(), 2);
        assert_eq!(xss[0][0].t, 5.0);
        assert_eq!(xss[0][1].t, 5.0);

        assert_eq!(xss[1].len(), 2);
        assert!(epsilon_eq(xss[1][0].t, 8.66025));
        assert!(epsilon_eq(xss[1][1].t, 8.66025));

        assert_eq!(xss[2].len(), 2);
        assert!(epsilon_eq(xss[2][0].t, 4.55006));
        assert!(epsilon_eq(xss[2][1].t, 49.44994));
    }

    // Scenario: Intersecting a cone with a ray parallel to one of its halves
    //   Given shape ← cone()
    //     And direction ← normalize(vector(0, 1, 1))
    //     And r ← ray(point(0, 0, -1), direction)
    //   When xs ← local_intersect(shape, r)
    //   Then xs.count = 1
    //     And xs[0].t = 0.35355
    #[test]
    fn intersecting_a_cone_with_a_ray_parallel_to_one_of_its_halves() {
        let mut world = World::new();
        world.scene.insert_object(
            Shape::Cone {
                minimum: f32::MIN,
                maximum: f32::MAX,
                closed: false,
            },
            None,
            None,
        );

        world.build();

        let rays = [Ray::new(
            Point::new(0.0, 0.0, -1.0),
            Vector::new(0.0, 1.0, 1.0).norm(),
        )];

        let c = world.get_object(0);
        let xss = rays
            .iter()
            .map(|r| {
                let mut xs = vec![];
                world.intersect_primitive(r, &mut xs, c);
                xs.clone()
            })
            .collect::<Vec<Vec<Intersection>>>();

        assert_eq!(xss[0].len(), 1);
        assert!(epsilon_eq(xss[0][0].t, 0.35355));
    }

    // Scenario Outline: Intersecting a cone's end caps
    //   Given shape ← cone()
    //     And shape.minimum ← -0.5
    //     And shape.maximum ← 0.5
    //     And shape.closed ← true
    //     And direction ← normalize(<direction>)
    //     And r ← ray(<origin>, direction)
    //   When xs ← local_intersect(shape, r)
    //   Then xs.count = <count>

    //   Examples:
    //     | origin             | direction       | count |
    //     | point(0, 0, -5)    | vector(0, 1, 0) | 0     |
    //     | point(0, 0, -0.25) | vector(0, 1, 1) | 2     |
    //     | point(0, 0, -0.25) | vector(0, 1, 0) | 4     |
    #[test]
    fn intersecting_a_cones_end_caps() {
        let mut world = World::new();

        world.scene.insert_object(
            Shape::Cone {
                minimum: -0.5,
                maximum: 0.5,
                closed: true,
            },
            None,
            None,
        );

        world.build();

        let rays = [
            Ray::new(
                Point::new(0.0, 0.0, -5.0),
                Vector::new(0.0, 1.0, 0.0).norm(),
            ),
            Ray::new(
                Point::new(0.0, 0.0, -0.25),
                Vector::new(0.0, 1.0, 1.0).norm(),
            ),
            Ray::new(
                Point::new(0.0, 0.0, -0.25),
                Vector::new(0.0, 1.0, 0.0).norm(),
            ),
        ];

        let c = world.get_object(0);
        let xss = rays
            .iter()
            .map(|r| {
                let mut xs = vec![];
                world.intersect_primitive(r, &mut xs, c);
                xs.clone()
            })
            .collect::<Vec<Vec<Intersection>>>();

        assert_eq!(xss[0].len(), 0);
        assert_eq!(xss[1].len(), 2);
        assert_eq!(xss[2].len(), 4);
    }

    // Scenario: Intersecting a ray with an empty group
    //   Given g ← group()
    //     And r ← ray(point(0, 0, 0), vector(0, 0, 1))
    //   When xs ← local_intersect(g, r)
    //   Then xs is empty
    // #[test]
    // NOTE: We do not intersect groups as a primitive
    // fn intersecting_a_ray_with_an_empty_group() {
    //     let mut world = World::new();

    //     world.scene.insert_group(SceneGroup::new(
    //     vec![],
    //     None,
    //     None,
    //     ));

    //     world.build();

    //     let mut xs = vec![];

    //     let r = Ray::new(Point::new(0.0, 0.0, 0.0), Vector::new(0.0, 0.0, 1.0).norm());

    //     world.intersect(&r, &mut xs);

    //     assert_eq!(xs.len(), 0)
    // }

    // Scenario: Intersecting a ray with a nonempty group
    //   Given g ← group()
    //     And s1 ← sphere()
    //     And s2 ← sphere()
    //     And set_transform(s2, translation(0, 0, -3))
    //     And s3 ← sphere()
    //     And set_transform(s3, translation(5, 0, 0))
    //     And add_child(g, s1)
    //     And add_child(g, s2)
    //     And add_child(g, s3)
    //   When r ← ray(point(0, 0, -5), vector(0, 0, 1))
    //     And xs ← local_intersect(g, r)
    //   Then xs.count = 4
    //     And xs[0].object = s2
    //     And xs[1].object = s2
    //     And xs[2].object = s1
    //     And xs[3].object = s1
    // #[test]
    // fn intersecting_a_ray_with_a_nonempty_group() {
    //     let mut world = World::new();

    //     // let s1 = world.push_sphere(None, None);
    //     // let s2 = world.push_sphere(Some(translation(0.0, 0.0, -3.0)), None);
    //     // let s3 = world.push_sphere(Some(translation(5.0, 0.0, 0.0)), None);

    //     world.push_group(
    //         vec![
    //             SceneObject::new(Shape::Sphere, None, None),
    //             SceneObject::new(Shape::Sphere, Some(translation(0.0, 0.0, -3.0)), None),
    //             SceneObject::new(Shape::Sphere, Some(translation(5.0, 0.0, 0.0)), None),
    //         ],
    //         None,
    //     );

    //     let mut xs = vec![];

    //     let r = Ray::new(
    //         Point::new(0.0, 0.0, -5.0),
    //         Vector::new(0.0, 0.0, 1.0).norm(),
    //     );

    //     world.intersect(&r, &mut xs);

    //     assert_eq!(xs.len(), 4)
    // }

    // Scenario: Intersecting a transformed group
    //   Given g ← group()
    //     And set_transform(g, scaling(2, 2, 2))
    //     And s ← sphere()
    //     And set_transform(s, translation(5, 0, 0))
    //     And add_child(g, s)
    //   When r ← ray(point(10, 0, -10), vector(0, 0, 1))
    //     And xs ← intersect(g, r)
    //   Then xs.count = 2
    // use crate::world::{SceneGroup, SceneTree};
    // #[test]
    // fn intersecting_a_transformed_group() {
    //     let mut world = World::new();

    //     let mut scene = SceneTree::new();
    //     let id_1 = scene.insert_object(SceneObject::new(
    //         Shape::Sphere,
    //         Some(translation(5.0, 0.0, 0.0)),
    //         None,
    //     ));

    //     let g_id_1 = scene.insert_group(SceneGroup::new(vec![id_1], None, None));

    //     let g_id = scene.insert_group(SceneGroup::new(
    //         vec![g_id_1],
    //         Some(scaling(2.0, 2.0, 2.0)),
    //         None,
    //     ));

    //     println!("{:#?}", scene);

    //     scene.apply_transforms(g_id, &None, &mut BoundingBox::default());

    //     println!("{:#?}", scene);

    //     let scene_objects = scene.build();
    //     println!("{:#?}", scene_objects);

    //     world.groups = vec![scene_objects];

    //     let mut xs = vec![];

    //     let r = Ray::new(
    //         Point::new(10.0, 0.0, -10.0),
    //         Vector::new(0.0, 0.0, 1.0).norm(),
    //     );

    //     world.intersect(&r, &mut xs);

    //     assert_eq!(xs.len(), 2)
    // }

    // Scenario: Precomputing the reflection vector
    //   Given shape ← plane()
    //     And r ← ray(point(0, 1, -1), vector(0, -√2/2, √2/2))
    //     And i ← intersection(√2, shape)
    //   When comps ← prepare_computations(i, r)
    //   Then comps.reflectv = vector(0, √2/2, √2/2)
    #[test]
    fn precomputing_the_reflection_vector() {
        let mut world = World::new();
        let s_id = world.scene.insert_object(Shape::Plane, None, None);
        world.build();

        let r = Ray::new(
            Point::new(0.0, 1.0, -1.0),
            Vector::new(0.0, -f32::sqrt(2.0) / 2.0, f32::sqrt(2.0) / 2.0),
        );
        let i = Intersection::new(f32::sqrt(2.0), s_id);
        let mut containers = vec![];
        let comps = i.compute(&world, &r, &[i.clone()], EPSILON, &mut containers);

        assert_eq!(
            comps.reflectv,
            Vector::new(0.0, f32::sqrt(2.0) / 2.0, f32::sqrt(2.0) / 2.0)
        );
    }

    // Scenario Outline: Finding n1 and n2 at various intersections
    //   Given A ← glass_sphere() with:
    //       | transform                 | scaling(2, 2, 2) |
    //       | material.refractive_index | 1.5              |
    //     And B ← glass_sphere() with:
    //       | transform                 | translation(0, 0, -0.25) |
    //       | material.refractive_index | 2.0                      |
    //     And C ← glass_sphere() with:
    //       | transform                 | translation(0, 0, 0.25) |
    //       | material.refractive_index | 2.5                     |
    //     And r ← ray(point(0, 0, -4), vector(0, 0, 1))
    //     And xs ← intersections(2:A, 2.75:B, 3.25:C, 4.75:B, 5.25:C, 6:A)
    //   When comps ← prepare_computations(xs[<index>], r, xs)
    //   Then comps.n1 = <n1>
    //     And comps.n2 = <n2>

    //   Examples:
    //     | index | n1  | n2  |
    //     | 0     | 1.0 | 1.5 |
    //     | 1     | 1.5 | 2.0 |
    //     | 2     | 2.0 | 2.5 |
    //     | 3     | 2.5 | 2.5 |
    //     | 4     | 2.5 | 1.5 |
    //     | 5     | 1.5 | 1.0 |
    use crate::materials::Material;

    #[test]
    fn finding_n1_and_n2_at_various_intersections() {
        let mut world = World::new();

        let a_id = world.scene.insert_object(
            Shape::Sphere,
            Some(scaling(2.0, 2.0, 2.0)),
            Some(Material {
                transparency: 1.0,
                refractive_index: 1.5,
                ..Default::default()
            }),
        );

        let b_id = world.scene.insert_object(
            Shape::Sphere,
            Some(translation(0.0, 0.0, -0.25)),
            Some(Material {
                transparency: 1.0,
                refractive_index: 2.0,
                ..Default::default()
            }),
        );

        let c_id = world.scene.insert_object(
            Shape::Sphere,
            Some(translation(0.0, 0.0, 0.25)),
            Some(Material {
                transparency: 1.0,
                refractive_index: 2.5,
                ..Default::default()
            }),
        );

        let g_id = world.scene.insert_group(vec![a_id, b_id, c_id], None);
        world.root_group_id = g_id;
        world.build();
        println!("arena: {:#?}", world.scene.arena.len());

        let r = Ray::new(Point::new(0.0, 0.0, -4.0), Vector::new(0.0, 0.0, 1.0));
        println!("id: {a_id:?}, {b_id:?}, {c_id:?}");

        let xs = [
            Intersection::new(2.0, a_id),
            Intersection::new(2.75, b_id),
            Intersection::new(3.25, c_id),
            Intersection::new(4.75, b_id),
            Intersection::new(5.25, c_id),
            Intersection::new(6.0, a_id),
        ];

        let mut containers = vec![];
        let comps = xs
            .iter()
            .map(|i| i.compute(&world, &r, &xs, EPSILON, &mut containers))
            .collect::<Vec<ComputedIntersection>>();

        assert_eq!(comps[0].n1, 1.0);
        assert_eq!(comps[0].n2, 1.5);

        assert_eq!(comps[1].n1, 1.5);
        assert_eq!(comps[1].n2, 2.0);

        assert_eq!(comps[2].n1, 2.0);
        assert_eq!(comps[2].n2, 2.5);

        assert_eq!(comps[3].n1, 2.5);
        assert_eq!(comps[3].n2, 2.5);

        assert_eq!(comps[4].n1, 2.5);
        assert_eq!(comps[4].n2, 1.5);

        assert_eq!(comps[5].n1, 1.5);
        assert_eq!(comps[5].n2, 1.0);
    }

    // Scenario: The under point is offset below the surface
    //   Given r ← ray(point(0, 0, -5), vector(0, 0, 1))
    //     And shape ← glass_sphere() with:
    //       | transform | translation(0, 0, 1) |
    //     And i ← intersection(5, shape)
    //     And xs ← intersections(i)
    //   When comps ← prepare_computations(i, r, xs)
    //   Then comps.under_point.z > EPSILON/2
    //     And comps.point.z < comps.under_point.z
    #[test]
    fn the_under_point_is_offset_below_the_surface() {
        let mut world = World::new();

        let s_id = world
            .scene
            .insert_object(Shape::Sphere, Some(translation(0.0, 0.0, 1.0)), None);
        world.build();

        let r = Ray::new(Point::new(0.0, 0.0, -5.0), Vector::new(0.0, 0.0, 1.0));

        let i = Intersection::new(5.0, s_id);
        let xs = [i.clone()];

        let mut containers = vec![];
        let comps = i.compute(&world, &r, &xs, EPSILON, &mut containers);

        assert!(comps.under_point.z > EPSILON / 2.0);
        assert!(comps.point.z < comps.under_point.z)
    }

    // Scenario: The Schlick approximation under total internal reflection
    //   Given shape ← glass_sphere()
    //     And r ← ray(point(0, 0, √2/2), vector(0, 1, 0))
    //     And xs ← intersections(-√2/2:shape, √2/2:shape)
    //   When comps ← prepare_computations(xs[1], r, xs)
    //     And reflectance ← schlick(comps)
    //   Then reflectance = 1.0
    #[test]
    fn the_schlick_approximation_under_total_internal_reflection() {
        let mut world = World::new();

        let s_id = world.scene.insert_object(
            Shape::Sphere,
            None,
            Some(Material {
                transparency: 1.0,
                refractive_index: 1.5,
                ..Material::default()
            }),
        );
        world.build();

        let r = Ray::new(
            Point::new(0.0, 0.0, f32::sqrt(2.0) / 2.0),
            Vector::new(0.0, 1.0, 0.0),
        );

        let xs = [
            Intersection::new(-f32::sqrt(2.0) / 2.0, s_id),
            Intersection::new(f32::sqrt(2.0) / 2.0, s_id),
        ];

        let mut containers = vec![];
        let comps = xs[1].compute(&world, &r, &xs, EPSILON, &mut containers);
        let reflectance = comps.schlick();

        assert_eq!(reflectance, 1.0)
    }

    // Scenario: The Schlick approximation with a perpendicular viewing angle
    //   Given shape ← glass_sphere()
    //     And r ← ray(point(0, 0, 0), vector(0, 1, 0))
    //     And xs ← intersections(-1:shape, 1:shape)
    //   When comps ← prepare_computations(xs[1], r, xs)
    //     And reflectance ← schlick(comps)
    //   Then reflectance = 0.04
    use crate::utils::epsilon_eq;

    #[test]
    fn the_schlick_approximation_with_a_perpendicular_viewing_angle() {
        let mut world = World::new();

        let s_id = world.scene.insert_object(
            Shape::Sphere,
            None,
            Some(Material {
                transparency: 1.0,
                refractive_index: 1.5,
                ..Material::default()
            }),
        );
        world.build();

        let r = Ray::new(Point::new(0.0, 0.0, 0.0), Vector::new(0.0, 1.0, 0.0));

        let xs = [Intersection::new(-1.0, s_id), Intersection::new(1.0, s_id)];

        let mut containers = vec![];
        let comps = xs[1].compute(&world, &r, &xs, EPSILON, &mut containers);
        let reflectance = comps.schlick();

        assert!(epsilon_eq(reflectance, 0.04))
    }

    // Scenario: The Schlick approximation with small angle and n2 > n1
    //   Given shape ← glass_sphere()
    //     And r ← ray(point(0, 0.99, -2), vector(0, 0, 1))
    //     And xs ← intersections(1.8589:shape)
    //   When comps ← prepare_computations(xs[0], r, xs)
    //     And reflectance ← schlick(comps)
    //   Then reflectance = 0.48873
    #[test]
    fn the_schlick_approximation_with_small_angle_and_n2_gt_n1() {
        let mut world = World::new();

        let s_id = world.scene.insert_object(
            Shape::Sphere,
            None,
            Some(Material {
                transparency: 1.0,
                refractive_index: 1.5,
                ..Material::default()
            }),
        );
        world.build();

        let r = Ray::new(Point::new(0.0, 0.99, -2.0), Vector::new(0.0, 0.0, 1.0));

        let xs = [Intersection::new(1.8589, s_id)];
        let mut containers = vec![];
        let comps = xs[0].compute(&world, &r, &xs, EPSILON, &mut containers);
        let reflectance = comps.schlick();

        assert!(epsilon_eq(reflectance, 0.48873))
    }

    // Scenario Outline: Intersecting a ray with a bounding box at the origin
    //   Given box ← bounding_box(min=point(-1, -1, -1) max=point(1, 1, 1))
    //     And direction ← normalize(<direction>)
    //     And r ← ray(<origin>, direction)
    //   Then intersects(box, r) is <result>

    //   Examples:
    //     | origin            | direction        | result |
    //     | point(5, 0.5, 0)  | vector(-1, 0, 0) | true   |
    //     | point(-5, 0.5, 0) | vector(1, 0, 0)  | true   |
    //     | point(0.5, 5, 0)  | vector(0, -1, 0) | true   |
    //     | point(0.5, -5, 0) | vector(0, 1, 0)  | true   |
    //     | point(0.5, 0, 5)  | vector(0, 0, -1) | true   |
    //     | point(0.5, 0, -5) | vector(0, 0, 1)  | true   |
    //     | point(0, 0.5, 0)  | vector(0, 0, 1)  | true   |
    //     | point(-2, 0, 0)   | vector(2, 4, 6)  | false  |
    //     | point(0, -2, 0)   | vector(6, 2, 4)  | false  |
    //     | point(0, 0, -2)   | vector(4, 6, 2)  | false  |
    //     | point(2, 0, 2)    | vector(0, 0, -1) | false  |
    //     | point(0, 2, 2)    | vector(0, -1, 0) | false  |
    //     | point(2, 2, 0)    | vector(-1, 0, 0) | false  |
    #[test]
    fn intersecting_a_ray_with_a_bounding_box_at_the_origin() {
        let world = World::new();

        let bbox = BoundingBox::new(Point::new(-1.0, -1.0, -1.0), Point::new(1.0, 1.0, 1.0));

        let rays = vec![
            (Point::new(5.0, 0.5, 0.0), Vector::new(-1.0, 0.0, 0.0)),
            (Point::new(-5.0, 0.5, 0.0), Vector::new(1.0, 0.0, 0.0)),
            (Point::new(0.5, 5.0, 0.0), Vector::new(0.0, -1.0, 0.0)),
            (Point::new(0.5, -5.0, 0.0), Vector::new(0.0, 1.0, 0.0)),
            (Point::new(0.5, 0.0, 5.0), Vector::new(0.0, 0.0, -1.0)),
            (Point::new(0.5, 0.0, -5.0), Vector::new(0.0, 0.0, 1.0)),
            (Point::new(0.0, 0.5, 0.0), Vector::new(0.0, 0.0, 1.0)),
            (Point::new(-2.0, 0.0, 0.0), Vector::new(2.0, 4.0, 6.0)),
            (Point::new(0.0, -2.0, 0.0), Vector::new(6.0, 2.0, 4.0)),
            (Point::new(0.0, 0.0, -2.0), Vector::new(4.0, 6.0, 2.0)),
            (Point::new(2.0, 0.0, 2.0), Vector::new(0.0, 0.0, -1.0)),
            (Point::new(0.0, 2.0, 2.0), Vector::new(0.0, -1.0, 0.0)),
            (Point::new(2.0, 2.0, 0.0), Vector::new(-1.0, 0.0, 0.0)),
        ]
        .into_iter()
        .map(|(o, d)| Ray::new(o, d))
        .collect::<Vec<Ray>>();

        let is = rays
            .iter()
            .map(|r| world.intersect_bounding_box(&bbox, r, &mut [], 0))
            .collect::<Vec<bool>>();

        assert_eq!(
            vec![
                true, true, true, true, true, true, true, false, false, false, false, false, false
            ],
            is
        )
    }

    // Scenario Outline: Intersecting a ray with a non-cubic bounding box
    //   Given box ← bounding_box(min=point(5, -2, 0) max=point(11, 4, 7))
    //     And direction ← normalize(<direction>)
    //     And r ← ray(<origin>, direction)
    //   Then intersects(box, r) is <result>

    //   Examples:
    //     | origin           | direction        | result |
    //     | point(15, 1, 2)  | vector(-1, 0, 0) | true   |
    //     | point(-5, -1, 4) | vector(1, 0, 0)  | true   |
    //     | point(7, 6, 5)   | vector(0, -1, 0) | true   |
    //     | point(9, -5, 6)  | vector(0, 1, 0)  | true   |
    //     | point(8, 2, 12)  | vector(0, 0, -1) | true   |
    //     | point(6, 0, -5)  | vector(0, 0, 1)  | true   |
    //     | point(8, 1, 3.5) | vector(0, 0, 1)  | true   |
    //     | point(9, -1, -8) | vector(2, 4, 6)  | false  |
    //     | point(8, 3, -4)  | vector(6, 2, 4)  | false  |
    //     | point(9, -1, -2) | vector(4, 6, 2)  | false  |
    //     | point(4, 0, 9)   | vector(0, 0, -1) | false  |
    //     | point(8, 6, -1)  | vector(0, -1, 0) | false  |
    //     | point(12, 5, 4)  | vector(-1, 0, 0) | false  |
    #[test]
    fn intersecting_a_ray_with_a_non_cubic_bounding_box() {
        let world = World::new();

        let bbox = BoundingBox::new(Point::new(5.0, -2.0, 0.0), Point::new(11.0, 4.0, 7.0));

        let rays = vec![
            (Point::new(15.0, 1.0, 2.0), Vector::new(-1.0, 0.0, 0.0)),
            (Point::new(-5.0, -1.0, 4.0), Vector::new(1.0, 0.0, 0.0)),
            (Point::new(7.0, 6.0, 5.0), Vector::new(0.0, -1.0, 0.0)),
            (Point::new(9.0, -5.0, 6.0), Vector::new(0.0, 1.0, 0.0)),
            (Point::new(8.0, 2.0, 12.0), Vector::new(0.0, 0.0, -1.0)),
            (Point::new(6.0, 0.0, -5.0), Vector::new(0.0, 0.0, 1.0)),
            (Point::new(8.0, 1.0, 3.5), Vector::new(0.0, 0.0, 1.0)),
            (Point::new(9.0, -1.0, -8.0), Vector::new(2.0, 4.0, 6.0)),
            (Point::new(8.0, 3.0, -4.0), Vector::new(6.0, 2.0, 4.0)),
            (Point::new(9.0, -1.0, -2.0), Vector::new(4.0, 6.0, 2.0)),
            (Point::new(4.0, 0.0, 9.0), Vector::new(0.0, 0.0, -1.0)),
            (Point::new(8.0, 6.0, -1.0), Vector::new(0.0, -1.0, 0.0)),
            (Point::new(12.0, 5.0, 4.0), Vector::new(-1.0, 0.0, 0.0)),
        ]
        .into_iter()
        .map(|(o, d)| Ray::new(o, d))
        .collect::<Vec<Ray>>();

        let is = rays
            .iter()
            .map(|r| world.intersect_bounding_box(&bbox, r, &mut [], 0))
            .collect::<Vec<bool>>();

        assert_eq!(
            vec![
                true, true, true, true, true, true, true, false, false, false, false, false, false
            ],
            is
        )
    }

    // Scenario: Intersecting ray+group doesn't test children if box is missed
    //   Given child ← test_shape()
    //     And shape ← group()
    //     And add_child(shape, child)
    //     And r ← ray(point(0, 0, -5), vector(0, 1, 0))
    //   When xs ← intersect(shape, r)
    //   Then child.saved_ray is unset
    #[test]
    fn intersecting_ray_group_doesnt_test_children_if_box_is_missed() {
        let mut world = World::new();

        let s_id = world.scene.insert_object(Shape::Sphere, None, None);

        let g_id = world.scene.insert_group(vec![s_id], None);

        // let mut bvh = BoundingVolume::BoundingVolumeNode {
        //     children: vec![],
        //     bounds: BoundingBox::default(),
        // };

        // scene.apply_transforms_2(
        //     &mut world.render_primitives,
        //     g_id,
        //     &None,
        //     &mut BoundingBox::default(),
        //     &mut bvh,
        // );

        // let scene_objects = scene.build();
        // //println!("Scene objects: {:#?}", scene_objects);
        // world.groups = vec![scene_objects];

        // let mut nodes = vec![];
        // let linear = bvh.flatten(&mut nodes);

        world.root_group_id = g_id;
        world.build();

        let mut intersections = vec![];

        world.intersect_bvh(
            &Ray::new(Point::new(0.0, 0.0, -5.0), Vector::new(0.0, 1.0, 0.0)),
            &mut intersections,
        );

        assert_eq!(0, intersections.len())
    }

    // Scenario: Intersecting ray+group tests children if box is hit
    //   Given child ← test_shape()
    //     And shape ← group()
    //     And add_child(shape, child)
    //     And r ← ray(point(0, 0, -5), vector(0, 0, 1))
    //   When xs ← intersect(shape, r)
    //   Then child.saved_ray is set
    #[test]
    fn intersecting_ray_group_tests_children_if_box_is_hit() {
        let mut world = World::new();

        let s_id = world.scene.insert_object(Shape::Sphere, None, None);

        let g_id = world.scene.insert_group(vec![s_id], None);

        world.root_group_id = g_id;
        world.build();

        let mut intersections = vec![];

        world.intersect_bvh(
            &Ray::new(Point::new(0.0, 0.0, -5.0), Vector::new(0.0, 0.0, 1.0)),
            &mut intersections,
        );

        assert_eq!(2, intersections.len())
    }
}
