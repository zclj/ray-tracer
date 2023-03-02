use crate::rays::Ray;
use crate::shape::{Shape, ShapeKind};
use crate::vector::{Point, Vector};
use crate::world::World;

#[derive(Debug, PartialEq, Clone)]
pub struct Intersection {
    pub t: f32,
    pub object: u32,
    pub kind: ShapeKind,
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
    pub kind: ShapeKind,
}

impl Intersection {
    #[must_use]
    pub fn new(t: f32, shape_id: u32, shape_kind: ShapeKind) -> Self {
        Intersection {
            t,
            object: shape_id,
            kind: shape_kind,
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
        containers: &mut Vec<(u32, ShapeKind)>,
    ) -> ComputedIntersection {
        let mut normalv = world
            .get_shape(self.object, &self.kind)
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
                        let (obj, k) = containers.last().unwrap();
                        n1 = world.get_shape(*obj, k).material().refractive_index;
                    }
                }

                if containers.contains(&(i.object, i.kind.clone())) {
                    containers.remove(
                        containers
                            .iter()
                            .position(|(x, k)| *x == i.object && *k == i.kind)
                            .unwrap(),
                    );
                } else {
                    containers.push((i.object, i.kind.clone()));
                }

                if i == self {
                    if containers.is_empty() {
                        n2 = 1.0;
                    } else {
                        let (obj, k) = containers.last().unwrap();
                        n2 = world.get_shape(*obj, k).material().refractive_index;
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
            kind: self.kind.clone(),
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

pub fn intersect(shape: &Shape, r: &Ray, intersections: &mut Vec<Intersection>) {
    // the ray transformed in local space
    let obj_ray = r.transform(shape.transform_inverse());

    shape.intersect(&obj_ray, intersections);
}

/// # Panics
///
/// Will panic if intersection t value is NaN
#[inline]
#[must_use]
pub fn hit(xs: &[Intersection]) -> Option<&Intersection> {
    // intersections are already sorted
    xs.iter().find(|x| x.t >= 0.0)
}

#[cfg(test)]
mod test {
    use super::*;
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
        let i = Intersection::new(3.5, s_id, ShapeKind::Sphere);

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
        let i1 = Intersection::new(1.0, s1_id, ShapeKind::Sphere);
        let i2 = Intersection::new(2.0, s2_id, ShapeKind::Sphere);

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
        let mut world = World::new();
        let s_id = world.push_sphere(None, None);

        let mut xs = vec![];
        intersect(world.get_shape(s_id, &ShapeKind::Sphere), &r, &mut xs);

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
        let mut world = World::new();
        let s_id = world.push_sphere(None, None);

        let mut xs = vec![];
        intersect(world.get_shape(s_id, &ShapeKind::Sphere), &r, &mut xs);

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
        let mut world = World::new();
        let s_id = world.push_sphere(None, None);

        let mut xs = vec![];
        intersect(world.get_shape(s_id, &ShapeKind::Sphere), &r, &mut xs);

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
        let mut world = World::new();
        let s_id = world.push_sphere(None, None);

        let mut xs = vec![];
        intersect(world.get_shape(s_id, &ShapeKind::Sphere), &r, &mut xs);

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
        let mut world = World::new();
        let s_id = world.push_sphere(None, None);

        let mut xs = vec![];
        intersect(world.get_shape(s_id, &ShapeKind::Sphere), &r, &mut xs);

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
        let mut world = World::new();
        let s_id = world.push_sphere(None, None);
        let mut xs = vec![];
        intersect(world.get_shape(s_id, &ShapeKind::Sphere), &r, &mut xs);

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
    #[test]
    fn the_hit_when_all_intersections_have_positive_t() {
        let mut world = World::new();
        let s_id = world.push_sphere(None, None);
        let i1 = Intersection::new(1.0, s_id, ShapeKind::Sphere);
        let i2 = Intersection::new(2.0, s_id, ShapeKind::Sphere);

        let mut is = [i1, i2];
        let i = hit(&mut is);

        assert_eq!(i, Some(&Intersection::new(1.0, s_id, ShapeKind::Sphere)));
    }

    // Scenario: The hit, when some intersections have negative t
    //   Given s ← sphere()
    //     And i1 ← intersection(-1, s)
    //     And i2 ← intersection(1, s)
    //     And xs ← intersections(i2, i1)
    //   When i ← hit(xs)
    //   Then i = i2
    #[test]
    fn the_hit_when_some_intersections_have_negative_t() {
        let mut world = World::new();
        let s_id = world.push_sphere(None, None);
        let i1 = Intersection::new(-1.0, s_id, ShapeKind::Sphere);
        let i2 = Intersection::new(1.0, s_id, ShapeKind::Sphere);

        let mut is = [i2, i1];
        let i = hit(&mut is);

        assert_eq!(i, Some(&Intersection::new(1.0, s_id, ShapeKind::Sphere)));
    }

    // Scenario: The hit, when all intersections have negative t
    //   Given s ← sphere()
    //     And i1 ← intersection(-2, s)
    //     And i2 ← intersection(-1, s)
    //     And xs ← intersections(i2, i1)
    //   When i ← hit(xs)
    //   Then i is nothing
    #[test]
    fn the_hit_when_all_intersections_have_negative_t() {
        let mut world = World::new();
        let s_id = world.push_sphere(None, None);
        let i1 = Intersection::new(-2.0, s_id, ShapeKind::Sphere);
        let i2 = Intersection::new(-1.0, s_id, ShapeKind::Sphere);

        let mut is = [i2, i1];
        let i = hit(&mut is);

        assert_eq!(i, None);
    }

    // Scenario: The hit is always the lowest nonnegative intersection
    //   Given s ← sphere()
    //   And i1 ← intersection(5, s)
    //   And i2 ← intersection(7, s)
    //   And i3 ← intersection(-3, s)
    //   And i4 ← intersection(2, s)
    //   And xs ← intersections(i1, i2, i3, i4)
    // When i ← hit(xs)
    // Then i = i4
    #[test]
    fn the_hit_is_always_the_lowest_nonnegative_intersection() {
        let mut world = World::new();
        let s_id = world.push_sphere(None, None);
        let i1 = Intersection::new(5.0, s_id, ShapeKind::Sphere);
        let i2 = Intersection::new(7.0, s_id, ShapeKind::Sphere);
        let i3 = Intersection::new(-3.0, s_id, ShapeKind::Sphere);
        let i4 = Intersection::new(2.0, s_id, ShapeKind::Sphere);

        let mut is = [i1, i2, i3, i4];
        // to not sort redundantly, the sort is outside of the hit fn
        sort_by_t(&mut is);
        let i = hit(&mut is);

        assert_eq!(i, Some(&Intersection::new(2.0, s_id, ShapeKind::Sphere)));
    }

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
        let mut world = World::new();
        let s_id = world.push_sphere(Some(scaling(2.0, 2.0, 2.0)), None);
        let mut xs = vec![];
        intersect(world.get_shape(s_id, &ShapeKind::Sphere), &r, &mut xs);

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
        let mut world = World::new();
        let s_id = world.push_sphere(Some(translation(5.0, 0.0, 0.0)), None);
        let mut xs = vec![];
        intersect(world.get_shape(s_id, &ShapeKind::Sphere), &r, &mut xs);

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
        let s_id = world.push_sphere(None, None);

        let i = Intersection::new(4.0, s_id, ShapeKind::Sphere);
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
        let s_id = world.push_sphere(Some(translation(0.0, 0.0, 1.0)), None);

        let i = Intersection::new(5.0, s_id, ShapeKind::Sphere);
        let mut containers = vec![];
        let comps = i.compute(&world, &r, &[i.clone()], EPSILON, &mut containers);

        assert_eq!(comps.over_point.z < -EPSILON / 2.0, true);
        assert_eq!(comps.point.z > comps.over_point.z, true);
    }

    // Scenario: Intersect with a ray parallel to the plane
    //   Given p ← plane()
    //     And r ← ray(point(0, 10, 0), vector(0, 0, 1))
    //   When xs ← local_intersect(p, r)
    //   Then xs is empty
    #[test]
    fn intersect_with_a_ray_parallel_to_the_plane() {
        let mut world = World::new();
        let p_id = world.push_plane(None, None);

        let p = world.get_shape(p_id, &ShapeKind::Plane);
        let r = Ray::new(Point::new(0.0, 10.0, 0.0), Vector::new(0.0, 0.0, 1.0));

        let mut xs = vec![];
        p.intersect(&r, &mut xs);

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
        let p_id = world.push_plane(None, None);

        let p = world.get_shape(p_id, &ShapeKind::Plane);
        let r = Ray::new(Point::new(0.0, 0.0, 0.0), Vector::new(0.0, 0.0, 1.0));

        let mut xs = vec![];
        p.intersect(&r, &mut xs);

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
        let p_id = world.push_plane(None, None);

        let p = world.get_shape(p_id, &ShapeKind::Plane);
        let r = Ray::new(Point::new(0.0, 1.0, 0.0), Vector::new(0.0, -1.0, 0.0));

        let mut xs = vec![];
        p.intersect(&r, &mut xs);

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
        let p_id = world.push_plane(None, None);

        let p = world.get_shape(p_id, &ShapeKind::Plane);
        let r = Ray::new(Point::new(0.0, -1.0, 0.0), Vector::new(0.0, 1.0, 0.0));

        let mut xs = vec![];
        p.intersect(&r, &mut xs);

        assert_eq!(xs.len(), 1);
        assert_eq!(xs[0].t, 1.0);
        assert_eq!(xs[0].object, p_id)
    }

    // Scenario: Precomputing the reflection vector
    //   Given shape ← plane()
    //     And r ← ray(point(0, 1, -1), vector(0, -√2/2, √2/2))
    //     And i ← intersection(√2, shape)
    //   When comps ← prepare_computations(i, r)
    //   Then comps.reflectv = vector(0, √2/2, √2/2)
    #[test]
    fn precomputing_the_reflection_vector() {
        let mut world = World::new();
        let s_id = world.push_plane(None, None);

        let r = Ray::new(
            Point::new(0.0, 1.0, -1.0),
            Vector::new(0.0, -f32::sqrt(2.0) / 2.0, f32::sqrt(2.0) / 2.0),
        );
        let i = Intersection::new(f32::sqrt(2.0), s_id, ShapeKind::Plane);
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

        let a_id = world.push_sphere(
            Some(scaling(2.0, 2.0, 2.0)),
            Some(Material {
                transparency: 1.0,
                refractive_index: 1.5,
                ..Default::default()
            }),
        );

        let b_id = world.push_sphere(
            Some(translation(0.0, 0.0, -0.25)),
            Some(Material {
                transparency: 1.0,
                refractive_index: 2.0,
                ..Default::default()
            }),
        );

        let c_id = world.push_sphere(
            Some(translation(0.0, 0.0, 0.25)),
            Some(Material {
                transparency: 1.0,
                refractive_index: 2.5,
                ..Default::default()
            }),
        );

        let r = Ray::new(Point::new(0.0, 0.0, -4.0), Vector::new(0.0, 0.0, 1.0));

        let xs = [
            Intersection::new(2.0, a_id, ShapeKind::Sphere),
            Intersection::new(2.75, b_id, ShapeKind::Sphere),
            Intersection::new(3.25, c_id, ShapeKind::Sphere),
            Intersection::new(4.75, b_id, ShapeKind::Sphere),
            Intersection::new(5.25, c_id, ShapeKind::Sphere),
            Intersection::new(6.0, a_id, ShapeKind::Sphere),
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

        let s_id = world.push_sphere(Some(translation(0.0, 0.0, 1.0)), None);

        let r = Ray::new(Point::new(0.0, 0.0, -5.0), Vector::new(0.0, 0.0, 1.0));

        let i = Intersection::new(5.0, s_id, ShapeKind::Sphere);
        let xs = [i.clone()];

        let mut containers = vec![];
        let comps = i.compute(&world, &r, &xs, EPSILON, &mut containers);

        assert_eq!(comps.under_point.z > EPSILON / 2.0, true);
        assert_eq!(comps.point.z < comps.under_point.z, true)
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

        let s_id = world.push_sphere(
            None,
            Some(Material {
                transparency: 1.0,
                refractive_index: 1.5,
                ..Material::default()
            }),
        );

        let r = Ray::new(
            Point::new(0.0, 0.0, f32::sqrt(2.0) / 2.0),
            Vector::new(0.0, 1.0, 0.0),
        );

        let xs = [
            Intersection::new(-f32::sqrt(2.0) / 2.0, s_id, ShapeKind::Sphere),
            Intersection::new(f32::sqrt(2.0) / 2.0, s_id, ShapeKind::Sphere),
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

        let s_id = world.push_sphere(
            None,
            Some(Material {
                transparency: 1.0,
                refractive_index: 1.5,
                ..Material::default()
            }),
        );

        let r = Ray::new(Point::new(0.0, 0.0, 0.0), Vector::new(0.0, 1.0, 0.0));

        let xs = [
            Intersection::new(-1.0, s_id, ShapeKind::Sphere),
            Intersection::new(1.0, s_id, ShapeKind::Sphere),
        ];

        let mut containers = vec![];
        let comps = xs[1].compute(&world, &r, &xs, EPSILON, &mut containers);
        let reflectance = comps.schlick();

        assert_eq!(epsilon_eq(reflectance, 0.04), true)
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

        let s_id = world.push_sphere(
            None,
            Some(Material {
                transparency: 1.0,
                refractive_index: 1.5,
                ..Material::default()
            }),
        );

        let r = Ray::new(Point::new(0.0, 0.99, -2.0), Vector::new(0.0, 0.0, 1.0));

        let xs = [Intersection::new(1.8589, s_id, ShapeKind::Sphere)];
        let mut containers = vec![];
        let comps = xs[0].compute(&world, &r, &xs, EPSILON, &mut containers);
        let reflectance = comps.schlick();

        assert_eq!(epsilon_eq(reflectance, 0.48873), true)
    }
}
