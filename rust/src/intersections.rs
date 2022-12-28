use crate::rays::Ray;
use crate::shape::Shape;
use crate::vector::{Point};

pub struct Intersection {
    pub t: f32,
    pub object: u32,
}

impl Intersection {
    fn new(t: f32, shape_id: u32) -> Self {
        Intersection {
            t,
            object: shape_id,
        }
    }
}

fn intersect(shape: &Shape, r: &Ray) -> Vec<Intersection> {
    let sphere_to_ray = &r.origin - &Point::new(0.0, 0.0, 0.0);

    let a = r.direction.dot(&r.direction);
    let b = 2.0 * r.direction.dot(&sphere_to_ray);
    let c = sphere_to_ray.dot(&sphere_to_ray) - 1.0;

    let discriminant = b.powf(2.0) - (4.0 * a * c);

    if discriminant < 0.0 {
        return Vec::new();
    }

    let t1 = (-b - f32::sqrt(discriminant)) / (2.0 * a);
    let t2 = (-b + f32::sqrt(discriminant)) / (2.0 * a);

    let s_id = match shape {
        Shape::Sphere { id } => *id,
    };

    vec![
        Intersection {
            t: t1,
            object: s_id,
        },
        Intersection {
            t: t2,
            object: s_id,
        },
    ]
}

#[cfg(test)]
mod test {
    use super::*;
    use crate::shape::Shape;

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
        let mut ctx = Context::new();
        let s_id = ctx.push_sphere();

        let xs = intersect(ctx.get_shape(s_id), &r);

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
        let mut ctx = Context::new();
        let s_id = ctx.push_sphere();

        let xs = intersect(ctx.get_shape(s_id), &r);

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
        let mut ctx = Context::new();
        let s_id = ctx.push_sphere();

        let xs = intersect(ctx.get_shape(s_id), &r);

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
        let mut ctx = Context::new();
        let s_id = ctx.push_sphere();

        let xs = intersect(ctx.get_shape(s_id), &r);

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
        let mut ctx = Context::new();
        let s_id = ctx.push_sphere();

        let xs = intersect(ctx.get_shape(s_id), &r);

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
        let mut ctx = Context::new();
        let s_id = ctx.push_sphere();
        let xs = intersect(ctx.get_shape(s_id), &r);

        assert_eq!(xs.len(), 2);
        assert_eq!(xs[0].object, s_id);
        assert_eq!(xs[1].object, s_id);
    }
}
