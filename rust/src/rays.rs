use crate::matrices::M4x4;
use crate::vector::{Point, Vector};

#[derive(Debug)]
pub struct Ray {
    pub origin: Point,
    pub direction: Vector,
}

impl Ray {
    #[must_use]
    pub fn new(origin: Point, direction: Vector) -> Self {
        Ray { origin, direction }
    }

    #[must_use]
    pub fn position(&self, t: f32) -> Point {
        &self.origin + &(&self.direction * t)
    }

    #[must_use]
    pub fn transform(&self, m: &M4x4) -> Ray {
        Ray {
            origin: m * &self.origin,
            direction: m * &self.direction,
        }
    }
}

#[cfg(test)]
mod test {
    use super::*;
    use crate::transformations::{scaling, translation};
    use crate::vector::{Point, Vector};

    // Scenario: Creating and querying a ray
    //   Given origin ← point(1, 2, 3)
    //     And direction ← vector(4, 5, 6)
    //   When r ← ray(origin, direction)
    //   Then r.origin = origin
    //     And r.direction = direction
    #[test]
    fn creating_and_querying_a_ray() {
        let origin = Point::new(1.0, 2.0, 3.0);
        let direction = Vector::new(4.0, 5.0, 6.0);

        let r = Ray::new(origin, direction);

        assert_eq!(r.origin, Point::new(1.0, 2.0, 3.0));
        assert_eq!(r.direction, Vector::new(4.0, 5.0, 6.0));
    }

    // Scenario: Computing a point from a distance
    //   Given r ← ray(point(2, 3, 4), vector(1, 0, 0))
    //   Then position(r, 0) = point(2, 3, 4)
    //     And position(r, 1) = point(3, 3, 4)
    //     And position(r, -1) = point(1, 3, 4)
    //     And position(r, 2.5) = point(4.5, 3, 4)
    #[test]
    fn computing_a_point_from_a_distance() {
        let r = Ray::new(Point::new(2.0, 3.0, 4.0), Vector::new(1.0, 0.0, 0.0));

        assert_eq!(r.position(0.0), Point::new(2.0, 3.0, 4.0));
        assert_eq!(r.position(1.0), Point::new(3.0, 3.0, 4.0));
        assert_eq!(r.position(-1.0), Point::new(1.0, 3.0, 4.0));
        assert_eq!(r.position(2.5), Point::new(4.5, 3.0, 4.0));
    }

    // Scenario: Translating a ray
    //   Given r ← ray(point(1, 2, 3), vector(0, 1, 0))
    //     And m ← translation(3, 4, 5)
    //   When r2 ← transform(r, m)
    //   Then r2.origin = point(4, 6, 8)
    //     And r2.direction = vector(0, 1, 0)
    #[test]
    fn translating_a_ray() {
        let r = Ray::new(Point::new(1.0, 2.0, 3.0), Vector::new(0.0, 1.0, 0.0));
        let m = translation(3.0, 4.0, 5.0);
        let r2 = r.transform(&m);

        assert_eq!(r2.origin, Point::new(4.0, 6.0, 8.0));
        assert_eq!(r2.direction, Vector::new(0.0, 1.0, 0.0));
    }

    // Scenario: Scaling a ray
    //   Given r ← ray(point(1, 2, 3), vector(0, 1, 0))
    //     And m ← scaling(2, 3, 4)
    //   When r2 ← transform(r, m)
    //   Then r2.origin = point(2, 6, 12)
    //     And r2.direction = vector(0, 3, 0)
    #[test]
    fn scaling_a_ray() {
        let r = Ray::new(Point::new(1.0, 2.0, 3.0), Vector::new(0.0, 1.0, 0.0));
        let m = scaling(2.0, 3.0, 4.0);
        let r2 = r.transform(&m);

        assert_eq!(r2.origin, Point::new(2.0, 6.0, 12.0));
        assert_eq!(r2.direction, Vector::new(0.0, 3.0, 0.0));
    }
}
