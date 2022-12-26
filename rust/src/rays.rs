use crate::vector::{Point, Vector};

pub struct Ray {
    pub origin: Point,
    pub direction: Vector,
}

impl Ray {
    #[must_use]
    pub fn new(origin: Point, direction: Vector) -> Self {
        Ray { origin, direction }
    }

    fn position(&self, t: f32) -> Point {
        &self.origin + &(&self.direction * t)
    }
}

#[cfg(test)]
mod test {
    use super::*;
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
}
