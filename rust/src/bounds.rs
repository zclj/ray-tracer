use crate::vector::Point;
use std::f32::{INFINITY, NEG_INFINITY};

#[derive(PartialEq, Debug)]
pub struct BoundingBox {
    pub min: Point,
    pub max: Point,
}

impl Default for BoundingBox {
    fn default() -> Self {
        BoundingBox::new(
            Point::new(INFINITY, INFINITY, INFINITY),
            Point::new(NEG_INFINITY, NEG_INFINITY, NEG_INFINITY),
        )
    }
}

impl BoundingBox {
    pub fn new(min: Point, max: Point) -> Self {
        BoundingBox { min, max }
    }

    pub fn add_point(&mut self, p: &Point) {
        if p.x < self.min.x {
            self.min.x = p.x;
        };
        if p.y < self.min.y {
            self.min.y = p.y;
        };
        if p.z < self.min.z {
            self.min.z = p.z;
        };

        if p.x > self.max.x {
            self.max.x = p.x;
        };
        if p.y > self.max.y {
            self.max.y = p.y;
        };
        if p.z > self.max.z {
            self.max.z = p.z;
        };
    }

    pub fn merge(&mut self, b: &BoundingBox) {
        self.add_point(&b.min);
        self.add_point(&b.max);
    }
}

#[cfg(test)]
mod test {
    use crate::bounds::BoundingBox;
    use crate::vector::Point;
    use std::f32::{INFINITY, NEG_INFINITY};

    // Scenario: Creating an empty bounding box
    //   Given box ← bounding_box(empty)
    //   Then box.min = point(infinity, infinity, infinity)
    //     And box.max = point(-infinity, -infinity, -infinity)
    #[test]
    fn creating_an_empty_bounding_box() {
        let bbox = BoundingBox::default();

        assert_eq!(INFINITY, INFINITY);
        assert_eq!(bbox.min.x, INFINITY);
        assert_eq!(bbox.min.y, INFINITY);
        assert_eq!(bbox.min.z, INFINITY);
        assert_eq!(bbox.max.x, NEG_INFINITY);
        assert_eq!(bbox.max.y, NEG_INFINITY);
        assert_eq!(bbox.max.z, NEG_INFINITY);
    }

    // Scenario: Creating a bounding box with volume
    //   Given box ← bounding_box(min=point(-1, -2, -3) max=point(3, 2, 1))
    //   Then box.min = point(-1, -2, -3)
    //     And box.max = point(3, 2, 1)
    #[test]
    fn creating_a_bounding_box_with_volume() {
        let bbox = BoundingBox::new(Point::new(-1.0, -2.0, -3.0), Point::new(3.0, 2.0, 1.0));

        assert_eq!(bbox.min, Point::new(-1.0, -2.0, -3.0));
        assert_eq!(bbox.max, Point::new(3.0, 2.0, 1.0))
    }

    // Scenario: Adding points to an empty bounding box
    //   Given box ← bounding_box(empty)
    //     And p1 ← point(-5, 2, 0)
    //     And p2 ← point(7, 0, -3)
    //   When p1 is added to box
    //     And p2 is added to box
    //   Then box.min = point(-5, 0, -3)
    //     And box.max = point(7, 2, 0)
    #[test]
    fn adding_points_to_an_empty_bounding_box() {
        let mut bbox = BoundingBox::default();

        let p1 = Point::new(-5.0, 2.0, 0.0);
        let p2 = Point::new(7.0, 0.0, -3.0);

        bbox.add_point(&p1);
        bbox.add_point(&p2);

        assert_eq!(bbox.min, Point::new(-5.0, 0.0, -3.0));
        assert_eq!(bbox.max, Point::new(7.0, 2.0, 0.0));
    }

    // Scenario: Adding one bounding box to another
    //   Given box1 ← bounding_box(min=point(-5, -2, 0) max=point(7, 4, 4))
    //     And box2 ← bounding_box(min=point(8, -7, -2) max=point(14, 2, 8))
    //   When box2 is added to box1
    //   Then box1.min = point(-5, -7, -2)
    //     And box1.max = point(14, 4, 8)
    #[test]
    fn adding_one_bounding_box_to_another() {
        let mut bbox_1 = BoundingBox::new(Point::new(-5.0, -2.0, 0.0), Point::new(7.0, 4.0, 4.0));
        let bbox_2 = BoundingBox::new(Point::new(8.0, -7.0, -2.0), Point::new(14.0, 2.0, 8.0));

        bbox_1.merge(&bbox_2);

        assert_eq!(bbox_1.min, Point::new(-5.0, -7.0, -2.0));
        assert_eq!(bbox_1.max, Point::new(14.0, 4.0, 8.0));
    }
}
