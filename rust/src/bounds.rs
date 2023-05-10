use crate::matrices::M4x4;
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

    pub fn contains_point(&self, p: &Point) -> bool {
        self.min.x <= p.x
            && p.x <= self.max.x
            && self.min.y <= p.y
            && p.y <= self.max.y
            && self.min.z <= p.z
            && p.z <= self.max.z
    }

    pub fn contains_box(&self, b: &BoundingBox) -> bool {
        self.contains_point(&b.min) && self.contains_point(&b.max)
    }

    pub fn transform(&self, transform: &M4x4) -> Self {
        let mut transformed_box = BoundingBox::default();

        // transform the points at all eight corners of the cube
        transformed_box.add_point(&(transform * &self.min));
        transformed_box.add_point(&(transform * &Point::new(self.min.x, self.min.y, self.max.z)));

        transformed_box.add_point(&(transform * &Point::new(self.min.x, self.max.y, self.min.z)));
        transformed_box.add_point(&(transform * &Point::new(self.min.x, self.max.y, self.max.z)));
        transformed_box.add_point(&(transform * &Point::new(self.max.x, self.min.y, self.min.z)));
        transformed_box.add_point(&(transform * &Point::new(self.max.x, self.min.y, self.max.z)));
        transformed_box.add_point(&(transform * &Point::new(self.max.x, self.max.y, self.min.z)));
        transformed_box.add_point(&(transform * &self.max));

        transformed_box
    }
}

#[cfg(test)]
mod test {
    use crate::bounds::BoundingBox;
    use crate::transformations::{rotation_x, rotation_y};
    use crate::vector::Point;
    use std::f32::consts::PI;
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

    // Scenario Outline: Checking to see if a box contains a given point
    //   Given box ← bounding_box(min=point(5, -2, 0) max=point(11, 4, 7))
    //     And p ← <point>
    //   Then box_contains_point(box, p) is <result>

    //   Examples:
    //     | point           | result |
    //     | point(5, -2, 0) | true   |
    //     | point(11, 4, 7) | true   |
    //     | point(8, 1, 3)  | true   |
    //     | point(3, 0, 3)  | false  |
    //     | point(8, -4, 3) | false  |
    //     | point(8, 1, -1) | false  |
    //     | point(13, 1, 3) | false  |
    //     | point(8, 5, 3)  | false  |
    //     | point(8, 1, 8)  | false  |
    #[test]
    fn checking_to_see_if_a_box_contains_a_given_point() {
        let bbox = BoundingBox::new(Point::new(5.0, -2.0, 0.0), Point::new(11.0, 4.0, 7.0));

        let points = vec![
            Point::new(5.0, -2.0, 0.0),
            Point::new(11.0, 4.0, 7.0),
            Point::new(8.0, 1.0, 3.0),
            Point::new(3.0, 0.0, 3.0),
            Point::new(8.0, -4.0, 3.0),
            Point::new(8.0, 1.0, -1.0),
            Point::new(13.0, 1.0, 3.0),
            Point::new(8.0, 5.0, 3.0),
            Point::new(8.0, 1.0, 8.0),
        ];

        let results: Vec<bool> = points.iter().map(|p| bbox.contains_point(p)).collect();
        let expected = vec![true, true, true, false, false, false, false, false, false];

        assert_eq!(results, expected)
    }

    // Scenario Outline: Checking to see if a box contains a given box
    //   Given box ← bounding_box(min=point(5, -2, 0) max=point(11, 4, 7))
    //     And box2 ← bounding_box(min=<min> max=<max>)
    //   Then box_contains_box(box, box2) is <result>

    //   Examples:
    //     | min              | max             | result |
    //     | point(5, -2, 0)  | point(11, 4, 7) | true   |
    //     | point(6, -1, 1)  | point(10, 3, 6) | true   |
    //     | point(4, -3, -1) | point(10, 3, 6) | false  |
    //     | point(6, -1, 1)  | point(12, 5, 8) | false  |
    #[test]
    fn checking_to_see_if_a_box_contains_a_given_box() {
        let bbox = BoundingBox::new(Point::new(5.0, -2.0, 0.0), Point::new(11.0, 4.0, 7.0));

        let boxes = vec![
            BoundingBox::new(Point::new(5.0, -2.0, 0.0), Point::new(11.0, 4.0, 7.0)),
            BoundingBox::new(Point::new(6.0, -1.0, 1.0), Point::new(10.0, 3.0, 6.0)),
            BoundingBox::new(Point::new(4.0, -3.0, -1.0), Point::new(10.0, 3.0, 6.0)),
            BoundingBox::new(Point::new(6.0, -1.0, 1.0), Point::new(12.0, 5.0, 8.0)),
        ];

        let results: Vec<bool> = boxes.iter().map(|b| bbox.contains_box(b)).collect();

        let expected = vec![true, true, false, false];

        assert_eq!(results, expected)
    }

    // Scenario: Transforming a bounding box
    //   Given box ← bounding_box(min=point(-1, -1, -1) max=point(1, 1, 1))
    //     And matrix ← rotation_x(π / 4) * rotation_y(π / 4)
    //   When box2 ← transform(box, matrix)
    //   Then box2.min = point(-1.4142, -1.7071, -1.7071)
    //     And box2.max = point(1.4142, 1.7071, 1.7071)
    #[test]
    fn transforming_a_bounding_box() {
        let bbox = BoundingBox::new(Point::new(-1.0, -1.0, -1.0), Point::new(1.0, 1.0, 1.0));

        let bbox_2 = bbox.transform(&rotation_x(PI / 4.0) * &rotation_y(PI / 4.0));

        assert_eq!(bbox_2.min, Point::new(-1.4142, -1.7071, -1.7071));
        assert_eq!(bbox_2.max, Point::new(1.4142, 1.7071, 1.7071))
    }
}
