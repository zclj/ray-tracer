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

    pub fn add_point(&self, p: &Point) -> BoundingBox {
        let min_x = if p.x < self.min.x { p.x } else { self.min.x };
        let min_y = if p.y < self.min.y { p.y } else { self.min.y };
        let min_z = if p.z < self.min.z { p.z } else { self.min.z };

        let max_x = if p.x > self.max.x { p.x } else { self.max.x };
        let max_y = if p.y > self.max.y { p.y } else { self.max.y };
        let max_z = if p.z > self.max.z { p.z } else { self.max.z };

        BoundingBox::new(
            Point::new(min_x, min_y, min_z),
            Point::new(max_x, max_y, max_z),
        )
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
        let bbox = BoundingBox::default();

        let p1 = Point::new(-5.0, 2.0, 0.0);
        let p2 = Point::new(7.0, 0.0, -3.0);

        let new_box_1 = bbox.add_point(&p1);
        let new_box_2 = new_box_1.add_point(&p2);

        assert_eq!(new_box_2.min, Point::new(-5.0, 0.0, -3.0));
        assert_eq!(new_box_2.max, Point::new(7.0, 2.0, 0.0));
    }
}
