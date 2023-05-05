use crate::vector::Point;
use std::f32::{INFINITY, NEG_INFINITY};

struct BoundingBox {
    min: Point,
    max: Point,
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
}
