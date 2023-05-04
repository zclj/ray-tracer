use crate::vector::Point;
use std::f32::{INFINITY, NEG_INFINITY};

struct BoundingBox {
    min: Point,
    max: Point,
}

impl BoundingBox {
    pub fn new() -> Self {
        BoundingBox {
            min: Point::new(INFINITY, INFINITY, INFINITY),
            max: Point::new(NEG_INFINITY, NEG_INFINITY, NEG_INFINITY),
        }
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
        let bbox = BoundingBox::new();

        assert_eq!(INFINITY, INFINITY);
        assert_eq!(bbox.min.x, INFINITY);
        assert_eq!(bbox.min.y, INFINITY);
        assert_eq!(bbox.min.z, INFINITY);
        assert_eq!(bbox.max.x, NEG_INFINITY);
        assert_eq!(bbox.max.y, NEG_INFINITY);
        assert_eq!(bbox.max.z, NEG_INFINITY);
    }
}
