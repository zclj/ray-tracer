use crate::matrices::M4x4;

#[must_use]
pub fn translation(x: f32, y: f32, z: f32) -> M4x4 {
    M4x4::from_elements(
        [1.0, 0.0, 0.0, x],
        [0.0, 1.0, 0.0, y],
        [0.0, 0.0, 1.0, z],
        [0.0, 0.0, 0.0, 1.0],
    )
}

pub fn scaling(x: f32, y: f32, z: f32) -> M4x4 {
    M4x4::from_elements(
        [x, 0.0, 0.0, 0.0],
        [0.0, y, 0.0, 0.0],
        [0.0, 0.0, z, 0.0],
        [0.0, 0.0, 0.0, 1.0],
    )
}

#[cfg(test)]
mod test {
    use crate::transformations::{scaling, translation};
    use crate::vector::{Point, Vector};

    // Scenario: Multiplying by a translation matrix
    // Given transform ← translation(5, -3, 2)
    // And p ← point(-3, 4, 5)
    // Then transform * p = point(2, 1, 7)
    #[test]
    fn multiplying_by_a_translation_matrix() {
        let transform = translation(5.0, -3.0, 2.0);
        let p = Point::new(-3.0, 4.0, 5.0);

        assert_eq!(&transform * &p, Point::new(2.0, 1.0, 7.0))
    }

    // Scenario: Multiplying by the inverse of a translation matrix
    // Given transform ← translation(5, -3, 2)
    //   And inv ← inverse(transform)
    //   And p ← point(-3, 4, 5)
    //  Then inv * p = point(-8, 7, 3)
    #[test]
    fn multiplying_by_the_inverse_of_a_translation_matrix() {
        let transform = translation(5.0, -3.0, 2.0).inverse();
        let p = Point::new(-3.0, 4.0, 5.0);

        assert_eq!(&transform * &p, Point::new(-8.0, 7.0, 3.0))
    }

    // Scenario: Translation does not affect vectors
    //   Given transform ← translation(5, -3, 2)
    //     And v ← vector(-3, 4, 5)
    //    Then transform * v = v
    #[test]
    fn translation_does_not_affect_vectors() {
        let transform = translation(5.0, -3.0, 2.0);
        let v = Vector::new(-3.0, 4.0, 5.0);

        assert_eq!(&transform * &v, Vector::new(-3.0, 4.0, 5.0))
    }

    use glam::{Mat4, Vec3};

    #[test]
    fn glam_multiplay_by_a_translation_matrix() {
        let transform = Mat4::from_translation(Vec3::new(5.0, -3.0, 2.0));
        let p = transform.transform_point3(Vec3::new(-3.0, 4.0, 5.0));

        assert_eq!(p, Vec3::new(2.0, 1.0, 7.0))
    }

    // Scenario: A scaling matrix applied to a point
    // Given transform ← scaling(2, 3, 4)
    //   And p ← point(-4, 6, 8)
    //  Then transform * p = point(-8, 18, 32)
    #[test]
    fn a_scaling_matrix_applied_to_a_point() {
        let transform = scaling(2.0, 3.0, 4.0);
        let p = Point::new(-4.0, 6.0, 8.0);

        assert_eq!(&transform * &p, Point::new(-8.0, 18.0, 32.0))
    }

    // Scenario: A scaling matrix applied to a vector
    // Given transform ← scaling(2, 3, 4)
    //   And v ← vector(-4, 6, 8)
    //  Then transform * v = vector(-8, 18, 32)
    #[test]
    fn a_scaling_matrix_applied_to_a_vector() {
        let transform = scaling(2.0, 3.0, 4.0);
        let v = Vector::new(-4.0, 6.0, 8.0);

        assert_eq!(&transform * &v, Vector::new(-8.0, 18.0, 32.0))
    }

    // Scenario: Multiplying by the inverse of a scaling matrix
    //   Given transform ← scaling(2, 3, 4)
    //     And inv ← inverse(transform)
    //     And v ← vector(-4, 6, 8)
    //    Then inv * v = vector(-2, 2, 2)
    #[test]
    fn multiplying_by_the_inverse_of_a_scaling_matrix() {
        let transform = scaling(2.0, 3.0, 4.0).inverse();
        let v = Vector::new(-4.0, 6.0, 8.0);

        assert_eq!(&transform * &v, Vector::new(-2.0, 2.0, 2.0))
    }

    // Scenario: Reflection is scaling by a negative value
    //   Given transform ← scaling(-1, 1, 1)
    //     And p ← point(2, 3, 4)
    //    Then transform * p = point(-2, 3, 4)
    #[test]
    fn reflection_is_scaling_by_a_negative_value() {
        let transform = scaling(-1.0, 1.0, 1.0);
        let p = Point::new(2.0, 3.0, 4.0);

        assert_eq!(&transform * &p, Point::new(-2.0, 3.0, 4.0))
    }
}
