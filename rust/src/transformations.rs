use crate::matrices::M4x4;

#[must_use]
pub fn translation(x: f32, y: f32, z: f32) -> M4x4 {
    M4x4([
        1.0, 0.0, 0.0, x, 0.0, 1.0, 0.0, y, 0.0, 0.0, 1.0, z, 0.0, 0.0, 0.0, 1.0,
    ])
}

#[cfg(test)]
mod test {
    use crate::transformations::translation;
    use crate::vector::Point;

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

    use glam::{Mat4, Vec3};

    #[test]
    fn glam_multiplay_by_a_translation_matrix() {
        let transform = Mat4::from_translation(Vec3::new(5.0, -3.0, 2.0));
        let p = transform.transform_point3(Vec3::new(-3.0, 4.0, 5.0));

        assert_eq!(p, Vec3::new(2.0, 1.0, 7.0))
    }
}
