use crate::matrices::M4x4;

pub struct Camera {
    hsize: u32,
    vsize: u32,
    field_of_view: f32,
    transform: M4x4,
}

impl Camera {
    #[allow(clippy::similar_names)]
    #[must_use]
    pub fn new(hsize: u32, vsize: u32, field_of_view: f32) -> Self {
        Camera {
            hsize,
            vsize,
            field_of_view,
            transform: M4x4::IDENTITY,
        }
    }
}

#[cfg(test)]
mod test {
    use super::*;
    use crate::matrices::M4x4;
    use std::f32::consts::PI;

    // Scenario: Constructing a camera
    //   Given hsize ← 160
    //     And vsize ← 120
    //     And field_of_view ← π/2
    //   When c ← camera(hsize, vsize, field_of_view)
    //   Then c.hsize = 160
    //     And c.vsize = 120
    //     And c.field_of_view = π/2
    //     And c.transform = identity_matrix
    #[test]
    fn constructing_a_camera() {
        let hsize = 160;
        let vsize = 120;
        let fov = PI / 2.0;

        let c = Camera::new(hsize, vsize, fov);

        assert_eq!(c.hsize, 160);
        assert_eq!(c.vsize, 120);
        assert_eq!(c.field_of_view, PI / 2.0);
        assert_eq!(c.transform, M4x4::IDENTITY);
    }
}
