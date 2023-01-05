use crate::matrices::M4x4;

pub struct Camera {
    hsize: u16,
    vsize: u16,
    field_of_view: f32,
    transform: M4x4,
    half_width: f32,
    half_height: f32,
    pixel_size: f32,
}

impl Camera {
    #[allow(clippy::similar_names)]
    #[must_use]
    pub fn new(hsize: u16, vsize: u16, field_of_view: f32) -> Self {
        let half_view = (field_of_view / 2.0).tan();
        let aspect: f32 = f32::from(hsize) / f32::from(vsize);

        let (half_width, half_height) = if aspect >= 1.0 {
            (half_view, half_view / aspect)
        } else {
            (half_view * aspect, half_view)
        };

        Camera {
            hsize,
            vsize,
            field_of_view,
            transform: M4x4::IDENTITY,
            half_width,
            half_height,
            pixel_size: (half_width * 2.0) / (f32::from(hsize)),
        }
    }

    //fn pixel_size(&self) -> f32 {
    // let half_view = (self.field_of_view / 2.0).tan();
    // let aspect = self.hsize / self.vsize;

    // if aspect >= 1.0 {

    // }
    //}
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
    // Scenario: The pixel size for a horizontal canvas
    //   Given c ← camera(200, 125, π/2)
    //   Then c.pixel_size = 0.01
    #[test]
    fn the_pixel_size_for_a_horizontal_canvas() {
        let c = Camera::new(200, 125, PI / 2.0);

        assert_eq!(c.pixel_size, 0.01)
    }

    // Scenario: The pixel size for a vertical canvas
    //   Given c ← camera(125, 200, π/2)
    //   Then c.pixel_size = 0.01
    #[test]
    fn the_pixel_size_for_a_vertical_canvas() {
        let c = Camera::new(125, 200, PI / 2.0);

        assert_eq!(c.pixel_size, 0.01)
    }
}
