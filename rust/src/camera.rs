use crate::matrices::M4x4;
use crate::rays::Ray;
use crate::vector::Point;

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

    #[must_use]
    pub fn ray_for_pixel(&self, px: u16, py: u16) -> Ray {
        // the offset from the edge of the canvas to the pixel's center
        let xoffset = (f32::from(px) + 0.5) * self.pixel_size;
        let yoffset = (f32::from(py) + 0.5) * self.pixel_size;

        // the untransformed coordinates of the pixel in world space.
        // (remember that the camera looks toward -z, so +x is to the *left*.)
        let world_x = self.half_width - xoffset;
        let world_y = self.half_height - yoffset;

        // using the camera matrix, transform the canvas point and the origin,
        // and then compute the ray's direction vector.
        // (remember that the canvas is at z=-1)
        let inverse_transform = self.transform.inverse();
        let pixel = &inverse_transform * &Point::new(world_x, world_y, -1.0);
        let origin = &inverse_transform * &Point::new(0.0, 0.0, 0.0);
        let direction = (&pixel - &origin).norm();

        Ray::new(origin, direction)
    }
}

#[cfg(test)]
mod test {
    use super::*;
    use crate::matrices::M4x4;
    use crate::transformations::{rotation_y, translation};
    use crate::vector::{Point, Vector};
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
    // Scenario: Constructing a ray through the center of the canvas
    //   Given c ← camera(201, 101, π/2)
    //   When r ← ray_for_pixel(c, 100, 50)
    //   Then r.origin = point(0, 0, 0)
    //     And r.direction = vector(0, 0, -1)
    #[test]
    fn constructing_a_ray_through_the_center_of_the_canvas() {
        let c = Camera::new(201, 101, PI / 2.0);
        let r = c.ray_for_pixel(100, 50);

        assert_eq!(r.origin, Point::new(0.0, 0.0, 0.0));
        assert_eq!(r.direction, Vector::new(0.0, 0.0, -1.0));
    }

    // Scenario: Constructing a ray through a corner of the canvas
    //   Given c ← camera(201, 101, π/2)
    //   When r ← ray_for_pixel(c, 0, 0)
    //   Then r.origin = point(0, 0, 0)
    //     And r.direction = vector(0.66519, 0.33259, -0.66851)
    #[test]
    fn constructing_a_ray_through_a_corner_of_the_canvas() {
        let c = Camera::new(201, 101, PI / 2.0);
        let r = c.ray_for_pixel(0, 0);

        assert_eq!(r.origin, Point::new(0.0, 0.0, 0.0));
        assert_eq!(r.direction, Vector::new(0.66519, 0.33259, -0.66851));
    }

    // Scenario: Constructing a ray when the camera is transformed
    //   Given c ← camera(201, 101, π/2)
    //   When c.transform ← rotation_y(π/4) * translation(0, -2, 5)
    //     And r ← ray_for_pixel(c, 100, 50)
    //   Then r.origin = point(0, 2, -5)
    //     And r.direction = vector(√2/2, 0, -√2/2)
    #[test]
    fn constructing_a_ray_when_the_camera_is_transformed() {
        let mut c = Camera::new(201, 101, PI / 2.0);
        c.transform = &rotation_y(PI / 4.0) * &translation(0.0, -2.0, 5.0);

        let r = c.ray_for_pixel(100, 50);

        assert_eq!(r.origin, Point::new(0.0, 2.0, -5.0));
        assert_eq!(
            r.direction,
            Vector::new(f32::sqrt(2.0) / 2.0, 0.0, -f32::sqrt(2.0) / 2.0)
        );
    }
}
