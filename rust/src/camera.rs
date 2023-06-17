use crate::canvas::Canvas;
use crate::color::Color;
use crate::intersections::Intersection;
use crate::matrices::M4x4;
use crate::rays::Ray;
use crate::vector::Point;
use crate::world::World;
use rayon::prelude::*;
use std::time::Instant;

pub struct Camera {
    hsize: u16,
    vsize: u16,
    transform_inverse: M4x4,
    half_width: f32,
    half_height: f32,
    pixel_size: f32,
}

impl Camera {
    #[allow(clippy::similar_names)]
    #[must_use]
    pub fn new(hsize: u16, vsize: u16, field_of_view: f32, transform: &M4x4) -> Self {
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
            transform_inverse: transform.inverse(),
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
        let pixel = &self.transform_inverse * &Point::new(world_x, world_y, -1.0);
        let origin = &self.transform_inverse * &Point::new(0.0, 0.0, 0.0);
        let direction = (&pixel - &origin).norm();

        Ray::new(origin, direction)
    }

    #[must_use]
    pub fn render(&self, world: &World, reflection_limit: u8) -> Canvas {
        // if is_x86_feature_detected!("avx2") {
        //     println!("AVX2 detected!");
        // }

        let start_time = Instant::now();
        let colors = (0..(self.vsize - 1))
            .into_par_iter()
            .map(|y| {
                let mut intersections = Vec::<Intersection>::with_capacity(100);
                let mut containers = Vec::<u32>::with_capacity(20);
                (0..(self.hsize - 1))
                    .map(|x| {
                        let ray = self.ray_for_pixel(x, y);
                        world.color_at(&ray, reflection_limit, &mut intersections, &mut containers)
                    })
                    .collect::<Vec<Color>>()
            })
            .collect::<Vec<Vec<Color>>>();
        let end_time = Instant::now();
        println!("Render time {:?}", end_time.duration_since(start_time));

        let image = Canvas::new(self.hsize.into(), self.vsize.into());
        image.from_colors(&colors)
    }
}

#[cfg(test)]
mod test {
    use super::*;
    use crate::color::Color;
    use crate::materials::Material;
    use crate::matrices::M4x4;
    use crate::transformations::{rotation_y, scaling, translation, view_transform};
    use crate::vector::{Point, Vector};
    use crate::world::World;
    use std::f32::consts::PI;

    fn test_world() -> World {
        let mut w = World::new();

        w.push_sphere(
            None,
            Some(Material {
                color: Color::new(0.8, 1.0, 0.6),
                diffuse: 0.7,
                specular: 0.2,
                ..Default::default()
            }),
        );

        w.push_sphere(Some(scaling(0.5, 0.5, 0.5)), Some(Material::default()));

        w
    }
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

        let c = Camera::new(hsize, vsize, fov, &M4x4::IDENTITY);

        assert_eq!(c.hsize, 160);
        assert_eq!(c.vsize, 120);
        //assert_eq!(c.field_of_view, PI / 2.0);
        //assert_eq!(c.transform, M4x4::IDENTITY);
    }
    // Scenario: The pixel size for a horizontal canvas
    //   Given c ← camera(200, 125, π/2)
    //   Then c.pixel_size = 0.01
    #[test]
    fn the_pixel_size_for_a_horizontal_canvas() {
        let c = Camera::new(200, 125, PI / 2.0, &M4x4::IDENTITY);

        assert_eq!(c.pixel_size, 0.01)
    }

    // Scenario: The pixel size for a vertical canvas
    //   Given c ← camera(125, 200, π/2)
    //   Then c.pixel_size = 0.01
    #[test]
    fn the_pixel_size_for_a_vertical_canvas() {
        let c = Camera::new(125, 200, PI / 2.0, &M4x4::IDENTITY);

        assert_eq!(c.pixel_size, 0.01)
    }
    // Scenario: Constructing a ray through the center of the canvas
    //   Given c ← camera(201, 101, π/2)
    //   When r ← ray_for_pixel(c, 100, 50)
    //   Then r.origin = point(0, 0, 0)
    //     And r.direction = vector(0, 0, -1)
    #[test]
    fn constructing_a_ray_through_the_center_of_the_canvas() {
        let c = Camera::new(201, 101, PI / 2.0, &M4x4::IDENTITY);
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
        let c = Camera::new(201, 101, PI / 2.0, &M4x4::IDENTITY);
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
        let c = Camera::new(
            201,
            101,
            PI / 2.0,
            &(&rotation_y(PI / 4.0) * &translation(0.0, -2.0, 5.0)),
        );

        let r = c.ray_for_pixel(100, 50);

        assert_eq!(r.origin, Point::new(0.0, 2.0, -5.0));
        assert_eq!(
            r.direction,
            Vector::new(f32::sqrt(2.0) / 2.0, 0.0, -f32::sqrt(2.0) / 2.0)
        );
    }

    // Scenario: Rendering a world with a camera
    //   Given w ← default_world()
    //     And c ← camera(11, 11, π/2)
    //     And from ← point(0, 0, -5)
    //     And to ← point(0, 0, 0)
    //     And up ← vector(0, 1, 0)
    //     And c.transform ← view_transform(from, to, up)
    //   When image ← render(c, w)
    //   Then pixel_at(image, 5, 5) = color(0.38066, 0.47583, 0.2855)
    #[test]
    fn rendering_a_world_with_a_camera() {
        let w = test_world();
        let from = Point::new(0.0, 0.0, -5.0);
        let to = Point::new(0.0, 0.0, 0.0);
        let up = Vector::new(0.0, 1.0, 0.0);
        let c = Camera::new(11, 11, PI / 2.0, &view_transform(&from, &to, &up));

        let image = c.render(&w, 1);

        assert_eq!(image.pixel_at(5, 5), &Color::new(0.38066, 0.47583, 0.2855))
    }
}
