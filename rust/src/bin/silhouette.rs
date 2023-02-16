use ray_tracer::canvas::Canvas;
use ray_tracer::color::Color;
use ray_tracer::intersections::{hit, intersect};
use ray_tracer::rays::Ray;
use ray_tracer::shape::Shape;
use ray_tracer::vector::Point;
use ray_tracer::world::World;
use std::fs::File;
use std::io::Write;
use std::path::Path;

fn silhouette_into_canvas() -> Canvas {
    let canvas_pixels = 1000;
    let mut canvas = Canvas::new(canvas_pixels as usize, canvas_pixels as usize);
    let red_color = Color::new(1., 0., 0.);

    let ray_origin = Point::new(0.0, 0.0, -5.0);
    let wall_z = 10.0;
    let wall_size = 7.0;

    let pixel_size: f32 = wall_size / canvas_pixels as f32;

    let half = wall_size / 2.0;

    let to_world_x = |x: i32| -half + pixel_size * x as f32;
    let to_world_y = |y: i32| -half + pixel_size * y as f32;

    let cast_on_pixel = |x, y, shape: &Shape, color: &Color| -> Color {
        let world_x = to_world_x(x);
        let world_y = to_world_y(y);

        let position = Point::new(world_x, world_y, wall_z);
        let ray_direction = (&position - &ray_origin).norm();
        let ray = Ray::new(Point::new(0.0, 0.0, -5.0), ray_direction);
        let mut xs = Vec::new();
        intersect(shape, &ray, &mut xs);

        let is_hit = hit(&xs);

        match is_hit {
            Some(_) => color.clone(),
            None => Color::new(0.0, 0.0, 0.0),
        }
    };

    let mut context = World::new();
    let sphere_id = context.push_sphere(None, None);
    let sphere = context.get_shape(sphere_id);

    for y in 0..(canvas_pixels - 1) {
        for x in 0..(canvas_pixels - 1) {
            let p_color = cast_on_pixel(x, y, sphere, &red_color);
            canvas.write_pixel(x as usize, y as usize, p_color)
        }
    }

    canvas
}

fn main() {
    println!("Silhouette demo");

    let ppm = silhouette_into_canvas().to_ppm();

    let path = Path::new("demos/ppms/silhouette.ppm");

    let mut file = match File::create(path) {
        Err(why) => panic!("could not create file {}: {} ", path.display(), why),
        Ok(file) => file,
    };

    if let Err(why) = file.write_all(ppm.as_bytes()) {
        panic!("could not write file {}: {} ", path.display(), why)
    }

    println!("Silhouette done!");
}
