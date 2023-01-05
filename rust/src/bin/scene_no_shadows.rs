use ray_tracer::camera::Camera;
use ray_tracer::color::Color;
use ray_tracer::lights::PointLight;
use ray_tracer::materials::Material;
use ray_tracer::transformations::{
    rotation_x, rotation_y, scaling, transform, translation, view_transform,
};
use ray_tracer::vector::{Point, Vector};
use ray_tracer::world::World;
use std::f32::consts::PI;
use std::fs::File;
use std::io::Write;
use std::path::Path;

fn main() {
    println!("Scene, no shadows - demo");

    let mut world = World::new();

    world.light = PointLight {
        position: Point::new(-10.0, 10.0, -10.0),
        intensity: Color::new(1.0, 1.0, 1.0),
    };

    // shapes
    let _floor_id = world.push_sphere(
        Some(scaling(10.0, 0.01, 10.0)),
        Some(Material {
            color: Color::new(1.0, 0.9, 0.9),
            specular: 0.0,
            ..Material::default()
        }),
    );

    let _left_wall_id = world.push_sphere(
        Some(transform(&[
            scaling(10.0, 0.01, 10.0),
            rotation_x(PI / 2.0),
            rotation_y(-PI / 4.0),
            translation(0.0, 0.0, 5.0),
        ])),
        Some(Material {
            color: Color::new(1.0, 0.9, 0.9),
            specular: 0.0,
            ..Material::default()
        }),
    );

    let _right_wall_id = world.push_sphere(
        Some(transform(&[
            scaling(10.0, 0.01, 10.0),
            rotation_x(PI / 2.0),
            rotation_y(PI / 4.0),
            translation(0.0, 0.0, 5.0),
        ])),
        Some(Material {
            color: Color::new(1.0, 0.9, 0.9),
            specular: 0.0,
            ..Material::default()
        }),
    );

    let _middle_sphere_id = world.push_sphere(
        Some(translation(-0.5, 1.0, 0.5)),
        Some(Material {
            color: Color::new(0.1, 1.0, 0.5),
            diffuse: 0.7,
            specular: 0.3,
            ..Material::default()
        }),
    );

    let _right_sphere_id = world.push_sphere(
        Some(transform(&[
            scaling(0.5, 0.5, 0.5),
            translation(1.5, 0.5, -0.5),
        ])),
        Some(Material {
            color: Color::new(0.5, 1.0, 0.1),
            diffuse: 0.7,
            specular: 0.3,
            ..Material::default()
        }),
    );

    let _left_sphere_id = world.push_sphere(
        Some(transform(&[
            scaling(0.33, 0.33, 0.33),
            translation(-1.5, 0.33, -0.75),
        ])),
        Some(Material {
            color: Color::new(1.0, 0.8, 0.1),
            diffuse: 0.7,
            specular: 0.3,
            ..Material::default()
        }),
    );

    let mut camera = Camera::new(1000, 500, PI / 3.0);
    camera.transform = view_transform(
        &Point::new(0.0, 1.5, -5.0),
        &Point::new(0.0, 1.0, 0.0),
        &Vector::new(0.0, 1.0, 0.0),
    );

    let canvas = camera.render(&world);

    let ppm = canvas.to_ppm();

    let path = Path::new("demos/ppms/scene_no_shadows.ppm");

    let mut file = match File::create(path) {
        Err(why) => panic!("could not create file {}: {} ", path.display(), why),
        Ok(file) => file,
    };

    if let Err(why) = file.write_all(ppm.as_bytes()) {
        panic!("could not write file {}: {} ", path.display(), why)
    }

    println!("Scene, no shadows - Done!");
}
