use ray_tracer::camera::Camera;
use ray_tracer::color::Color;
use ray_tracer::lights::PointLight;
use ray_tracer::materials::{Material, Pattern, PatternKind};
use ray_tracer::matrices::M4x4;
use ray_tracer::shape::*;
use ray_tracer::transformations::{
    rotation_x, rotation_y, scaling, transform, translation, view_transform,
};
use ray_tracer::vector::{Point, Vector};
use ray_tracer::world::{SceneGroup, World};
use std::f32::consts::PI;
use std::fs::File;
use std::io::Write;
use std::path::Path;

fn main() {
    println!("Simple scene with pattern - demo");

    let mut world = World::new();

    world.light = PointLight {
        position: Point::new(-10.0, 10.0, -10.0),
        intensity: Color::new(1.0, 1.0, 1.0),
    };

    world.shadow_bias = 0.01;

    // shapes
    let floor_id = world.scene.insert_object(
        Shape::Plane,
        None,
        Some(Material {
            color: Color::new(1.0, 1.0, 1.0),
            diffuse: 0.7,
            specular: 0.3,
            pattern: Some(Pattern::new(
                Color::new(0.3, 0.3, 0.9),
                Color::new(0.7, 0.3, 0.3),
                PatternKind::Checkers,
                M4x4::IDENTITY,
            )),
            ..Material::default()
        }),
    );

    let middle_sphere_id = world.scene.insert_object(
        Shape::Sphere,
        Some(translation(-0.5, 1.0, 0.5)),
        Some(Material {
            color: Color::new(0.1, 1.0, 0.5),
            diffuse: 0.7,
            specular: 0.3,
            pattern: Some(Pattern::new(
                Color::new(0.1, 1.0, 0.5),
                Color::new(1.0, 0.1, 0.5),
                PatternKind::Ring,
                &scaling(0.2, 0.2, 0.2) * &rotation_x(PI / 4.0),
            )),
            ..Material::default()
        }),
    );

    let right_sphere_id = world.scene.insert_object(
        Shape::Sphere,
        Some(transform(&[
            scaling(0.5, 0.5, 0.5),
            translation(1.5, 0.5, -0.5),
        ])),
        Some(Material {
            color: Color::new(0.5, 1.0, 0.1),
            diffuse: 0.7,
            specular: 0.3,
            pattern: Some(Pattern::new(
                Color::new(0.5, 1.0, 0.1),
                Color::new(1.0, 0.5, 0.0),
                PatternKind::Stripe,
                &rotation_y(PI / 3.0) * &scaling(0.3, 0.3, 0.3),
            )),
            ..Material::default()
        }),
    );

    let left_sphere_id = world.scene.insert_object(
        Shape::Sphere,
        Some(transform(&[
            scaling(0.33, 0.33, 0.33),
            translation(-1.5, 0.33, -0.75),
        ])),
        Some(Material {
            color: Color::new(1.0, 0.8, 0.1),
            diffuse: 0.7,
            specular: 0.3,
            pattern: Some(Pattern::new(
                Color::new(0.5, 1.0, 0.1),
                Color::new(0.2, 0.5, 0.3),
                PatternKind::Gradient,
                scaling(0.3, 0.3, 0.3),
            )),
            ..Material::default()
        }),
    );

    ////////////////////////////////////////
    // world setup

    let root = world.scene.insert_group(SceneGroup::new(
        vec![floor_id, middle_sphere_id, right_sphere_id, left_sphere_id],
        None,
        None,
    ));

    world.root_group_id = root;
    world.build();

    ////////////////////////////////////////
    // Camera and rendering

    let camera = Camera::new(
        1000,
        500,
        PI / 3.0,
        &view_transform(
            &Point::new(0.1, 1.5, -7.0),
            &Point::new(0.0, 1.0, 0.0),
            &Vector::new(0.0, 1.0, 0.0),
        ),
    );

    let canvas = camera.render(&world, 1);

    let ppm = canvas.to_ppm();

    let path = Path::new("demos/ppms/simple_pattern_scene.ppm");

    let mut file = match File::create(path) {
        Err(why) => panic!("could not create file {}: {} ", path.display(), why),
        Ok(file) => file,
    };

    if let Err(why) = file.write_all(ppm.as_bytes()) {
        panic!("could not write file {}: {} ", path.display(), why)
    }

    println!("Scene Done!");
}
