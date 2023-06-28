use ray_tracer::camera::Camera;
use ray_tracer::color::Color;
use ray_tracer::lights::PointLight;
use ray_tracer::materials::Material;
use ray_tracer::materials::{Pattern, PatternKind};
use ray_tracer::shape::*;
use ray_tracer::transformations::{rotation_x, scaling, transform, translation, view_transform};
use ray_tracer::vector::{Point, Vector};
use ray_tracer::world::{SceneGroup, World};
use std::f32::consts::PI;
use std::fs::File;
use std::io::Write;
use std::path::Path;

fn main() {
    println!("Glass sphere with air bubble - demo");

    let mut world = World::new();

    world.light = PointLight {
        position: Point::new(2.0, 10.0, -5.0),
        intensity: Color::new(0.9, 0.9, 0.9),
    };

    world.shadow_bias = 0.0001;

    let wall_id = world.scene.insert_object(
        Shape::Plane,
        Some(transform(&[
            rotation_x(PI / 2.0),
            translation(0.0, 0.0, 10.0),
        ])),
        Some(Material {
            pattern: Some(Pattern {
                a: Color::new(0.15, 0.15, 0.15),
                b: Color::new(0.85, 0.85, 0.85),
                kind: PatternKind::Checkers,
                ..Pattern::default()
            }),
            ambient: 0.8,
            specular: 0.0,
            diffuse: 0.2,
            ..Material::default()
        }),
    );

    let glass_ball_id = world.scene.insert_object(
        Shape::Sphere,
        None,
        Some(Material {
            color: Color::new(1.0, 1.0, 1.0),
            ambient: 0.0,
            specular: 0.9,
            diffuse: 0.0,
            shininess: 300.0,
            reflective: 0.9,
            transparency: 0.9,
            refractive_index: 1.5,
            ..Material::default()
        }),
    );

    let hollow_center_id = world.scene.insert_object(
        Shape::Sphere,
        Some(scaling(0.5, 0.5, 0.5)),
        Some(Material {
            color: Color::new(1.0, 1.0, 1.0),
            ambient: 0.0,
            specular: 0.9,
            diffuse: 0.0,
            shininess: 300.0,
            reflective: 0.9,
            transparency: 0.9,
            refractive_index: 1.0000034,
            ..Material::default()
        }),
    );

    ////////////////////////////////////////
    // world setup

    let planes = world
        .scene
        .insert_group(SceneGroup::new(vec![wall_id], None, None));

    let root = world.scene.insert_group(SceneGroup::new(
        vec![planes, glass_ball_id, hollow_center_id],
        None,
        None,
    ));

    world.root_group_id = root;
    world.build();

    ////////////////////////////////////////
    // Camera and rendering

    let camera = Camera::new(
        600,
        600,
        0.45,
        &view_transform(
            &Point::new(0.0, 0.0, -5.0),
            &Point::new(0.0, 0.0, 0.0),
            &Vector::new(0.0, 1.0, 0.0),
        ),
    );

    let canvas = camera.render(&world, 5);

    let ppm = canvas.to_ppm();

    let path = Path::new("demos/ppms/glass_with_air.ppm");

    let mut file = match File::create(path) {
        Err(why) => panic!("could not create file {}: {} ", path.display(), why),
        Ok(file) => file,
    };

    if let Err(why) = file.write_all(ppm.as_bytes()) {
        panic!("could not write file {}: {} ", path.display(), why)
    }

    println!("Done");
}
