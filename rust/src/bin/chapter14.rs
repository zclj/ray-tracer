use ray_tracer::camera::Camera;
use ray_tracer::color::Color;
use ray_tracer::lights::PointLight;
use ray_tracer::materials::Material;
use ray_tracer::shape::*;
use ray_tracer::transformations::{
    rotation_y, rotation_z, scaling, transform, translation, view_transform,
};
use ray_tracer::vector::{Point, Vector};
use ray_tracer::world::{SceneGroup, SceneTree, World};
use std::f32::consts::PI;
use std::fs::File;
use std::io::Write;
use std::path::Path;

fn hex_corner(scene: &mut SceneTree) -> u32 {
    scene.insert_object(
        Shape::Sphere,
        Some(transform(&[
            scaling(0.25, 0.25, 0.25),
            translation(0.0, 0.0, -1.0),
        ])),
        Some(Material {
            color: Color::new(0.8, 0.5, 0.3),
            shininess: 50.0,
            ..Material::default()
        }),
    )
}

fn hex_edge(scene: &mut SceneTree) -> u32 {
    scene.insert_object(
        Shape::Cylinder {
            minimum: 0.0,
            maximum: 1.0,
            closed: true,
        },
        Some(transform(&[
            scaling(0.25, 1.0, 0.25),
            rotation_z(-PI / 2.0),
            rotation_y(-PI / 6.0),
            translation(0.0, 0.0, -1.0),
        ])),
        Some(Material {
            color: Color::new(0.8, 0.5, 0.3),
            shininess: 50.0,
            ..Material::default()
        }),
    )
}

fn hex_side(scene: &mut SceneTree, i: u32) -> u32 {
    let hex_corner_id = hex_corner(scene);

    let hex_edge_id = hex_edge(scene);

    scene.insert_group(SceneGroup::new(
        vec![hex_edge_id, hex_corner_id],
        Some(rotation_y(i as f32 * PI / 3.0)),
        None,
    ))
}

fn main() {
    let mut world = World::new();

    world.light = PointLight {
        position: Point::new(-4.9, 4.9, -1.0),
        intensity: Color::new(1.0, 1.0, 1.0),
    };

    world.shadow_bias = 0.0001;

    let ids = (0..6)
        .map(|i| hex_side(&mut world.scene, i))
        .collect::<Vec<u32>>();

    let hexagon = world.scene.insert_group(SceneGroup::new(
        ids,
        Some(transform(&[
            rotation_z(PI / 3.0),
            translation(0.5, 1.0, 0.0),
        ])),
        None,
    ));

    world.root_group_id = hexagon;
    world.build();

    ////////////////////////////////////////
    // Camera and rendering

    let camera = Camera::new(
        1200, //3840,
        600,  //2160,
        1.152,
        &view_transform(
            &Point::new(-2.6, 1.5, -3.9),
            &Point::new(-0.6, 1.0, -0.8),
            &Vector::new(0.0, 1.0, 0.0),
        ),
    );

    let canvas = camera.render(&world, 5);

    let ppm = canvas.to_ppm();

    let path = Path::new("demos/ppms/chapter14.ppm");

    let mut file = match File::create(path) {
        Err(why) => panic!("could not create file {}: {} ", path.display(), why),
        Ok(file) => file,
    };

    if let Err(why) = file.write_all(ppm.as_bytes()) {
        panic!("could not write file {}: {} ", path.display(), why)
    }
}
