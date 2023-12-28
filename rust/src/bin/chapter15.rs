use ray_tracer::camera::Camera;
use ray_tracer::color::Color;
use ray_tracer::lights::PointLight;
use ray_tracer::materials::Material;
use ray_tracer::materials::Pattern;
use ray_tracer::materials::PatternKind::Checkers;
use ray_tracer::materials::PatternKind::Stripe;
use ray_tracer::obj_file_parser::parse;
use ray_tracer::shape::*;
use ray_tracer::transformations::{
    rotation_x, rotation_y, rotation_z, scaling, transform, translation, view_transform,
};
use ray_tracer::vector::{Point, Vector};
use ray_tracer::world::SceneNode;
use ray_tracer::world::{SceneTree, World};
use std::f32::consts::PI;
use std::fs::File;
use std::io::Write;
use std::path::Path;

fn main() {
    println!("Chapter 15 demo!");

    let obj_content =
        std::fs::read_to_string("./obj-files/teapot.obj").expect("Could not read file");

    //println!("File: {:#?}", obj_content);
    let mut result = parse(&obj_content);

    //println!("Result: {:#?}", result);

    ////////////////////////////////////////
    // Planes
    let wall_material = Material {
        pattern: Some(Pattern::new(
            Color::new(0.45, 0.45, 0.45),
            Color::new(0.55, 0.55, 0.55),
            Checkers,
            transform(&[scaling(0.25, 0.25, 0.25), rotation_y(PI / 2.0)]),
        )),
        ambient: 0.0,
        diffuse: 0.4,
        specular: 0.0,
        reflective: 0.3,
        ..Material::default()
    };

    let floor_plane_id = result.world.scene.insert_object(
        Shape::Plane,
        Some(rotation_y(0.31415)),
        Some(Material {
            pattern: Some(Pattern {
                a: Color::new(0.35, 0.35, 0.35),
                b: Color::new(0.65, 0.65, 0.65),
                kind: Checkers,
                ..Pattern::default()
            }),
            specular: 0.0,
            reflective: 0.4,
            ..Material::default()
        }),
    );

    let ceiling_plane_id = result.world.scene.insert_object(
        Shape::Plane,
        Some(translation(0.0, 5.0, 0.0)),
        Some(Material {
            color: Color::new(0.8, 0.8, 0.8),
            ambient: 0.3,
            specular: 0.0,
            ..Material::default()
        }),
    );

    let west_wall_id = result.world.scene.insert_object(
        Shape::Plane,
        Some(transform(&[
            rotation_y(PI / 2.0),
            rotation_z(PI / 2.0),
            translation(-5.0, 0.0, 0.0),
        ])),
        Some(wall_material.clone()),
    );

    let east_wall_id = result.world.scene.insert_object(
        Shape::Plane,
        Some(transform(&[
            rotation_y(PI / 2.0),
            rotation_z(PI / 2.0),
            translation(5.0, 0.0, 0.0),
        ])),
        Some(wall_material.clone()),
    );

    let north_wall_id = result.world.scene.insert_object(
        Shape::Plane,
        Some(transform(&[
            rotation_x(PI / 2.0),
            translation(0.0, 0.0, 5.0),
        ])),
        Some(wall_material.clone()),
    );

    let south_wall_id = result.world.scene.insert_object(
        Shape::Plane,
        Some(transform(&[
            rotation_x(PI / 2.0),
            translation(0.0, 0.0, -5.0),
        ])),
        Some(wall_material),
    );

    let mut planes = result.world.scene.insert_group(
        vec![
            floor_plane_id,
            // ceiling_plane_id,
            // west_wall_id,
            east_wall_id,
            north_wall_id,
            // south_wall_id,
        ],
        None,
    );

    ////////////////////////////////////////
    // Build world

    let g = &mut result.world.scene.arena[result.world.root_group_id as usize];

    match g {
        SceneNode::Group { children, .. } => {
            children.push(planes);
        }
        _ => panic!(),
    }

    result.world.build();

    //println!("{:#?}", g);

    //println!("Scene: {:#?}", result.world.scene);
    ////////////////////////////////////////
    // Camera and rendering

    let camera = Camera::new(
        1200, //1200, //3840,
        600,  //600,  //2160,
        1.152,
        &view_transform(
            &Point::new(-2.6, 1.5, -7.5),
            &Point::new(-0.6, 1.0, -0.8),
            &Vector::new(0.0, 1.0, 0.0),
        ),
    );

    let canvas = camera.render(&result.world, 5);

    let ppm = canvas.to_ppm();

    let path = Path::new("demos/ppms/chapter15.ppm");

    let mut file = match File::create(path) {
        Err(why) => panic!("could not create file {}: {} ", path.display(), why),
        Ok(file) => file,
    };

    if let Err(why) = file.write_all(ppm.as_bytes()) {
        panic!("could not write file {}: {} ", path.display(), why)
    }
}
