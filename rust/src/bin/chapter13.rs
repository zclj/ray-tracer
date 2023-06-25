use ray_tracer::camera::Camera;
use ray_tracer::color::Color;
use ray_tracer::lights::PointLight;
use ray_tracer::materials::Material;
use ray_tracer::materials::{
    Pattern,
    PatternKind::{Checkers, Stripe},
};
use ray_tracer::transformations::{
    rotation_x, rotation_y, rotation_z, scaling, transform, translation, view_transform,
};
use ray_tracer::vector::{Point, Vector};
use ray_tracer::shape::*;
use ray_tracer::world::{SceneGroup, SceneObject, SceneTree, World};
use std::f32::consts::PI;
use std::fs::File;
use std::io::Write;
use std::path::Path;

fn main() {
    //println!("Chapter 13 - demo");

    let mut world = World::new();

    world.light = PointLight {
        position: Point::new(-4.9, 4.9, -1.0),
        intensity: Color::new(1.0, 1.0, 1.0),
    };

    world.shadow_bias = 0.0001;

    let wall_material = Material {
        pattern: Some(Pattern::new(
            Color::new(0.45, 0.45, 0.45),
            Color::new(0.55, 0.55, 0.55),
            Stripe,
            transform(&[scaling(0.25, 0.25, 0.25), rotation_y(PI / 2.0)]),
        )),
        ambient: 0.0,
        diffuse: 0.4,
        specular: 0.0,
        reflective: 0.3,
        ..Material::default()
    };

    let floor_plane_id = world.scene.insert_object(SceneObject::new(
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
    ));

    let ceiling_plane_id = world.scene.insert_object(SceneObject::new(
        Shape::Plane,
        Some(translation(0.0, 5.0, 0.0)),
        Some(Material {
            color: Color::new(0.8, 0.8, 0.8),
            ambient: 0.3,
            specular: 0.0,
            ..Material::default()
        }),
    ));

    let west_wall_id = world.scene.insert_object(SceneObject::new(
        Shape::Plane,
        Some(transform(&[
            rotation_y(PI / 2.0),
            rotation_z(PI / 2.0),
            translation(-5.0, 0.0, 0.0),
        ])),
        Some(wall_material.clone()),
    ));

    let east_wall_id = world.scene.insert_object(SceneObject::new(
        Shape::Plane,
        Some(transform(&[
            rotation_y(PI / 2.0),
            rotation_z(PI / 2.0),
            translation(5.0, 0.0, 0.0),
        ])),
        Some(wall_material.clone()),
    ));

    let north_wall_id = world.scene.insert_object(SceneObject::new(
        Shape::Plane,
        Some(transform(&[
            rotation_x(PI / 2.0),
            translation(0.0, 0.0, 5.0),
        ])),
        Some(wall_material.clone()),
    ));

    let south_wall_id = world.scene.insert_object(SceneObject::new(
        Shape::Plane,
        Some(transform(&[
            rotation_x(PI / 2.0),
            translation(0.0, 0.0, -5.0),
        ])),
        Some(wall_material),
    ));

    ////////////////////////////////////////
    // Background Cylinders

    let cylinder_1 = world.scene.insert_object(SceneObject::new(
        Shape::Cylinder { minimum: 0.0, maximum: 0.2, closed: true},
        Some(transform(&[
            scaling(0.4, 0.4, 0.4),
            translation(4.6, 0.0, 1.0),
        ])),
        Some(Material {
            color: Color::new(0.8, 0.5, 0.3),
            shininess: 50.0,
            ..Material::default()
        }),
    ));

    let cylinder_2 = world.scene.insert_object(SceneObject::new(
        Shape::Cylinder { minimum: 0.0, maximum: 0.6, closed: true},
        Some(transform(&[
            scaling(0.3, 0.3, 0.3),
            translation(4.7, 0.0, 0.4),
        ])),
        Some(Material {
            color: Color::new(0.9, 0.4, 0.5),
            shininess: 50.0,
            ..Material::default()
        }),
    ));

    let cylinder_3 = world.scene.insert_object(SceneObject::new(
        Shape::Cylinder { minimum: -1.0, maximum: 0.4, closed: false},
        Some(transform(&[
            scaling(0.5, 0.5, 0.5),
            translation(-1.0, 0.5, 4.4),
        ])),
        Some(Material {
            color: Color::new(0.4, 0.9, 0.6),
            shininess: 50.0,
            ..Material::default()
        }),
    ));

    let cylinder_4 = world.scene.insert_object(SceneObject::new(
        Shape::Cylinder { minimum: -1.0, maximum: 0.5, closed: true},
        Some(transform(&[
            scaling(0.3, 0.3, 0.3),
            translation(-1.7, 0.3, 4.7),
        ])),
        Some(Material {
            color: Color::new(0.4, 0.6, 0.9),
            shininess: 50.0,
            ..Material::default()
        }),
    ));

    ////////////////////////////////////////
    // Foreground cylinders

    let red_cylinder = world.scene.insert_object(SceneObject::new(
        Shape::Cylinder { minimum: -1.0, maximum: 1.0, closed: false},
        Some(transform(&[translation(-0.6, 1.0, 0.6)])),
        Some(Material {
            color: Color::new(1.0, 0.3, 0.2),
            specular: 0.4,
            shininess: 5.0,
            ..Material::default()
        }),
    ));

    let red_cone = world.scene.insert_object(SceneObject::new(
        Shape::Cone { minimum: -1.0, maximum: 1.0, closed: false},
        Some(transform(&[
            scaling(0.4, 0.4, 0.4),
            translation(-0.6, 0.4, -1.0),
        ])),
        Some(Material {
            color: Color::new(1.0, 0.3, 0.2),
            specular: 0.4,
            shininess: 5.0,
            ..Material::default()
        }),
    ));

    let blue_cone = world.scene.insert_object(SceneObject::new(
        Shape::Cone { minimum: -1.0, maximum: 1.0, closed: true},
        Some(transform(&[
            scaling(0.3, 0.3, 0.3),
            translation(0.6, 0.301, -1.8),
        ])),
        Some(Material {
            color: Color::new(0.0, 0.2, 0.2),
            ambient: 0.0,
            diffuse: 0.4,
            specular: 0.9,
            shininess: 300.0,
            reflective: 0.9,
            transparency: 0.9,
            refractive_index: 1.5,
            ..Material::default()
        }),
    ));

    let blue_glass_cylinder = world.scene.insert_object(SceneObject::new(
        Shape::Cylinder { minimum: -1.1, maximum: 0.7, closed: true},
        Some(transform(&[
            scaling(0.7, 0.7, 0.7),
            translation(0.6, 0.7, -0.6),
        ])),
        Some(Material {
            color: Color::new(0.0, 0.0, 0.2),
            ambient: 0.0,
            diffuse: 0.4,
            specular: 0.9,
            shininess: 300.0,
            reflective: 0.9,
            transparency: 0.9,
            refractive_index: 1.5,
            ..Material::default()
        }),
    ));

    let green_glass_cylinder = world.scene.insert_object(SceneObject::new(
        Shape::Cylinder { minimum: -1.0, maximum: 1.0, closed: false},
        Some(transform(&[
            scaling(0.5, 0.5, 0.5),
            translation(-1.9, 0.5, -0.8),
        ])),
        Some(Material {
            color: Color::new(0.0, 0.2, 0.0),
            ambient: 0.0,
            diffuse: 0.4,
            specular: 0.9,
            shininess: 300.0,
            reflective: 0.9,
            transparency: 0.9,
            refractive_index: 1.5,
            ..Material::default()
        }),
    ));

    ////////////////////////////////////////
    // world setup

    let planes = world.scene.insert_group(SceneGroup::new(
        vec![
            floor_plane_id,
            ceiling_plane_id,
            west_wall_id,
            east_wall_id,
            north_wall_id,
            south_wall_id,
        ],
        None,
        None,
    ));

    let background_1 =
        world
            .scene
            .insert_group(SceneGroup::new(vec![cylinder_3, cylinder_4], None, None));

    let background_2 =
        world
            .scene
            .insert_group(SceneGroup::new(vec![cylinder_1, cylinder_2], None, None));

    let foreground = world.scene.insert_group(SceneGroup::new(
        vec![red_cylinder, red_cone, blue_cone, blue_glass_cylinder, green_glass_cylinder],
        None,
        None,
    ));

    let root = world.scene.insert_group(SceneGroup::new(
        vec![
            planes,
            background_1,
            background_2,
            foreground,
        ],
        None,
        None,
    ));

    world.root_group_id = root;
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

    let path = Path::new("demos/ppms/chapter13.ppm");

    let mut file = match File::create(path) {
        Err(why) => panic!("could not create file {}: {} ", path.display(), why),
        Ok(file) => file,
    };

    if let Err(why) = file.write_all(ppm.as_bytes()) {
        panic!("could not write file {}: {} ", path.display(), why)
    }

    //println!("Done");
}
