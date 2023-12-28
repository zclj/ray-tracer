use ray_tracer::camera::Camera;
use ray_tracer::color::Color;
use ray_tracer::lights::PointLight;
use ray_tracer::materials::Material;
use ray_tracer::shape::*;
use ray_tracer::transformations::{
    rotation_y, rotation_z, scaling, transform, translation, view_transform,
};
use ray_tracer::vector::{Point, Vector};
use ray_tracer::world::{SceneTree, World};
use ray_tracer::obj_file_parser::parse;
use std::f32::consts::PI;
use std::fs::File;
use std::io::Write;
use std::path::Path;

fn main() {
    println!("Chapter 15 demo!");

    let obj_content = std::fs::read_to_string("./obj-files/teapot.obj").expect("Could not read file");

    //println!("File: {:#?}", obj_content);
    let mut result = parse(&obj_content);

    //println!("Result: {:#?}", result);
    result.world.build();

    let g = &result.world.scene.arena[result.world.root_group_id as usize];
    println!("{:#?}", g);
    ////////////////////////////////////////
    // Camera and rendering

    let camera = Camera::new(
        400,//1200, //3840,
        200,//600,  //2160,
        1.152,
        &view_transform(
            &Point::new(-2.6, 1.5, -5.5),
            &Point::new(-0.6, 1.0, -0.8),
            &Vector::new(0.0, 1.0, 0.0),
        ),
    );

    // let canvas = camera.render(&result.world, 5);

    // let ppm = canvas.to_ppm();

    // let path = Path::new("demos/ppms/chapter15.ppm");

    // let mut file = match File::create(path) {
    //     Err(why) => panic!("could not create file {}: {} ", path.display(), why),
    //     Ok(file) => file,
    // };

    // if let Err(why) = file.write_all(ppm.as_bytes()) {
    //     panic!("could not write file {}: {} ", path.display(), why)
    // }
}
