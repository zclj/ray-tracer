use criterion::{black_box, criterion_group, criterion_main, Criterion};
use ray_tracer::camera::Camera;
use ray_tracer::color::Color;
use ray_tracer::lights::PointLight;
use ray_tracer::materials::Material;
use ray_tracer::materials::{
    Pattern,
    PatternKind::{Checkers, Stripe},
};
use ray_tracer::transformations::*;
use ray_tracer::vector::{Point, Vector};
use ray_tracer::world::World;
use std::f32::consts::PI;

pub fn render_chapter_11(c: &mut Criterion) {
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

    let _floor_plane_id = world.push_plane(
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

    let _ceiling_plane_id = world.push_plane(
        Some(translation(0.0, 5.0, 0.0)),
        Some(Material {
            color: Color::new(0.8, 0.8, 0.8),
            ambient: 0.3,
            specular: 0.0,
            ..Material::default()
        }),
    );

    let _west_wall_id = world.push_plane(
        Some(transform(&[
            rotation_y(PI / 2.0),
            rotation_z(PI / 2.0),
            translation(-5.0, 0.0, 0.0),
        ])),
        Some(wall_material.clone()),
    );

    let _east_wall_id = world.push_plane(
        Some(transform(&[
            rotation_y(PI / 2.0),
            rotation_z(PI / 2.0),
            translation(5.0, 0.0, 0.0),
        ])),
        Some(wall_material.clone()),
    );

    let _north_wall_id = world.push_plane(
        Some(transform(&[
            rotation_x(PI / 2.0),
            translation(0.0, 0.0, 5.0),
        ])),
        Some(wall_material.clone()),
    );

    let _south_wall_id = world.push_plane(
        Some(transform(&[
            rotation_x(PI / 2.0),
            translation(0.0, 0.0, -5.0),
        ])),
        Some(wall_material),
    );

    ////////////////////////////////////////
    // Background Balls

    let _ball_1 = world.push_sphere(
        Some(transform(&[
            scaling(0.4, 0.4, 0.4),
            translation(4.6, 0.4, 1.0),
        ])),
        Some(Material {
            color: Color::new(0.8, 0.5, 0.3),
            shininess: 50.0,
            ..Material::default()
        }),
    );

    let _ball_2 = world.push_sphere(
        Some(transform(&[
            scaling(0.3, 0.3, 0.3),
            translation(4.7, 0.3, 0.4),
        ])),
        Some(Material {
            color: Color::new(0.9, 0.4, 0.5),
            shininess: 50.0,
            ..Material::default()
        }),
    );

    let _ball_3 = world.push_sphere(
        Some(transform(&[
            scaling(0.5, 0.5, 0.5),
            translation(-1.0, 0.5, 4.5),
        ])),
        Some(Material {
            color: Color::new(0.4, 0.9, 0.6),
            shininess: 50.0,
            ..Material::default()
        }),
    );

    let _ball_4 = world.push_sphere(
        Some(transform(&[
            scaling(0.3, 0.3, 0.3),
            translation(-1.7, 0.3, 4.7),
        ])),
        Some(Material {
            color: Color::new(0.4, 0.6, 0.9),
            shininess: 50.0,
            ..Material::default()
        }),
    );

    ////////////////////////////////////////
    // Foreground balls

    let _red_ball = world.push_sphere(
        Some(transform(&[translation(-0.6, 1.0, 0.6)])),
        Some(Material {
            color: Color::new(1.0, 0.3, 0.2),
            specular: 0.4,
            shininess: 5.0,
            ..Material::default()
        }),
    );

    let _blue_glass_ball = world.push_sphere(
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
    );

    let _green_glass_ball = world.push_sphere(
        Some(transform(&[
            scaling(0.5, 0.5, 0.5),
            translation(-0.7, 0.5, -0.8),
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
    );

    ////////////////////////////////////////
    // Camera and rendering

    let camera = Camera::new(
        1200,
        600,
        1.152,
        &view_transform(
            &Point::new(-2.6, 1.5, -3.9),
            &Point::new(-0.6, 1.0, -0.8),
            &Vector::new(0.0, 1.0, 0.0),
        ),
    );

    //let canvas = camera.render(black_box(&world), black_box(5));
    c.bench_function("render chapter 11", |b| {
        b.iter(|| camera.render(black_box(&world), black_box(5)))
    });
    //c.bench_function("render chapter 11", |b| b.iter(|| canvas.to_ppm()));
}

criterion_group!(benches, render_chapter_11,);
criterion_main!(benches);
