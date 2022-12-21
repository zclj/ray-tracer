use criterion::{black_box, criterion_group, criterion_main, Criterion};
use glam::{Mat4, Vec3};
use ray_tracer::canvas::Canvas;
use ray_tracer::color::Color;
use ray_tracer::matrices::{M2x2, M3x3, M4x4};
use ray_tracer::transformations::translation;
use ray_tracer::vector::Point;

pub fn criterion_benchmark(c: &mut Criterion) {
    let canvas = Canvas::new(black_box(900), black_box(600));

    c.bench_function("canvas to ppm", |b| b.iter(|| canvas.to_ppm()));
}

pub fn canvas_colored(c: &mut Criterion) {
    let mut canvas = Canvas::new(black_box(900), black_box(600));
    let white = Color::new(1.0, 1.0, 1.0);

    for i in 0..900 {
        for j in 0..600 {
            canvas.write_pixel(i, j, white.clone());
        }
    }
    c.bench_function("colored canvas to ppm", |b| b.iter(|| canvas.to_ppm()));
}

pub fn transform_translation(c: &mut Criterion) {
    let translation = translation(5.0, -3.0, 2.0);
    let p = Point::new(-3.0, 4.0, 5.0);

    let glam_translation = Mat4::from_translation(Vec3::new(5.0, -3.0, 2.0));

    let mut group = c.benchmark_group("Transform");

    group.bench_function("Translation", |b| b.iter(|| black_box(&translation * &p)));

    group.bench_function("glam Translation", |b| {
        b.iter(|| black_box(glam_translation.transform_point3(Vec3::new(-3.0, 4.0, 5.0))))
    });
    group.finish()
}

// @TODO - scaling, rotation, shearing, multiple?

// pub fn glam_transform_translation(c: &mut Criterion) {
//     let transform = Mat4::from_translation(Vec3::new(5.0, -3.0, 2.0));
//     c.bench_function("glam Translation", |b| {
//         b.iter(|| black_box(transform.transform_point3(Vec3::new(-3.0, 4.0, 5.0))))
//     });
// }

criterion_group!(
    benches,
    criterion_benchmark,
    canvas_colored,
    transform_translation,
);
criterion_main!(benches);
