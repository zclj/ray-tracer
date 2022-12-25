use criterion::{black_box, criterion_group, criterion_main, Criterion};
use glam::{Mat4, Vec3};
use ray_tracer::matrices::{M2x2, M3x3, M4x4};
use ray_tracer::transformations::*;
use ray_tracer::vector::Point;

pub fn transform_translation(c: &mut Criterion) {
    let translation = translation(5.0, -3.0, 2.0);
    let p = Point::new(-3.0, 4.0, 5.0);

    let glam_translation = Mat4::from_translation(Vec3::new(5.0, -3.0, 2.0));

    let mut group = c.benchmark_group("Transform - Translation");

    group.bench_function("Translation", |b| b.iter(|| black_box(&translation * &p)));

    group.bench_function("glam Translation", |b| {
        b.iter(|| black_box(glam_translation.transform_point3(Vec3::new(-3.0, 4.0, 5.0))))
    });
    group.finish()
}

pub fn transform_scaling(c: &mut Criterion) {
    let transform = scaling(-1.0, 1.0, 1.0);
    let p = Point::new(2.0, 3.0, 4.0);

    let glam_transform = Mat4::from_scale(Vec3::new(-1.0, 1.0, 1.0));

    let mut group = c.benchmark_group("Transform - Scaling");

    group.bench_function("scaling", |b| b.iter(|| black_box(&transform * &p)));

    group.bench_function("glam scaling", |b| {
        b.iter(|| black_box(glam_transform.transform_point3(Vec3::new(2.0, 3.0, 4.0))))
    });
    group.finish()
}

// @TODO - rotation, shearing, multiple?

criterion_group!(benches, transform_translation, transform_scaling,);
criterion_main!(benches);
