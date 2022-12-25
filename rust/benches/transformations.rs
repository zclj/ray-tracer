use criterion::{black_box, criterion_group, criterion_main, Criterion};
use glam::{Mat4, Vec3};
use ray_tracer::matrices::{M2x2, M3x3, M4x4};
use ray_tracer::transformations::*;
use ray_tracer::vector::Point;
use std::f32::consts::PI;

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

pub fn transform_rotation(c: &mut Criterion) {
    let p = Point::new(1.0, 1.0, 1.0);
    let rot_x = rotation_x(PI / 2.0);
    let rot_y = rotation_y(PI / 2.0);
    let rot_z = rotation_z(PI / 2.0);

    let mut group = c.benchmark_group("Transform - Rotation");

    group.bench_function("rotation X", |b| b.iter(|| black_box(&rot_x * &p)));
    group.bench_function("rotation Y", |b| b.iter(|| black_box(&rot_y * &p)));
    group.bench_function("rotation Z", |b| b.iter(|| black_box(&rot_z * &p)));

    let glam_rot_x = Mat4::from_rotation_x(PI / 2.0);
    let glam_rot_y = Mat4::from_rotation_y(PI / 2.0);
    let glam_rot_z = Mat4::from_rotation_z(PI / 2.0);

    group.bench_function("glam rotation X", |b| {
        b.iter(|| black_box(glam_rot_x.transform_point3(Vec3::new(1.0, 1.0, 1.0))))
    });
    group.bench_function("glam rotation Y", |b| {
        b.iter(|| black_box(glam_rot_y.transform_point3(Vec3::new(1.0, 1.0, 1.0))))
    });
    group.bench_function("glam rotation Z", |b| {
        b.iter(|| black_box(glam_rot_z.transform_point3(Vec3::new(1.0, 1.0, 1.0))))
    });

    group.finish()
}

pub fn transform_shearing(c: &mut Criterion) {
    let transform = shearing(1.0, 0.0, 0.0, 0.0, 0.0, 0.0);
    let p = Point::new(2.0, 3.0, 4.0);

    c.bench_function("Transform - Shearing", |b| {
        b.iter(|| black_box(&transform * &p))
    });
}

criterion_group!(
    benches,
    transform_translation,
    transform_scaling,
    transform_rotation,
    transform_shearing,
);
criterion_main!(benches);
