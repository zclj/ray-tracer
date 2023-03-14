use criterion::{black_box, criterion_group, criterion_main, Criterion};
use glam::Vec4;
use ray_tracer::vector as V;
use ray_tracer::vector_simd::Vector;
// {dot, Vector}
pub fn vector_dot(c: &mut Criterion) {
    let mut group = c.benchmark_group("Vector dot");

    let av = V::Vector::new(1.0, 2.0, 3.0);
    let bv = V::Vector::new(2.0, 3.0, 4.0);

    group.bench_function("Vector4 dot", |b| b.iter(|| black_box(av.dot(&bv))));

    let x = Vector {
        f32x4: [1.0, 2.0, 3.0, 0.0],
    };
    let y = Vector {
        f32x4: [2.0, 3.0, 4.0, 0.0],
    };

    group.bench_function("Vector4 dot SIMD", |b| b.iter(|| black_box(x.simd_dot(&y))));

    let gx = Vec4::from_array([1.0, 2.0, 3.0, 0.0]);
    let gy = Vec4::from_array([2.0, 3.0, 4.0, 0.0]);

    group.bench_function("Vector4 dot (glam)", |b| b.iter(|| black_box(gx.dot(gy))));

    group.finish()
}

criterion_group!(benches, vector_dot,);

criterion_main!(benches);
