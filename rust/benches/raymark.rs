use criterion::{black_box, criterion_group, criterion_main, Criterion};
use ray_tracer::canvas::Canvas;

pub fn criterion_benchmark(c: &mut Criterion) {
    c.bench_function("projectile", |b| b.iter(|| Canvas::new(black_box(10),black_box(10))));
}

criterion_group!(benches, criterion_benchmark);
criterion_main!(benches);
