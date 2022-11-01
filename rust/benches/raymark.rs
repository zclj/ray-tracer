use criterion::{black_box, criterion_group, criterion_main, Criterion};
use ray_tracer::canvas::Canvas;
use ray_tracer::color::Color;

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

// pub fn push_three_digits(c: &mut Criterion) {
//     let mut s = "".to_string();
//     c.bench_function("push_three_digits", |b| {
//         b.iter(|| push_digits(black_box(123), black_box(&mut s)))
//     });

//     c.bench_function("push_two_digits", |b| {
//         b.iter(|| push_digits(black_box(12), black_box(&mut s)))
//     });

//     c.bench_function("push_one_digits", |b| {
//         b.iter(|| push_digits(black_box(1), black_box(&mut s)))
//     });
// }

criterion_group!(
    benches,
    criterion_benchmark,
    canvas_colored,
    //push_three_digits
);
criterion_main!(benches);
