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

pub fn matrix_multiplication(c: &mut Criterion) {
    let x = M4x4::from_elements(
        [1.0, 2.0, 3.0, 4.0],
        [5.0, 6.0, 7.0, 8.0],
        [9.0, 8.0, 7.0, 6.0],
        [5.0, 4.0, 3.0, 2.0],
    );

    let y = M4x4::from_elements(
        [-2.0, 1.0, 2.0, 3.0],
        [3.0, 2.0, 1.0, -1.0],
        [4.0, 3.0, 6.0, 5.0],
        [1.0, 2.0, 7.0, 8.0],
    );

    c.bench_function("Matrix 4x4 multiplication", |b| {
        b.iter(|| black_box(&x * &y))
    });
}

pub fn glam_matrix_multiplication(c: &mut Criterion) {
    let x = Mat4::from_cols_array_2d(&[
        [1.0, 5.0, 9.0, 5.0],
        [2.0, 6.0, 8.0, 4.0],
        [3.0, 7.0, 7.0, 3.0],
        [4.0, 8.0, 6.0, 2.0],
    ]);

    let y = Mat4::from_cols_array_2d(&[
        [-2.0, 3.0, 4.0, 1.0],
        [1.0, 2.0, 3.0, 2.0],
        [2.0, 1.0, 6.0, 7.0],
        [3.0, -1.0, 5.0, 8.0],
    ]);

    c.bench_function("glam Matrix 4x4 multiplication", |b| {
        b.iter(|| black_box(x * y))
    });
}

pub fn matrix_assign_multiplication(c: &mut Criterion) {
    let mut x = M4x4::from_elements(
        [1.0, 2.0, 3.0, 4.0],
        [5.0, 6.0, 7.0, 8.0],
        [9.0, 8.0, 7.0, 6.0],
        [5.0, 4.0, 3.0, 2.0],
    );

    let y = M4x4::from_elements(
        [-2.0, 1.0, 2.0, 3.0],
        [3.0, 2.0, 1.0, -1.0],
        [4.0, 3.0, 6.0, 5.0],
        [1.0, 2.0, 7.0, 8.0],
    );

    c.bench_function("Matrix 4x4 assign multiplication", |b| {
        b.iter(|| black_box(x *= &y))
    });
}

pub fn matrix_transpose(c: &mut Criterion) {
    let a = M4x4::from_elements(
        [0.0, 9.0, 3.0, 0.0],
        [9.0, 8.0, 0.0, 8.0],
        [1.0, 8.0, 5.0, 3.0],
        [0.0, 0.0, 5.0, 8.0],
    );

    c.bench_function("Matrix 4x4 Transpose", |b| {
        b.iter(|| black_box(a.transpose()))
    });
}

pub fn glam_matrix_transpose(c: &mut Criterion) {
    let x = Mat4::from_cols_array_2d(&[
        [0.0, 9.0, 3.0, 0.0],
        [9.0, 8.0, 0.0, 8.0],
        [1.0, 8.0, 5.0, 3.0],
        [0.0, 0.0, 5.0, 8.0],
    ]);

    c.bench_function("glam Matrix 4x4 Transpose", |b| {
        b.iter(|| black_box(x.transpose()))
    });
}

// @NOTE - don't find any glam correspondence to sub matrix
pub fn matrix_sub_matrix(c: &mut Criterion) {
    let a = M4x4::from_elements(
        [-6.0, 1.0, 1.0, 6.0],
        [-8.0, 5.0, 8.0, 6.0],
        [-1.0, 0.0, 8.0, 2.0],
        [-7.0, 1.0, -1.0, 1.0],
    );

    c.bench_function("Matrix 4x4 Submatrix", |b| {
        b.iter(|| black_box(a.sub_matrix(2, 1)))
    });
}

pub fn matrix_determinant(c: &mut Criterion) {
    let a = M4x4::from_elements(
        [0.0, 9.0, 3.0, 0.0],
        [9.0, 8.0, 0.0, 8.0],
        [1.0, 8.0, 5.0, 3.0],
        [0.0, 0.0, 5.0, 8.0],
    );

    c.bench_function("Matrix 4x4 Determinant", |b| {
        b.iter(|| black_box(a.determinant()))
    });
}

pub fn glam_matrix_determinant(c: &mut Criterion) {
    let x = Mat4::from_cols_array_2d(&[
        [0.0, 9.0, 3.0, 0.0],
        [9.0, 8.0, 0.0, 8.0],
        [1.0, 8.0, 5.0, 3.0],
        [0.0, 0.0, 5.0, 8.0],
    ]);

    c.bench_function("glam Matrix 4x4 Determinant", |b| {
        b.iter(|| black_box(x.determinant()))
    });
}

pub fn matrix_inverse(c: &mut Criterion) {
    let a = M4x4::from_elements(
        [0.0, 9.0, 3.0, 0.0],
        [9.0, 8.0, 0.0, 8.0],
        [1.0, 8.0, 5.0, 3.0],
        [0.0, 0.0, 5.0, 8.0],
    );

    c.bench_function("Matrix 4x4 Inverse", |b| b.iter(|| black_box(a.inverse())));
}

pub fn glam_matrix_inverse(c: &mut Criterion) {
    let x = Mat4::from_cols_array_2d(&[
        [0.0, 9.0, 3.0, 0.0],
        [9.0, 8.0, 0.0, 8.0],
        [1.0, 8.0, 5.0, 3.0],
        [0.0, 0.0, 5.0, 8.0],
    ]);

    c.bench_function("glam Matrix 4x4 Inverse", |b| {
        b.iter(|| black_box(x.inverse()))
    });
}

pub fn matrix_equality(c: &mut Criterion) {
    let x = M4x4::from_elements(
        [1.0, 2.0, 3.0, 4.0],
        [5.0, 6.0, 7.0, 8.0],
        [9.0, 8.0, 7.0, 6.0],
        [5.0, 4.0, 3.0, 2.0],
    );

    let y = M4x4::from_elements(
        [2.0, 3.0, 4.0, 5.0],
        [6.0, 7.0, 8.0, 9.0],
        [8.0, 7.0, 6.0, 5.0],
        [4.0, 3.0, 2.0, 1.0],
    );

    c.bench_function("Matrix 4x4 Equality", |b| b.iter(|| black_box(x == y)));
}

pub fn glam_matrix_equality(c: &mut Criterion) {
    let x = Mat4::from_cols_array_2d(&[
        [1.0, 2.0, 3.0, 4.0],
        [5.0, 6.0, 7.0, 8.0],
        [9.0, 8.0, 7.0, 6.0],
        [5.0, 4.0, 3.0, 2.0],
    ]);

    let y = Mat4::from_cols_array_2d(&[
        [2.0, 3.0, 4.0, 5.0],
        [6.0, 7.0, 8.0, 9.0],
        [8.0, 7.0, 6.0, 5.0],
        [4.0, 3.0, 2.0, 1.0],
    ]);

    c.bench_function("glam Matrix 4x4 Equality", |b| b.iter(|| black_box(x == y)));
}

pub fn matrix_indexing(c: &mut Criterion) {
    let x = M4x4::from_elements(
        [1.0, 2.0, 3.0, 4.0],
        [5.0, 6.0, 7.0, 8.0],
        [9.0, 8.0, 7.0, 6.0],
        [5.0, 4.0, 3.0, 2.0],
    );

    c.bench_function("Matrix 4x4 Indexing", |b| b.iter(|| black_box(x[(2, 2)])));
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
    matrix_multiplication,
    glam_matrix_multiplication,
    matrix_assign_multiplication,
    matrix_transpose,
    glam_matrix_transpose,
    matrix_sub_matrix,
    matrix_determinant,
    glam_matrix_determinant,
    matrix_inverse,
    glam_matrix_inverse,
    matrix_equality,
    glam_matrix_equality,
    matrix_indexing,
    transform_translation,
    //glam_transform_translation,
);
criterion_main!(benches);
