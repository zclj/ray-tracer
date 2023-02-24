use criterion::{black_box, criterion_group, criterion_main, Criterion};
use glam::{Mat4};
use ray_tracer::matrices::{M4x4};

pub fn matrix_multiplication(c: &mut Criterion) {
    let mut group = c.benchmark_group("Matrix multiplication");

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

    let gx = Mat4::from_cols_array_2d(&[
        [1.0, 5.0, 9.0, 5.0],
        [2.0, 6.0, 8.0, 4.0],
        [3.0, 7.0, 7.0, 3.0],
        [4.0, 8.0, 6.0, 2.0],
    ]);

    let gy = Mat4::from_cols_array_2d(&[
        [-2.0, 3.0, 4.0, 1.0],
        [1.0, 2.0, 3.0, 2.0],
        [2.0, 1.0, 6.0, 7.0],
        [3.0, -1.0, 5.0, 8.0],
    ]);

    group.bench_function("Matrix 4x4 multiplication", |b| {
        b.iter(|| black_box(&x * &y))
    });

    group.bench_function("Matrix 4x4 (glam) multiplication", |b| {
        b.iter(|| black_box(gx * gy))
    });

    group.finish()
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

    c.bench_function("Matrix 4x4 (glam) Transpose", |b| {
        b.iter(|| black_box(x.transpose()))
    });
}

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

    c.bench_function("Matrix 4x4 (glam) Determinant", |b| {
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

    c.bench_function("Matrix 4x4 (glam) Inverse", |b| {
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

    c.bench_function("Matrix 4x4 (glam) Equality", |b| {
        b.iter(|| black_box(x == y))
    });
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
criterion_group!(
    benches,
    matrix_multiplication,
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
);

criterion_main!(benches);
