# Rust implementation

## Running tests

With nextest, `cargo nextest run`

Test coverage with `cargo install cargo-llvm-cov` and `cargo llvm-cov nextest`

Coverage report, `cargo llvm-cov nextest --open` or `--html`. [Docs](https://lib.rs/crates/cargo-llvm-cov)

## Profiling

Using [Cargo Flamegraph](https://github.com/flamegraph-rs/flamegraph).

## Benchmarks

Using [Criterion](https://github.com/bheisler/criterion.rs).

Run with `cargo bench`

## Inspection

Using [cargo-show-asm](https://github.com/pacak/cargo-show-asm).

For example `cargo asm --lib "ray_tracer::canvas::Canvas::to_ppm" --rust`

## To check out

- https://nexte.st/
- https://github.com/flamegraph-rs/flamegraph#systems-performance-work-guided-by-flamegraphs
