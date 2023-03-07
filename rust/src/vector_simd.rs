// Resoruces

// https://docs.rs/rustc-std-workspace-std/1.0.1/std/arch/x86_64/struct.__m128.html
// https://stackoverflow.com/questions/65984936/efficient-simd-dot-product-in-rust

// https://doc.rust-lang.org/stable/core/arch/x86/fn._mm_extract_ps.html

// https://stackoverflow.com/questions/6996764/fastest-way-to-do-horizontal-sse-vector-sum-or-other-reduction/35270026#35270026

/// first experiment is to convert dot to SIMD
// pub fn dot(&self, rhs: &Vector) -> f32 {
//         self.x * rhs.x + self.y * rhs.y + self.z * rhs.z
//     }

// current impl
// #[derive(Debug, Clone)]
// pub struct Vector {
//     pub x: f32,
//     pub y: f32,
//     pub z: f32,
// }

// SIMD: AVX2 use 256 bit lanes = 32B
// a vector is 4B * 3 = 12B = 12 * 8 = 96b

use std::arch::x86_64::{__m128, _mm_mul_ps, _mm_set_ps};

// basic example - multiply numbers
fn simd_multiply() -> __m128 {
    unsafe {
        let x = _mm_set_ps(1.0, 2.0, 3.0, 4.0);
        let y = _mm_set_ps(4.0, 3.0, 2.0, 1.0);

        _mm_mul_ps(x, y)
    }
}

// let's represent a vector as a union with SIMD types
union Vector {
    f32x4: [f32; 4],
    m128: __m128,
}

// _mm256_dp_ps

#[cfg(test)]
mod tests {

    use crate::vector_simd::{simd_multiply, Vector};
    use std::arch::x86_64::{__m128, _mm_extract_ps, _mm_mul_ps, _mm_set_ps};

    #[test]
    fn simd_vector() {
        let v = Vector { f32x4: [1.0, 2.0, 3.0, 4.0] };

        // mem::transmute
        unsafe {
            assert_eq!([1.0, 2.0, 3.0, 4.0], v.f32x4);
            //assert_eq!(_mm_set_ps(1.0, 2.0, 3.0, 4.0), v.m128)
        }
    }
    
    #[test]
    fn example_simd_multiply() {
        unsafe {
            let foo = simd_multiply();

            let res_bits: i32 = _mm_extract_ps::<2>(foo);

            println!("Result : {:#b}", res_bits);

            let res = f32::from_bits(res_bits as u32);

            assert_eq!(6.0, res)
        }
    }
}
