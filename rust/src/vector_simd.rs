// Resoruces

// https://docs.rs/rustc-std-workspace-std/1.0.1/std/arch/x86_64/struct.__m128.html
// https://stackoverflow.com/questions/65984936/efficient-simd-dot-product-in-rust

// https://doc.rust-lang.org/stable/core/arch/x86/fn._mm_extract_ps.html

// https://stackoverflow.com/questions/6996764/fastest-way-to-do-horizontal-sse-vector-sum-or-other-reduction/35270026#35270026

/// first experiment is to convert dot to SIMD
// pub fn dot(&self, rhs: &Vector) -> f32 {
//         self.x * rhs.x + self.y * rhs.y + self.z * rhs.z
//     }

// SIMD: AVX2 use 256 bit lanes = 32B
// a vector is 4B * 3 = 12B = 12 * 8 = 96b
use std::arch::x86_64::{
    __m128, _mm_add_ps, _mm_cvtss_f32, _mm_dp_ps, _mm_extract_ps, _mm_mul_ps, _mm_set_ps,
    _mm_shuffle_ps,
};

// let's represent a vector as a union with SIMD types
// size 128b (16B), 0.25 cache lines
pub union Vector {
    pub f32x4: [f32; 4],
    m128: __m128,
}
// @NOTE: vectors have w=0, points have w=1

// basic example - multiply numbers
fn simd_multiply() -> __m128 {
    unsafe {
        let x = _mm_set_ps(1.0, 2.0, 3.0, 4.0);
        let y = _mm_set_ps(4.0, 3.0, 2.0, 1.0);

        _mm_mul_ps(x, y)
    }
}

impl Vector {
    #[inline(always)]
    pub fn simd_dot(&self, rhs: &Vector) -> f32 {
        // lhs.xyzw * rhs.xyzw
        unsafe {
            // Multiplies __m128 vectors
            //let res = _mm_mul_ps(lhs.m128, rhs.m128);

            // Horizontally adds adjacent pairs of single-precision (32-bit)
            // _mm_hadd_ps(a: __m128, b: __m128)

            //let res = _mm_dp_ps(lhs.m128, rhs.m128, 0x71);

            //let res = _mm_mul_ps(self.m128, rhs.m128);

            //let res_bits: i32 = _mm_extract_ps::<0>(res);
            //f32::from_bits(res_bits as u32)
            let x2_y2_z2_w2 = _mm_mul_ps(self.m128, rhs.m128);
            let z2_w2_0_0 = _mm_shuffle_ps(x2_y2_z2_w2, x2_y2_z2_w2, 0b00_00_11_10);
            let x2z2_y2w2_0_0 = _mm_add_ps(x2_y2_z2_w2, z2_w2_0_0);
            let y2w2_0_0_0 = _mm_shuffle_ps(x2z2_y2w2_0_0, x2z2_y2w2_0_0, 0b00_00_00_01);
            _mm_cvtss_f32(_mm_add_ps(x2z2_y2w2_0_0, y2w2_0_0_0))
        }
    }
}
// __m128 shuf   = _mm_shuffle_ps(r1, r1, _MM_SHUFFLE(2, 3, 0, 1));
// __m128 sums   = _mm_add_ps(r1, shuf);
// shuf          = _mm_movehl_ps(shuf, sums);
// sums          = _mm_add_ss(sums, shuf);
// float result =  _mm_cvtss_f32(sums);

// _mm256_dp_ps

#[cfg(test)]
mod tests {

    use crate::vector_simd::{simd_multiply, Vector};
    use std::arch::x86_64::{__m128, _mm_extract_ps, _mm_mul_ps, _mm_set_ps};
    use std::mem::transmute;

    // Scenario: The dot product of two tuples
    // Given a ← vector(1, 2, 3)
    //   And b ← vector(2, 3, 4)
    // Then dot(a, b) = 20
    #[test]
    fn test_simd_dot() {
        let x = Vector {
            f32x4: [1.0, 2.0, 3.0, 0.0],
        };
        let y = Vector {
            f32x4: [2.0, 3.0, 4.0, 0.0],
        };

        let r = x.simd_dot(&y);

        assert_eq!(20.0, r)
    }

    // #[test]
    // fn the_dot_product_of_two_tuples() {
    //     let a = Vector::new(1.0, 2.0, 3.0);
    //     let b = Vector::new(2.0, 3.0, 4.0);

    //     assert_eq!(a.dot(&b), 20.0);
    // }

    #[test]
    fn simd_vector() {
        let v = Vector {
            f32x4: [1.0, 2.0, 3.0, 4.0],
        };

        unsafe {
            let x: [f32; 4] = transmute(v.m128);

            assert_eq!([1.0, 2.0, 3.0, 4.0], v.f32x4);
            assert_eq!([1.0, 2.0, 3.0, 4.0], x)
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
