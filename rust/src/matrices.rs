use crate::utils::epsilon_eq;
use std::ops::Index;

struct M4x4([f32; 16]);

impl M4x4 {
    pub fn from_elements(
        [x0, y0, z0, w0]: [f32; 4],
        [x1, y1, z1, w1]: [f32; 4],
        [x2, y2, z2, w2]: [f32; 4],
        [x3, y3, z3, w3]: [f32; 4],
    ) -> M4x4 {
        M4x4([
            x0, y0, z0, w0, x1, y1, z1, w1, x2, y2, z2, w2, x3, y3, z3, w3,
        ])
    }
}

impl PartialEq<M4x4> for M4x4 {
    fn eq(&self, rhs: &M4x4) -> bool {
        for i in 0..16 {
            if !epsilon_eq(self.0[i], rhs.0[i]) {
                return false;
            }
        }

        true
    }
}

impl Index<usize> for M4x4 {
    type Output = f32;

    fn index(&self, idx: usize) -> &f32 {
        &self.0[idx]
    }
}

impl Index<(usize, usize)> for M4x4 {
    type Output = f32;

    fn index(&self, (idxi, idxj): (usize, usize)) -> &f32 {
        assert!(idxi < 4 && idxj < 4, "Matrix index out of bounds.");

        &self.0[idxj + (idxi * 4)]
    }
}

struct M2x2([f32; 4]);

impl M2x2 {
    pub fn from_elements([x0, y0]: [f32; 2], [x1, y1]: [f32; 2]) -> M2x2 {
        M2x2([x0, y0, x1, y1])
    }
}

impl Index<usize> for M2x2 {
    type Output = f32;

    fn index(&self, idx: usize) -> &f32 {
        &self.0[idx]
    }
}

impl Index<(usize, usize)> for M2x2 {
    type Output = f32;

    fn index(&self, (idxi, idxj): (usize, usize)) -> &f32 {
        assert!(idxi < 2 && idxj < 2, "Matrix index out of bounds.");

        &self.0[idxj + (idxi * 2)]
    }
}

struct M3x3([f32; 9]);

impl M3x3 {
    pub fn from_elements(
        [x0, y0, z0]: [f32; 3],
        [x1, y1, z1]: [f32; 3],
        [x2, y2, z2]: [f32; 3],
    ) -> M3x3 {
        M3x3([x0, y0, z0, x1, y1, z1, x2, y2, z2])
    }
}

impl Index<usize> for M3x3 {
    type Output = f32;

    fn index(&self, idx: usize) -> &f32 {
        &self.0[idx]
    }
}

impl Index<(usize, usize)> for M3x3 {
    type Output = f32;

    fn index(&self, (idxi, idxj): (usize, usize)) -> &f32 {
        assert!(idxi < 3 && idxj < 3, "Matrix index out of bounds.");

        &self.0[idxj + (idxi * 3)]
    }
}

mod test {
    use crate::matrices::{M2x2, M3x3, M4x4};

    // Scenario: Constructing and inspecting a 4x4 matrix
    // Given the following 4x4 matrix M:
    //   |  1   |  2   |  3   |  4   |
    //   |  5.5 |  6.5 |  7.5 |  8.5 |
    //   |  9   | 10   | 11   | 12   |
    //   | 13.5 | 14.5 | 15.5 | 16.5 |
    // Then M[0,0] = 1
    //   And M[0,3] = 4
    //   And M[1,0] = 5.5
    //   And M[1,2] = 7.5
    //   And M[2,2] = 11
    //   And M[3,0] = 13.5
    //   And M[3,2] = 15.5
    #[test]
    fn constructing_and_inspecting_a_4x4_matrix() {
        let m = M4x4::from_elements(
            [1.0, 2.0, 3.0, 4.0],
            [5.5, 6.5, 7.5, 8.5],
            [9.0, 10.0, 11.0, 12.0],
            [13.5, 14.5, 15.5, 16.5],
        );

        assert_eq!(m[(0, 0)], 1.0);
        assert_eq!(m[(0, 3)], 4.0);
        assert_eq!(m[(1, 0)], 5.5);
        assert_eq!(m[(1, 2)], 7.5);
        assert_eq!(m[(2, 2)], 11.0);
        assert_eq!(m[(3, 0)], 13.5);
        assert_eq!(m[(3, 2)], 15.5);
    }

    // Scenario: A 2x2 matrix ought to be representable
    // Given the following 2x2 matrix M:
    //   | -3 |  5 |
    //   |  1 | -2 |
    // Then M[0,0] = -3
    //   And M[0,1] = 5
    //   And M[1,0] = 1
    //   And M[1,1] = -2
    #[test]
    fn constructing_and_inspecting_a_2x2_matrix() {
        let m = M2x2::from_elements([-3.0, 5.0], [1.0, -2.0]);

        assert_eq!(m[(0, 0)], -3.0);
        assert_eq!(m[(0, 1)], 5.0);
        assert_eq!(m[(1, 0)], 1.0);
        assert_eq!(m[(1, 1)], -2.0);
    }

    // Scenario: A 3x3 matrix ought to be representable
    // Given the following 3x3 matrix M:
    //   | -3 |  5 |  0 |
    //   |  1 | -2 | -7 |
    //   |  0 |  1 |  1 |
    // Then M[0,0] = -3
    //   And M[1,1] = -2
    //   And M[2,2] = 1
    #[test]
    fn constructing_and_inspecting_a_3x3_matrix() {
        let m = M3x3::from_elements([-3.0, 5.0, 0.0], [1.0, -2.0, -7.0], [0.0, 1.0, 1.0]);

        assert_eq!(m[(0, 0)], -3.0);
        assert_eq!(m[(1, 1)], -2.0);
        assert_eq!(m[(2, 2)], 1.0);
    }

    // Scenario: Matrix equality with identical matrices
    // Given the following matrix A:
    //     | 1 | 2 | 3 | 4 |
    //     | 5 | 6 | 7 | 8 |
    //     | 9 | 8 | 7 | 6 |
    //     | 5 | 4 | 3 | 2 |
    //   And the following matrix B:
    //     | 1 | 2 | 3 | 4 |
    //     | 5 | 6 | 7 | 8 |
    //     | 9 | 8 | 7 | 6 |
    //     | 5 | 4 | 3 | 2 |
    // Then A = B
    #[test]
    fn matrix_equality_with_identical_matrices() {
        let a = M4x4::from_elements(
            [1.0, 2.0, 3.0, 4.0],
            [5.0, 6.0, 7.0, 8.0],
            [9.0, 8.0, 7.0, 6.0],
            [5.0, 4.0, 3.0, 2.0],
        );

        let b = M4x4::from_elements(
            [1.0, 2.0, 3.0, 4.0],
            [5.0, 6.0, 7.0, 8.0],
            [9.0, 8.0, 7.0, 6.0],
            [5.0, 4.0, 3.0, 2.0],
        );

        assert_eq!(a == b, true);
    }

    // Scenario: Matrix equality with different matrices
    // Given the following matrix A:
    //     | 1 | 2 | 3 | 4 |
    //     | 5 | 6 | 7 | 8 |
    //     | 9 | 8 | 7 | 6 |
    //     | 5 | 4 | 3 | 2 |
    //   And the following matrix B:
    //     | 2 | 3 | 4 | 5 |
    //     | 6 | 7 | 8 | 9 |
    //     | 8 | 7 | 6 | 5 |
    //     | 4 | 3 | 2 | 1 |
    // Then A != B
    #[test]
    fn matrix_equality_with_different_matrices() {
        let a = M4x4::from_elements(
            [1.0, 2.0, 3.0, 4.0],
            [5.0, 6.0, 7.0, 8.0],
            [9.0, 8.0, 7.0, 6.0],
            [5.0, 4.0, 3.0, 2.0],
        );

        let b = M4x4::from_elements(
            [2.0, 3.0, 4.0, 5.0],
            [6.0, 7.0, 8.0, 9.0],
            [8.0, 7.0, 6.0, 5.0],
            [4.0, 3.0, 2.0, 1.0],
        );

        assert_eq!(a == b, false);
    }
}
