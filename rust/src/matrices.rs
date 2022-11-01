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

mod test {
    use crate::matrices::M4x4;

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
}
