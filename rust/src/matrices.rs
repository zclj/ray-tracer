use crate::utils::epsilon_eq;
use crate::vector::{Point, Vector};
use std::ops::{Index, Mul, MulAssign};

////////////////////////////////////////
// M4x4

#[derive(Debug)]
pub struct M4x4(pub [f32; 16]);

impl M4x4 {
    pub const IDENTITY: Self = Self::from_elements(
        [1.0, 0.0, 0.0, 0.0],
        [0.0, 1.0, 0.0, 0.0],
        [0.0, 0.0, 1.0, 0.0],
        [0.0, 0.0, 0.0, 1.0],
    );

    #[must_use]
    pub const fn from_elements(
        [x0, y0, z0, w0]: [f32; 4],
        [x1, y1, z1, w1]: [f32; 4],
        [x2, y2, z2, w2]: [f32; 4],
        [x3, y3, z3, w3]: [f32; 4],
    ) -> M4x4 {
        M4x4([
            x0, y0, z0, w0, x1, y1, z1, w1, x2, y2, z2, w2, x3, y3, z3, w3,
        ])
    }

    #[rustfmt::skip]
    #[must_use]
    pub fn transpose(&self) -> Self {
        M4x4([
            self[(0,0)], self[(1,0)], self[(2,0)], self[(3,0)],
            self[(0,1)], self[(1,1)], self[(2,1)], self[(3,1)],
            self[(0,2)], self[(1,2)], self[(2,2)], self[(3,2)],
            self[(0,3)], self[(1,3)], self[(2,3)], self[(3,3)],
        ])
    }

    /// # Panics
    ///
    /// Will panic if row or column is out of bounds
    #[must_use]
    pub fn sub_matrix(&self, row: u8, column: u8) -> M3x3 {
        let [x0, y0, z0, w0, x1, y1, z1, w1, x2, y2, z2, w2, x3, y3, z3, w3] = self.0;

        let a = match row {
            0 => [x1, y1, z1, w1, x2, y2, z2, w2, x3, y3, z3, w3],
            1 => [x0, y0, z0, w0, x2, y2, z2, w2, x3, y3, z3, w3],
            2 => [x0, y0, z0, w0, x1, y1, z1, w1, x3, y3, z3, w3],
            3 => [x0, y0, z0, w0, x1, y1, z1, w1, x2, y2, z2, w2],
            _ => panic!("row out of bounds"),
        };

        let [x0, y0, z0, w0, x1, y1, z1, w1, x2, y2, z2, w2] = a;

        let b = match column {
            0 => [y0, z0, w0, y1, z1, w1, y2, z2, w2],
            1 => [x0, z0, w0, x1, z1, w1, x2, z2, w2],
            2 => [x0, y0, w0, x1, y1, w1, x2, y2, w2],
            3 => [x0, y0, z0, x1, y1, z1, x2, y2, z2],
            _ => panic!("column out of bounds"),
        };

        M3x3(b)
    }

    fn minor(&self, row: u8, column: u8) -> f32 {
        self.sub_matrix(row, column).determinant()
    }

    fn cofactor(&self, row: u8, column: u8) -> f32 {
        let minor = self.minor(row, column);

        // negate if the sum is odd
        if (row + column) % 2 == 0 {
            minor
        } else {
            -minor
        }
    }

    #[must_use]
    pub fn determinant(&self) -> f32 {
        (self[(0, 0)] * self.cofactor(0, 0))
            + (self[(0, 1)] * self.cofactor(0, 1))
            + (self[(0, 2)] * self.cofactor(0, 2))
            + (self[(0, 3)] * self.cofactor(0, 3))
    }

    fn is_invertible(&self) -> bool {
        self.determinant() != 0.0
    }

    /// # Panics
    ///
    /// Will panic if matrix is not invertible
    #[must_use]
    pub fn inverse(&self) -> Self {
        assert!(self.is_invertible());

        let mut m: [f32; 16] = [0.0; 16];

        for row in 0..4 {
            for col in 0..4 {
                let c = self.cofactor(row, col);
                m[(col * 4) as usize + row as usize] = c / self.determinant();
            }
        }

        M4x4(m)
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

    fn index(&self, (idx_row, idx_col): (usize, usize)) -> &f32 {
        assert!(idx_row < 4 && idx_col < 4, "Matrix index out of bounds.");

        &self.0[idx_col + (idx_row * 4)]
    }
}

impl Mul<&M4x4> for &M4x4 {
    type Output = M4x4;

    fn mul(self, rhs: &M4x4) -> M4x4 {
        let mut m: [f32; 16] = [0.0; 16];

        for i in 0..4 {
            for j in 0..4 {
                for k in 0..4 {
                    m[j + (i * 4)] += self[(i, k)] * rhs[(k, j)];
                }
            }
        }
        M4x4(m)
    }
}

impl Mul<&Point> for &M4x4 {
    type Output = Point;

    fn mul(self, rhs: &Point) -> Point {
        Point::new(
            self[(0, 0)] * rhs.x + self[(0, 1)] * rhs.y + self[(0, 2)] * rhs.z + self[(0, 3)],
            self[(1, 0)] * rhs.x + self[(1, 1)] * rhs.y + self[(1, 2)] * rhs.z + self[(1, 3)],
            self[(2, 0)] * rhs.x + self[(2, 1)] * rhs.y + self[(2, 2)] * rhs.z + self[(2, 3)],
        )
    }
}

impl Mul<&Vector> for &M4x4 {
    type Output = Vector;

    fn mul(self, rhs: &Vector) -> Vector {
        Vector::new(
            self[(0, 0)] * rhs.x + self[(0, 1)] * rhs.y + self[(0, 2)] * rhs.z,
            self[(1, 0)] * rhs.x + self[(1, 1)] * rhs.y + self[(1, 2)] * rhs.z,
            self[(2, 0)] * rhs.x + self[(2, 1)] * rhs.y + self[(2, 2)] * rhs.z,
        )
    }
}

impl MulAssign<&M4x4> for M4x4 {
    fn mul_assign(&mut self, rhs: &M4x4) {
        *self = &*self * rhs;
    }
}

////////////////////////////////////////
// M2x2

#[derive(Debug)]
pub struct M2x2([f32; 4]);

impl M2x2 {
    #[must_use]
    pub fn from_elements([x0, y0]: [f32; 2], [x1, y1]: [f32; 2]) -> M2x2 {
        M2x2([x0, y0, x1, y1])
    }

    fn determinant(&self) -> f32 {
        self[(0, 0)] * self[(1, 1)] - self[(0, 1)] * self[(1, 0)]
    }
}

impl PartialEq<M2x2> for M2x2 {
    fn eq(&self, rhs: &M2x2) -> bool {
        for i in 0..4 {
            if !epsilon_eq(self.0[i], rhs.0[i]) {
                return false;
            }
        }

        true
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

    fn index(&self, (idx_row, idx_col): (usize, usize)) -> &f32 {
        assert!(idx_row < 2 && idx_col < 2, "Matrix index out of bounds.");

        &self.0[idx_col + (idx_row * 2)]
    }
}

#[derive(Debug)]
pub struct M3x3([f32; 9]);

impl M3x3 {
    #[must_use]
    pub fn from_elements(
        [x0, y0, z0]: [f32; 3],
        [x1, y1, z1]: [f32; 3],
        [x2, y2, z2]: [f32; 3],
    ) -> M3x3 {
        M3x3([x0, y0, z0, x1, y1, z1, x2, y2, z2])
    }

    fn sub_matrix(&self, row: u8, column: u8) -> M2x2 {
        let [x0, y0, z0, x1, y1, z1, x2, y2, z2] = self.0;

        let a = match row {
            0 => [x1, y1, z1, x2, y2, z2],
            1 => [x0, y0, z0, x2, y2, z2],
            2 => [x0, y0, z0, x1, y1, z1],
            _ => panic!("row out of bounds"),
        };

        let [x0, y0, z0, x1, y1, z1] = a;

        let b = match column {
            0 => [y0, z0, y1, z1],
            1 => [x0, z0, x1, z1],
            2 => [x0, y0, x1, y1],
            _ => panic!("column out of bounds"),
        };

        M2x2(b)
    }

    fn minor(&self, row: u8, column: u8) -> f32 {
        self.sub_matrix(row, column).determinant()
    }

    fn cofactor(&self, row: u8, column: u8) -> f32 {
        let minor = self.minor(row, column);

        // negate if the sum if odd
        if (row + column) % 2 == 0 {
            minor
        } else {
            -minor
        }
    }

    fn determinant(&self) -> f32 {
        (self[(0, 0)] * self.cofactor(0, 0))
            + (self[(0, 1)] * self.cofactor(0, 1))
            + (self[(0, 2)] * self.cofactor(0, 2))
    }
}

impl PartialEq<M3x3> for M3x3 {
    fn eq(&self, rhs: &M3x3) -> bool {
        for i in 0..9 {
            if !epsilon_eq(self.0[i], rhs.0[i]) {
                return false;
            }
        }

        true
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

    fn index(&self, (idx_row, idx_col): (usize, usize)) -> &f32 {
        assert!(idx_row < 3 && idx_col < 3, "Matrix index out of bounds.");

        &self.0[idx_col + (idx_row * 3)]
    }
}

#[cfg(test)]
mod test {
    use crate::matrices::{M2x2, M3x3, M4x4};
    use crate::vector::{Point, Vector};

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

    // Scenario: Multiplying two matrices
    // Given the following matrix A:
    //     | 1 | 2 | 3 | 4 |
    //     | 5 | 6 | 7 | 8 |
    //     | 9 | 8 | 7 | 6 |
    //     | 5 | 4 | 3 | 2 |
    //   And the following matrix B:
    //     | -2 | 1 | 2 |  3 |
    //     |  3 | 2 | 1 | -1 |
    //     |  4 | 3 | 6 |  5 |
    //     |  1 | 2 | 7 |  8 |
    // Then A * B is the following 4x4 matrix:
    //     | 20|  22 |  50 |  48 |
    //     | 44|  54 | 114 | 108 |
    //     | 40|  58 | 110 | 102 |
    //     | 16|  26 |  46 |  42 |
    #[test]
    fn multiplying_two_matrices() {
        let a = M4x4::from_elements(
            [1.0, 2.0, 3.0, 4.0],
            [5.0, 6.0, 7.0, 8.0],
            [9.0, 8.0, 7.0, 6.0],
            [5.0, 4.0, 3.0, 2.0],
        );

        let b = M4x4::from_elements(
            [-2.0, 1.0, 2.0, 3.0],
            [3.0, 2.0, 1.0, -1.0],
            [4.0, 3.0, 6.0, 5.0],
            [1.0, 2.0, 7.0, 8.0],
        );

        let c = M4x4::from_elements(
            [20.0, 22.0, 50.0, 48.0],
            [44.0, 54.0, 114.0, 108.0],
            [40.0, 58.0, 110.0, 102.0],
            [16.0, 26.0, 46.0, 42.0],
        );

        assert_eq!(&a * &b, c);
    }

    #[test]
    fn multiplying_assign_matrices() {
        let mut a = M4x4::from_elements(
            [1.0, 2.0, 3.0, 4.0],
            [5.0, 6.0, 7.0, 8.0],
            [9.0, 8.0, 7.0, 6.0],
            [5.0, 4.0, 3.0, 2.0],
        );

        let b = M4x4::from_elements(
            [-2.0, 1.0, 2.0, 3.0],
            [3.0, 2.0, 1.0, -1.0],
            [4.0, 3.0, 6.0, 5.0],
            [1.0, 2.0, 7.0, 8.0],
        );

        let c = M4x4::from_elements(
            [20.0, 22.0, 50.0, 48.0],
            [44.0, 54.0, 114.0, 108.0],
            [40.0, 58.0, 110.0, 102.0],
            [16.0, 26.0, 46.0, 42.0],
        );

        a *= &b;

        assert_eq!(a, c);
    }

    use glam::Mat4;

    #[test]
    fn glam_4x4_multiplication() {
        // let a = Mat4::from_cols_array_2d(
        //     &[[1.0, 2.0, 3.0, 4.0],
        //       [5.0, 6.0, 7.0, 8.0],
        //       [9.0, 8.0, 7.0, 6.0],
        //       [5.0, 4.0, 3.0, 2.0]]);

        let a = Mat4::from_cols_array_2d(&[
            [1.0, 5.0, 9.0, 5.0],
            [2.0, 6.0, 8.0, 4.0],
            [3.0, 7.0, 7.0, 3.0],
            [4.0, 8.0, 6.0, 2.0],
        ]);

        // let b = Mat4::from_cols_array_2d(
        //     &[[-2.0, 1.0, 2.0, 3.0],
        //       [3.0, 2.0, 1.0, -1.0],
        //       [4.0, 3.0, 6.0, 5.0],
        //       [1.0, 2.0, 7.0, 8.0]],
        // );

        let b = Mat4::from_cols_array_2d(&[
            [-2.0, 3.0, 4.0, 1.0],
            [1.0, 2.0, 3.0, 2.0],
            [2.0, 1.0, 6.0, 7.0],
            [3.0, -1.0, 5.0, 8.0],
        ]);

        // let c = Mat4::from_cols_array_2d(
        //     &[[20.0, 22.0, 50.0, 48.0],
        //       [44.0, 54.0, 114.0, 108.0],
        //       [40.0, 58.0, 110.0, 102.0],
        //       [16.0, 26.0, 46.0, 42.0]],
        // );

        let c = Mat4::from_cols_array_2d(&[
            [20.0, 44.0, 40.0, 16.0],
            [22.0, 54.0, 58.0, 26.0],
            [50.0, 114.0, 110.0, 46.0],
            [48.0, 108.0, 102.0, 42.0],
        ]);

        assert_eq!(a * b, c);
    }

    // Scenario: A matrix multiplied by a tuple
    // Given the following matrix A:
    //     | 1 | 2 | 3 | 4 |
    //     | 2 | 4 | 4 | 2 |
    //     | 8 | 6 | 4 | 1 |
    //     | 0 | 0 | 0 | 1 |
    //   And b ← tuple(1, 2, 3, 1)
    // Then A * b = tuple(18, 24, 33, 1)
    #[test]
    fn a_matrix_multiplied_by_a_point() {
        let a = M4x4::from_elements(
            [1.0, 2.0, 3.0, 4.0],
            [2.0, 4.0, 4.0, 2.0],
            [8.0, 6.0, 4.0, 1.0],
            [0.0, 0.0, 0.0, 1.0],
        );

        let b = Point::new(1.0, 2.0, 3.0);

        assert_eq!(&a * &b, Point::new(18.0, 24.0, 33.0));
    }

    #[test]
    fn a_matrix_multiplied_by_a_vector() {
        let a = M4x4::from_elements(
            [1.0, 2.0, 3.0, 4.0],
            [2.0, 4.0, 4.0, 2.0],
            [8.0, 6.0, 4.0, 1.0],
            [0.0, 0.0, 0.0, 1.0],
        );

        let b = Vector::new(1.0, 2.0, 3.0);

        assert_eq!(&a * &b, Vector::new(14.0, 22.0, 32.0));
    }

    // Scenario: Multiplying a matrix by the identity matrix
    // Given the following matrix A:
    //   | 0 | 1 |  2 |  4 |
    //   | 1 | 2 |  4 |  8 |
    //   | 2 | 4 |  8 | 16 |
    //   | 4 | 8 | 16 | 32 |
    // Then A * identity_matrix = A
    #[test]
    fn multiplying_a_matrix_by_the_identity_matrix() {
        let a = M4x4::from_elements(
            [0.0, 1.0, 2.0, 4.0],
            [1.0, 2.0, 4.0, 8.0],
            [2.0, 4.0, 8.0, 16.0],
            [4.0, 8.0, 16.0, 32.0],
        );

        assert_eq!(&a * &M4x4::IDENTITY, a);
    }

    // Scenario: Multiplying the identity matrix by a tuple
    // Given a ← tuple(1, 2, 3, 4)
    // Then identity_matrix * a = a
    #[test]
    fn multiplying_the_identity_matrix_by_a_tuple() {
        let a = Point::new(1.0, 2.0, 3.0);

        assert_eq!(&M4x4::IDENTITY * &a, a);
    }

    // Scenario: Transposing a matrix
    // Given the following matrix A:
    //   | 0 | 9 | 3 | 0 |
    //   | 9 | 8 | 0 | 8 |
    //   | 1 | 8 | 5 | 3 |
    //   | 0 | 0 | 5 | 8 |
    // Then transpose(A) is the following matrix:
    //   | 0 | 9 | 1 | 0 |
    //   | 9 | 8 | 8 | 0 |
    //   | 3 | 0 | 5 | 5 |
    //   | 0 | 8 | 3 | 8 |
    #[test]
    fn transposing_a_matrix() {
        let a = M4x4::from_elements(
            [0.0, 9.0, 3.0, 0.0],
            [9.0, 8.0, 0.0, 8.0],
            [1.0, 8.0, 5.0, 3.0],
            [0.0, 0.0, 5.0, 8.0],
        );

        let b = M4x4::from_elements(
            [0.0, 9.0, 1.0, 0.0],
            [9.0, 8.0, 8.0, 0.0],
            [3.0, 0.0, 5.0, 5.0],
            [0.0, 8.0, 3.0, 8.0],
        );

        assert_eq!(a.transpose(), b)
    }

    // Scenario: Transposing the identity matrix
    // Given A ← transpose(identity_matrix)
    // Then A = identity_matrix
    #[test]
    fn transposing_the_identity_matrix() {
        assert_eq!(M4x4::IDENTITY.transpose(), M4x4::IDENTITY)
    }

    // Scenario: Calculating the determinant of a 2x2 matrix
    // Given the following 2x2 matrix A:
    //   |  1 | 5 |
    //   | -3 | 2 |
    // Then determinant(A) = 17
    #[test]
    fn calculating_the_determinant_of_a_2x2_matrix() {
        let a = M2x2::from_elements([1.0, 5.0], [-3.0, 2.0]);

        assert_eq!(a.determinant(), 17.0)
    }

    // Scenario: A submatrix of a 3x3 matrix is a 2x2 matrix
    // Given the following 3x3 matrix A:
    //   |  1 | 5 |  0 |
    //   | -3 | 2 |  7 |
    //   |  0 | 6 | -3 |
    // Then submatrix(A, 0, 2) is the following 2x2 matrix:
    //   | -3 | 2 |
    //   |  0 | 6 |
    #[test]
    fn a_submatrix_of_a_3x3_matrix_is_a_2x2_matrix() {
        let a = M3x3::from_elements([1.0, 5.0, 0.0], [-3.0, 2.0, 7.0], [0.0, 6.0, -3.0]);

        let b = M2x2::from_elements([-3.0, 2.0], [0.0, 6.0]);

        assert_eq!(a.sub_matrix(0, 2), b)
    }

    // Scenario: A submatrix of a 4x4 matrix is a 3x3 matrix
    // Given the following 4x4 matrix A:
    //   | -6 |  1 |  1 |  6 |
    //   | -8 |  5 |  8 |  6 |
    //   | -1 |  0 |  8 |  2 |
    //   | -7 |  1 | -1 |  1 |
    // Then submatrix(A, 2, 1) is the following 3x3 matrix:
    //   | -6 |  1 | 6 |
    //   | -8 |  8 | 6 |
    //   | -7 | -1 | 1 |
    #[test]
    fn a_submatrix_of_a_4x4_matrix_is_a_3x3_matrix() {
        let a = M4x4::from_elements(
            [-6.0, 1.0, 1.0, 6.0],
            [-8.0, 5.0, 8.0, 6.0],
            [-1.0, 0.0, 8.0, 2.0],
            [-7.0, 1.0, -1.0, 1.0],
        );

        let b = M3x3::from_elements([-6.0, 1.0, 6.0], [-8.0, 8.0, 6.0], [-7.0, -1.0, 1.0]);

        assert_eq!(a.sub_matrix(2, 1), b)
    }

    // Scenario: Calculating a minor of a 3x3 matrix
    // Given the following 3x3 matrix A:
    //     |  3 |  5 |  0 |
    //     |  2 | -1 | -7 |
    //     |  6 | -1 |  5 |
    //   And B ← submatrix(A, 1, 0)
    // Then determinant(B) = 25
    //   And minor(A, 1, 0) = 25
    #[test]
    #[rustfmt::skip]
    fn calculating_a_minor_of_a_3x3_matrix() {
        let a = M3x3::from_elements([3.0, 5.0, 0.0],
                                    [2.0, -1.0, -7.0],
                                    [6.0, -1.0, 5.0]);

        let b = a.sub_matrix(1,0);

        let det = b.determinant();
        let min = a.minor(1,0);

        assert_eq!(det, 25.0);
        assert_eq!(min, 25.0)
    }

    // Scenario: Calculating a cofactor of a 3x3 matrix
    // Given the following 3x3 matrix A:
    //     |  3 |  5 |  0 |
    //     |  2 | -1 | -7 |
    //     |  6 | -1 |  5 |
    // Then minor(A, 0, 0) = -12
    //   And cofactor(A, 0, 0) = -12
    //   And minor(A, 1, 0) = 25
    //   And cofactor(A, 1, 0) = -25
    #[test]
    #[rustfmt::skip]
    fn calculating_a_cofactor_of_a_3x3_matrix() {
        let a = M3x3::from_elements([3.0, 5.0, 0.0],
                                    [2.0, -1.0, -7.0],
                                    [6.0, -1.0, 5.0]);

        let min1 = a.minor(0,0);
        let cof1 = a.cofactor(0,0);
        let min2 = a.minor(1,0);
        let cof2 = a.cofactor(1,0);

        assert_eq!(min1, -12.0);
        assert_eq!(min2, 25.0);
        assert_eq!(cof1, -12.0);
        assert_eq!(cof2, -25.0);
    }

    // Scenario: Calculating the determinant of a 3x3 matrix
    // Given the following 3x3 matrix A:
    //   |  1 |  2 |  6 |
    //   | -5 |  8 | -4 |
    //   |  2 |  6 |  4 |
    // Then cofactor(A, 0, 0) = 56
    //   And cofactor(A, 0, 1) = 12
    //   And cofactor(A, 0, 2) = -46
    //   And determinant(A) = -196
    #[test]
    #[rustfmt::skip]
    fn calculating_the_determinant_of_a_3x3_matrix() {
        let a = M3x3::from_elements([1.0, 2.0, 6.0],
                                    [-5.0, 8.0, -4.0],
                                    [2.0, 6.0, 4.0]);

        let cof1 = a.cofactor(0,0);
        let cof2 = a.cofactor(0,1);
        let cof3 = a.cofactor(0,2);
        let det = a.determinant();

        assert_eq!(cof1, 56.0);
        assert_eq!(cof2, 12.0);
        assert_eq!(cof3, -46.0);
        assert_eq!(det, -196.0);
    }

    // Scenario: Calculating the determinant of a 4x4 matrix
    // Given the following 4x4 matrix A:
    //   | -2 | -8 |  3 |  5 |
    //   | -3 |  1 |  7 |  3 |
    //   |  1 |  2 | -9 |  6 |
    //   | -6 |  7 |  7 | -9 |
    // Then cofactor(A, 0, 0) = 690
    //   And cofactor(A, 0, 1) = 447
    //   And cofactor(A, 0, 2) = 210
    //   And cofactor(A, 0, 3) = 51
    //   And determinant(A) = -4071
    #[test]
    #[rustfmt::skip]
    fn calculating_the_determinant_of_a_4x4_matrix() {
        let a = M4x4::from_elements(
            [-2.0, -8.0, 3.0, 5.0],
            [-3.0, 1.0, 7.0, 3.0],
            [1.0, 2.0, -9.0, 6.0],
            [-6.0, 7.0, 7.0, -9.0],
        );

        let cof1 = a.cofactor(0,0);
        let cof2 = a.cofactor(0,1);
        let cof3 = a.cofactor(0,2);
        let cof4 = a.cofactor(0,3);
        let det = a.determinant();

        assert_eq!(cof1, 690.0);
        assert_eq!(cof2, 447.0);
        assert_eq!(cof3, 210.0);
        assert_eq!(cof4, 51.0);
        assert_eq!(det, -4071.0);
    }

    // Scenario: Testing an invertible matrix for invertibility
    // Given the following 4x4 matrix A:
    //   |  6 |  4 |  4 |  4 |
    //   |  5 |  5 |  7 |  6 |
    //   |  4 | -9 |  3 | -7 |
    //   |  9 |  1 |  7 | -6 |
    // Then determinant(A) = -2120
    //   And A is invertible
    #[test]
    fn testing_an_invertible_matrix_for_invertibility() {
        let a = M4x4::from_elements(
            [6.0, 4.0, 4.0, 4.0],
            [5.0, 5.0, 7.0, 6.0],
            [4.0, -9.0, 3.0, -7.0],
            [9.0, 1.0, 7.0, -6.0],
        );

        let det = a.determinant();

        assert_eq!(det, -2120.0);
        assert_eq!(a.is_invertible(), true);
    }

    // Scenario: Testing a noninvertible matrix for invertibility
    // Given the following 4x4 matrix A:
    //   | -4 |  2 | -2 | -3 |
    //   |  9 |  6 |  2 |  6 |
    //   |  0 | -5 |  1 | -5 |
    //   |  0 |  0 |  0 |  0 |
    // Then determinant(A) = 0
    //   And A is not invertible
    #[test]
    fn testing_a_noninvertible_matrix_for_invertibility() {
        let a = M4x4::from_elements(
            [-4.0, 2.0, -2.0, -3.0],
            [9.0, 6.0, 2.0, 6.0],
            [0.0, -5.0, 1.0, -5.0],
            [0.0, 0.0, 0.0, 0.0],
        );

        let det = a.determinant();

        assert_eq!(det, 0.0);
        assert_eq!(a.is_invertible(), false);
    }

    // Scenario: Calculating the inverse of a matrix
    // Given the following 4x4 matrix A:
    //     | -5 |  2 |  6 | -8 |
    //     |  1 | -5 |  1 |  8 |
    //     |  7 |  7 | -6 | -7 |
    //     |  1 | -3 |  7 |  4 |
    //   And B ← inverse(A)
    // Then determinant(A) = 532
    //   And cofactor(A, 2, 3) = -160
    //   And B[3,2] = -160/532
    //   And cofactor(A, 3, 2) = 105
    //   And B[2,3] = 105/532
    //   And B is the following 4x4 matrix:
    //     |  0.21805 |  0.45113 |  0.24060 | -0.04511 |
    //     | -0.80827 | -1.45677 | -0.44361 |  0.52068 |
    //     | -0.07895 | -0.22368 | -0.05263 |  0.19737 |
    //     | -0.52256 | -0.81391 | -0.30075 |  0.30639 |
    #[test]
    fn calculating_the_inverse_of_a_matrix() {
        let a = M4x4::from_elements(
            [-5.0, 2.0, 6.0, -8.0],
            [1.0, -5.0, 1.0, 8.0],
            [7.0, 7.0, -6.0, -7.0],
            [1.0, -3.0, 7.0, 4.0],
        );

        let b_cmp = M4x4::from_elements(
            [0.21805, 0.45113, 0.24060, -0.04511],
            [-0.80827, -1.45677, -0.44361, 0.52068],
            [-0.07895, -0.22368, -0.05263, 0.19737],
            [-0.52256, -0.81391, -0.30075, 0.30639],
        );

        let b = a.inverse();

        let det = a.determinant();
        let cof1 = a.cofactor(2, 3);
        let cof2 = a.cofactor(3, 2);

        assert_eq!(det, 532.0);
        assert_eq!(cof1, -160.0);
        assert_eq!(cof2, 105.0);
        assert_eq!(b[(3, 2)], -160.0 / 532.0);
        assert_eq!(b[(2, 3)], 105.0 / 532.0);
        assert_eq!(b, b_cmp);
    }

    // Scenario: Calculating the inverse of another matrix
    // Given the following 4x4 matrix A:
    //   |  8 | -5 |  9 |  2 |
    //   |  7 |  5 |  6 |  1 |
    //   | -6 |  0 |  9 |  6 |
    //   | -3 |  0 | -9 | -4 |
    // Then inverse(A) is the following 4x4 matrix:
    //   | -0.15385 | -0.15385 | -0.28205 | -0.53846 |
    //   | -0.07692 |  0.12308 |  0.02564 |  0.03077 |
    //   |  0.35897 |  0.35897 |  0.43590 |  0.92308 |
    //   | -0.69231 | -0.69231 | -0.76923 | -1.92308 |
    #[test]
    fn calculating_the_inverse_of_another_matrix() {
        let a = M4x4::from_elements(
            [8.0, -5.0, 9.0, 2.0],
            [7.0, 5.0, 6.0, 1.0],
            [-6.0, 0.0, 9.0, 6.0],
            [-3.0, 0.0, -9.0, -4.0],
        );

        let b_cmp = M4x4::from_elements(
            [-0.15385, -0.15385, -0.28205, -0.53846],
            [-0.07692, 0.12308, 0.02564, 0.03077],
            [0.35897, 0.35897, 0.43590, 0.92308],
            [-0.69231, -0.69231, -0.76923, -1.92308],
        );

        let b = a.inverse();

        assert_eq!(b, b_cmp);
    }

    // Scenario: Calculating the inverse of a third matrix
    // Given the following 4x4 matrix A:
    //   |  9 |  3 |  0 |  9 |
    //   | -5 | -2 | -6 | -3 |
    //   | -4 |  9 |  6 |  4 |
    //   | -7 |  6 |  6 |  2 |
    // Then inverse(A) is the following 4x4 matrix:
    //   | -0.04074 | -0.07778 |  0.14444 | -0.22222 |
    //   | -0.07778 |  0.03333 |  0.36667 | -0.33333 |
    //   | -0.02901 | -0.14630 | -0.10926 |  0.12963 |
    //   |  0.17778 |  0.06667 | -0.26667 |  0.33333 |
    #[test]
    fn calculating_the_inverse_of_a_third_matrix() {
        let a = M4x4::from_elements(
            [9.0, 3.0, 0.0, 9.0],
            [-5.0, -2.0, -6.0, -3.0],
            [-4.0, 9.0, 6.0, 4.0],
            [-7.0, 6.0, 6.0, 2.0],
        );

        let b_cmp = M4x4::from_elements(
            [-0.04074, -0.07778, 0.14444, -0.22222],
            [-0.07778, 0.03333, 0.36667, -0.33333],
            [-0.02901, -0.14630, -0.10926, 0.12963],
            [0.17778, 0.06667, -0.26667, 0.33333],
        );

        let b = a.inverse();

        assert_eq!(b, b_cmp);
    }

    // Scenario: Multiplying a product by its inverse
    // Given the following 4x4 matrix A:
    //     |  3 | -9 |  7 |  3 |
    //     |  3 | -8 |  2 | -9 |
    //     | -4 |  4 |  4 |  1 |
    //     | -6 |  5 | -1 |  1 |
    //   And the following 4x4 matrix B:
    //     |  8 |  2 |  2 |  2 |
    //     |  3 | -1 |  7 |  0 |
    //     |  7 |  0 |  5 |  4 |
    //     |  6 | -2 |  0 |  5 |
    //   And C ← A * B
    // Then C * inverse(B) = A
    #[test]
    fn multiplying_a_product_by_its_inverse() {
        let a = M4x4::from_elements(
            [3.0, -9.0, 7.0, 3.0],
            [3.0, -8.0, 2.0, -9.0],
            [-4.0, 4.0, 4.0, 1.0],
            [-6.0, 5.0, -1.0, 1.0],
        );

        let b = M4x4::from_elements(
            [8.0, 2.0, 2.0, 2.0],
            [3.0, -1.0, 7.0, 0.0],
            [7.0, 0.0, 5.0, 4.0],
            [6.0, -2.0, 0.0, 5.0],
        );

        let c = &a * &b;

        assert_eq!(a, &c * &b.inverse())
    }
}
