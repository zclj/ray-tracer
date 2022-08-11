/// Basic vector implementation
///
/// ```
/// # use ray_tracer::vector::Vector4D;
/// assert_eq!(Vector4D::new(4.0, 1.0, 4.0, 1.0).x() , 4.0)
/// ```
pub struct Vector4D {
    // @TODO: measure if the different stores make a performance diff.
    data: [f32; 4], //Vec<f32>,
}

impl Vector4D {
    pub fn new(x: f32, y: f32, z: f32, w: f32) -> Vector4D {
        Vector4D { data: [x, y, z, w] }
    }

    pub fn x(&self) -> f32 {
        self.data[0]
    }

    pub fn y(&self) -> f32 {
        self.data[1]
    }

    pub fn z(&self) -> f32 {
        self.data[2]
    }

    pub fn w(&self) -> f32 {
        self.data[3]
    }

    pub fn is_point(&self) -> bool {
        self.data[3] == 1.0
    }

    pub fn is_vector(&self) -> bool {
        self.data[3] == 0.0
    }
}

#[cfg(test)]
mod tests {

    use crate::vector::Vector4D;

    // Scenario: A tuple with w=1.0 is a point
    // Given a ← tuple(4.3, -4.2, 3.1, 1.0)
    // Then a.x = 4.3
    //   And a.y = -4.2
    //   And a.z = 3.1
    //   And a.w = 1.0
    //   And a is a point
    //   And a is not a vector
    #[test]
    fn a_tulpe_with_w_1_is_a_point() {
        let a = Vector4D::new(4.3, -4.2, 3.1, 1.0);

        assert_eq!(a.x(), 4.3);
        assert_eq!(a.y(), -4.2);
        assert_eq!(a.z(), 3.1);
        assert_eq!(a.w(), 1.0);
        assert_eq!(a.is_point(), true);
        assert_eq!(a.is_vector(), false);
    }

    // Scenario: A tuple with w=0 is a vector
    // Given a ← tuple(4.3, -4.2, 3.1, 0.0)
    // Then a.x = 4.3
    //   And a.y = -4.2
    //   And a.z = 3.1
    //   And a.w = 0.0
    //   And a is not a point
    //   And a is a vector
    #[test]
    fn a_tulpe_with_w_0_is_a_vector() {
        let a = Vector4D::new(4.3, -4.2, 3.1, 0.0);

        assert_eq!(a.x(), 4.3);
        assert_eq!(a.y(), -4.2);
        assert_eq!(a.z(), 3.1);
        assert_eq!(a.w(), 0.0);
        assert_eq!(a.is_point(), false);
        assert_eq!(a.is_vector(), true);
    }
}
