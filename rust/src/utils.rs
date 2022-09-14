const EPSILON: f32 = 0.00001;

pub(crate) fn epsilon_eq(lhs: f32, rhs: f32) -> bool {
    (lhs - rhs).abs() < EPSILON
}
