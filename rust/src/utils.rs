pub(crate) const EPSILON: f32 = 0.0001;

pub(crate) fn epsilon_eq(lhs: f32, rhs: f32) -> bool {
    (lhs - rhs).abs() < EPSILON
}
