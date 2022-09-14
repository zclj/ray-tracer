use crate::utils::epsilon_eq;

#[derive(Debug)]
struct Color {
    red: f32,
    green: f32,
    blue: f32,
}

impl Color {
    fn new(red: f32, green: f32, blue: f32) -> Color {
        Color { red, green, blue }
    }
}

impl std::cmp::PartialEq<Color> for Color {
    fn eq(&self, rhs: &Self) -> bool {
        epsilon_eq(self.red, rhs.red)
            && epsilon_eq(self.green, rhs.green)
            && epsilon_eq(self.blue, rhs.blue)
    }
}

impl std::ops::Add<Color> for Color {
    type Output = Self;

    fn add(self, rhs: Color) -> Self {
        Color {
            red: self.red + rhs.red,
            green: self.green + rhs.green,
            blue: self.blue + rhs.blue,
        }
    }
}

impl std::ops::Sub<Color> for Color {
    type Output = Self;

    fn sub(self, rhs: Self) -> Self {
        Color::new(
            self.red - rhs.red,
            self.green - rhs.green,
            self.blue - rhs.blue,
        )
    }
}

#[cfg(test)]
mod tests {

    use crate::color::Color;

    // Scenario: Colors are (red, green, blue) tuples
    // Given c ← color(-0.5, 0.4, 1.7)
    // Then c.red = -0.5
    //   And c.green = 0.4
    //   And c.blue = 1.7
    #[test]
    fn color_are_structs() {
        let c = Color::new(-0.5, 0.4, 1.7);

        assert_eq!(c.red, -0.5);
        assert_eq!(c.green, 0.4);
        assert_eq!(c.blue, 1.7);
    }

    // Scenario: Adding colors
    // Given c1 ← color(0.9, 0.6, 0.75)
    //   And c2 ← color(0.7, 0.1, 0.25)
    //  Then c1 + c2 = color(1.6, 0.7, 1.0)
    #[test]
    fn adding_colors() {
        let c1 = Color::new(0.9, 0.6, 0.75);
        let c2 = Color::new(0.7, 0.1, 0.25);

        assert_eq!(c1 + c2, Color::new(1.6, 0.7, 1.0));
    }

    // Scenario: Subtracting colors
    //   Given c1 ← color(0.9, 0.6, 0.75)
    //     And c2 ← color(0.7, 0.1, 0.25)
    //    Then c1 - c2 = color(0.2, 0.5, 0.5)
    #[test]
    fn subtracting_colors() {
        let c1 = Color::new(0.9, 0.6, 0.75);
        let c2 = Color::new(0.7, 0.1, 0.25);

        assert_eq!(c1 - c2, Color::new(0.2, 0.5, 0.5))
    }

    // Scenario: Multiplying a color by a scalar
    //   Given c ← color(0.2, 0.3, 0.4)
    //   Then c * 2 = color(0.4, 0.6, 0.8)

    // Scenario: Multiplying colors
    //   Given c1 ← color(1, 0.2, 0.4)
    //     And c2 ← color(0.9, 1, 0.1)
    //    Then c1 * c2 = color(0.9, 0.2, 0.04)
}
