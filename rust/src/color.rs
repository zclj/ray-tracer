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
}
