use crate::color::Color;

struct Canvas {
    width: u16,
    height: u16,
    pixels: Vec<Color>,
}

impl Canvas {
    fn new(width: u16, height: u16) -> Self {
        Canvas {
            width,
            height,
            pixels: vec![Color::new(0.0, 0.0, 0.0); (width * height).into()],
        }
    }
}

mod test {
    use crate::canvas::Canvas;
    use crate::color::Color;

    // Scenario: Creating a canvas
    // Given c ← canvas(10, 20)
    // Then c.width = 10
    //   And c.height = 20
    //   And every pixel of c is color(0, 0, 0)
    #[test]
    fn creating_a_canvas() {
        let c = Canvas::new(10, 20);
        let black = Color::new(0.0, 0.0, 0.0);

        assert_eq!(c.width, 10);
        assert_eq!(c.height, 20);
        assert_eq!(c.pixels.iter().all(|c| *c == black), true);
    }
}
