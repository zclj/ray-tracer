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

    fn write_pixel(&mut self, width: u16, height: u16, color: Color) {
        let idx: usize = (width * height).into();
        self.pixels[idx] = color
    }

    fn pixel_at(&self, width: u16, height: u16) -> &Color {
        let idx: usize = (width * height).into();
        &self.pixels[idx]
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

    // Scenario: Writing pixels to a canvas
    // Given c ← canvas(10, 20)
    //   And red ← color(1, 0, 0)
    // When write_pixel(c, 2, 3, red)
    // Then pixel_at(c, 2, 3) = red
    #[test]
    fn writing_pixels_to_a_canvas() {
        let mut c = Canvas::new(10, 20);
        let red = Color::new(1.0, 0.0, 0.0);

        c.write_pixel(2, 3, red);

        assert_eq!(*c.pixel_at(2, 3), Color::new(1.0, 0.0, 0.0))
    }
}
