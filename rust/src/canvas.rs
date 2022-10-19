use crate::color::Color;

struct Canvas {
    width: usize,
    height: usize,
    pixels: Vec<Color>,
}

fn process_line(cs: &[Color]) -> String {
    let mut count = 0;
    let mut ppm_line: Vec<String> = vec![];

    let to_ppm_sample_string =
        |s: f32| f32::max(f32::min(255.0, (s * 255.0).ceil()), 0.0).to_string();

    for c in cs {
        let samples = [
            to_ppm_sample_string(c.red),
            to_ppm_sample_string(c.green),
            to_ppm_sample_string(c.blue),
        ];

        for s in samples {
            let size = s.len();

            if count + size <= 70 {
                ppm_line.push(" ".to_string() + &s);
                count += 1;
            } else {
                ppm_line.push("\n".to_string() + &s);
                count = 0;
            }

            count += size;
        }
    }

    ppm_line.concat().trim().to_string()
}

impl Canvas {
    fn new(width: usize, height: usize) -> Self {
        Canvas {
            width,
            height,
            pixels: vec![Color::new(0.0, 0.0, 0.0); width * height],
        }
    }

    fn write_pixel(&mut self, width: usize, height: usize, color: Color) {
        let idx = width + (self.width * height);
        self.pixels[idx] = color
    }

    fn pixel_at(&self, width: usize, height: usize) -> &Color {
        let idx = width + (self.width * height);
        &self.pixels[idx]
    }

    fn to_ppm(&self) -> String {
        let header = format!("P3\n{} {}\n255\n", self.width, self.height);

        // need to process for each 'line' of pixels
        let pixel_strs = self
            .pixels
            .chunks(self.width)
            .map(process_line)
            .collect::<Vec<String>>();

        header + &pixel_strs.join("\n")
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

    // Scenario: Constructing the PPM header
    // Given c ← canvas(5, 3)
    // When ppm ← canvas_to_ppm(c)
    // Then lines 1-3 of ppm are
    //   """
    //   P3
    //   5 3
    //   255
    //   """
    #[test]
    fn constructing_the_ppm_header() {
        let c = Canvas::new(5, 3);
        let ppm = c.to_ppm().lines().take(3).collect::<Vec<&str>>().join("\n");

        assert_eq!("P3\n5 3\n255", ppm)
    }

    // Scenario: Constructing the PPM pixel data
    // Given c ← canvas(5, 3)
    //   And c1 ← color(1.5, 0, 0)
    //   And c2 ← color(0, 0.5, 0)
    //   And c3 ← color(-0.5, 0, 1)
    // When write_pixel(c, 0, 0, c1)
    //   And write_pixel(c, 2, 1, c2)
    //   And write_pixel(c, 4, 2, c3)
    //   And ppm ← canvas_to_ppm(c)
    // Then lines 4-6 of ppm are
    //   """
    //   255 0 0 0 0 0 0 0 0 0 0 0 0 0 0
    //   0 0 0 0 0 0 0 128 0 0 0 0 0 0 0
    //   0 0 0 0 0 0 0 0 0 0 0 0 0 0 255
    //   """
    #[test]
    fn constructing_the_ppm_pixel_data() {
        let mut c = Canvas::new(5, 3);
        let c1 = Color::new(1.5, 0.0, 0.0);
        let c2 = Color::new(0.0, 0.5, 0.0);
        let c3 = Color::new(-0.5, 0.0, 1.0);

        c.write_pixel(0, 0, c1);
        c.write_pixel(2, 1, c2);
        c.write_pixel(4, 2, c3);

        let ppm = c.to_ppm().lines().skip(3).collect::<Vec<&str>>().join("\n");

        let result = "255 0 0 0 0 0 0 0 0 0 0 0 0 0 0\n0 0 0 0 0 0 0 128 0 0 0 0 0 0 0\n0 0 0 0 0 0 0 0 0 0 0 0 0 0 255";
        assert_eq!(ppm, result);
    }

    // Scenario: Splitting long lines in PPM files
    // Given c ← canvas(10, 2)
    // When every pixel of c is set to color(1, 0.8, 0.6)
    //   And ppm ← canvas_to_ppm(c)
    // Then lines 4-7 of ppm are
    //   """
    //   255 204 153 255 204 153 255 204 153 255 204 153 255 204 153 255 204
    //   153 255 204 153 255 204 153 255 204 153 255 204 153
    //   255 204 153 255 204 153 255 204 153 255 204 153 255 204 153 255 204
    //   153 255 204 153 255 204 153 255 204 153 255 204 153
    //   """
    #[test]
    fn splitting_long_lines_in_ppm_files() {
        let mut c = Canvas::new(10, 2);

        for i in 0..10 {
            for j in 0..2 {
                c.write_pixel(i, j, Color::new(1., 0.8, 0.6))
            }
        }

        let complete_ppm = c.to_ppm();
        let ppm = &complete_ppm
            .lines()
            .skip(3)
            .collect::<Vec<&str>>()
            .join("\n");

        let result = "255 204 153 255 204 153 255 204 153 255 204 153 255 204 153 255 204\n153 255 204 153 255 204 153 255 204 153 255 204 153\n255 204 153 255 204 153 255 204 153 255 204 153 255 204 153 255 204\n153 255 204 153 255 204 153 255 204 153 255 204 153";

        assert_eq!(ppm, result);
        assert_eq!(complete_ppm.lines().count(), 7);
    }
}
