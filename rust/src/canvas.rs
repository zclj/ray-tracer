use crate::color::Color;

pub struct Canvas {
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

            // current length + new size + padding should not > 70
            if count + size + 1 <= 70 {
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

fn process_line2(cs: &[Color]) -> String {
    let mut count = 0;
    let mut ppm_line_str = String::with_capacity(6000);

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

            // current length + new size + padding should not > 70
            if count + size + 1 <= 70 {
                ppm_line_str.push(' ');
                ppm_line_str.push_str(&s);
                count += 1;
            } else {
                ppm_line_str.push('\n');
                ppm_line_str.push_str(&s);
                count = 0;
            }

            count += size;
        }
    }

    //println!("Capacity: {}", ppm_line_str.capacity());
    ppm_line_str.trim().to_string()
}

fn process_line3(cs: &[Color], line_str: &mut String) -> () {
    let mut count = 0;

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

            // current length + new size + padding should not > 70
            if count + size + 1 <= 70 {
                line_str.push_str(&s);
                line_str.push(' ');
                count += 1;
            } else {
                line_str.pop();
                line_str.push('\n');
                line_str.push_str(&s);
                line_str.push(' ');
                count = 0;
            }

            count += size;
        }
    }

    line_str.pop();
}

fn push_digits(d: u8, s: &mut String) -> () {
    if d == 0 {
        s.push('0');
        return;
    }

    if d < 10 {
        s.push(char::from_digit(d.into(), 10).unwrap());
        return;
    }

    let mut num = d;
    let mut digit = 0;
    let mut is: [char; 3] = ['-'; 3];

    while num != 0 {
        is[digit] = char::from_digit((num % 10).into(), 10).unwrap();
        num /= 10;
        digit += 1;
    }

    for i in 0..3 {
        if is[2 - i] != '-' {
            s.push(is[2 - i]);
        }
    }
}

fn process_line4(cs: &[Color], line_str: &mut String) -> () {
    let mut count = 0;

    let to_ppm_sample = |s: f32| f32::max(f32::min(255.0, (s * 255.0).ceil()), 0.0);

    for c in cs {
        let samples = [
            to_ppm_sample(c.red),
            to_ppm_sample(c.green),
            to_ppm_sample(c.blue),
        ];

        for s in samples {
            let size = if s >= 100.0 {
                3
            } else if s >= 10.0 {
                2
            } else {
                1
            };

            // current length + new size + padding should not > 70
            if count + size + 1 <= 70 {
                push_digits(s as u8, line_str);
                line_str.push(' ');
                count += 1;
            } else {
                line_str.pop();
                line_str.push('\n');
                push_digits(s as u8, line_str);
                line_str.push(' ');
                count = 0;
            }

            count += size;
        }
    }

    line_str.pop();
}

impl Canvas {
    pub fn new(width: usize, height: usize) -> Self {
        Canvas {
            width,
            height,
            pixels: vec![Color::new(0.0, 0.0, 0.0); width * height],
        }
    }

    pub fn write_pixel(&mut self, width: usize, height: usize, color: Color) {
        let idx = width + (self.width * height);
        self.pixels[idx] = color
    }

    fn pixel_at(&self, width: usize, height: usize) -> &Color {
        let idx = width + (self.width * height);
        &self.pixels[idx]
    }

    pub fn to_ppm(&self) -> String {
        let mut header = String::with_capacity(self.width * self.height * 12);

        //println!("Pre Capacity: {}", header.capacity());
        header.push_str(&format!("P3\n{} {}\n255\n", self.width, self.height));

        for pxs in self.pixels.chunks(self.width) {
            process_line4(pxs, &mut header);
            header.push('\n');
        }

        //println!("Post Capacity: {}", header.capacity());
        //println!("Length: {}", header.len());
        header
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

        let result = "255 0 0 0 0 0 0 0 0 0 0 0 0 0 0\n\
                      0 0 0 0 0 0 0 128 0 0 0 0 0 0 0\n\
                      0 0 0 0 0 0 0 0 0 0 0 0 0 0 255";
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

        let result = "255 204 153 255 204 153 255 204 153 255 204 153 255 204 153 255 204\n\
             153 255 204 153 255 204 153 255 204 153 255 204 153\n\
             255 204 153 255 204 153 255 204 153 255 204 153 255 204 153 255 204\n\
             153 255 204 153 255 204 153 255 204 153 255 204 153";

        assert_eq!(ppm, result);
        assert_eq!(complete_ppm.lines().count(), 7);
    }

    // Scenario: PPM files are terminated by a newline character
    // Given c ← canvas(5, 3)
    // When ppm ← canvas_to_ppm(c)
    // Then ppm ends with a newline character
    #[test]
    fn ppm_files_are_terminated_by_a_newline_character() {
        let c = Canvas::new(5, 3);
        let ppm_end = c.to_ppm().chars().last().unwrap();

        assert_eq!(ppm_end, '\n');
    }
}
