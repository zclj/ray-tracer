use crate::color::Color;

#[derive(Debug, PartialEq)]
pub struct Material {
    pub color: Color,
    pub ambient: f32,
    pub diffuse: f32,
    pub specular: f32,
    pub shininess: f32,
}

impl Material {
    fn new(color: Color, ambient: f32, diffuse: f32, specular: f32, shininess: f32) -> Self {
        Material {
            color,
            ambient,
            diffuse,
            specular,
            shininess,
        }
    }
}

impl Default for Material {
    fn default() -> Self {
        Material {
            color: Color::new(1.0, 1.0, 1.0),
            ambient: 0.1,
            diffuse: 0.9,
            specular: 0.9,
            shininess: 200.0,
        }
    }
}

#[cfg(test)]
mod test {
    use super::*;
    // Scenario: The default material
    //   Given m ← material()
    //   Then m.color = color(1, 1, 1)
    //     And m.ambient = 0.1
    //     And m.diffuse = 0.9
    //     And m.specular = 0.9
    //     And m.shininess = 200.0
    #[test]
    fn the_default_material() {
        let m: Material = Default::default();

        assert_eq!(m.color, Color::new(1.0, 1.0, 1.0));
        assert_eq!(m.ambient, 0.1);
        assert_eq!(m.diffuse, 0.9);
        assert_eq!(m.specular, 0.9);
        assert_eq!(m.shininess, 200.0);
    }
}
