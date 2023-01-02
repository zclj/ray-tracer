use crate::color::Color;
use crate::vector::Point;

#[derive(Debug, PartialEq)]
pub struct PointLight {
    pub position: Point,
    pub intensity: Color,
}

impl PointLight {
    #[must_use]
    pub fn new(position: Point, intensity: Color) -> PointLight {
        PointLight {
            position,
            intensity,
        }
    }
}

#[cfg(test)]
mod test {
    use super::*;
    use crate::color::Color;
    use crate::vector::Point;

    // Scenario: A point light has a position and intensity
    //   Given intensity ← color(1, 1, 1)
    //     And position ← point(0, 0, 0)
    //   When light ← point_light(position, intensity)
    //   Then light.position = position
    //     And light.intensity = intensity
    #[test]
    fn a_point_light_has_a_position_and_intensity() {
        let intensity = Color::new(0.0, 0.0, 0.0);
        let position = Point::new(0.0, 0.0, 0.0);

        let light = PointLight::new(position, intensity);

        assert_eq!(light.position, Point::new(0.0, 0.0, 0.0));
        assert_eq!(light.intensity, Color::new(0.0, 0.0, 0.0));
    }
}
