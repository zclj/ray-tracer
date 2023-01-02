use crate::color::Color;
use crate::lights::PointLight;
use crate::vector::{Point, Vector};

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

    fn lighting(
        &self,
        light: &PointLight,
        position: &Point,
        eyev: &Vector,
        normalv: &Vector,
    ) -> Color {
        // combine the surface color with the light's color/intensity
        let effective_color = &self.color * &light.intensity;

        // find the direction to the light source
        let lightv = (&light.position - position).norm();

        // compute the ambient contribution
        let ambient = &effective_color * self.ambient;

        // light_dot_normal represents the cosine of the angle between the
        // light vector and the normal vector. A negative number means the
        // light is on the other side of the surface.
        let light_dot_normal = &lightv.dot(normalv);
        let (diffuse, specular) = if *light_dot_normal < 0.0 {
            (Color::new(0.0, 0.0, 0.0), Color::new(0.0, 0.0, 0.0))
        } else {
            // compute the diffuse contribution
            let diffuse = &effective_color * self.diffuse * *light_dot_normal;

            // reflect_dot_eye represents the cosine of the angle between the
            // reflection vector and the eye vector.
            // A negative number means the light reflects away from the eye.
            let reflectv = -(&lightv.reflect(normalv));
            let reflect_dot_eye = &reflectv.dot(eyev);

            let specular = if *reflect_dot_eye <= 0.0 {
                Color::new(0.0, 0.0, 0.0)
            } else {
                // compute the specular contribution
                let factor = reflect_dot_eye.powf(self.shininess);
                &light.intensity * self.specular * factor
            };
            (diffuse, specular)
        };

        ambient + diffuse + specular
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
    use crate::color::Color;
    use crate::lights::PointLight;
    use crate::vector::{Point, Vector};

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

    // Background:
    //   Given m ← material()
    //     And position ← point(0, 0, 0)

    // Scenario: Lighting with the eye between the light and the surface
    //   Given eyev ← vector(0, 0, -1)
    //     And normalv ← vector(0, 0, -1)
    //     And light ← point_light(point(0, 0, -10), color(1, 1, 1))
    //   When result ← lighting(m, light, position, eyev, normalv)
    //   Then result = color(1.9, 1.9, 1.9)
    #[test]
    fn lighting_with_the_eye_between_the_light_and_the_surface() {
        let m: Material = Default::default();
        let position: Point = Point::new(0.0, 0.0, 0.0);
        let eyev = Vector::new(0.0, 0.0, -1.0);
        let normalv = Vector::new(0.0, 0.0, -1.0);
        let light = PointLight::new(Point::new(0.0, 0.0, -10.0), Color::new(1.0, 1.0, 1.0));
        let result = m.lighting(&light, &position, &eyev, &normalv);

        assert_eq!(result, Color::new(1.9, 1.9, 1.9))
    }

    // Scenario: Lighting with the eye between light and surface, eye offset 45°
    //   Given eyev ← vector(0, √2/2, -√2/2)
    //     And normalv ← vector(0, 0, -1)
    //     And light ← point_light(point(0, 0, -10), color(1, 1, 1))
    //   When result ← lighting(m, light, position, eyev, normalv)
    //   Then result = color(1.0, 1.0, 1.0)
    #[test]
    fn lighting_with_the_eye_between_light_and_surface_eye_offset_45() {
        let m: Material = Default::default();
        let position: Point = Point::new(0.0, 0.0, 0.0);

        let eyev = Vector::new(0.0, f32::sqrt(2.0) / 2.0, f32::sqrt(2.0) / 2.0);
        let normalv = Vector::new(0.0, 0.0, -1.0);
        let light = PointLight::new(Point::new(0.0, 0.0, -10.0), Color::new(1.0, 1.0, 1.0));
        let result = m.lighting(&light, &position, &eyev, &normalv);

        assert_eq!(result, Color::new(1.0, 1.0, 1.0))
    }

    // Scenario: Lighting with eye opposite surface, light offset 45°
    //   Given eyev ← vector(0, 0, -1)
    //     And normalv ← vector(0, 0, -1)
    //     And light ← point_light(point(0, 10, -10), color(1, 1, 1))
    //   When result ← lighting(m, light, position, eyev, normalv)
    //   Then result = color(0.7364, 0.7364, 0.7364)
    #[test]
    fn lighting_with_eye_opposite_surface_light_offset_45() {
        let m: Material = Default::default();
        let position: Point = Point::new(0.0, 0.0, 0.0);

        let eyev = Vector::new(0.0, 0.0, -1.0);
        let normalv = Vector::new(0.0, 0.0, -1.0);
        let light = PointLight::new(Point::new(0.0, 10.0, -10.0), Color::new(1.0, 1.0, 1.0));
        let result = m.lighting(&light, &position, &eyev, &normalv);

        assert_eq!(result, Color::new(0.7364, 0.7364, 0.7364))
    }

    // Scenario: Lighting with eye in the path of the reflection vector
    //   Given eyev ← vector(0, -√2/2, -√2/2)
    //     And normalv ← vector(0, 0, -1)
    //     And light ← point_light(point(0, 10, -10), color(1, 1, 1))
    //   When result ← lighting(m, light, position, eyev, normalv)
    //   Then result = color(1.6364, 1.6364, 1.6364)
    #[test]
    fn lighting_with_eye_in_the_path_of_the_reflection_vector() {
        let m: Material = Default::default();
        let position: Point = Point::new(0.0, 0.0, 0.0);

        let eyev = Vector::new(0.0, -f32::sqrt(2.0) / 2.0, -f32::sqrt(2.0) / 2.0);
        let normalv = Vector::new(0.0, 0.0, -1.0);
        let light = PointLight::new(Point::new(0.0, 10.0, -10.0), Color::new(1.0, 1.0, 1.0));
        let result = m.lighting(&light, &position, &eyev, &normalv);

        assert_eq!(result, Color::new(1.6363853, 1.6363853, 1.6363853))
    }

    // Scenario: Lighting with the light behind the surface
    //   Given eyev ← vector(0, 0, -1)
    //     And normalv ← vector(0, 0, -1)
    //     And light ← point_light(point(0, 0, 10), color(1, 1, 1))
    //   When result ← lighting(m, light, position, eyev, normalv)
    //   Then result = color(0.1, 0.1, 0.1)
    #[test]
    fn lighting_with_the_light_behind_the_surface() {
        let m: Material = Default::default();
        let position: Point = Point::new(0.0, 0.0, 0.0);

        let eyev = Vector::new(0.0, 0.0, -1.0);
        let normalv = Vector::new(0.0, 0.0, -1.0);
        let light = PointLight::new(Point::new(0.0, 0.0, 10.0), Color::new(1.0, 1.0, 1.0));
        let result = m.lighting(&light, &position, &eyev, &normalv);

        assert_eq!(result, Color::new(0.1, 0.1, 0.1))
    }
}
