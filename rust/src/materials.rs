use crate::color::Color;
use crate::lights::PointLight;
use crate::matrices::M4x4;
use crate::shape::RenderObject;
use crate::vector::{Point, Vector};

#[derive(Debug, PartialEq, Clone)]
pub struct Material {
    pub color: Color,
    pub ambient: f32,
    pub diffuse: f32,
    pub specular: f32,
    pub shininess: f32,
    pub reflective: f32,
    pub pattern: Option<Pattern>,
    pub transparency: f32,
    pub refractive_index: f32,
}

impl Material {
    #[allow(clippy::too_many_arguments)]
    fn new(
        color: Color,
        ambient: f32,
        diffuse: f32,
        specular: f32,
        shininess: f32,
        reflective: f32,
        pattern: Option<Pattern>,
        transparency: f32,
        refractive_index: f32,
    ) -> Self {
        Material {
            color,
            ambient,
            diffuse,
            specular,
            shininess,
            reflective,
            pattern,
            transparency,
            refractive_index,
        }
    }

    #[must_use]
    pub fn lighting(
        &self,
        shape: &RenderObject,
        light: &PointLight,
        position: &Point,
        eyev: &Vector,
        normalv: &Vector,
        in_shadow: bool,
    ) -> Color {
        // combine the surface color with the light's color/intensity
        let effective_color = if let Some(p) = &self.pattern {
            &p.pattern_at_shape(shape, position) * &light.intensity
        } else {
            &self.color * &light.intensity
        };

        // find the direction to the light source
        let lightv = (&light.position - position).norm();

        // compute the ambient contribution
        let ambient = &effective_color * self.ambient;

        if in_shadow {
            ambient
        } else {
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
}

impl Default for Material {
    fn default() -> Self {
        Material::new(
            Color::new(1.0, 1.0, 1.0),
            0.1,
            0.9,
            0.9,
            200.0,
            0.0,
            None,
            0.0,
            1.0,
        )
    }
}

#[derive(Debug, PartialEq, Clone)]
pub enum PatternKind {
    Stripe,
    Gradient,
    Ring,
    Checkers,
    Point,
}

#[derive(Debug, PartialEq, Clone)]
pub struct Pattern {
    pub a: Color,
    pub b: Color,
    pub kind: PatternKind,
    pub transform: M4x4,
    pub transform_inverse: M4x4,
}

impl Pattern {
    #[must_use]
    pub fn new(a: Color, b: Color, kind: PatternKind, transform: M4x4) -> Self {
        Pattern {
            a,
            b,
            kind,
            transform_inverse: transform.inverse(),
            transform,
        }
    }

    fn pattern_at(&self, point: &Point) -> Color {
        match self.kind {
            PatternKind::Stripe => {
                if (point.x.floor() % 2.0) == 0.0 {
                    self.a.clone()
                } else {
                    self.b.clone()
                }
            }
            PatternKind::Gradient => {
                let distance = &self.b - &self.a;
                let fraction = point.x - f32::floor(point.x);

                &self.a + &(distance * fraction)
            }
            PatternKind::Ring => {
                if f32::floor(f32::sqrt(point.x.powf(2.0) + point.z.powf(2.0))) % 2.0 == 0.0 {
                    self.a.clone()
                } else {
                    self.b.clone()
                }
            }
            PatternKind::Checkers => {
                if ((point.x.floor() + point.y.floor() + point.z.floor()) % 2.0) == 0.0 {
                    self.a.clone()
                } else {
                    self.b.clone()
                }
            }
            PatternKind::Point => Color::new(point.x, point.y, point.z),
        }
    }

    fn pattern_at_shape(&self, shape: &RenderObject, world_point: &Point) -> Color {
        let object_point = &shape.transform_inverse * world_point;
        let pattern_point = &self.transform_inverse * &object_point;

        self.pattern_at(&pattern_point)
    }
}

impl Default for Pattern {
    fn default() -> Self {
        Pattern::new(
            Color::new(1.0, 1.0, 1.0),
            Color::new(0.0, 0.0, 0.0),
            PatternKind::Stripe,
            M4x4::IDENTITY,
        )
    }
}

#[cfg(test)]
mod test {
    use super::*;
    use crate::color::Color;
    use crate::lights::PointLight;
    use crate::shape::Shape;
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
        let shape = RenderObject {
            id: 1,
            kind: Shape::Sphere,
            transform: M4x4::IDENTITY,
            transform_inverse: M4x4::IDENTITY,
            transform_inverse_transpose: M4x4::IDENTITY,
            material: Material::default(),
        };
        let result = m.lighting(&shape, &light, &position, &eyev, &normalv, false);

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
        let shape = RenderObject {
            id: 1,
            kind: Shape::Sphere,
            transform: M4x4::IDENTITY,
            transform_inverse: M4x4::IDENTITY,
            transform_inverse_transpose: M4x4::IDENTITY,
            material: Material::default(),
        };
        let result = m.lighting(&shape, &light, &position, &eyev, &normalv, false);

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
        let shape = RenderObject {
            id: 1,
            kind: Shape::Sphere,
            transform: M4x4::IDENTITY,
            transform_inverse: M4x4::IDENTITY,
            transform_inverse_transpose: M4x4::IDENTITY,
            material: Material::default(),
        };

        let result = m.lighting(&shape, &light, &position, &eyev, &normalv, false);

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
        let shape = RenderObject {
            id: 1,
            kind: Shape::Sphere,
            transform: M4x4::IDENTITY,
            transform_inverse: M4x4::IDENTITY,
            transform_inverse_transpose: M4x4::IDENTITY,
            material: Material::default(),
        };

        let result = m.lighting(&shape, &light, &position, &eyev, &normalv, false);

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
        let shape = RenderObject {
            id: 1,
            kind: Shape::Sphere,
            transform: M4x4::IDENTITY,
            transform_inverse: M4x4::IDENTITY,
            transform_inverse_transpose: M4x4::IDENTITY,
            material: Material::default(),
        };

        let result = m.lighting(&shape, &light, &position, &eyev, &normalv, false);

        assert_eq!(result, Color::new(0.1, 0.1, 0.1))
    }

    // Scenario: Lighting with the surface in shadow
    //   Given eyev ← vector(0, 0, -1)
    //     And normalv ← vector(0, 0, -1)
    //     And light ← point_light(point(0, 0, -10), color(1, 1, 1))
    //     And in_shadow ← true
    //   When result ← lighting(m, light, position, eyev, normalv, in_shadow)
    //   Then result = color(0.1, 0.1, 0.1)
    #[test]
    fn lighting_with_the_surface_in_shadow() {
        let m: Material = Default::default();
        let position: Point = Point::new(0.0, 0.0, 0.0);

        let eyev = Vector::new(0.0, 0.0, -1.0);
        let normalv = Vector::new(0.0, 0.0, -1.0);
        let light = PointLight::new(Point::new(0.0, 0.0, -10.0), Color::new(1.0, 1.0, 1.0));
        let in_shadow = true;
        let shape = RenderObject {
            id: 1,
            kind: Shape::Sphere,
            transform: M4x4::IDENTITY,
            transform_inverse: M4x4::IDENTITY,
            transform_inverse_transpose: M4x4::IDENTITY,
            material: Material::default(),
        };

        let result = m.lighting(&shape, &light, &position, &eyev, &normalv, in_shadow);

        assert_eq!(result, Color::new(0.1, 0.1, 0.1))
    }

    // Background:
    //   Given black ← color(0, 0, 0)
    //     And white ← color(1, 1, 1)
    const BLACK: Color = Color {
        red: 0.0,
        green: 0.0,
        blue: 0.0,
    };
    const WHITE: Color = Color {
        red: 1.0,
        green: 1.0,
        blue: 1.0,
    };

    // Scenario: Creating a stripe pattern
    //   Given pattern ← stripe_pattern(white, black)
    //   Then pattern.a = white
    //     And pattern.b = black
    #[test]
    fn creating_a_stripe_pattern() {
        let pattern = Pattern::default();

        assert_eq!(pattern.a, WHITE);
        assert_eq!(pattern.b, BLACK);
        assert_eq!(pattern.kind, PatternKind::Stripe)
    }

    // Scenario: A stripe pattern is constant in y
    //   Given pattern ← stripe_pattern(white, black)
    //   Then stripe_at(pattern, point(0, 0, 0)) = white
    //     And stripe_at(pattern, point(0, 1, 0)) = white
    //     And stripe_at(pattern, point(0, 2, 0)) = white
    #[test]
    fn a_stripe_pattern_is_constant_in_y() {
        let pattern = Pattern::default();

        assert_eq!(pattern.pattern_at(&Point::new(0.0, 0.0, 0.0)), WHITE);
        assert_eq!(pattern.pattern_at(&Point::new(0.0, 1.0, 0.0)), WHITE);
        assert_eq!(pattern.pattern_at(&Point::new(0.0, 2.0, 0.0)), WHITE)
    }

    // Scenario: A stripe pattern is constant in z
    //   Given pattern ← stripe_pattern(white, black)
    //   Then stripe_at(pattern, point(0, 0, 0)) = white
    //     And stripe_at(pattern, point(0, 0, 1)) = white
    //     And stripe_at(pattern, point(0, 0, 2)) = white
    #[test]
    fn a_stripe_pattern_is_constant_in_z() {
        let pattern = Pattern::default();

        assert_eq!(pattern.pattern_at(&Point::new(0.0, 0.0, 0.0)), WHITE);
        assert_eq!(pattern.pattern_at(&Point::new(0.0, 0.0, 1.0)), WHITE);
        assert_eq!(pattern.pattern_at(&Point::new(0.0, 0.0, 2.0)), WHITE)
    }

    // Scenario: A stripe pattern alternates in x
    //   Given pattern ← stripe_pattern(white, black)
    //   Then stripe_at(pattern, point(0, 0, 0)) = white
    //     And stripe_at(pattern, point(0.9, 0, 0)) = white
    //     And stripe_at(pattern, point(1, 0, 0)) = black
    //     And stripe_at(pattern, point(-0.1, 0, 0)) = black
    //     And stripe_at(pattern, point(-1, 0, 0)) = black
    //     And stripe_at(pattern, point(-1.1, 0, 0)) = white
    #[test]
    fn a_stripe_pattern_alternates_in_x() {
        let pattern = Pattern::default();

        assert_eq!(pattern.pattern_at(&Point::new(0.0, 0.0, 0.0)), WHITE);
        assert_eq!(pattern.pattern_at(&Point::new(0.9, 0.0, 0.0)), WHITE);
        assert_eq!(pattern.pattern_at(&Point::new(1.0, 0.0, 0.0)), BLACK);
        assert_eq!(pattern.pattern_at(&Point::new(-0.1, 0.0, 0.0)), BLACK);
        assert_eq!(pattern.pattern_at(&Point::new(-1.0, 0.0, 0.0)), BLACK);
        assert_eq!(pattern.pattern_at(&Point::new(-1.1, 0.0, 0.0)), WHITE);
    }

    // Scenario: Lighting with a pattern applied
    //   Given m.pattern ← stripe_pattern(color(1, 1, 1), color(0, 0, 0))
    //     And m.ambient ← 1
    //     And m.diffuse ← 0
    //     And m.specular ← 0
    //     And eyev ← vector(0, 0, -1)
    //     And normalv ← vector(0, 0, -1)
    //     And light ← point_light(point(0, 0, -10), color(1, 1, 1))
    //   When c1 ← lighting(m, light, point(0.9, 0, 0), eyev, normalv, false)
    //     And c2 ← lighting(m, light, point(1.1, 0, 0), eyev, normalv, false)
    //   Then c1 = color(1, 1, 1)
    //     And c2 = color(0, 0, 0)
    #[test]
    fn lighting_with_a_pattern_applied() {
        let m = Material {
            color: Color::new(1.0, 1.0, 1.0),
            ambient: 1.0,
            diffuse: 0.0,
            specular: 0.0,
            shininess: 0.0,
            reflective: 0.0,
            pattern: Some(Pattern::default()),
            ..Default::default()
        };

        let eyev = Vector::new(0.0, 0.0, -1.0);
        let normalv = Vector::new(0.0, 0.0, -1.0);
        let light = PointLight::new(Point::new(0.0, 0.0, -10.0), Color::new(1.0, 1.0, 1.0));
        let shape = RenderObject {
            id: 1,
            kind: Shape::Sphere,
            transform: M4x4::IDENTITY,
            transform_inverse: M4x4::IDENTITY,
            transform_inverse_transpose: M4x4::IDENTITY,
            material: Material::default(),
        };

        let c1 = m.lighting(
            &shape,
            &light,
            &Point::new(0.9, 0.0, 0.0),
            &eyev,
            &normalv,
            false,
        );
        let c2 = m.lighting(
            &shape,
            &light,
            &Point::new(1.1, 0.0, 0.0),
            &eyev,
            &normalv,
            false,
        );

        assert_eq!(c1, Color::new(1.0, 1.0, 1.0));
        assert_eq!(c2, Color::new(0.0, 0.0, 0.0));
    }

    // Scenario: Stripes with an object transformation
    //   Given object ← sphere()
    //     And set_transform(object, scaling(2, 2, 2))
    //     And pattern ← stripe_pattern(white, black)
    //   When c ← stripe_at_object(pattern, object, point(1.5, 0, 0))
    //   Then c = white
    use crate::transformations::{scaling, translation};
    use crate::world::World;
    #[test]
    fn stripes_with_an_object_transformation() {
        let mut world = World::new();
        let s_id = world.push_sphere(
            Some(scaling(2.0, 2.0, 2.0)),
            Some(Material {
                pattern: Some(Pattern::default()),
                ..Material::default()
            }),
        );

        let sphere = world.get_object(s_id);
        let pattern = sphere.material.pattern.as_ref().unwrap();

        let c = pattern.pattern_at_shape(&sphere, &Point::new(1.5, 0.0, 0.0));

        assert_eq!(c, WHITE);
    }

    // Scenario: Stripes with a pattern transformation
    //   Given object ← sphere()
    //     And pattern ← stripe_pattern(white, black)
    //     And set_pattern_transform(pattern, scaling(2, 2, 2))
    //   When c ← stripe_at_object(pattern, object, point(1.5, 0, 0))
    //   Then c = white
    #[test]
    fn stripes_with_a_pattern_transformation() {
        let mut world = World::new();
        let s_id = world.push_sphere(
            None,
            Some(Material {
                pattern: Some(Pattern {
                    transform: scaling(2.0, 2.0, 2.0),
                    transform_inverse: scaling(2.0, 2.0, 2.0).inverse(),
                    ..Pattern::default()
                }),
                ..Material::default()
            }),
        );

        let sphere = world.get_object(s_id);
        let pattern = sphere.material.pattern.as_ref().unwrap();

        let c = pattern.pattern_at_shape(&sphere, &Point::new(1.5, 0.0, 0.0));

        assert_eq!(c, WHITE);
    }

    // Scenario: Stripes with both an object and a pattern transformation
    //   Given object ← sphere()
    //     And set_transform(object, scaling(2, 2, 2))
    //     And pattern ← stripe_pattern(white, black)
    //     And set_pattern_transform(pattern, translation(0.5, 0, 0))
    //   When c ← stripe_at_object(pattern, object, point(2.5, 0, 0))
    //   Then c = white
    #[test]
    fn stripes_with_both_an_object_and_a_pattern_transformation() {
        let mut world = World::new();
        let s_id = world.push_sphere(
            Some(scaling(2.0, 2.0, 2.0)),
            Some(Material {
                pattern: Some(Pattern {
                    transform: translation(0.5, 0.0, 0.0),
                    transform_inverse: translation(0.5, 0.0, 0.0).inverse(),
                    ..Pattern::default()
                }),
                ..Material::default()
            }),
        );

        let sphere = world.get_object(s_id);
        let pattern = sphere.material.pattern.as_ref().unwrap();

        let c = pattern.pattern_at_shape(&sphere, &Point::new(2.5, 0.0, 0.0));

        assert_eq!(c, WHITE);
    }

    // Scenario: A gradient linearly interpolates between colors
    //   Given pattern ← gradient_pattern(white, black)
    //   Then pattern_at(pattern, point(0, 0, 0)) = white
    //     And pattern_at(pattern, point(0.25, 0, 0)) = color(0.75, 0.75, 0.75)
    //     And pattern_at(pattern, point(0.5, 0, 0)) = color(0.5, 0.5, 0.5)
    //     And pattern_at(pattern, point(0.75, 0, 0)) = color(0.25, 0.25, 0.25)
    #[test]
    fn a_gradient_linearly_interpolates_between_colors() {
        let pattern = Pattern::new(WHITE, BLACK, PatternKind::Gradient, M4x4::IDENTITY);

        assert_eq!(pattern.pattern_at(&Point::new(0.0, 0.0, 0.0)), WHITE);
        assert_eq!(
            pattern.pattern_at(&Point::new(0.25, 0.0, 0.0)),
            Color::new(0.75, 0.75, 0.75)
        );
        assert_eq!(
            pattern.pattern_at(&Point::new(0.5, 0.0, 0.0)),
            Color::new(0.5, 0.5, 0.5)
        );
        assert_eq!(
            pattern.pattern_at(&Point::new(0.75, 0.0, 0.0)),
            Color::new(0.25, 0.25, 0.25)
        )
    }

    // Scenario: A ring should extend in both x and z
    //   Given pattern ← ring_pattern(white, black)
    //   Then pattern_at(pattern, point(0, 0, 0)) = white
    //     And pattern_at(pattern, point(1, 0, 0)) = black
    //     And pattern_at(pattern, point(0, 0, 1)) = black
    //     # 0.708 = just slightly more than √2/2
    //     And pattern_at(pattern, point(0.708, 0, 0.708)) = black
    #[test]
    fn a_ring_should_extend_in_both_x_and_z() {
        let pattern = Pattern::new(WHITE, BLACK, PatternKind::Ring, M4x4::IDENTITY);
        assert_eq!(pattern.pattern_at(&Point::new(0.0, 0.0, 0.0)), WHITE);
        assert_eq!(pattern.pattern_at(&Point::new(1.0, 0.0, 0.0)), BLACK);
        assert_eq!(pattern.pattern_at(&Point::new(0.0, 0.0, 1.0)), BLACK);
        assert_eq!(pattern.pattern_at(&Point::new(0.708, 0.0, 0.708)), BLACK)
    }

    // Scenario: Checkers should repeat in x
    //   Given pattern ← checkers_pattern(white, black)
    //   Then pattern_at(pattern, point(0, 0, 0)) = white
    //     And pattern_at(pattern, point(0.99, 0, 0)) = white
    //     And pattern_at(pattern, point(1.01, 0, 0)) = black
    #[test]
    fn checkers_should_repeat_in_x() {
        let pattern = Pattern::new(WHITE, BLACK, PatternKind::Checkers, M4x4::IDENTITY);
        assert_eq!(pattern.pattern_at(&Point::new(0.0, 0.0, 0.0)), WHITE);
        assert_eq!(pattern.pattern_at(&Point::new(0.99, 0.0, 0.0)), WHITE);
        assert_eq!(pattern.pattern_at(&Point::new(1.01, 0.0, 0.0)), BLACK);
    }

    // Scenario: Checkers should repeat in y
    //   Given pattern ← checkers_pattern(white, black)
    //   Then pattern_at(pattern, point(0, 0, 0)) = white
    //     And pattern_at(pattern, point(0, 0.99, 0)) = white
    //     And pattern_at(pattern, point(0, 1.01, 0)) = black
    #[test]
    fn checkers_should_repeat_in_y() {
        let pattern = Pattern::new(WHITE, BLACK, PatternKind::Checkers, M4x4::IDENTITY);
        assert_eq!(pattern.pattern_at(&Point::new(0.0, 0.0, 0.0)), WHITE);
        assert_eq!(pattern.pattern_at(&Point::new(0.0, 0.99, 0.0)), WHITE);
        assert_eq!(pattern.pattern_at(&Point::new(0.0, 1.01, 0.0)), BLACK);
    }

    // Scenario: Checkers should repeat in z
    //   Given pattern ← checkers_pattern(white, black)
    //   Then pattern_at(pattern, point(0, 0, 0)) = white
    //     And pattern_at(pattern, point(0, 0, 0.99)) = white
    //     And pattern_at(pattern, point(0, 0, 1.01)) = black
    #[test]
    fn checkers_should_repeat_in_z() {
        let pattern = Pattern::new(WHITE, BLACK, PatternKind::Checkers, M4x4::IDENTITY);
        assert_eq!(pattern.pattern_at(&Point::new(0.0, 0.0, 0.0)), WHITE);
        assert_eq!(pattern.pattern_at(&Point::new(0.0, 0.0, 0.99)), WHITE);
        assert_eq!(pattern.pattern_at(&Point::new(0.0, 0.0, 1.01)), BLACK);
    }

    // Scenario: Reflectivity for the default material
    //   Given m ← material()
    //   Then m.reflective = 0.0
    #[test]
    fn reflectivity_for_the_default_material() {
        let m = Material::default();

        assert_eq!(m.reflective, 0.0)
    }

    // Scenario: Transparency and Refractive Index for the default material
    //   Given m ← material()
    //   Then m.transparency = 0.0
    //     And m.refractive_index = 1.0
    #[test]
    fn transparency_and_refractive_index_for_the_default_material() {
        let m = Material::default();

        assert_eq!(m.transparency, 0.0);
        assert_eq!(m.refractive_index, 1.0)
    }
}
