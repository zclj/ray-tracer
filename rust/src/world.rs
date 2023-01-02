use crate::color::Color;
use crate::lights::PointLight;
use crate::materials::Material;
use crate::matrices::M4x4;
use crate::shape::Shape;
use crate::transformations::scaling;
use crate::vector::Point;

pub struct World {
    shapes: Vec<Shape>,
    light: PointLight,
}

impl World {
    fn test_default() -> Self {
        let mut w = World::new();

        w.push_sphere(
            None,
            Some(Material {
                color: Color::new(0.8, 1.0, 0.6),
                diffuse: 0.7,
                specular: 0.2,
                ..Default::default()
            }),
        );

        w.push_sphere(
            Some(scaling(0.5, 0.5, 0.5)),
            Some(Material {
                color: Color::new(0.8, 1.0, 0.6),
                diffuse: 0.7,
                specular: 0.2,
                ..Default::default()
            }),
        );

        w
    }

    #[must_use]
    pub fn new() -> Self {
        // @TODO - we could have knowledge about the capacity
        World {
            shapes: Vec::new(),
            light: PointLight {
                position: Point::new(-10.0, -10.0, -10.0),
                intensity: Color::new(1.0, 1.0, 1.0),
            },
        }
    }

    #[allow(clippy::cast_possible_truncation)]
    pub fn push_sphere(
        &mut self,
        transform_option: Option<M4x4>,
        material_option: Option<Material>,
    ) -> u32 {
        let id = self.shapes.len() as u32;

        let transform = match transform_option {
            Some(t) => t,
            None => M4x4::IDENTITY,
        };

        let material = match material_option {
            Some(m) => m,
            None => Material::default(),
        };

        self.shapes.push(Shape::Sphere {
            id,
            transform,
            material,
        });

        id
    }

    #[must_use]
    pub fn get_shape(&self, id: u32) -> &Shape {
        &self.shapes[id as usize]
    }
}

impl Default for World {
    fn default() -> Self {
        World::new()
    }
}

#[cfg(test)]
mod test {
    use super::*;
    use crate::materials::Material;
    use crate::shape::Shape;

    #[test]
    fn world_contain_shapes() {
        let mut ctx = World::new();
        ctx.push_sphere(None, None);

        let s_id = match ctx.get_shape(0) {
            Shape::Sphere { id, .. } => id,
            _ => panic!(),
        };

        assert_eq!(*s_id, 0);
    }

    // Scenario: The default world
    //   Given light ← point_light(point(-10, 10, -10), color(1, 1, 1))
    //     And s1 ← sphere() with:
    //       | material.color     | (0.8, 1.0, 0.6)        |
    //       | material.diffuse   | 0.7                    |
    //       | material.specular  | 0.2                    |
    //     And s2 ← sphere() with:
    //       | transform | scaling(0.5, 0.5, 0.5) |
    //   When w ← default_world()
    //   Then w.light = light
    //     And w contains s1
    //     And w contains s2
    #[test]
    fn the_default_world() {
        let w = World::test_default();
        let s1 = w.get_shape(0);
        let s2 = w.get_shape(1);

        assert_eq!(
            s1,
            &Shape::Sphere {
                id: 0,
                transform: M4x4::IDENTITY,
                material: Material {
                    color: Color::new(0.8, 1.0, 0.6),
                    diffuse: 0.7,
                    specular: 0.2,
                    ..Default::default()
                },
            }
        );

        assert_eq!(
            s2,
            &Shape::Sphere {
                id: 1,
                transform: scaling(0.5, 0.5, 0.5),
                material: Material {
                    color: Color::new(0.8, 1.0, 0.6),
                    diffuse: 0.7,
                    specular: 0.2,
                    ..Default::default()
                },
            }
        );

        assert_eq!(
            w.light,
            PointLight {
                position: Point::new(-10.0, -10.0, -10.0),
                intensity: Color::new(1.0, 1.0, 1.0)
            }
        )
    }
}
