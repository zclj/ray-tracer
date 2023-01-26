use crate::color::Color;
use crate::intersections::{hit, intersect, sort_by_t, ComputedIntersection, Intersection};
use crate::lights::PointLight;
use crate::materials::Material;
use crate::matrices::M4x4;
use crate::rays::Ray;
use crate::shape::Shape;
use crate::utils::EPSILON;
use crate::vector::Point;

pub struct World {
    shapes: Vec<Shape>,
    pub light: PointLight,
    pub shadow_bias: f32,
}

impl World {
    #[must_use]
    pub fn new() -> Self {
        // @TODO - we could have knowledge about the capacity
        World {
            shapes: Vec::new(),
            light: PointLight {
                position: Point::new(-10.0, 10.0, -10.0),
                intensity: Color::new(1.0, 1.0, 1.0),
            },
            shadow_bias: EPSILON,
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

    #[allow(clippy::cast_possible_truncation)]
    pub fn push_plane(
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

        self.shapes.push(Shape::Plane {
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

    #[must_use]
    pub fn intersect(&self, ray: &Ray) -> Vec<Intersection> {
        let mut is: Vec<Intersection> =
            self.shapes.iter().flat_map(|s| intersect(s, ray)).collect();
        sort_by_t(&mut is);
        is
    }

    #[must_use]
    pub fn shade_hit(&self, comp: &ComputedIntersection) -> Color {
        let shape = &self.get_shape(comp.object);
        let m = &shape.material();
        let shadowed = self.is_shadowed(&comp.over_point);

        let surface = m.lighting(
            shape,
            &self.light,
            &comp.over_point,
            &comp.eyev,
            &comp.normalv,
            shadowed,
        );

        &self.reflected_color(comp) + &surface
    }

    #[must_use]
    pub fn color_at(&self, ray: &Ray) -> Color {
        let mut is = self.intersect(ray);
        let the_hit = hit(&mut is);

        match the_hit {
            Some(i) => {
                let comp = i.compute(self, ray, self.shadow_bias);
                self.shade_hit(&comp)
            }
            None => Color::new(0.0, 0.0, 0.0),
        }
    }

    #[must_use]
    pub fn is_shadowed(&self, point: &Point) -> bool {
        let v = &self.light.position - point;
        let distance = v.mag();
        let direction = v.norm();

        let r = Ray::new((*point).clone(), direction);

        let mut intersections = self.intersect(&r);

        let h = hit(&mut intersections);

        match h {
            Some(i) => i.t < distance,
            None => false,
        }
    }

    #[must_use]
    pub fn reflected_color(&self, comp: &ComputedIntersection) -> Color {
        let shape = self.get_shape(comp.object);
        if shape.material().reflective == 0.0 {
            Color::new(0.0, 0.0, 0.0)
        } else {
            let reflect_ray = Ray::new(comp.over_point.clone(), comp.reflectv.clone());
            let color = self.color_at(&reflect_ray);

            &color * shape.material().reflective
        }
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
    use crate::rays::Ray;
    use crate::shape::Shape;
    use crate::transformations::{scaling, translation};
    use crate::vector::{Point, Vector};

    fn test_default() -> World {
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

        w.push_sphere(Some(scaling(0.5, 0.5, 0.5)), Some(Material::default()));

        w
    }

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
        let w = test_default();
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
                material: Material::default(),
            }
        );

        assert_eq!(
            w.light,
            PointLight {
                position: Point::new(-10.0, 10.0, -10.0),
                intensity: Color::new(1.0, 1.0, 1.0)
            }
        )
    }

    // Scenario: Intersect a world with a ray
    //   Given w ← default_world()
    //     And r ← ray(point(0, 0, -5), vector(0, 0, 1))
    //   When xs ← intersect_world(w, r)
    //   Then xs.count = 4
    //     And xs[0].t = 4
    //     And xs[1].t = 4.5
    //     And xs[2].t = 5.5
    //     And xs[3].t = 6
    #[test]
    fn intersect_a_world_with_a_ray() {
        let w = test_default();
        let r = Ray::new(Point::new(0.0, 0.0, -5.0), Vector::new(0.0, 0.0, 1.0));

        let xs = w.intersect(&r);

        assert_eq!(xs.len(), 4);
        assert_eq!(xs[0].t, 4.0);
        assert_eq!(xs[1].t, 4.5);
        assert_eq!(xs[2].t, 5.5);
        assert_eq!(xs[3].t, 6.0);
    }

    // Scenario: Shading an intersection
    //   Given w ← default_world()
    //     And r ← ray(point(0, 0, -5), vector(0, 0, 1))
    //     And shape ← the first object in w
    //     And i ← intersection(4, shape)
    //   When comps ← prepare_computations(i, r)
    //     And c ← shade_hit(w, comps)
    //   Then c = color(0.38066, 0.47583, 0.2855)
    #[test]
    fn shading_an_intersection() {
        let w = test_default();
        let r = Ray::new(Point::new(0.0, 0.0, -5.0), Vector::new(0.0, 0.0, 1.0));

        // first shape in test world has id 0
        let i = Intersection::new(4.0, 0);

        let comps = i.compute(&w, &r, EPSILON);
        let c = w.shade_hit(&comps);

        assert_eq!(c, Color::new(0.38066, 0.47583, 0.2855));
    }

    // Scenario: Shading an intersection from the inside
    //   Given w ← default_world()
    //     And w.light ← point_light(point(0, 0.25, 0), color(1, 1, 1))
    //     And r ← ray(point(0, 0, 0), vector(0, 0, 1))
    //     And shape ← the second object in w
    //     And i ← intersection(0.5, shape)
    //   When comps ← prepare_computations(i, r)
    //     And c ← shade_hit(w, comps)
    //   Then c = color(0.90498, 0.90498, 0.90498)
    #[test]
    fn shading_an_intersection_from_the_inside() {
        let w = World {
            light: PointLight::new(Point::new(0.0, 0.25, 0.0), Color::new(1.0, 1.0, 1.0)),
            ..test_default()
        };

        let r = Ray::new(Point::new(0.0, 0.0, 0.0), Vector::new(0.0, 0.0, 1.0));

        // second shape in test world has id 1
        let i = Intersection::new(0.5, 1);

        let comps = i.compute(&w, &r, EPSILON);
        let c = w.shade_hit(&comps);

        assert_eq!(c, Color::new(0.90498, 0.90498, 0.90498));
    }

    // Scenario: The color when a ray misses
    //   Given w ← default_world()
    //     And r ← ray(point(0, 0, -5), vector(0, 1, 0))
    //   When c ← color_at(w, r)
    //   Then c = color(0, 0, 0)
    #[test]
    fn the_color_when_a_ray_misses() {
        let w = test_default();
        let r = Ray::new(Point::new(0.0, 0.0, -5.0), Vector::new(0.0, 1.0, 0.0));

        let c = w.color_at(&r);

        assert_eq!(c, Color::new(0.0, 0.0, 0.0))
    }

    // Scenario: The color when a ray hits
    //   Given w ← default_world()
    //     And r ← ray(point(0, 0, -5), vector(0, 0, 1))
    //   When c ← color_at(w, r)
    //   Then c = color(0.38066, 0.47583, 0.2855)
    #[test]
    fn the_color_when_a_ray_hits() {
        let w = test_default();
        let r = Ray::new(Point::new(0.0, 0.0, -5.0), Vector::new(0.0, 0.0, 1.0));

        let c = w.color_at(&r);

        assert_eq!(c, Color::new(0.38066, 0.47583, 0.2855))
    }

    // Scenario: The color with an intersection behind the ray
    //   Given w ← default_world()
    //     And outer ← the first object in w
    //     And outer.material.ambient ← 1
    //     And inner ← the second object in w
    //     And inner.material.ambient ← 1
    //     And r ← ray(point(0, 0, 0.75), vector(0, 0, -1))
    //   When c ← color_at(w, r)
    //   Then c = inner.material.color
    #[test]
    fn the_color_with_an_intersection_behind_the_ray() {
        let mut w = World::default();

        w.push_sphere(
            None,
            Some(Material {
                color: Color::new(0.8, 1.0, 0.6),
                diffuse: 0.7,
                specular: 0.2,
                ambient: 1.0,
                ..Default::default()
            }),
        );

        let inner_id = w.push_sphere(
            Some(scaling(0.5, 0.5, 0.5)),
            Some(Material {
                ambient: 1.0,
                ..Material::default()
            }),
        );

        let r = Ray::new(Point::new(0.0, 0.0, 0.75), Vector::new(0.0, 0.0, -1.0));
        let c = w.color_at(&r);

        assert_eq!(c, w.get_shape(inner_id).material().color)
    }

    // Scenario: There is no shadow when nothing is collinear with point and light
    //   Given w ← default_world()
    //     And p ← point(0, 10, 0)
    //    Then is_shadowed(w, p) is false
    #[test]
    fn there_is_no_shadow_when_nothing_is_collinear_with_point_and_light() {
        let w = test_default();
        let p = Point::new(0.0, 10.0, 0.0);

        let is_in_shadow = w.is_shadowed(&p);

        assert_eq!(is_in_shadow, false)
    }

    // Scenario: The shadow when an object is between the point and the light
    //   Given w ← default_world()
    //     And p ← point(10, -10, 10)
    //    Then is_shadowed(w, p) is true
    #[test]
    fn the_shadow_when_an_object_is_between_the_point_and_the_light() {
        let w = test_default();
        let p = Point::new(10.0, -10.0, 10.0);

        let is_in_shadow = w.is_shadowed(&p);

        assert_eq!(is_in_shadow, true)
    }

    // Scenario: There is no shadow when an object is behind the light
    //   Given w ← default_world()
    //     And p ← point(-20, 20, -20)
    //    Then is_shadowed(w, p) is false
    #[test]
    fn there_is_no_shadow_when_an_object_is_behind_the_light() {
        let w = test_default();
        let p = Point::new(-20.0, 20.0, -20.0);

        let is_in_shadow = w.is_shadowed(&p);

        assert_eq!(is_in_shadow, false)
    }

    // Scenario: There is no shadow when an object is behind the point
    //   Given w ← default_world()
    //     And p ← point(-2, 2, -2)
    //    Then is_shadowed(w, p) is false
    #[test]
    fn there_is_no_shadow_when_an_object_is_behind_the_point() {
        let w = test_default();
        let p = Point::new(-2.0, 2.0, -2.0);

        let is_in_shadow = w.is_shadowed(&p);

        assert_eq!(is_in_shadow, false)
    }

    // Scenario: shade_hit() is given an intersection in shadow
    //   Given w ← world()
    //     And w.light ← point_light(point(0, 0, -10), color(1, 1, 1))
    //     And s1 ← sphere()
    //     And s1 is added to w
    //     And s2 ← sphere() with:
    //       | transform | translation(0, 0, 10) |
    //     And s2 is added to w
    //     And r ← ray(point(0, 0, 5), vector(0, 0, 1))
    //     And i ← intersection(4, s2)
    //   When comps ← prepare_computations(i, r)
    //     And c ← shade_hit(w, comps)
    //   Then c = color(0.1, 0.1, 0.1)
    #[test]
    fn shade_hit_is_given_an_intersection_in_shadow() {
        let mut w = World::new();
        w.light = PointLight::new(Point::new(0.0, 0.0, -10.0), Color::new(1.0, 1.0, 1.0));
        w.push_sphere(None, None);
        let s2_id = w.push_sphere(Some(translation(0.0, 0.0, 10.0)), None);

        let r = Ray::new(Point::new(0.0, 0.0, 5.0), Vector::new(0.0, 0.0, 1.0));
        let i = Intersection::new(4.0, s2_id);

        let comps = i.compute(&w, &r, EPSILON);

        let c = w.shade_hit(&comps);

        assert_eq!(c, Color::new(0.1, 0.1, 0.1))
    }

    // Scenario: The reflected color for a nonreflective material
    //   Given w ← default_world()
    //     And r ← ray(point(0, 0, 0), vector(0, 0, 1))
    //     And shape ← the second object in w
    //     And shape.material.ambient ← 1
    //     And i ← intersection(1, shape)
    //   When comps ← prepare_computations(i, r)
    //     And color ← reflected_color(w, comps)
    //   Then color = color(0, 0, 0)
    #[test]
    fn the_reflected_color_for_a_nonreflective_material() {
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

        let sid = w.push_sphere(
            Some(scaling(0.5, 0.5, 0.5)),
            Some(Material {
                ambient: 1.0,
                ..Default::default()
            }),
        );

        let r = Ray::new(Point::new(0.0, 0.0, 0.0), Vector::new(0.0, 0.0, 1.0));

        let i = Intersection::new(1.0, sid);
        let comps = i.compute(&w, &r, EPSILON);
        let color = w.reflected_color(&comps);

        assert_eq!(color, Color::new(0.0, 0.0, 0.0));
    }

    // Scenario: The reflected color for a reflective material
    //   Given w ← default_world()
    //     And shape ← plane() with:
    //       | material.reflective | 0.5                   |
    //       | transform           | translation(0, -1, 0) |
    //     And shape is added to w
    //     And r ← ray(point(0, 0, -3), vector(0, -√2/2, √2/2))
    //     And i ← intersection(√2, shape)
    //   When comps ← prepare_computations(i, r)
    //     And color ← reflected_color(w, comps)
    //   Then color = color(0.19032, 0.2379, 0.14274)
    #[test]
    fn the_reflected_color_for_a_reflective_material() {
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
                ..Default::default()
            }),
        );

        let pid = w.push_plane(
            Some(translation(0.0, -1.0, 0.0)),
            Some(Material {
                reflective: 0.5,
                ..Default::default()
            }),
        );

        let r = Ray::new(
            Point::new(0.0, 0.0, -3.0),
            Vector::new(0.0, -f32::sqrt(2.0) / 2.0, f32::sqrt(2.0) / 2.0),
        );

        let i = Intersection::new(f32::sqrt(2.0), pid);
        let comps = i.compute(&w, &r, EPSILON);
        let color = w.reflected_color(&comps);

        assert_eq!(color, Color::new(0.190332, 0.23791, 0.142749));
    }

    // Scenario: shade_hit() with a reflective material
    //   Given w ← default_world()
    //     And shape ← plane() with:
    //       | material.reflective | 0.5                   |
    //       | transform           | translation(0, -1, 0) |
    //     And shape is added to w
    //     And r ← ray(point(0, 0, -3), vector(0, -√2/2, √2/2))
    //     And i ← intersection(√2, shape)
    //   When comps ← prepare_computations(i, r)
    //     And color ← shade_hit(w, comps)
    //   Then color = color(0.87677, 0.92436, 0.82918)
    #[test]
    fn shade_hit_with_a_reflective_material() {
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
                ..Default::default()
            }),
        );

        let pid = w.push_plane(
            Some(translation(0.0, -1.0, 0.0)),
            Some(Material {
                reflective: 0.5,
                ..Default::default()
            }),
        );

        let r = Ray::new(
            Point::new(0.0, 0.0, -3.0),
            Vector::new(0.0, -f32::sqrt(2.0) / 2.0, f32::sqrt(2.0) / 2.0),
        );

        let i = Intersection::new(f32::sqrt(2.0), pid);
        let comps = i.compute(&w, &r, EPSILON);
        let color = w.shade_hit(&comps);

        assert_eq!(color, Color::new(0.876757, 0.92434, 0.82917));
    }
}
