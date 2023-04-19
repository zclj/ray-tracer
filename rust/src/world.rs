use crate::color::Color;
use crate::intersections::{sort_by_t, ComputedIntersection, Intersection};
use crate::lights::PointLight;
use crate::materials::Material;
use crate::matrices::M4x4;
use crate::rays::Ray;
use crate::shape::{RenderObject, RenderObjectTemplate, Shape};
use crate::utils::{epsilon_eq, EPSILON};
use crate::vector::Point;

#[derive(Debug)]
pub struct RenderGroup {
    id: u32,
    objects: Vec<RenderObject>,
}

impl RenderGroup {
    #[must_use]
    #[allow(clippy::cast_possible_truncation)]
    pub fn new(id: u32, objects: Vec<RenderObjectTemplate>) -> Self {
        let render_objects = objects
            .into_iter()
            .enumerate()
            .map(|(i, o)| RenderObject::new(i as u32, o))
            .collect::<Vec<RenderObject>>();

        RenderGroup {
            id,
            objects: render_objects,
        }
    }
}

#[derive(Debug)]
pub struct World {
    pub light: PointLight,
    pub shadow_bias: f32,
    objects: Vec<RenderObject>,
    //groups: HashMap<u32, Vec<u32>>,
    groups: Vec<RenderGroup>,
}

fn check_cap(ray: &Ray, t: f64, r: f64) -> bool {
    let x = f64::from(ray.origin.x) + t * f64::from(ray.direction.x);
    let z = f64::from(ray.origin.z) + t * f64::from(ray.direction.z);

    (x * x + z * z) <= r * r
}

#[allow(clippy::cast_possible_truncation)]
fn intersect_caps(s: &RenderObject, ray: &Ray, xs: &mut Vec<Intersection>) {
    // caps only matter if the cylinder is closed, and might possibly be
    // intersected by the ray.
    if !s.closed() || epsilon_eq(ray.direction.y, 0.0) {
        return;
    }

    let (id, r_min, r_max) = match s.kind {
        Shape::Cylinder { .. } => (s.id, 1.0, 1.0),
        Shape::Cone {
            minimum, maximum, ..
        } => (s.id, minimum, maximum),
        _ => panic!("Caps only supported for Cones and Cylinders"),
    };

    // check for an intersection with the lower end cap by intersecting
    // the ray with the plane at y=s.minimum
    let tmin = (f64::from(s.minimum()) - f64::from(ray.origin.y)) / f64::from(ray.direction.y);

    if check_cap(ray, tmin, r_min.into()) {
        xs.push(Intersection::new(tmin as f32, id));
    }

    // check for an intersection with the upper end cap by intersecting
    // the ray with the plane at y=s.maximum
    let tmax = (f64::from(s.maximum()) - f64::from(ray.origin.y)) / f64::from(ray.direction.y);
    if check_cap(ray, tmax, r_max.into()) {
        xs.push(Intersection::new(tmax as f32, id));
    }
}

impl World {
    #[must_use]
    pub fn new() -> Self {
        World {
            objects: Vec::new(),
            groups: Vec::new(),
            light: PointLight {
                position: Point::new(-10.0, 10.0, -10.0),
                intensity: Color::new(1.0, 1.0, 1.0),
            },
            shadow_bias: EPSILON,
        }
    }

    #[allow(clippy::cast_possible_truncation)]
    pub fn push_group(&mut self, objects: Vec<RenderObjectTemplate>) -> u32 {
        let gid = self.groups.len() as u32;

        self.groups.push(RenderGroup::new(gid, objects));

        gid
    }

    pub fn push_sphere(
        &mut self,
        transform_option: Option<M4x4>,
        material_option: Option<Material>,
    ) -> u32 {
        self.push_shape(Shape::Sphere, transform_option, material_option)
    }

    pub fn push_plane(
        &mut self,
        transform_option: Option<M4x4>,
        material_option: Option<Material>,
    ) -> u32 {
        self.push_shape(Shape::Plane, transform_option, material_option)
    }

    pub fn push_cube(
        &mut self,
        transform_option: Option<M4x4>,
        material_option: Option<Material>,
    ) -> u32 {
        self.push_shape(Shape::Cube, transform_option, material_option)
    }

    #[allow(clippy::cast_possible_truncation)]
    pub fn push_cylinder(
        &mut self,
        transform_option: Option<M4x4>,
        material_option: Option<Material>,
        minimum: f32,
        maximum: f32,
        closed: bool,
    ) -> u32 {
        self.push_shape(
            Shape::Cylinder {
                minimum,
                maximum,
                closed,
            },
            transform_option,
            material_option,
        )
    }

    #[allow(clippy::cast_possible_truncation)]
    pub fn push_cone(
        &mut self,
        transform_option: Option<M4x4>,
        material_option: Option<Material>,
        minimum: f32,
        maximum: f32,
        closed: bool,
    ) -> u32 {
        self.push_shape(
            Shape::Cone {
                minimum,
                maximum,
                closed,
            },
            transform_option,
            material_option,
        )
    }

    #[allow(clippy::cast_possible_truncation)]
    pub fn push_shape(
        &mut self,
        kind: Shape,
        transform_option: Option<M4x4>,
        material_option: Option<Material>,
    ) -> u32 {
        let transform = match transform_option {
            Some(t) => t,
            None => M4x4::IDENTITY,
        };

        let material = match material_option {
            Some(m) => m,
            None => Material::default(),
        };

        let transform_inverse = transform.inverse();
        let transform_inverse_transpose = transform_inverse.transpose();

        let id = self.objects.len() as u32;

        self.objects.push(RenderObject {
            id,
            kind,
            transform,
            material,
            transform_inverse,
            transform_inverse_transpose,
        });

        id
    }

    #[must_use]
    pub fn get_object(&self, id: u32) -> &RenderObject {
        &self.objects[id as usize]
    }

    pub fn intersect_2(
        &self,
        world_ray: &Ray,
        intersections: &mut Vec<Intersection>,
    ) -> Option<usize> {
        intersections.clear();

        ////
        // Groups
        // - We want to check boundaries of groups
        // - If we hit the group, check all objects in that group

        for g in &self.groups {
            g.objects.iter().for_each(|s| {
                let ray = world_ray.transform(&s.transform_inverse);

                match s.kind {
                    Shape::Group { .. } => {}
                    Shape::Sphere => {
                        let sphere_to_ray = &ray.origin - &Point::new(0.0, 0.0, 0.0);

                        let a = ray.direction.dot(&ray.direction);
                        let b = 2.0 * ray.direction.dot(&sphere_to_ray);
                        let c = sphere_to_ray.dot(&sphere_to_ray) - 1.0;

                        let discriminant = b.powf(2.0) - (4.0 * a * c);

                        // invert?
                        if discriminant < 0.0 {
                            return;
                        }

                        let t1 = (-b - f32::sqrt(discriminant)) / (2.0 * a);
                        let t2 = (-b + f32::sqrt(discriminant)) / (2.0 * a);

                        let id = s.id;
                        intersections.push(Intersection { t: t1, object: id });
                        intersections.push(Intersection { t: t2, object: id });
                    }
                    Shape::Plane => {
                        if ray.direction.y.abs() >= EPSILON {
                            intersections
                                .push(Intersection::new(-ray.origin.y / ray.direction.y, s.id));
                        }
                    }
                    Shape::Cube => {
                        let (xtmin, xtmax) = s.check_axis(ray.origin.x, ray.direction.x);
                        let (ytmin, ytmax) = s.check_axis(ray.origin.y, ray.direction.y);
                        let (ztmin, ztmax) = s.check_axis(ray.origin.z, ray.direction.z);

                        let tmin = f32::max(f32::max(xtmin, ytmin), ztmin);
                        let tmax = f32::min(f32::min(xtmax, ytmax), ztmax);

                        if tmax >= tmin {
                            intersections.push(Intersection::new(tmin, s.id));
                            intersections.push(Intersection::new(tmax, s.id));
                        }
                    }
                    Shape::Cylinder { .. } => {
                        let xd = ray.direction.x;
                        let zd = ray.direction.z;
                        let a = xd * xd + zd * zd;

                        // ray is NOT parallel with the y axis
                        if !epsilon_eq(a, 0.0) {
                            let xo = ray.origin.x;
                            let zo = ray.origin.z;
                            let b = (2.0 * xo * xd) + (2.0 * zo * zd);
                            let c = xo * xo + zo * zo - 1.0;

                            let discriminant = b.powf(2.0) - (4.0 * a * c);

                            // ray does intersect the cylinder
                            if discriminant >= 0.0 {
                                let a2 = 2.0 * a;
                                let t0 = (-b - f32::sqrt(discriminant)) / a2;
                                let t1 = (-b + f32::sqrt(discriminant)) / a2;

                                let (t0, t1) = if t0 > t1 { (t1, t0) } else { (t0, t1) };

                                let y0 = (ray.origin.y) + t0 * (ray.direction.y);

                                if (s.minimum()) < y0 && y0 < (s.maximum()) {
                                    intersections.push(Intersection {
                                        t: t0,
                                        object: s.id,
                                    });
                                }
                                let y1 = (ray.origin.y) + t1 * (ray.direction.y);

                                if (s.minimum()) < y1 && y1 < (s.maximum()) {
                                    intersections.push(Intersection {
                                        t: t1,
                                        object: s.id,
                                    });
                                }
                            }
                        }
                        intersect_caps(s, &ray, intersections);
                    }
                    Shape::Cone { .. } => {
                        let xd = ray.direction.x;
                        let yd = ray.direction.y;
                        let zd = ray.direction.z;

                        let xo = ray.origin.x;
                        let yo = ray.origin.y;
                        let zo = ray.origin.z;

                        let a = xd * xd - yd * yd + zd * zd;
                        let b = (2.0 * xo * xd) - (2.0 * yo * yd) + (2.0 * zo * zd);
                        let c = xo * xo - yo * yo + zo * zo;

                        if epsilon_eq(a, 0.0) && !epsilon_eq(b, 0.0) {
                            intersections.push(Intersection {
                                t: -c / (2.0 * b),
                                object: s.id,
                            });
                        } else {
                            let discriminant = b.powf(2.0) - (4.0 * a * c);

                            // ray does intersect the cylinder
                            if discriminant >= (-EPSILON) {
                                let sqrt_discriminant = f32::sqrt(f32::max(0.0, discriminant));
                                let a2 = 2.0 * a;
                                let t0 = (-b - sqrt_discriminant) / a2;
                                let t1 = (-b + sqrt_discriminant) / a2;

                                let (t0, t1) = if t0 > t1 { (t1, t0) } else { (t0, t1) };

                                let y0 = (ray.origin.y) + t0 * (ray.direction.y);
                                if (s.minimum()) < y0 && y0 < (s.maximum()) {
                                    intersections.push(Intersection {
                                        t: t0,
                                        object: s.id,
                                    });
                                }
                                let y1 = (ray.origin.y) + t1 * (ray.direction.y);
                                if (s.minimum()) < y1 && y1 < (s.maximum()) {
                                    intersections.push(Intersection {
                                        t: t1,
                                        object: s.id,
                                    });
                                }
                            }
                        }

                        intersect_caps(s, &ray, intersections);
                    }
                }
            })
        }
        // Sort the intersections and return the index of the 'hit'
        sort_by_t(intersections);

        intersections.iter().position(|x| x.t >= 0.0)
    }

    #[allow(clippy::similar_names)]
    #[allow(clippy::too_many_lines)]
    pub fn intersect(
        &self,
        world_ray: &Ray,
        intersections: &mut Vec<Intersection>,
    ) -> Option<usize> {
        intersections.clear();

        ////
        // Groups
        // - We want to check boundaries of groups
        // - If we hit the group, check all objects in that group

        self.objects.iter().for_each(|s| {
            let ray = world_ray.transform(&s.transform_inverse);

            match s.kind {
                Shape::Group { .. } => {}
                Shape::Sphere => {
                    let sphere_to_ray = &ray.origin - &Point::new(0.0, 0.0, 0.0);

                    let a = ray.direction.dot(&ray.direction);
                    let b = 2.0 * ray.direction.dot(&sphere_to_ray);
                    let c = sphere_to_ray.dot(&sphere_to_ray) - 1.0;

                    let discriminant = b.powf(2.0) - (4.0 * a * c);

                    // invert?
                    if discriminant < 0.0 {
                        return;
                    }

                    let t1 = (-b - f32::sqrt(discriminant)) / (2.0 * a);
                    let t2 = (-b + f32::sqrt(discriminant)) / (2.0 * a);

                    let id = s.id;
                    intersections.push(Intersection { t: t1, object: id });
                    intersections.push(Intersection { t: t2, object: id });
                }
                Shape::Plane => {
                    if ray.direction.y.abs() >= EPSILON {
                        intersections
                            .push(Intersection::new(-ray.origin.y / ray.direction.y, s.id));
                    }
                }
                Shape::Cube => {
                    let (xtmin, xtmax) = s.check_axis(ray.origin.x, ray.direction.x);
                    let (ytmin, ytmax) = s.check_axis(ray.origin.y, ray.direction.y);
                    let (ztmin, ztmax) = s.check_axis(ray.origin.z, ray.direction.z);

                    let tmin = f32::max(f32::max(xtmin, ytmin), ztmin);
                    let tmax = f32::min(f32::min(xtmax, ytmax), ztmax);

                    if tmax >= tmin {
                        intersections.push(Intersection::new(tmin, s.id));
                        intersections.push(Intersection::new(tmax, s.id));
                    }
                }
                Shape::Cylinder { .. } => {
                    let xd = ray.direction.x;
                    let zd = ray.direction.z;
                    let a = xd * xd + zd * zd;

                    // ray is NOT parallel with the y axis
                    if !epsilon_eq(a, 0.0) {
                        let xo = ray.origin.x;
                        let zo = ray.origin.z;
                        let b = (2.0 * xo * xd) + (2.0 * zo * zd);
                        let c = xo * xo + zo * zo - 1.0;

                        let discriminant = b.powf(2.0) - (4.0 * a * c);

                        // ray does intersect the cylinder
                        if discriminant >= 0.0 {
                            let a2 = 2.0 * a;
                            let t0 = (-b - f32::sqrt(discriminant)) / a2;
                            let t1 = (-b + f32::sqrt(discriminant)) / a2;

                            let (t0, t1) = if t0 > t1 { (t1, t0) } else { (t0, t1) };

                            let y0 = (ray.origin.y) + t0 * (ray.direction.y);

                            if (s.minimum()) < y0 && y0 < (s.maximum()) {
                                intersections.push(Intersection {
                                    t: t0,
                                    object: s.id,
                                });
                            }
                            let y1 = (ray.origin.y) + t1 * (ray.direction.y);

                            if (s.minimum()) < y1 && y1 < (s.maximum()) {
                                intersections.push(Intersection {
                                    t: t1,
                                    object: s.id,
                                });
                            }
                        }
                    }
                    intersect_caps(s, &ray, intersections);
                }
                Shape::Cone { .. } => {
                    let xd = ray.direction.x;
                    let yd = ray.direction.y;
                    let zd = ray.direction.z;

                    let xo = ray.origin.x;
                    let yo = ray.origin.y;
                    let zo = ray.origin.z;

                    let a = xd * xd - yd * yd + zd * zd;
                    let b = (2.0 * xo * xd) - (2.0 * yo * yd) + (2.0 * zo * zd);
                    let c = xo * xo - yo * yo + zo * zo;

                    if epsilon_eq(a, 0.0) && !epsilon_eq(b, 0.0) {
                        intersections.push(Intersection {
                            t: -c / (2.0 * b),
                            object: s.id,
                        });
                    } else {
                        let discriminant = b.powf(2.0) - (4.0 * a * c);

                        // ray does intersect the cylinder
                        if discriminant >= (-EPSILON) {
                            let sqrt_discriminant = f32::sqrt(f32::max(0.0, discriminant));
                            let a2 = 2.0 * a;
                            let t0 = (-b - sqrt_discriminant) / a2;
                            let t1 = (-b + sqrt_discriminant) / a2;

                            let (t0, t1) = if t0 > t1 { (t1, t0) } else { (t0, t1) };

                            let y0 = (ray.origin.y) + t0 * (ray.direction.y);
                            if (s.minimum()) < y0 && y0 < (s.maximum()) {
                                intersections.push(Intersection {
                                    t: t0,
                                    object: s.id,
                                });
                            }
                            let y1 = (ray.origin.y) + t1 * (ray.direction.y);
                            if (s.minimum()) < y1 && y1 < (s.maximum()) {
                                intersections.push(Intersection {
                                    t: t1,
                                    object: s.id,
                                });
                            }
                        }
                    }

                    intersect_caps(s, &ray, intersections);
                }
            }
        });

        // Sort the intersections and return the index of the 'hit'
        sort_by_t(intersections);

        intersections.iter().position(|x| x.t >= 0.0)
    }

    #[must_use]
    pub fn shade_hit(
        &self,
        comp: &ComputedIntersection,
        remaining: u8,
        intersections: &mut Vec<Intersection>,
        containers: &mut Vec<u32>,
    ) -> Color {
        let shape = &self.get_object(comp.object);
        let m = &shape.material;
        let shadowed = self.is_shadowed(&comp.over_point, intersections);

        let surface = m.lighting(
            shape,
            &self.light,
            &comp.over_point,
            &comp.eyev,
            &comp.normalv,
            shadowed,
        );

        let reflected = self.reflected_color(comp, remaining, intersections, containers);
        let refracted = self.refracted_color(comp, remaining, intersections, containers);

        if m.reflective > 0.0 && m.transparency > 0.0 {
            let reflectance = comp.schlick();
            return &surface + &(&reflected * reflectance) + refracted * (1.0 - reflectance);
        }

        &surface + &reflected + refracted
    }

    #[must_use]
    pub fn color_at(
        &self,
        ray: &Ray,
        remaining: u8,
        intersections: &mut Vec<Intersection>,
        containers: &mut Vec<u32>,
    ) -> Color {
        let the_hit = self.intersect(ray, intersections);

        match the_hit {
            Some(idx) => {
                let i = &intersections[idx];
                let comp = i.compute(self, ray, intersections, self.shadow_bias, containers);
                self.shade_hit(&comp, remaining, intersections, containers)
            }
            _ => Color::new(0.0, 0.0, 0.0),
        }
    }

    #[must_use]
    pub fn is_shadowed(&self, point: &Point, intersections: &mut Vec<Intersection>) -> bool {
        let v = &self.light.position - point;
        let distance = v.mag();
        let direction = v.norm();

        let r = Ray::new((*point).clone(), direction);

        let h = self.intersect(&r, intersections);

        match h {
            Some(idx) => intersections[idx].t < distance,
            None => false,
        }
    }

    #[must_use]
    pub fn reflected_color(
        &self,
        comp: &ComputedIntersection,
        remaining: u8,
        intersections: &mut Vec<Intersection>,
        containers: &mut Vec<u32>,
    ) -> Color {
        if remaining == 0 {
            return Color::new(0.0, 0.0, 0.0);
        }
        let shape = self.get_object(comp.object);
        if shape.material.reflective == 0.0 {
            Color::new(0.0, 0.0, 0.0)
        } else {
            let reflect_ray = Ray::new(comp.over_point.clone(), comp.reflectv.clone());
            let color = self.color_at(&reflect_ray, remaining - 1, intersections, containers);

            &color * shape.material.reflective
        }
    }

    #[must_use]
    pub fn refracted_color(
        &self,
        comp: &ComputedIntersection,
        remaining: u8,
        intersections: &mut Vec<Intersection>,
        containers: &mut Vec<u32>,
    ) -> Color {
        if remaining == 0 {
            return Color::new(0.0, 0.0, 0.0);
        }

        let shape = self.get_object(comp.object);

        let n_ratio = comp.n1 / comp.n2;
        let cos_i = comp.cos_i; //comp.eyev.dot(&comp.normalv);
        let sin2_t = n_ratio.powf(2.0) * (1.0 - cos_i.powf(2.0));

        // check for total internal reflection
        if sin2_t > 1.0 {
            // internal reflection, i.e, return black
            return Color::new(0.0, 0.0, 0.0);
        }

        if shape.material.transparency == 0.0 {
            Color::new(0.0, 0.0, 0.0)
        } else {
            let cos_t = f32::sqrt(1.0 - sin2_t);
            // compute the direction of the refracted ray
            let direction = &comp.normalv * (n_ratio * cos_i - cos_t) - &comp.eyev * n_ratio;
            // create the refracted ray
            let refracted_ray = Ray::new(comp.under_point.clone(), direction);

            // find the color of the refracted ray
            self.color_at(&refracted_ray, remaining - 1, intersections, containers)
                * shape.material.transparency
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

        let s_id = ctx.get_object(0).id;

        assert_eq!(s_id, 0);
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
        let s1 = w.get_object(0);
        let s2 = w.get_object(1);

        assert_eq!(
            s1,
            &RenderObject {
                id: 0,
                kind: Shape::Sphere,
                transform: M4x4::IDENTITY,
                transform_inverse: M4x4::IDENTITY,
                transform_inverse_transpose: M4x4::IDENTITY,
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
            &RenderObject {
                id: 1,
                kind: Shape::Sphere,
                transform: scaling(0.5, 0.5, 0.5),
                material: Material::default(),
                transform_inverse: scaling(0.5, 0.5, 0.5).inverse(),
                transform_inverse_transpose: scaling(0.5, 0.5, 0.5).inverse().transpose(),
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

        let mut xs = vec![];
        w.intersect(&r, &mut xs);

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

        let mut containers = vec![];
        let comps = i.compute(&w, &r, &[i.clone()], EPSILON, &mut containers);
        let mut intersections = vec![];
        let c = w.shade_hit(&comps, 0, &mut intersections, &mut containers);

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

        let mut containers = vec![];
        let comps = i.compute(&w, &r, &[i.clone()], EPSILON, &mut containers);
        let mut intersections = vec![];
        let c = w.shade_hit(&comps, 0, &mut intersections, &mut containers);

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

        let mut intersections = vec![];
        let mut containers = vec![];
        let c = w.color_at(&r, 1, &mut intersections, &mut containers);

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

        let mut containers = vec![];
        let mut intersections = vec![];
        let c = w.color_at(&r, 1, &mut intersections, &mut containers);

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
        let mut containers = vec![];
        let mut intersections = vec![];
        let c = w.color_at(&r, 1, &mut intersections, &mut containers);

        assert_eq!(c, w.get_object(inner_id).material.color)
    }

    // Scenario: There is no shadow when nothing is collinear with point and light
    //   Given w ← default_world()
    //     And p ← point(0, 10, 0)
    //    Then is_shadowed(w, p) is false
    #[test]
    fn there_is_no_shadow_when_nothing_is_collinear_with_point_and_light() {
        let w = test_default();
        let p = Point::new(0.0, 10.0, 0.0);

        let mut intersections = vec![];
        let is_in_shadow = w.is_shadowed(&p, &mut intersections);

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

        let mut intersections = vec![];
        let is_in_shadow = w.is_shadowed(&p, &mut intersections);

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

        let mut intersections = vec![];
        let is_in_shadow = w.is_shadowed(&p, &mut intersections);

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

        let mut intersections = vec![];
        let is_in_shadow = w.is_shadowed(&p, &mut intersections);

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

        let mut containers = vec![];
        let comps = i.compute(&w, &r, &[i.clone()], EPSILON, &mut containers);
        let mut intersections = vec![];
        let c = w.shade_hit(&comps, 0, &mut intersections, &mut containers);

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
        let mut containers = vec![];
        let comps = i.compute(&w, &r, &[i.clone()], EPSILON, &mut containers);
        let mut intersections = vec![];
        let color = w.reflected_color(&comps, 0, &mut intersections, &mut containers);

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
        let mut containers = vec![];
        let comps = i.compute(&w, &r, &[i.clone()], EPSILON, &mut containers);
        let mut intersections = vec![];
        let color = w.reflected_color(&comps, 1, &mut intersections, &mut containers);

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
        let mut containers = vec![];
        let comps = i.compute(&w, &r, &[i.clone()], EPSILON, &mut containers);

        let mut intersections = vec![];
        let color = w.shade_hit(&comps, 1, &mut intersections, &mut containers);

        assert_eq!(color, Color::new(0.876757, 0.92434, 0.82917));
    }

    // Scenario: color_at() with mutually reflective surfaces
    //   Given w ← world()
    //     And w.light ← point_light(point(0, 0, 0), color(1, 1, 1))
    //     And lower ← plane() with:
    //       | material.reflective | 1                     |
    //       | transform           | translation(0, -1, 0) |
    //     And lower is added to w
    //     And upper ← plane() with:
    //       | material.reflective | 1                    |
    //       | transform           | translation(0, 1, 0) |
    //     And upper is added to w
    //     And r ← ray(point(0, 0, 0), vector(0, 1, 0))
    //   Then color_at(w, r) should terminate successfully
    #[test]
    fn color_at_with_mutually_reflective_surfaces() {
        let mut w = World::new();
        w.light = PointLight::new(Point::new(0.0, 0.0, 0.0), Color::new(1.0, 1.0, 1.0));
        let _lower_id = w.push_plane(
            Some(translation(0.0, -1.0, 0.0)),
            Some(Material {
                reflective: 1.0,
                ..Default::default()
            }),
        );

        let _upper_id = w.push_plane(
            Some(translation(0.0, 1.0, 0.0)),
            Some(Material {
                reflective: 1.0,
                ..Default::default()
            }),
        );

        let r = Ray::new(Point::new(0.0, 0.0, -3.0), Vector::new(0.0, 1.0, 0.0));
        let mut containers = vec![];
        let mut intersections = vec![];

        assert_eq!(
            w.color_at(&r, 1, &mut intersections, &mut containers),
            Color::new(0.7692048, 0.7692048, 0.7692048)
        );
    }

    // Scenario: The reflected color at the maximum recursive depth
    //   Given w ← default_world()
    //     And shape ← plane() with:
    //       | material.reflective | 0.5                   |
    //       | transform           | translation(0, -1, 0) |
    //     And shape is added to w
    //     And r ← ray(point(0, 0, -3), vector(0, -√2/2, √2/2))
    //     And i ← intersection(√2, shape)
    //   When comps ← prepare_computations(i, r)
    //     And color ← reflected_color(w, comps, 0)
    //   Then color = color(0, 0, 0)
    #[test]
    fn the_reflected_color_at_the_maximum_recursive_depth() {
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
        let mut containers = vec![];
        let comps = i.compute(&w, &r, &[i.clone()], EPSILON, &mut containers);
        let mut intersections = vec![];
        let color = w.reflected_color(&comps, 0, &mut intersections, &mut containers);

        assert_eq!(color, Color::new(0.0, 0.0, 0.0));
    }

    // Scenario: The refracted color with an opaque surface
    //   Given w ← default_world()
    //     And shape ← the first object in w
    //     And r ← ray(point(0, 0, -5), vector(0, 0, 1))
    //     And xs ← intersections(4:shape, 6:shape)
    //   When comps ← prepare_computations(xs[0], r, xs)
    //     And c ← refracted_color(w, comps, 5)
    //   Then c = color(0, 0, 0)
    #[test]
    fn the_refracted_color_with_an_opaque_surface() {
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

        let r = Ray::new(Point::new(0.0, 0.0, -5.0), Vector::new(0.0, 0.0, 1.0));

        let xs = [Intersection::new(4.0, sid), Intersection::new(6.0, sid)];
        let mut containers = vec![];
        let comps = xs[0].compute(&w, &r, &xs, EPSILON, &mut containers);

        let mut intersections = vec![];
        let c = w.refracted_color(&comps, 5, &mut intersections, &mut containers);

        assert_eq!(c, Color::new(0.0, 0.0, 0.0))
    }

    // Scenario: The refracted color at the maximum recursive depth
    //   Given w ← default_world()
    //     And shape ← the first object in w
    //     And shape has:
    //       | material.transparency     | 1.0 |
    //       | material.refractive_index | 1.5 |
    //     And r ← ray(point(0, 0, -5), vector(0, 0, 1))
    //     And xs ← intersections(4:shape, 6:shape)
    //   When comps ← prepare_computations(xs[0], r, xs)
    //     And c ← refracted_color(w, comps, 0)
    //   Then c = color(0, 0, 0)
    #[test]
    fn the_refracted_color_at_the_maximum_recursive_depth() {
        let mut w = World::new();

        let sid = w.push_sphere(
            None,
            Some(Material {
                color: Color::new(0.8, 1.0, 0.6),
                diffuse: 0.7,
                specular: 0.2,
                transparency: 1.0,
                refractive_index: 1.5,
                ..Default::default()
            }),
        );

        w.push_sphere(
            Some(scaling(0.5, 0.5, 0.5)),
            Some(Material {
                ambient: 1.0,
                ..Default::default()
            }),
        );

        let r = Ray::new(Point::new(0.0, 0.0, -5.0), Vector::new(0.0, 0.0, 1.0));

        let xs = [Intersection::new(4.0, sid), Intersection::new(6.0, sid)];
        let mut containers = vec![];
        let comps = xs[0].compute(&w, &r, &xs, EPSILON, &mut containers);

        let mut intersections = vec![];
        let c = w.refracted_color(&comps, 0, &mut intersections, &mut containers);

        assert_eq!(c, Color::new(0.0, 0.0, 0.0))
    }

    // Scenario: The refracted color under total internal reflection
    //   Given w ← default_world()
    //     And shape ← the first object in w
    //     And shape has:
    //       | material.transparency     | 1.0 |
    //       | material.refractive_index | 1.5 |
    //     And r ← ray(point(0, 0, √2/2), vector(0, 1, 0))
    //     And xs ← intersections(-√2/2:shape, √2/2:shape)
    //   # NOTE: this time you're inside the sphere, so you need
    //   # to look at the second intersection, xs[1], not xs[0]
    //   When comps ← prepare_computations(xs[1], r, xs)
    //     And c ← refracted_color(w, comps, 5)
    //   Then c = color(0, 0, 0)
    #[test]
    fn the_refracted_color_under_total_internal_reflection() {
        let mut w = World::new();

        let sid = w.push_sphere(
            None,
            Some(Material {
                color: Color::new(0.8, 1.0, 0.6),
                diffuse: 0.7,
                specular: 0.2,
                transparency: 1.0,
                refractive_index: 1.5,
                ..Default::default()
            }),
        );

        w.push_sphere(
            Some(scaling(0.5, 0.5, 0.5)),
            Some(Material {
                ambient: 1.0,
                ..Default::default()
            }),
        );

        let r = Ray::new(
            Point::new(0.0, 0.0, f32::sqrt(2.0) / 2.0),
            Vector::new(0.0, 1.0, 0.0),
        );

        let xs = [
            Intersection::new(-f32::sqrt(2.0) / 2.0, sid),
            Intersection::new(f32::sqrt(2.0) / 2.0, sid),
        ];
        let mut containers = vec![];
        let comps = xs[1].compute(&w, &r, &xs, EPSILON, &mut containers);

        let mut intersections = vec![];
        let c = w.refracted_color(&comps, 5, &mut intersections, &mut containers);

        assert_eq!(c, Color::new(0.0, 0.0, 0.0))
    }

    // Scenario: The refracted color with a refracted ray
    //   Given w ← default_world()
    //     And A ← the first object in w
    //     And A has:
    //       | material.ambient | 1.0            |
    //       | material.pattern | test_pattern() |
    //     And B ← the second object in w
    //     And B has:
    //       | material.transparency     | 1.0 |
    //       | material.refractive_index | 1.5 |
    //     And r ← ray(point(0, 0, 0.1), vector(0, 1, 0))
    //     And xs ← intersections(-0.9899:A, -0.4899:B, 0.4899:B, 0.9899:A)
    //   When comps ← prepare_computations(xs[2], r, xs)
    //     And c ← refracted_color(w, comps, 5)
    //   Then c = color(0, 0.99888, 0.04725)
    use crate::materials::{Pattern, PatternKind};
    #[test]
    fn the_refracted_color_with_a_refracted_ray() {
        let mut w = World::new();

        let aid = w.push_sphere(
            None,
            Some(Material {
                color: Color::new(0.8, 1.0, 0.6),
                diffuse: 0.7,
                specular: 0.2,
                ambient: 1.0,
                pattern: Some(Pattern::new(
                    Color::new(0.0, 0.0, 0.0),
                    Color::new(0.0, 0.0, 0.0),
                    PatternKind::Point,
                    M4x4::IDENTITY,
                )),
                ..Default::default()
            }),
        );

        let bid = w.push_sphere(
            Some(scaling(0.5, 0.5, 0.5)),
            Some(Material {
                ambient: 1.0,
                transparency: 1.0,
                refractive_index: 1.5,
                ..Default::default()
            }),
        );

        let r = Ray::new(Point::new(0.0, 0.0, 0.1), Vector::new(0.0, 1.0, 0.0));

        let xs = [
            Intersection::new(-0.9899, aid),
            Intersection::new(-0.4899, bid),
            Intersection::new(0.4899, bid),
            Intersection::new(0.9899, aid),
        ];
        let mut containers = vec![];
        let comps = xs[2].compute(&w, &r, &xs, EPSILON, &mut containers);

        let mut intersections = vec![];
        let c = w.refracted_color(&comps, 5, &mut intersections, &mut containers);

        assert_eq!(c, Color::new(0.0, 0.99888, 0.04725))
    }

    // Scenario: shade_hit() with a transparent material
    //   Given w ← default_world()
    //     And floor ← plane() with:
    //       | transform                 | translation(0, -1, 0) |
    //       | material.transparency     | 0.5                   |
    //       | material.refractive_index | 1.5                   |
    //     And floor is added to w
    //     And ball ← sphere() with:
    //       | material.color     | (1, 0, 0)                  |
    //       | material.ambient   | 0.5                        |
    //       | transform          | translation(0, -3.5, -0.5) |
    //     And ball is added to w
    //     And r ← ray(point(0, 0, -3), vector(0, -√2/2, √2/2))
    //     And xs ← intersections(√2:floor)
    //   When comps ← prepare_computations(xs[0], r, xs)
    //     And color ← shade_hit(w, comps, 5)
    //   Then color = color(0.93642, 0.68642, 0.68642)
    #[test]
    fn shade_hit_with_a_transparent_material() {
        let mut w = World::new();

        w.push_sphere(
            None,
            Some(Material {
                color: Color::new(0.8, 1.0, 0.6),
                ambient: 0.1,
                diffuse: 0.7,
                specular: 0.2,
                shininess: 200.0,
                reflective: 0.0,
                transparency: 0.0,
                refractive_index: 1.0,
                ..Default::default()
            }),
        );

        w.push_sphere(
            Some(scaling(0.5, 0.5, 0.5)),
            Some(Material {
                ..Default::default()
            }),
        );

        let floor_id = w.push_plane(
            Some(translation(0.0, -1.0, 0.0)),
            Some(Material {
                transparency: 0.5,
                refractive_index: 1.5,
                ..Default::default()
            }),
        );

        let _ball = w.push_sphere(
            Some(translation(0.0, -3.5, -0.5)),
            Some(Material {
                ambient: 0.5,
                color: Color::new(1.0, 0.0, 0.0),
                ..Default::default()
            }),
        );

        let r = Ray::new(
            Point::new(0.0, 0.0, -3.0),
            Vector::new(0.0, -f32::sqrt(2.0) / 2.0, f32::sqrt(2.0) / 2.0),
        );

        let xs = [Intersection::new(f32::sqrt(2.0), floor_id)];
        let mut containers = vec![];
        let comps = xs[0].compute(&w, &r, &xs, EPSILON, &mut containers);

        let mut intersections = vec![];
        let c = w.shade_hit(&comps, 5, &mut intersections, &mut containers);

        assert_eq!(c, Color::new(0.93642, 0.68642, 0.68642))
    }

    // Scenario: shade_hit() with a reflective, transparent material
    //   Given w ← default_world()
    //     And r ← ray(point(0, 0, -3), vector(0, -√2/2, √2/2))
    //     And floor ← plane() with:
    //       | transform                 | translation(0, -1, 0) |
    //       | material.reflective       | 0.5                   |
    //       | material.transparency     | 0.5                   |
    //       | material.refractive_index | 1.5                   |
    //     And floor is added to w
    //     And ball ← sphere() with:
    //       | material.color     | (1, 0, 0)                  |
    //       | material.ambient   | 0.5                        |
    //       | transform          | translation(0, -3.5, -0.5) |
    //     And ball is added to w
    //     And xs ← intersections(√2:floor)
    //   When comps ← prepare_computations(xs[0], r, xs)
    //     And color ← shade_hit(w, comps, 5)
    //   Then color = color(0.93391, 0.69643, 0.69243)
    #[test]
    fn shade_hit_with_a_reflective_transparent_material() {
        let mut w = World::new();

        w.push_sphere(
            None,
            Some(Material {
                color: Color::new(0.8, 1.0, 0.6),
                ambient: 0.1,
                diffuse: 0.7,
                specular: 0.2,
                shininess: 200.0,
                reflective: 0.0,
                transparency: 0.0,
                refractive_index: 1.0,
                ..Default::default()
            }),
        );

        w.push_sphere(
            Some(scaling(0.5, 0.5, 0.5)),
            Some(Material {
                ..Default::default()
            }),
        );

        let floor_id = w.push_plane(
            Some(translation(0.0, -1.0, 0.0)),
            Some(Material {
                reflective: 0.5,
                transparency: 0.5,
                refractive_index: 1.5,
                ..Default::default()
            }),
        );

        let _ball = w.push_sphere(
            Some(translation(0.0, -3.5, -0.5)),
            Some(Material {
                ambient: 0.5,
                color: Color::new(1.0, 0.0, 0.0),
                ..Default::default()
            }),
        );

        let r = Ray::new(
            Point::new(0.0, 0.0, -3.0),
            Vector::new(0.0, -f32::sqrt(2.0) / 2.0, f32::sqrt(2.0) / 2.0),
        );

        let xs = [Intersection::new(f32::sqrt(2.0), floor_id)];
        let mut containers = vec![];
        let comps = xs[0].compute(&w, &r, &xs, EPSILON, &mut containers);

        let mut intersections = vec![];
        let c = w.shade_hit(&comps, 5, &mut intersections, &mut containers);

        assert_eq!(c, Color::new(0.93391, 0.69643, 0.69243))
    }
}
