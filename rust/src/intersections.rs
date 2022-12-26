use crate::spheres::Sphere;

struct Intersection {
    t: f32,
    object: Sphere,
}

impl Intersection {
    fn new(t: f32, sphere: Sphere) -> Self {
        Intersection { t, object: sphere }
    }
}

#[cfg(test)]
mod test {
    use super::*;
    use crate::spheres::Sphere;

    // Scenario: An intersection encapsulates t and object
    //   Given s ← sphere()
    //   When i ← intersection(3.5, s)
    //   Then i.t = 3.5
    //     And i.object = s
    fn an_intersection_encapsulates_t_and_object() {
        let s = Sphere::new(1);
        let i = Intersection::new(3.5, s);

        assert_eq!(i.t, 3.5);
        assert_eq!(i.object.id(), 1);
    }
}
