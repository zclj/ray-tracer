use crate::matrices::M4x4;
use crate::shape::Shape;

pub struct Context {
    objects: Vec<Shape>,
}

impl Context {
    #[must_use]
    pub fn new() -> Self {
        // @TODO - we could have knowledge about the capacity
        Context {
            objects: Vec::new(),
        }
    }

    #[allow(clippy::cast_possible_truncation)]
    pub fn push_sphere(&mut self, transform_option: Option<M4x4>) -> u32 {
        let id = self.objects.len() as u32;

        let transform = match transform_option {
            Some(t) => t,
            None => M4x4::IDENTITY,
        };

        self.objects.push(Shape::Sphere { id, transform });

        id
    }

    #[must_use]
    pub fn get_shape(&self, id: u32) -> &Shape {
        &self.objects[id as usize]
    }
}

impl Default for Context {
    fn default() -> Self {
        Context::new()
    }
}

#[cfg(test)]
mod test {
    use super::*;
    use crate::shape::Shape;

    #[test]
    fn context_contain_shapes() {
        let mut ctx = Context::new();
        ctx.push_sphere(None);

        let s_id = match ctx.get_shape(0) {
            Shape::Sphere { id, .. } => id,
            _ => panic!(),
        };

        assert_eq!(*s_id, 0);
    }
}
