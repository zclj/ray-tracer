use crate::bounds::BoundingBox;
use crate::color::Color;
use crate::intersections::{sort_by_t, ComputedIntersection, Intersection};
use crate::lights::PointLight;
use crate::materials::Material;
use crate::matrices::M4x4;
use crate::rays::Ray;
use crate::shape::{bounds, check_axis, RenderObject, Shape};
use crate::utils::{epsilon_eq, EPSILON};
use crate::vector::Point;
use std::collections::VecDeque;

#[derive(Debug)]
#[allow(dead_code)] // id is usefull for debug
pub struct RenderGroup {
    id: u32,
    pub objects: Vec<RenderObject>,
    pub transform: Option<M4x4>,
    pub material: Option<Material>,
}

impl RenderGroup {
    #[must_use]
    #[allow(clippy::cast_possible_truncation)]
    pub fn new(
        id: u32,
        objects: Vec<SceneObject>,
        transform: Option<M4x4>,
        material: Option<Material>,
    ) -> Self {
        let render_objects = objects
            .into_iter()
            .enumerate()
            .map(|(i, o)| RenderObject::new(i as u32, &o))
            .collect::<Vec<RenderObject>>();

        RenderGroup {
            id,
            objects: render_objects,
            transform,
            material,
        }
    }
}

#[derive(Debug)]
pub enum BoundingVolume {
    BoundingVolumeNode {
        children: Vec<BoundingVolume>,
        bounds: BoundingBox,
    },
    BoundingVolumePrimitive {
        id: u32,
        bounds: BoundingBox,
    },
}

impl BoundingVolume {
    /// # Panics
    ///
    /// Panics if not colled on a Node
    pub fn push_child(&mut self, child: BoundingVolume) {
        match self {
            BoundingVolume::BoundingVolumeNode { children, .. } => children.push(child),
            BoundingVolume::BoundingVolumePrimitive { .. } => {
                panic!("'push_child' can only be performed on BoundingVolumeNode")
            }
        }
    }

    pub fn set_bounds(&mut self, new_bounds: BoundingBox) {
        match self {
            BoundingVolume::BoundingVolumeNode { bounds, .. }
            | BoundingVolume::BoundingVolumePrimitive { bounds, .. } => *bounds = new_bounds,
        }
    }

    //pub fn children(&self) ->

    pub fn flatten(&self, nodes: &mut VecDeque<LinearBVHNode>, current_offset: &mut usize) {
        // increase the current index used to index into nodes
        *current_offset += 1;
        match self {
            BoundingVolume::BoundingVolumeNode { bounds, children } => {
                // Add the node, put its children next to it.
                // Set the nodes childrens indexs

                let mut child_offsets = Vec::with_capacity(children.len());
                let node_idx = nodes.len();

                nodes.push_back(LinearBVHNode::Node {
                    bounds: bounds.clone(),
                    children: vec![],
                });

                for c in children {
                    child_offsets.push(*current_offset);
                    c.flatten(nodes, current_offset);
                }

                for c in &child_offsets {
                    nodes[node_idx].push_child(*c);
                }
            }
            BoundingVolume::BoundingVolumePrimitive { bounds, id } => {
                nodes.push_back(LinearBVHNode::Primitive {
                    bounds: bounds.clone(),
                    offset: *id as usize,
                });
            }
        }
    }
}

#[derive(Debug, PartialEq)]
pub enum LinearBVHNode {
    Node {
        children: Vec<usize>,
        bounds: BoundingBox,
    },
    Primitive {
        offset: usize,
        bounds: BoundingBox,
    },
}

impl LinearBVHNode {
    /// # Panics
    ///
    /// Will panic if not called on a Node
    pub fn push_child(&mut self, child: usize) -> usize {
        match self {
            LinearBVHNode::Node { children, .. } => {
                let idx = children.len();
                children.push(child);
                idx
            }
            LinearBVHNode::Primitive { .. } => panic!("Can only add child to Node"),
        }
    }

    pub fn bounds(&self) -> &BoundingBox {
        match self {
            LinearBVHNode::Node { bounds, .. } | LinearBVHNode::Primitive { bounds, .. } => bounds,
        }
    }
}

////////////////////////////////////////
// Scene creation

// TODO: separate 'build time' from 'render time'
//  Groups should work with IDs when we build. When the tree is ready
//  it can be reified to render objects holding local content

#[derive(Debug, Clone)]
pub enum SceneNode {
    Group {
        children: Vec<u32>,
        transform: Option<M4x4>,
        bounding_box: BoundingBox,
    },
    Object {
        kind: Shape,
        transform: Option<M4x4>,
        material: Option<Material>,
        bounding_box: BoundingBox,
    },
}

#[derive(Debug)]
pub struct SceneTree {
    pub arena: Vec<SceneNode>,
}

impl Default for SceneTree {
    fn default() -> Self {
        Self::new()
    }
}

impl SceneTree {
    #[must_use]
    pub fn new() -> Self {
        SceneTree { arena: Vec::new() }
    }

    #[allow(clippy::cast_possible_truncation)]
    pub fn insert_object(&mut self, object: SceneObject) -> u32 {
        let id = self.arena.len() as u32;

        self.arena.push(SceneNode::Object {
            transform: object.transform,
            material: object.material,
            bounding_box: bounds(&object.kind),
            kind: object.kind,
        });

        id
    }

    #[allow(clippy::cast_possible_truncation)]
    pub fn insert_group(&mut self, group: SceneGroup) -> u32 {
        let id = self.arena.len() as u32;

        self.arena.push(SceneNode::Group {
            children: group.children,
            transform: group.transform,
            bounding_box: BoundingBox::default(),
        });

        id
    }

    // we want to end up with a BVH
    // start with a basic one
    #[allow(clippy::cast_possible_truncation)]
    pub fn apply_transforms_2(
        &mut self,
        render_primitives: &mut Vec<RenderObject>, //World,
        current: u32,
        current_transform: &Option<M4x4>,
        current_bounds: &mut BoundingBox,
        bvh: &mut BoundingVolume,
    ) {
        let current_node = self.arena[current as usize].clone();

        match current_node {
            SceneNode::Object {
                transform,
                kind,
                material,
                bounding_box,
            } => {
                let new_transform = match (transform, current_transform) {
                    (Some(t), Some(ct)) => Some(ct * &t),
                    (None, Some(ct)) => Some(ct.clone()),
                    (Some(t), None) => Some(t),
                    (_, _) => None,
                };

                // Add this primitive to the know list of primitives
                let id = render_primitives.len();

                // if the transform was changed, we need to update the
                // object and also transform the bounding box.
                // If not, just add the shapes bounds to the total

                if let Some(new_transform) = new_transform {
                    let bbox = bounding_box.transform(&new_transform);

                    current_bounds.merge(&bbox);
                    bvh.set_bounds(current_bounds.clone());
                    //current_bounds.transform(&new_transform);
                    //bvh.set_bounds(current_bounds.clone());
                    //bvh.set_bounds(bbox.clone().transform(&new_transform));
                    // current_bounds.merge(&bounding_box);
                    // bvh.set_bounds(bounding_box.clone());

                    bvh.push_child(BoundingVolume::BoundingVolumePrimitive {
                        id: id as u32,
                        bounds: bbox.clone(),
                    });

                    println!("push primitive");
                    render_primitives.push(RenderObject::new(
                        id as u32,
                        &SceneObject {
                            // TODO: TEMP!
                            kind: kind.clone(),
                            transform: Some(new_transform.clone()),
                            material: material.clone(),
                            bounding_box: bbox.clone(),
                        },
                    ));

                    self.arena[current as usize] = SceneNode::Object {
                        kind,
                        material,
                        transform: Some(new_transform),
                        bounding_box: bbox,
                    }
                } else {
                    current_bounds.merge(&bounding_box);
                    //bvh.set_bounds(current_bounds.clone());

                    bvh.push_child(BoundingVolume::BoundingVolumePrimitive {
                        id: id as u32,
                        bounds: bounding_box.clone(),
                    });

                    println!("push primitive");
                    render_primitives.push(RenderObject::new(
                        id as u32,
                        &SceneObject {
                            // TODO: TEMP!
                            kind,
                            transform: new_transform,
                            material,
                            bounding_box: bounding_box.clone(),
                        },
                    ));
                }
            }
            SceneNode::Group {
                transform,
                children,
                mut bounding_box,
            } => {
                let new_transform = match (&transform, current_transform) {
                    (Some(t), Some(ct)) => Some(ct * t),
                    (None, Some(ct)) => Some(ct.clone()),
                    (_, _) => transform,
                };

                let mut new_bvh_branch = BoundingVolume::BoundingVolumeNode {
                    children: vec![],
                    bounds: BoundingBox::default(),
                };

                ////////////////////////////////////////
                // Experiment
                for c in &children {
                    self.apply_transforms_2(
                        render_primitives,
                        *c,
                        &new_transform,
                        &mut bounding_box,
                        &mut new_bvh_branch,
                    );
                }
                ////////////////////////////////////////

                // merge the groups bounds into parent
                // NOTE: this do not seem to have an effect..
                current_bounds.merge(&bounding_box);

                // Add BVH node
                //println!("bounds coming out: {:?}", bounding_box);
                //println!("current bounds out: {:?}", current_bounds);

                bvh.push_child(new_bvh_branch);
                //println!("bvh: {:#?}", bvh);
                //bvh.set_bounds(bounding_box.clone());
                // NOTE: no effect?
                bvh.set_bounds(current_bounds.clone());

                //println!("bvh: {:#?}", bvh);

                self.arena[current as usize] = SceneNode::Group {
                    transform: new_transform,
                    children,
                    bounding_box,
                };
            }
        }
    }

    pub fn apply_transforms(
        &mut self,
        current: u32,
        current_transform: &Option<M4x4>,
        current_bounds: &mut BoundingBox,
    ) {
        let current_node = self.arena[current as usize].clone();

        match current_node {
            SceneNode::Object {
                transform,
                kind,
                material,
                bounding_box,
            } => {
                let new_transform = match (transform, current_transform) {
                    (Some(t), Some(ct)) => Some(ct * &t),
                    (None, Some(ct)) => Some(ct.clone()),
                    (Some(t), None) => Some(t),
                    (_, _) => None,
                };

                // if the transform was changed, we need to update the
                // object and also transform the bounding box.
                // If not, just add the shapes bounds to the total
                if let Some(new_transform) = new_transform {
                    let bbox = bounding_box.transform(&new_transform);
                    current_bounds.merge(&bbox);

                    self.arena[current as usize] = SceneNode::Object {
                        kind,
                        material,
                        transform: Some(new_transform),
                        bounding_box: bbox,
                    }
                } else {
                    current_bounds.merge(&bounding_box);
                }
            }
            SceneNode::Group {
                transform,
                children,
                mut bounding_box,
            } => {
                let new_transform = match (&transform, current_transform) {
                    (Some(t), Some(ct)) => Some(ct * t),
                    (None, Some(ct)) => Some(ct.clone()),
                    (_, _) => transform,
                };

                // apply transforms and merge bounds for the groups children
                for c in &children {
                    self.apply_transforms(*c, &new_transform, &mut bounding_box);
                }

                self.arena[current as usize] = SceneNode::Group {
                    transform: new_transform,
                    children,
                    bounding_box,
                };
            }
        }
    }

    #[allow(clippy::cast_possible_truncation)]
    #[must_use]
    pub fn build(&self) -> RenderGroup {
        let mut root = RenderGroup::new(0, vec![], None, None);

        // https://www.pbr-book.org/3ed-2018/Primitives_and_Intersection_Acceleration/Bounding_Volume_Hierarchies
        // TODO: Nodes should be pushed into their group, not all nodes
        //  should be put in the root group
        // - NOTE: it might be better to just use the bounding boxes as
        //   hierarchy?
        // A bounding box includes its bounds and either a list of contained
        // children boxes, or, no children but a list of primitives
        for i in 0..self.arena.len() {
            //println!("Build item: {:?}", i);
            match &self.arena[i] {
                SceneNode::Group {
                    transform,
                    bounding_box,
                    ..
                } => {
                    //println!("children: {:?}", children);
                    root.objects.push(RenderObject::new(
                        i as u32,
                        &SceneObject {
                            // TODO: TEMP!
                            kind: Shape::Sphere,
                            transform: transform.clone(),
                            material: None,
                            bounding_box: bounding_box.clone(),
                        },
                    ));
                }
                SceneNode::Object {
                    kind,
                    transform,
                    material,
                    bounding_box,
                } => root.objects.push(RenderObject::new(
                    i as u32,
                    &SceneObject {
                        kind: kind.clone(),
                        transform: transform.clone(),
                        material: material.clone(),
                        bounding_box: bounding_box.clone(),
                    },
                )),
            }
        }

        root
    }
}

#[derive(Debug)]
pub struct SceneGroup {
    children: Vec<u32>,
    pub transform: Option<M4x4>,
    pub material: Option<Material>,
}

impl SceneGroup {
    #[must_use]
    pub fn new(children: Vec<u32>, transform: Option<M4x4>, material: Option<Material>) -> Self {
        SceneGroup {
            children,
            transform,
            material,
        }
    }
}

#[derive(Debug, Clone)]
pub struct SceneObject {
    pub kind: Shape,
    pub transform: Option<M4x4>,
    pub material: Option<Material>,
    pub bounding_box: BoundingBox,
}

impl SceneObject {
    #[must_use]
    pub fn new(kind: Shape, transform: Option<M4x4>, material: Option<Material>) -> Self {
        SceneObject {
            transform,
            material,
            bounding_box: bounds(&kind),
            kind,
        }
    }
}

////////////////////////////////////////

#[derive(Debug)]
pub struct World {
    pub light: PointLight,
    pub shadow_bias: f32,
    pub groups: Vec<RenderGroup>,
    pub render_primitives: Vec<RenderObject>,
    pub scene: SceneTree,
    pub root_group_id: u32,
    pub bvh: Vec<LinearBVHNode>,
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
            groups: vec![RenderGroup::new(0, vec![], None, None)],
            render_primitives: vec![],
            light: PointLight {
                position: Point::new(-10.0, 10.0, -10.0),
                intensity: Color::new(1.0, 1.0, 1.0),
            },
            shadow_bias: EPSILON,
            scene: SceneTree::new(),
            root_group_id: 0,
            bvh: vec![],
        }
    }

    pub fn build(&mut self) {
        // TODO: clean up API
        let mut bvh = BoundingVolume::BoundingVolumeNode {
            children: vec![],
            bounds: BoundingBox::default(),
        };

        self.scene.apply_transforms_2(
            &mut self.render_primitives,
            self.root_group_id,
            &None,
            &mut BoundingBox::default(),
            &mut bvh,
        );

        // TODO: keep track of total number of nodes
        //let mut nodes = Vec::with_capacity(64);

        //println!("BVH pre flatten: {:#?}", bvh);
        let mut node_deque = VecDeque::new();
        bvh.flatten(&mut node_deque, &mut 0);

        //println!("BVH POST flatten: {:#?}", node_deque);

        self.bvh = node_deque.into();
        //println!("More: {:#?}", self.bvh);
    }

    #[allow(clippy::cast_possible_truncation)]
    pub fn push_group(&mut self, objects: Vec<SceneObject>, transform: Option<M4x4>) -> u32 {
        let gid = self.groups.len() as u32;

        self.groups
            .push(RenderGroup::new(gid, objects, transform, None));

        // let mut pending = match transform {
        //     Some(t) => self.update_group_transforms(gid, &t),
        //     _ => Vec::new(),
        // };

        // for p in pending.clone() {
        //     let current = pending.pop();
        //     println!("current: {:?}", p)
        // }

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
    // TODO: this looks very much like the new for shape..
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

        let bounding_box = (bounds(&kind)).transform(&transform);

        // TODO: add const for default group id
        let id = self.groups[0].objects.len() as u32;

        self.groups[0].objects.push(RenderObject {
            id,
            kind,
            transform,
            material,
            transform_inverse,
            transform_inverse_transpose,
            bounding_box,
        });

        id
    }

    #[must_use]
    pub fn get_object(&self, id: u32) -> &RenderObject {
        //&self.groups[0].objects[id as usize]
        println!("GET OBJECT: {:?}", id);
        println!("PRIMS: {:#?}", &self.render_primitives.len());
        &self.render_primitives[id as usize]
    }

    #[allow(clippy::similar_names)]
    pub fn intersect_bounding_box(
        &self,
        bbox: &BoundingBox,
        ray: &Ray,
        _intersections: &mut [Intersection],
        _index: usize,
    ) -> bool {
        let (xtmin, xtmax) = check_axis(ray.origin.x, ray.direction.x, bbox.min.x, bbox.max.x);
        let (ytmin, ytmax) = check_axis(ray.origin.y, ray.direction.y, bbox.min.y, bbox.max.y);
        let (ztmin, ztmax) = check_axis(ray.origin.z, ray.direction.z, bbox.min.z, bbox.max.z);

        let tmin = f32::max(f32::max(xtmin, ytmin), ztmin);
        let tmax = f32::min(f32::min(xtmax, ytmax), ztmax);

        // NOTE: use to visualize bounding boxes
        // if index == 4 {
        //     if tmax >= tmin {
        //         intersections.push(Intersection::new(tmin, 0));
        //         intersections.push(Intersection::new(tmax, 0));
        //     }
        // }

        tmax >= tmin
    }

    #[allow(clippy::similar_names)]
    #[allow(clippy::too_many_lines)]
    pub fn intersect_bvh(
        &self,
        world_ray: &Ray,
        intersections: &mut Vec<Intersection>,
        //bounding_nodes: &[LinearBVHNode],
    ) -> Option<usize> {
        // TODO: This might be reduntant, check with intersect_primitives
        intersections.clear();

        // start with the root of the BVH tree
        let mut current_node_index = 0;

        // TODO: consider Vec vs array
        //let mut nodesToVisit: [usize; 64] = [0; 64];
        let mut nodes_to_visit: Vec<usize> = Vec::with_capacity(64);

        //println!("Self bvh len: {:?}", &self.bvh.len());
        //nodes_to_visit.extend([0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19]);
        //nodes_to_visit.extend([0,1,2,3]);
        loop {
            let node = &self.bvh[current_node_index];

            match node {
                LinearBVHNode::Node { children, bounds } => {
                    if self.intersect_bounding_box(
                        bounds,
                        world_ray,
                        intersections,
                        current_node_index,
                    ) {
                        // we hit this node, check children
                        nodes_to_visit.extend(children);
                    }
                }
                LinearBVHNode::Primitive { bounds, offset } => {
                    if self.intersect_bounding_box(
                        bounds,
                        world_ray,
                        intersections,
                        current_node_index,
                    ) {
                        // we hit this primitive, check the primitive
                        let hit = self.intersect_primitive(
                            world_ray,
                            intersections,
                            &self.render_primitives[*offset],
                        );
                    }
                }
            }

            if nodes_to_visit.is_empty() {
                break;
            }
            // we just checked for empty
            current_node_index = nodes_to_visit.remove(0);
        }

        sort_by_t(intersections);

        intersections.iter().position(|x| x.t >= 0.0)
    }

    #[allow(clippy::similar_names)]
    #[allow(clippy::too_many_lines)]
    pub fn intersect_primitive(
        &self,
        world_ray: &Ray,
        intersections: &mut Vec<Intersection>,
        s: &RenderObject,
    ) -> Option<usize> {
        //intersections.clear();

        //println!("intersecting object: {s:#?}");
        let ray = world_ray.transform(&s.transform_inverse);

        // self.intersect_bounding_box(&s.bounding_box, &world_ray, intersections, current_node_index);
        match s.kind {
            Shape::Sphere => {
                let sphere_to_ray = &ray.origin - &Point::new(0.0, 0.0, 0.0);

                let a = ray.direction.dot(&ray.direction);
                let b = 2.0 * ray.direction.dot(&sphere_to_ray);
                let c = sphere_to_ray.dot(&sphere_to_ray) - 1.0;

                let discriminant = b.powf(2.0) - (4.0 * a * c);

                // invert?
                if discriminant < 0.0 {
                    return None;
                }

                let t1 = (-b - f32::sqrt(discriminant)) / (2.0 * a);
                let t2 = (-b + f32::sqrt(discriminant)) / (2.0 * a);

                let id = s.id;
                intersections.push(Intersection { t: t1, object: id });
                intersections.push(Intersection { t: t2, object: id });
            }
            Shape::Plane => {
                if ray.direction.y.abs() >= EPSILON {
                    intersections.push(Intersection::new(-ray.origin.y / ray.direction.y, s.id));
                }
            }
            Shape::Cube => {
                let (xtmin, xtmax) = check_axis(ray.origin.x, ray.direction.x, -1.0, 1.0);
                let (ytmin, ytmax) = check_axis(ray.origin.y, ray.direction.y, -1.0, 1.0);
                let (ztmin, ztmax) = check_axis(ray.origin.z, ray.direction.z, -1.0, 1.0);

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
        //intersections.clear();

        ////
        // Groups
        // - We want to check boundaries of groups
        // - If we hit the group, check all objects in that group

        // for g in &self.groups {
        //     g.objects.iter().for_each(|s| {
        //         //println!("intersecting object: {s:#?}");
        //         let ray = world_ray.transform(&s.transform_inverse);

        //         match s.kind {
        //             Shape::Sphere => {
        //                 let sphere_to_ray = &ray.origin - &Point::new(0.0, 0.0, 0.0);

        //                 let a = ray.direction.dot(&ray.direction);
        //                 let b = 2.0 * ray.direction.dot(&sphere_to_ray);
        //                 let c = sphere_to_ray.dot(&sphere_to_ray) - 1.0;

        //                 let discriminant = b.powf(2.0) - (4.0 * a * c);

        //                 // invert?
        //                 if discriminant < 0.0 {
        //                     return;
        //                 }

        //                 let t1 = (-b - f32::sqrt(discriminant)) / (2.0 * a);
        //                 let t2 = (-b + f32::sqrt(discriminant)) / (2.0 * a);

        //                 let id = s.id;
        //                 intersections.push(Intersection { t: t1, object: id });
        //                 intersections.push(Intersection { t: t2, object: id });
        //             }
        //             Shape::Plane => {
        //                 if ray.direction.y.abs() >= EPSILON {
        //                     intersections
        //                         .push(Intersection::new(-ray.origin.y / ray.direction.y, s.id));
        //                 }
        //             }
        //             Shape::Cube => {
        //                 let (xtmin, xtmax) = check_axis(ray.origin.x, ray.direction.x, -1.0, 1.0);
        //                 let (ytmin, ytmax) = check_axis(ray.origin.y, ray.direction.y, -1.0, 1.0);
        //                 let (ztmin, ztmax) = check_axis(ray.origin.z, ray.direction.z, -1.0, 1.0);

        //                 let tmin = f32::max(f32::max(xtmin, ytmin), ztmin);
        //                 let tmax = f32::min(f32::min(xtmax, ytmax), ztmax);

        //                 if tmax >= tmin {
        //                     intersections.push(Intersection::new(tmin, s.id));
        //                     intersections.push(Intersection::new(tmax, s.id));
        //                 }
        //             }
        //             Shape::Cylinder { .. } => {
        //                 let xd = ray.direction.x;
        //                 let zd = ray.direction.z;
        //                 let a = xd * xd + zd * zd;

        //                 // ray is NOT parallel with the y axis
        //                 if !epsilon_eq(a, 0.0) {
        //                     let xo = ray.origin.x;
        //                     let zo = ray.origin.z;
        //                     let b = (2.0 * xo * xd) + (2.0 * zo * zd);
        //                     let c = xo * xo + zo * zo - 1.0;

        //                     let discriminant = b.powf(2.0) - (4.0 * a * c);

        //                     // ray does intersect the cylinder
        //                     if discriminant >= 0.0 {
        //                         let a2 = 2.0 * a;
        //                         let t0 = (-b - f32::sqrt(discriminant)) / a2;
        //                         let t1 = (-b + f32::sqrt(discriminant)) / a2;

        //                         let (t0, t1) = if t0 > t1 { (t1, t0) } else { (t0, t1) };

        //                         let y0 = (ray.origin.y) + t0 * (ray.direction.y);

        //                         if (s.minimum()) < y0 && y0 < (s.maximum()) {
        //                             intersections.push(Intersection {
        //                                 t: t0,
        //                                 object: s.id,
        //                             });
        //                         }
        //                         let y1 = (ray.origin.y) + t1 * (ray.direction.y);

        //                         if (s.minimum()) < y1 && y1 < (s.maximum()) {
        //                             intersections.push(Intersection {
        //                                 t: t1,
        //                                 object: s.id,
        //                             });
        //                         }
        //                     }
        //                 }
        //                 intersect_caps(s, &ray, intersections);
        //             }
        //             Shape::Cone { .. } => {
        //                 let xd = ray.direction.x;
        //                 let yd = ray.direction.y;
        //                 let zd = ray.direction.z;

        //                 let xo = ray.origin.x;
        //                 let yo = ray.origin.y;
        //                 let zo = ray.origin.z;

        //                 let a = xd * xd - yd * yd + zd * zd;
        //                 let b = (2.0 * xo * xd) - (2.0 * yo * yd) + (2.0 * zo * zd);
        //                 let c = xo * xo - yo * yo + zo * zo;

        //                 if epsilon_eq(a, 0.0) && !epsilon_eq(b, 0.0) {
        //                     intersections.push(Intersection {
        //                         t: -c / (2.0 * b),
        //                         object: s.id,
        //                     });
        //                 } else {
        //                     let discriminant = b.powf(2.0) - (4.0 * a * c);

        //                     // ray does intersect the cylinder
        //                     if discriminant >= (-EPSILON) {
        //                         let sqrt_discriminant = f32::sqrt(f32::max(0.0, discriminant));
        //                         let a2 = 2.0 * a;
        //                         let t0 = (-b - sqrt_discriminant) / a2;
        //                         let t1 = (-b + sqrt_discriminant) / a2;

        //                         let (t0, t1) = if t0 > t1 { (t1, t0) } else { (t0, t1) };

        //                         let y0 = (ray.origin.y) + t0 * (ray.direction.y);
        //                         if (s.minimum()) < y0 && y0 < (s.maximum()) {
        //                             intersections.push(Intersection {
        //                                 t: t0,
        //                                 object: s.id,
        //                             });
        //                         }
        //                         let y1 = (ray.origin.y) + t1 * (ray.direction.y);
        //                         if (s.minimum()) < y1 && y1 < (s.maximum()) {
        //                             intersections.push(Intersection {
        //                                 t: t1,
        //                                 object: s.id,
        //                             });
        //                         }
        //                     }
        //                 }

        //                 intersect_caps(s, &ray, intersections);
        //             }
        //         }
        //     });
        // }

        // Sort the intersections and return the index of the 'hit'
        //sort_by_t(intersections);

        //intersections.iter().position(|x| x.t >= 0.0)
        None
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
        let the_hit = self.intersect_bvh(ray, intersections);

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

        let h = self.intersect_bvh(&r, intersections);

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
    use crate::transformations::{rotation_y, rotation_z, scaling, transform, translation};
    use crate::vector::{Point, Vector};
    use crate::world::LinearBVHNode::*;
    use std::f32::consts::PI;

    fn test_default() -> World {
        let mut world = World::new();
        let s1_id = world.scene.insert_object(SceneObject::new(
            Shape::Sphere,
            None,
            Some(Material {
                color: Color::new(0.8, 1.0, 0.6),
                diffuse: 0.7,
                specular: 0.2,
                ..Default::default()
            }),
        ));

        let s2_id = world.scene.insert_object(SceneObject::new(
            Shape::Sphere,
            Some(scaling(0.5, 0.5, 0.5)),
            Some(Material::default()),
        ));

        let g1_id = world
            .scene
            .insert_group(SceneGroup::new(vec![s1_id, s2_id], None, None));
        world.root_group_id = g1_id;
        world.build();

        world
    }

    #[test]
    fn world_contain_shapes() {
        let mut world = World::new();
        world
            .scene
            .insert_object(SceneObject::new(Shape::Sphere, None, None));
        world.build();

        let s_id = world.get_object(0).id;

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
        println!("World: {:#?}", w);
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
                bounding_box: BoundingBox::new(
                    Point::new(-1.0, -1.0, -1.0),
                    Point::new(1.0, 1.0, 1.0)
                ),
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
                bounding_box: BoundingBox::new(
                    Point::new(-0.5, -0.5, -0.5),
                    Point::new(0.5, 0.5, 0.5)
                ),
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
        w.intersect_bvh(&r, &mut xs);

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
        let mut world = World::default();

        let s1_id = world.scene.insert_object(SceneObject::new(
            Shape::Sphere,
            None,
            Some(Material {
                color: Color::new(0.8, 1.0, 0.6),
                diffuse: 0.7,
                specular: 0.2,
                ambient: 1.0,
                ..Default::default()
            }),
        ));

        let inner_id = world.scene.insert_object(SceneObject::new(
            Shape::Sphere,
            Some(scaling(0.5, 0.5, 0.5)),
            Some(Material {
                ambient: 1.0,
                ..Material::default()
            }),
        ));

        let g1_id = world
            .scene
            .insert_group(SceneGroup::new(vec![s1_id, inner_id], None, None));
        world.root_group_id = g1_id;
        world.build();

        let r = Ray::new(Point::new(0.0, 0.0, 0.75), Vector::new(0.0, 0.0, -1.0));
        let mut containers = vec![];
        let mut intersections = vec![];
        let c = world.color_at(&r, 1, &mut intersections, &mut containers);

        assert_eq!(c, world.get_object(inner_id).material.color)
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
        let mut world = World::new();
        world.light = PointLight::new(Point::new(0.0, 0.0, -10.0), Color::new(1.0, 1.0, 1.0));

        let s1_id = world
            .scene
            .insert_object(SceneObject::new(Shape::Sphere, None, None));

        let s2_id = world.scene.insert_object(SceneObject::new(
            Shape::Sphere,
            Some(translation(0.0, 0.0, 10.0)),
            None,
        ));

        let g1_id = world
            .scene
            .insert_group(SceneGroup::new(vec![s1_id, s2_id], None, None));
        world.root_group_id = g1_id;
        world.build();

        let r = Ray::new(Point::new(0.0, 0.0, 5.0), Vector::new(0.0, 0.0, 1.0));
        let i = Intersection::new(4.0, s2_id);

        let mut containers = vec![];
        let comps = i.compute(&world, &r, &[i.clone()], EPSILON, &mut containers);
        let mut intersections = vec![];
        let c = world.shade_hit(&comps, 0, &mut intersections, &mut containers);

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
        let mut world = World::new();

        let s1_id = world.scene.insert_object(SceneObject::new(
            Shape::Sphere,
            None,
            Some(Material {
                color: Color::new(0.8, 1.0, 0.6),
                diffuse: 0.7,
                specular: 0.2,
                ..Default::default()
            }),
        ));

        let s2_id = world.scene.insert_object(SceneObject::new(
            Shape::Sphere,
            Some(scaling(0.5, 0.5, 0.5)),
            Some(Material {
                ambient: 1.0,
                ..Default::default()
            }),
        ));

        let g1_id = world
            .scene
            .insert_group(SceneGroup::new(vec![s1_id, s2_id], None, None));
        world.root_group_id = g1_id;
        world.build();

        let r = Ray::new(Point::new(0.0, 0.0, 0.0), Vector::new(0.0, 0.0, 1.0));

        let i = Intersection::new(1.0, s2_id);
        let mut containers = vec![];
        let comps = i.compute(&world, &r, &[i.clone()], EPSILON, &mut containers);
        let mut intersections = vec![];
        let color = world.reflected_color(&comps, 0, &mut intersections, &mut containers);

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
        let mut world = World::new();

        let s1_id = world.scene.insert_object(SceneObject::new(
            Shape::Sphere,
            None,
            Some(Material {
                color: Color::new(0.8, 1.0, 0.6),
                diffuse: 0.7,
                specular: 0.2,
                ..Default::default()
            }),
        ));

        let s2_id = world.scene.insert_object(SceneObject::new(
            Shape::Sphere,
            Some(scaling(0.5, 0.5, 0.5)),
            Some(Material {
                ..Default::default()
            }),
        ));

        let p_id = world.scene.insert_object(SceneObject::new(
            Shape::Plane,
            Some(translation(0.0, -1.0, 0.0)),
            Some(Material {
                reflective: 0.5,
                ..Default::default()
            }),
        ));

        let g1_id = world
            .scene
            .insert_group(SceneGroup::new(vec![s1_id, s2_id, p_id], None, None));
        world.root_group_id = g1_id;
        world.build();

        let r = Ray::new(
            Point::new(0.0, 0.0, -3.0),
            Vector::new(0.0, -f32::sqrt(2.0) / 2.0, f32::sqrt(2.0) / 2.0),
        );

        let i = Intersection::new(f32::sqrt(2.0), p_id);
        let mut containers = vec![];
        let comps = i.compute(&world, &r, &[i.clone()], EPSILON, &mut containers);
        let mut intersections = vec![];
        let color = world.reflected_color(&comps, 1, &mut intersections, &mut containers);

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
        let mut world = World::new();

        let s1_id = world.scene.insert_object(SceneObject::new(
            Shape::Sphere,
            None,
            Some(Material {
                color: Color::new(0.8, 1.0, 0.6),
                diffuse: 0.7,
                specular: 0.2,
                ..Default::default()
            }),
        ));

        let s2_id = world.scene.insert_object(SceneObject::new(
            Shape::Sphere,
            Some(scaling(0.5, 0.5, 0.5)),
            Some(Material {
                ..Default::default()
            }),
        ));

        let p_id = world.scene.insert_object(SceneObject::new(
            Shape::Plane,
            Some(translation(0.0, -1.0, 0.0)),
            Some(Material {
                reflective: 0.5,
                ..Default::default()
            }),
        ));

        let g1_id = world
            .scene
            .insert_group(SceneGroup::new(vec![s1_id, s2_id, p_id], None, None));
        world.root_group_id = g1_id;
        world.build();

        let r = Ray::new(
            Point::new(0.0, 0.0, -3.0),
            Vector::new(0.0, -f32::sqrt(2.0) / 2.0, f32::sqrt(2.0) / 2.0),
        );

        let i = Intersection::new(f32::sqrt(2.0), p_id);
        let mut containers = vec![];
        let comps = i.compute(&world, &r, &[i.clone()], EPSILON, &mut containers);

        let mut intersections = vec![];
        let color = world.shade_hit(&comps, 1, &mut intersections, &mut containers);

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
        let mut world = World::new();
        world.light = PointLight::new(Point::new(0.0, 0.0, 0.0), Color::new(1.0, 1.0, 1.0));

        let lower_id = world.scene.insert_object(SceneObject::new(
            Shape::Plane,
            Some(translation(0.0, -1.0, 0.0)),
            Some(Material {
                reflective: 1.0,
                ..Default::default()
            }),
        ));

        let upper_id = world.scene.insert_object(SceneObject::new(
            Shape::Plane,
            Some(translation(0.0, 1.0, 0.0)),
            Some(Material {
                reflective: 1.0,
                ..Default::default()
            }),
        ));

        let g1_id = world
            .scene
            .insert_group(SceneGroup::new(vec![lower_id, upper_id], None, None));
        world.root_group_id = g1_id;
        world.build();

        let r = Ray::new(Point::new(0.0, 0.0, -3.0), Vector::new(0.0, 1.0, 0.0));
        let mut containers = vec![];
        let mut intersections = vec![];

        assert_eq!(
            world.color_at(&r, 1, &mut intersections, &mut containers),
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
        let mut world = World::new();

        let s1_id = world.scene.insert_object(SceneObject::new(
            Shape::Sphere,
            None,
            Some(Material {
                color: Color::new(0.8, 1.0, 0.6),
                diffuse: 0.7,
                specular: 0.2,
                ..Default::default()
            }),
        ));

        let s2_id = world.scene.insert_object(SceneObject::new(
            Shape::Sphere,
            Some(scaling(0.5, 0.5, 0.5)),
            Some(Material {
                ..Default::default()
            }),
        ));

        let p1_id = world.scene.insert_object(SceneObject::new(
            Shape::Plane,
            Some(translation(0.0, -1.0, 0.0)),
            Some(Material {
                reflective: 0.5,
                ..Default::default()
            }),
        ));

        let g1_id =
            world
                .scene
                .insert_group(SceneGroup::new(vec![s1_id, s2_id, p1_id], None, None));
        world.root_group_id = g1_id;
        world.build();

        let r = Ray::new(
            Point::new(0.0, 0.0, -3.0),
            Vector::new(0.0, -f32::sqrt(2.0) / 2.0, f32::sqrt(2.0) / 2.0),
        );

        let i = Intersection::new(f32::sqrt(2.0), p1_id);
        let mut containers = vec![];
        let comps = i.compute(&world, &r, &[i.clone()], EPSILON, &mut containers);
        let mut intersections = vec![];
        let color = world.reflected_color(&comps, 0, &mut intersections, &mut containers);

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
        let mut world = World::new();

        let s1_id = world.scene.insert_object(SceneObject::new(
            Shape::Sphere,
            None,
            Some(Material {
                color: Color::new(0.8, 1.0, 0.6),
                diffuse: 0.7,
                specular: 0.2,
                ..Default::default()
            }),
        ));

        let s2_id = world.scene.insert_object(SceneObject::new(
            Shape::Sphere,
            Some(scaling(0.5, 0.5, 0.5)),
            Some(Material {
                ambient: 1.0,
                ..Default::default()
            }),
        ));

        let g1_id = world
            .scene
            .insert_group(SceneGroup::new(vec![s1_id, s2_id], None, None));
        world.root_group_id = g1_id;
        world.build();

        let r = Ray::new(Point::new(0.0, 0.0, -5.0), Vector::new(0.0, 0.0, 1.0));

        let xs = [Intersection::new(4.0, s2_id), Intersection::new(6.0, s2_id)];
        let mut containers = vec![];
        let comps = xs[0].compute(&world, &r, &xs, EPSILON, &mut containers);

        let mut intersections = vec![];
        let c = world.refracted_color(&comps, 5, &mut intersections, &mut containers);

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
        let mut world = World::new();

        let sid = world.scene.insert_object(SceneObject::new(
            Shape::Sphere,
            None,
            Some(Material {
                color: Color::new(0.8, 1.0, 0.6),
                diffuse: 0.7,
                specular: 0.2,
                transparency: 1.0,
                refractive_index: 1.5,
                ..Default::default()
            }),
        ));

        let s2_id = world.scene.insert_object(SceneObject::new(
            Shape::Sphere,
            Some(scaling(0.5, 0.5, 0.5)),
            Some(Material {
                ambient: 1.0,
                ..Default::default()
            }),
        ));

        let g1_id = world
            .scene
            .insert_group(SceneGroup::new(vec![sid, s2_id], None, None));
        world.root_group_id = g1_id;
        world.build();

        let r = Ray::new(Point::new(0.0, 0.0, -5.0), Vector::new(0.0, 0.0, 1.0));

        let xs = [Intersection::new(4.0, sid), Intersection::new(6.0, sid)];
        let mut containers = vec![];
        let comps = xs[0].compute(&world, &r, &xs, EPSILON, &mut containers);

        let mut intersections = vec![];
        let c = world.refracted_color(&comps, 0, &mut intersections, &mut containers);

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
        let mut world = World::new();

        let sid = world.scene.insert_object(SceneObject::new(
            Shape::Sphere,
            None,
            Some(Material {
                color: Color::new(0.8, 1.0, 0.6),
                diffuse: 0.7,
                specular: 0.2,
                transparency: 1.0,
                refractive_index: 1.5,
                ..Default::default()
            }),
        ));

        let s2_id = world.scene.insert_object(SceneObject::new(
            Shape::Sphere,
            Some(scaling(0.5, 0.5, 0.5)),
            Some(Material {
                ambient: 1.0,
                ..Default::default()
            }),
        ));

        let g1_id = world
            .scene
            .insert_group(SceneGroup::new(vec![sid, s2_id], None, None));
        world.root_group_id = g1_id;
        world.build();

        let r = Ray::new(
            Point::new(0.0, 0.0, f32::sqrt(2.0) / 2.0),
            Vector::new(0.0, 1.0, 0.0),
        );

        let xs = [
            Intersection::new(-f32::sqrt(2.0) / 2.0, sid),
            Intersection::new(f32::sqrt(2.0) / 2.0, sid),
        ];
        let mut containers = vec![];
        let comps = xs[1].compute(&world, &r, &xs, EPSILON, &mut containers);

        let mut intersections = vec![];
        let c = world.refracted_color(&comps, 5, &mut intersections, &mut containers);

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
        let mut world = World::new();

        let aid = world.scene.insert_object(SceneObject::new(
            Shape::Sphere,
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
        ));

        let bid = world.scene.insert_object(SceneObject::new(
            Shape::Sphere,
            Some(scaling(0.5, 0.5, 0.5)),
            Some(Material {
                ambient: 1.0,
                transparency: 1.0,
                refractive_index: 1.5,
                ..Default::default()
            }),
        ));

        let g1_id = world
            .scene
            .insert_group(SceneGroup::new(vec![aid, bid], None, None));
        world.root_group_id = g1_id;
        world.build();

        let r = Ray::new(Point::new(0.0, 0.0, 0.1), Vector::new(0.0, 1.0, 0.0));

        let xs = [
            Intersection::new(-0.9899, aid),
            Intersection::new(-0.4899, bid),
            Intersection::new(0.4899, bid),
            Intersection::new(0.9899, aid),
        ];
        let mut containers = vec![];
        let comps = xs[2].compute(&world, &r, &xs, EPSILON, &mut containers);

        let mut intersections = vec![];
        let c = world.refracted_color(&comps, 5, &mut intersections, &mut containers);

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
        let mut world = World::new();

        let s1_id = world.scene.insert_object(SceneObject::new(
            Shape::Sphere,
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
        ));

        let s2_id = world.scene.insert_object(SceneObject::new(
            Shape::Sphere,
            Some(scaling(0.5, 0.5, 0.5)),
            Some(Material {
                ..Default::default()
            }),
        ));

        let floor_id = world.scene.insert_object(SceneObject::new(
            Shape::Plane,
            Some(translation(0.0, -1.0, 0.0)),
            Some(Material {
                transparency: 0.5,
                refractive_index: 1.5,
                ..Default::default()
            }),
        ));

        let ball = world.scene.insert_object(SceneObject::new(
            Shape::Sphere,
            Some(translation(0.0, -3.5, -0.5)),
            Some(Material {
                ambient: 0.5,
                color: Color::new(1.0, 0.0, 0.0),
                ..Default::default()
            }),
        ));

        let g1_id = world.scene.insert_group(SceneGroup::new(
            vec![s1_id, s2_id, floor_id, ball],
            None,
            None,
        ));
        world.root_group_id = g1_id;
        world.build();

        let r = Ray::new(
            Point::new(0.0, 0.0, -3.0),
            Vector::new(0.0, -f32::sqrt(2.0) / 2.0, f32::sqrt(2.0) / 2.0),
        );

        let xs = [Intersection::new(f32::sqrt(2.0), floor_id)];
        let mut containers = vec![];
        let comps = xs[0].compute(&world, &r, &xs, EPSILON, &mut containers);

        let mut intersections = vec![];
        let c = world.shade_hit(&comps, 5, &mut intersections, &mut containers);

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
        let mut world = World::new();

        let s1_id = world.scene.insert_object(SceneObject::new(
            Shape::Sphere,
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
        ));

        let s2_id = world.scene.insert_object(SceneObject::new(
            Shape::Sphere,
            Some(scaling(0.5, 0.5, 0.5)),
            Some(Material {
                ..Default::default()
            }),
        ));

        let floor_id = world.scene.insert_object(SceneObject::new(
            Shape::Plane,
            Some(translation(0.0, -1.0, 0.0)),
            Some(Material {
                reflective: 0.5,
                transparency: 0.5,
                refractive_index: 1.5,
                ..Default::default()
            }),
        ));

        let ball = world.scene.insert_object(SceneObject::new(
            Shape::Sphere,
            Some(translation(0.0, -3.5, -0.5)),
            Some(Material {
                ambient: 0.5,
                color: Color::new(1.0, 0.0, 0.0),
                ..Default::default()
            }),
        ));

        let g1_id = world.scene.insert_group(SceneGroup::new(
            vec![s1_id, s2_id, floor_id, ball],
            None,
            None,
        ));
        world.root_group_id = g1_id;
        world.build();

        let r = Ray::new(
            Point::new(0.0, 0.0, -3.0),
            Vector::new(0.0, -f32::sqrt(2.0) / 2.0, f32::sqrt(2.0) / 2.0),
        );

        let xs = [Intersection::new(f32::sqrt(2.0), floor_id)];
        let mut containers = vec![];
        let comps = xs[0].compute(&world, &r, &xs, EPSILON, &mut containers);

        let mut intersections = vec![];
        let c = world.shade_hit(&comps, 5, &mut intersections, &mut containers);

        assert_eq!(c, Color::new(0.93391, 0.69643, 0.69243))
    }

    ////////////////////////////////////////
    // Render group experiment

    fn hex_corner(scene: &mut SceneTree) -> u32 {
        scene.insert_object(SceneObject::new(
            Shape::Sphere,
            Some(transform(&[
                scaling(0.25, 0.25, 0.25),
                translation(0.0, 0.0, -1.0),
            ])),
            Some(Material {
                color: Color::new(0.0, 0.2, 0.0),
                ambient: 0.9,
                diffuse: 0.0,
                specular: 0.1,
                shininess: 10.0,
                reflective: 0.0,
                transparency: 1.0,
                refractive_index: 1.0,
                ..Material::default()
            }),
            // Some(Material {
            //     color: Color::new(0.8, 0.5, 0.3),
            //     shininess: 50.0,
            //     // pattern: Some(Pattern::new(
            //     //     Color::new(0.1, 1.0, 0.5),
            //     //     Color::new(1.0, 0.1, 0.5),
            //     //     PatternKind::Ring,
            //     //     &scaling(0.2, 0.2, 0.2) * &rotation_x(PI / 4.0),
            //     // )),
            //     ..Material::default()
            // }),
        ))
    }

    fn hex_edge(scene: &mut SceneTree) -> u32 {
        scene.insert_object(SceneObject::new(
            Shape::Cylinder {
                minimum: 0.0,
                maximum: 1.0,
                closed: true,
            },
            Some(transform(&[
                scaling(0.25, 1.0, 0.25),
                rotation_z(-PI / 2.0),
                rotation_y(-PI / 6.0),
                translation(0.0, 0.0, -1.0),
            ])),
            Some(Material {
                color: Color::new(0.0, 0.2, 0.0),
                ambient: 0.9,
                diffuse: 0.0,
                specular: 0.1,
                shininess: 10.0,
                reflective: 0.0,
                transparency: 1.0,
                refractive_index: 1.0,
                ..Material::default()
            }),
        ))
    }

    fn hex_side(scene: &mut SceneTree, i: u32) -> u32 {
        let hex_corner_id = hex_corner(scene);

        let hex_edge_id = hex_edge(scene);

        scene.insert_group(SceneGroup::new(
            vec![hex_edge_id, hex_corner_id],
            Some(rotation_y(i as f32 * PI / 3.0)),
            None,
        ))
    }

    #[test]
    fn creation_of_render_template() {
        let mut world = World::new();
        //let mut scene = SceneTree::new();

        let ids = (0..2)
            .map(|i| hex_side(&mut world.scene, i))
            .collect::<Vec<u32>>();

        let hexagon = world.scene.insert_group(SceneGroup::new(
            //vec![g0, g1, g2, g3, g4], //ids, //,
            //None,
            ids,
            Some(transform(&[
                rotation_z(PI / 3.0),
                translation(0.5, 1.0, 0.0),
            ])),
            None,
        ));

        world.root_group_id = hexagon;
        world.build();

        let mut intersections: Vec<Intersection> = vec![];

        println!("Final BVH: {:#?}", world.bvh);
        //println!("World render primitives: {:?}", world.render_primitives);
        // println!(
        //     "World render primitives len: {:?}",
        //     world.render_primitives.len()
        // );

        // world.intersect_bvh(
        //     &Ray::new(Point::new(0.0, 0.0, -5.0), Vector::new(0.0, 1.0, 0.0)),
        //     &mut intersections,
        // );

        //assert_eq!(55, intersections.len())
        assert_eq!(
            world.bvh,
            vec![
                Node {
                    children: vec![1,],
                    bounds: BoundingBox {
                        min: Point {
                            x: -0.32027224,
                            y: -0.17075324,
                            z: -1.25,
                        },
                        max: Point {
                            x: 1.2120191,
                            y: 1.9832532,
                            z: -0.15849361,
                        },
                    },
                },
                Node {
                    children: vec![2, 5,],
                    bounds: BoundingBox {
                        min: Point {
                            x: -0.32027224,
                            y: -0.17075324,
                            z: -1.25,
                        },
                        max: Point {
                            x: 1.2120191,
                            y: 1.9832532,
                            z: -0.15849361,
                        },
                    },
                },
                Node {
                    children: vec![3, 4,],
                    bounds: BoundingBox {
                        min: Point {
                            x: 0.15849364,
                            y: 0.65849364,
                            z: -1.25,
                        },
                        max: Point {
                            x: 1.2120191,
                            y: 1.9832532,
                            z: -0.28349364,
                        },
                    },
                },
                Primitive {
                    offset: 0,
                    bounds: BoundingBox {
                        min: Point {
                            x: 0.22099364,
                            y: 0.7667468,
                            z: -1.2165064,
                        },
                        max: Point {
                            x: 1.2120191,
                            y: 1.9832532,
                            z: -0.28349364,
                        },
                    },
                },
                Primitive {
                    offset: 1,
                    bounds: BoundingBox {
                        min: Point {
                            x: 0.15849364,
                            y: 0.65849364,
                            z: -1.25,
                        },
                        max: Point {
                            x: 0.84150636,
                            y: 1.3415064,
                            z: -0.75,
                        },
                    },
                },
                Node {
                    children: vec![6, 7,],
                    bounds: BoundingBox {
                        min: Point {
                            x: -0.32027224,
                            y: -0.17075324,
                            z: -1.2165064,
                        },
                        max: Point {
                            x: 0.77900636,
                            y: 1.2332531,
                            z: -0.15849361,
                        },
                    },
                },
                Primitive {
                    offset: 2,
                    bounds: BoundingBox {
                        min: Point {
                            x: -0.21201906,
                            y: 0.01674676,
                            z: -1.2165064,
                        },
                        max: Point {
                            x: 0.77900636,
                            y: 1.2332531,
                            z: -0.28349364,
                        },
                    },
                },
                Primitive {
                    offset: 3,
                    bounds: BoundingBox {
                        min: Point {
                            x: -0.32027224,
                            y: -0.17075324,
                            z: -0.84150636,
                        },
                        max: Point {
                            x: 0.45424685,
                            y: 0.6707531,
                            z: -0.15849361,
                        },
                    },
                },
            ]
        )
    }
}
