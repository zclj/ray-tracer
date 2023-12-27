use crate::{
    shape::Shape,
    vector::Point,
    world::{SceneNode, SceneTree, World},
};

#[derive(Debug)]
pub struct ParseResult {
    ignored: Vec<String>,
    vertices: Vec<Point>,
    default_group: Option<SceneNode>,
    world: World,
}

impl ParseResult {
    pub fn new() -> Self {
        ParseResult {
            ignored: vec![],
            vertices: vec![],
            default_group: None,
            world: World::new(),
        }
    }
}

pub fn parse(content: &str) -> ParseResult {
    let mut result = ParseResult::new();
    let mut default_group_ids = vec![];

    for l in content.lines() {
        if l.is_empty() {
            continue;
        }
        let line = l.trim();

        let mut parts = line.split_ascii_whitespace();

        let part_type = parts.next();

        if let Some(obj_type) = part_type {
            match obj_type {
                "v" => {
                    let p1 = parts.next().unwrap().parse().unwrap();
                    let p2 = parts.next().unwrap().parse().unwrap();
                    let p3 = parts.next().unwrap().parse().unwrap();

                    result.vertices.push(Point::new(p1, p2, p3))
                }
                "f" => {
                    let p1_idx: usize = parts.next().unwrap().parse().unwrap();
                    let p2_idx: usize = parts.next().unwrap().parse().unwrap();
                    let p3_idx: usize = parts.next().unwrap().parse().unwrap();

                    let p1 = result.vertices[p1_idx - 1].clone();
                    let p2 = result.vertices[p2_idx - 1].clone();
                    let p3 = result.vertices[p3_idx - 1].clone();

                    let s_id = result.world.scene.insert_object(
                        Shape::new_triangle(p1, p2, p3),
                        None,
                        None,
                    );

                    default_group_ids.push(s_id);
                }
                _ => result.ignored.push(line.to_string()),
            }
        } else {
            result.ignored.push(line.to_string())
        }
    }

    let g_id = result.world.scene.insert_group(default_group_ids, None);
    result.world.root_group_id = g_id;

    result
}

#[cfg(test)]
mod tests {

    use crate::shape::RenderObject;

    use super::*;

    // Scenario: Ignoring unrecognized lines
    //   Given gibberish ← a file containing:
    //     """
    //     There was a young lady named Bright
    //     who traveled much faster than light.
    //     She set out one day
    //     in a relative way,
    //     and came back the previous night.
    //     """
    //   When parser ← parse_obj_file(gibberish)
    //   Then parser should have ignored 5 lines
    #[test]
    fn ignoring_unrecognized_lines() {
        let content = "There was a young lady named Bright
             who traveled much faster than light.
             She set out one day
             in a relative way,
             and came back the previous night.";

        let res = parse(content);

        assert_eq!(5, res.ignored.len())
    }

    // Scenario: Vertex records
    //   Given file ← a file containing:
    //     """
    //     v -1 1 0
    //     v -1.0000 0.5000 0.0000
    //     v 1 0 0
    //     v 1 1 0
    //     """
    //   When parser ← parse_obj_file(file)
    //   Then parser.vertices[1] = point(-1, 1, 0)
    //     And parser.vertices[2] = point(-1, 0.5, 0)
    //     And parser.vertices[3] = point(1, 0, 0)
    //     And parser.vertices[4] = point(1, 1, 0)
    #[test]
    fn vertex_records() {
        let content = "
            v -1 1 0
            v -1.0000 0.5000 0.0000
            v 1 0 0
            v 1 1 0";

        let res = parse(content);

        println!("res {:#?}", res);
        assert_eq!(4, res.vertices.len());
        assert_eq!(res.vertices[0], Point::new(-1.0, 1.0, 0.0));
        assert_eq!(res.vertices[1], Point::new(-1.0, 0.5, 0.0));
        assert_eq!(res.vertices[2], Point::new(1.0, 0.0, 0.0));
        assert_eq!(res.vertices[3], Point::new(1.0, 1.0, 0.0));
        assert_eq!(0, res.ignored.len())
    }

    // Scenario: Parsing triangle faces
    //   Given file ← a file containing:
    //     """
    //     v -1 1 0
    //     v -1 0 0
    //     v 1 0 0
    //     v 1 1 0

    //     f 1 2 3
    //     f 1 3 4
    //     """
    //   When parser ← parse_obj_file(file)
    //     And g ← parser.default_group
    //     And t1 ← first child of g
    //     And t2 ← second child of g
    //   Then t1.p1 = parser.vertices[1]
    //     And t1.p2 = parser.vertices[2]
    //     And t1.p3 = parser.vertices[3]
    //     And t2.p1 = parser.vertices[1]
    //     And t2.p2 = parser.vertices[3]
    //     And t2.p3 = parser.vertices[4]
    #[test]
    fn parsing_triangle_faces() {
        let content = "
            v -1 1 0
            v -1 0 0
            v 1 0 0
            v 1 1 0

            f 1 2 3
            f 1 3 4";

        let mut res = parse(content);
        res.world.build();

        let g_id = res.world.root_group_id;

        println!("World: {:#?}", res.world);

        let group = &res.world.scene.arena[g_id as usize];
        println!("Group {:#?}", res.world.scene.arena[g_id as usize]);

        match &res.world.get_object(0).kind {
            Shape::Triangle { p1, p2, p3, .. } => {
                assert_eq!(p1, &res.vertices[0]);
                assert_eq!(p2, &res.vertices[1]);
                assert_eq!(p3, &res.vertices[2]);
            }
            _ => panic!(),
        };

        match &res.world.get_object(1).kind {
            Shape::Triangle { p1, p2, p3, .. } => {
                assert_eq!(p1, &res.vertices[0]);
                assert_eq!(p2, &res.vertices[2]);
                assert_eq!(p3, &res.vertices[3]);
            }
            _ => panic!(),
        };

        assert_eq!(4, res.vertices.len());
        assert_eq!(0, res.ignored.len())
    }
}
