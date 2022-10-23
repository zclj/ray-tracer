use ray_tracer::canvas::Canvas;
use ray_tracer::color::Color;
use ray_tracer::vector::{Point, Vector};
use std::fs::File;
use std::io::Write;
use std::path::Path;

#[derive(Debug)]
struct Projectile {
    position: Point,
    velocity: Vector,
}

struct Environment {
    gravity: Vector,
    wind: Vector,
}

fn tick(env: &Environment, proj: &Projectile) -> Projectile {
    Projectile {
        position: &proj.position + &proj.velocity,
        velocity: &proj.velocity + &env.gravity + &env.wind,
    }
}

fn launch(g: f32, w: f32) -> Vec<Projectile> {
    let env = Environment {
        gravity: Vector::new(0., g, 0.),
        wind: Vector::new(w, 0., 0.),
    };
    let proj = Projectile {
        position: Point::new(0., 1., 0.),
        velocity: (Vector::new(1., 1.8, 0.)).norm() * 11.25,
    };

    let mut ps = vec![proj];

    while ps.last().unwrap().position.y > 0. {
        let next = tick(&env, ps.last().unwrap());
        if next.position.y > 0. {
            ps.push(next)
        } else {
            return ps;
        }
    }

    ps
}

fn projectile_into_canvas(ps: &Vec<Projectile>) -> Canvas {
    let mut canvas = Canvas::new(900, 550);
    let color = Color::new(1., 0.8, 0.6);

    for Projectile { position, .. } in ps {
        println!(
            "Wrote : {:?}-{:?}",
            position.x.floor() as usize,
            (550. - position.y).floor() as usize
        );
        canvas.write_pixel(
            position.x.floor() as usize,
            (549. - position.y).floor() as usize,
            color.clone(),
        )
    }

    canvas
}

fn main() {
    let projectile_path = launch(-0.1, -0.01);

    println!("{:?}", projectile_path);

    let ppm = projectile_into_canvas(&projectile_path).to_ppm();

    let path = Path::new("projectile.ppm");

    let mut file = match File::create(&path) {
        Err(why) => panic!("could not create file {}: {} ", path.display(), why),
        Ok(file) => file,
    };

    if let Err(why) = file.write_all(ppm.as_bytes()) {
        panic!("could not write file {}: {} ", path.display(), why)
    }

    println!("Projectile done!");
}
