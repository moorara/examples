#![allow(dead_code)]

struct Point {
  x: f64,
  y: f64
}

struct Line {
  start: Point,
  end: Point
}

fn structs() {
  let p1 = Point{x: 3.0, y: 4.0};
  let p2 = Point{x: 5.0, y: 8.0};

  println!("P1 is at ({}, {})", p1.x, p1.y);
  println!("P2 is at ({}, {})", p2.x, p2.y);

  let _line = Line{start: p1, end: p2};
}

fn main() {
  structs();
}
