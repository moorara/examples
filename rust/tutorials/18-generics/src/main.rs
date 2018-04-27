#![allow(dead_code)]

struct Point<T> {
  x: T,
  y: T
}

struct Line<T> {
  start: Point<T>,
  end: Point<T>
}

fn generics() {
  let a = Point{x: 1.2, y: 3.4};
  let b:Point<f64> = Point{x: 0.0, y: 4f64};
  let _line = Line{start: a, end: b};
}

fn main() {
  generics();
}
