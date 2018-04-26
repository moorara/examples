// https://doc.rust-lang.org/book/patterns.html

#![allow(dead_code)]

enum Color {
  Red,
  Green,
  Blue,
  Rgb(u8, u8, u8),
  Cmyk{cyan:u8, magenta:u8, yellow:u8, black:u8},
}

fn how_many(x:i32) -> &'static str {
  match x {
    0 => "no",
    1 | 2 => "one or two",
    12 => "a dozen",
    _z @ 9...11 => "lots of",
    _ if (x % 2 == 0) => "some",
    _ => "few"
  }
}

pub fn pattern_matching() {
  for i in 0..13 {
    println!("{}: I have {} objects!", i, how_many(i));
  }

  let point = (3, 4);
  match point {
    (0, 0) => println!("origin"),
    (0, y) => println!("x axis, y = {}", y),
    (x, 0) => println!("y axis, x = {}", x),
    (_, y) => println!("(?, {})", y)
  }

  let c:Color = Color::Cmyk{cyan:32, magenta:64, yellow:128, black:255};
  match c {
    Color::Red   => println!("r"),
    Color::Green => println!("g"),
    Color::Blue  => println!("b"),
    Color::Rgb(0, 0, 0) | Color::Cmyk{black: 255, ..} => println!("black"),
    _ => ()
  } 
}

fn main() {
  pattern_matching()
}
