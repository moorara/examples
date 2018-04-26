#![allow(dead_code)]

enum Color {
  Red,
  Green,
  Blue,
  Rgb(u8, u8, u8),
  Cmyk{cyan:u8, magenta:u8, yellow:u8, black:u8},  // struct
}

fn enums() {
  // let c:Color = Color::Blue;
  // let cc:Color = Color::Rgb(0, 255, 0);
  let c:Color = Color::Cmyk{cyan:32, magenta:64, yellow:128, black:255};
  match c {
    Color::Red   => println!("r"),
    Color::Green => println!("g"),
    Color::Blue  => println!("b"),
    Color::Rgb(255, 0, 0) => println!("r"),
    Color::Rgb(0, 255, 0) => println!("g"),
    Color::Rgb(0, 0, 255) => println!("b"),
    Color::Cmyk{cyan: _, magenta: _, yellow: _, black: 255} => println!("black"),
    _ => ()
  } 
}

fn main() {
  enums();
}
