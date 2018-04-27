// https://doc.rust-lang.org/std/option

fn option() {
  let x = 3.0;
  let y = 2.0;

  let result:Option<f64> =
    if y != 0.0 { Some(x/y) } else { None };
  
  println!("{:?}", result);

  match result {
    Some(z) => println!("{}/{} = {}", x, y, z),
    None    => println!("Cannot divide {} by {}", x, y)
  }

  // if let or while let
  if let Some(z) = result {
    println!("z = {}", z)
  }
}

fn main() {
  option();
}
