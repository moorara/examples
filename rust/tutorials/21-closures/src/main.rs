fn say_hello() {
  println!("Hello!");
}

fn closures() {
  let sh = say_hello;
  sh();

  let plus_one = |x:i32| -> i32 { x+ 1 };
  let a = 6;
  println!("{} + 1 = {}", a, plus_one(a));

  let mut some = 2;
  {
    let plus_some = |x| {
        let mut z = x;
        z += some;
        z
    };
    let b = 3;
    println!("{} + {} = {}", b, some, plus_some(b));
  }
  let _borrow_some = &mut some;

  let plus_three = |mut _x:i32| _x += 3;
  let f = 12;
  plus_three(f);
  println!("f = {}", f);
}

fn main() {
  closures();
}
