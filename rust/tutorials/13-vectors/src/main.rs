fn vectors() {
  let mut a = Vec::new();
  a.push(1);
  a.push(2);

  println!("a = {:?}", a);

  // The pointer-sized unsigned integer type
  let i:usize = 0;
  a[i] = 27;
  println!("a[0] = {}", a[i]);

  for x in &a {
    println!("{}", x);
  }

  a.push(69);
  match a.get(3) {
    Some(x) => println!("a[3] = {}", x),
    None    => println!("element not found!")
  }

  let last_elem = a.pop();
  println!("last element is {:?}, a = {:?}", last_elem, a);

  while let Some(x) = a.pop() {
    println!("{}", x);
  }
}

fn main() {
  vectors();
}
