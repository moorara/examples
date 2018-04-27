fn ownership() {
  let u = 27;
  let u2 = u;
  println!("{:?}", u);

  let v = vec![1, 2, 3, 4];
  // let v2 = v;
  // println!("{:?}", v);

  // let w = Box::new(1);
  // let w2 = w;
  // println!("{:?}", w);

  let x = vec![1, 2, 3, 4];
  let print_vec = |x:Vec<i32>| -> Vec<i32> {
    println!("{:?}", x);
    x
  };
  let y = print_vec(x);
  println!("{:?}", y);
}

fn borrowing() {
  let print_vec = |x:&Vec<i32>| {
    println!("{:?}", x);
  };

  let v = vec![4, 3, 2, 1];
  print_vec(&v);
  println!("{:?}", v);

  let mut a = 40;
  {
    let b = &mut a;
    *b += 2;
  }
  println!("a = {}", a);

  let mut z = vec![4, 3, 2, 1];
  for i in &z {
    println!("i = {}", i);
    // z.push(5);
  }
}

fn main() {
  ownership();
  borrowing();
}
