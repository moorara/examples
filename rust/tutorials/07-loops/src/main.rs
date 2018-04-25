fn while_and_loop() {
  let mut x = 1;
  while x < 1000 {
    x *= 2;
    if x == 64 {
      continue;
    }
    println!("x = {}", x);
  }

  let mut y = 1;
  loop {
    y *= 2;
    println!("y = {}", y);
    if y == 1 << 10 {
      break;
    }
  }
}

fn for_loop() {
  for i in 1..11 {
    if i == 3 { continue; }
    if i == 8 { break; }
    println!("i = {}", i)
  }

  for (pos, val) in (30..41).enumerate() {
    println!("{}: {}", pos, val);
  }
}

fn main() {
  while_and_loop();
  for_loop();
}
