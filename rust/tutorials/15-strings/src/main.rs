fn strings() {
  let s1 = "Hello, World!";
  println!("s1: {}", s1);

  let s2:&str = "Hello there!";  // &str = string slice
  println!("s2: {}", s2);

  for c in s1.chars() {
    print!("{}", c);
  }

  println!();

  for c in s2.chars().rev() {
    print!("{}", c);
  }

  println!();

  if let Some(first_char) = s1.chars().nth(0) {
    println!("first letter of s1 is {}", first_char);
  }

  // String in heap
  let mut letters = String::new();
  let mut c = 'a' as u8;
  while c <= ('z' as u8) {
    letters.push(c as char);
    letters.push_str(",");
    c += 1;
  }

  println!("letters: {}", letters);

  // &str <> String
  let s3:&str = &letters;
  println!("s3: {}", s3);

  let mut s4 = String::from("Hello World");
  s4.push_str("!!");
  println!("s4: {}", s4);

  let mut s5 = "Hello World".to_string();
  s5.remove(0);
  s5.replace("ello", "Hi");
  println!("s5: {}", s5);

  let hello = "Hello ".to_string();
  let world = "world!";
  let s6 = hello + world;
  println!("s6: {}", s6);

  let hi = "Hi ".to_string();
  let there = "there".to_string();
  let s7 = hi + &there;
  println!("s7: {}", s7);
}

fn main() {
  strings();
}
