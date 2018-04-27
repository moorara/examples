use std::mem;

fn main() {
  // Integer types: i8, u8, i16, u16, i32, u32, i64, u64

  let a:u8 = 120;  // immutable
  println!("a = {}", a);

  let mut b:i8 = 0;  // mutable
  b = -42;
  println!("b = {}", b);

  let mut c = 123456789;  // i32
  println!("c = {}, size = {} bytes", c, mem::size_of_val(&c));

  let z:isize = 123;  // isize, usize
  let size_of_z = mem::size_of_val(&z);
  println!("z = {}, takes up {} bytes, {}-bit os", z, size_of_z, size_of_z * 8);

  let d:char = 'D';
  println!("d = {}, size = {} bytes", d, mem::size_of_val(&d));

  let e:f32 = 2.71;
  println!("e = {}, size = {} bytes", e, mem::size_of_val(&e));

  let f = 3.14;  // f64
  println!("f = {}, size = {} bytes", f, mem::size_of_val(&f));

  let g = false;
  println!("g = {}, size = {} bytes", g, mem::size_of_val(&g));
}
