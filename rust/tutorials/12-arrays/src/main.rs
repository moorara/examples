use std::mem;

fn arrays() {
  let mut a:[i32;5] = [1, 2, 3, 4, 5];
  a[0] = 27;

  println!("{:?}", a);
  println!("a has {} elements, first is {}", a.len(), a[0]);

  if a == [27, 2, 3, 4, 5] {
    println!("Match!");
  }

  let b = [1u16; 3];  // b.len() == 3
  println!("b takes up {} bytes", mem::size_of_val(&b));
  for i in 0..b.len() {
    println!("{}", b[i]);
  }

  let matrix:[[f32;3];2] = [
    [1.0, 0.0, 0.0],
    [0.0, 2.0, 0.0]
  ];
  println!("{:?}", matrix);

  for i in 0..matrix.len() {
    for j in 0..matrix.len() {
      if i == j {
        println!("matrix[{}][{}] = {}", i, j, matrix[i][j]);
      }
    }
  }
}

fn main() {
  arrays();
}
