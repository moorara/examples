
fn use_slice(slice: &mut[i32]) {
  println!("first element = {}, len ={}", slice[0], slice.len());

  slice[0] = 27
}

fn slices() {
  let mut data = [1, 2, 3, 4, 5];
  use_slice(&mut data);
  use_slice(&mut data[1..4]);

  println!("data = {:?}", data);
}

fn main() {
  slices();
}
