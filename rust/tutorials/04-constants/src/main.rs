const MY_NUMBER:u8 = 27;             // No memory address
static STATIC_VAR:i32 = 69;          // Static variable
static mut MUT_STATIC_VAR:i32 = 88;  // Mutable static variable

fn main() {
  println!("{} is a constant", MY_NUMBER);
  println!("{} is a static variable", STATIC_VAR);

  unsafe {
    println!("{} is a mutable static variable", MUT_STATIC_VAR)
  }
}
