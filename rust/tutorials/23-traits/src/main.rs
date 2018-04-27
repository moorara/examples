trait Animal {
  // static function
  fn create(name: &'static str) -> Self;

  // instance method
  fn name(&self) -> &'static str;

  // instance method
  fn talk(&self) {
    println!("{} cannot talk!", self.name());
  }
}

struct Human {
  name: &'static str
}

impl Animal for Human {
  fn create(name: &'static str) -> Human {
    Human{name: name}
  }
  fn name(&self) -> &'static str {
    self.name
  }
  fn talk(&self) {
    println!("{} says hello!", self.name);
  }
}

struct Cat {
  name: &'static str
}

impl Animal for Cat {
  fn create(name: &'static str) -> Cat {
    Cat{name: name}
  }
  fn name(&self) -> &'static str {
    self.name
  }
  fn talk(&self) {
    println!("{} says meow!", self.name);
  }
}

trait Summable<T> {
  fn sum(&self) -> T;
}

impl Summable<i32> for Vec<i32> {
  fn sum(&self) -> i32 {
    let mut res:i32 = 0;
    for i in self {
      res += *i;
    }
    return res;
  }
}

fn traits() {
  // let h:Human = Animal::create("Milad");
  let h = Human::create("Milad");
  h.talk();

  let c = Cat::create("Misty");
  c.talk();

  let a = vec![1, 2, 3, 4];
  println!("sum = {}", a.sum());
}

fn main() {
  traits();
}
