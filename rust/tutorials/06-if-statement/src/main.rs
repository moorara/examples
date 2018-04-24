fn if_statement() {
  let temp = 25;

  if temp > 30 {
    println!("It's hot outside!");
  } else if temp < 10 {
    println!("It's cold outside!");
  } else {
    println!("It's OK outside!");
  }

  let weather = if temp > 20 {"sunny"} else {"cloudy"};
  println!("today is {}!", weather);

  println!("It's {} outside!", if temp > 30 {"hot"} else if temp < 10 {"cold"} else {"OK"});
  println!("It's {} outside!",
    if temp > 20 {
      if temp > 30 {"hot"} else {"warm"}
    } else if temp < 10 {"cold"} else {"OK}"});
}

fn main() {
  if_statement();
}
