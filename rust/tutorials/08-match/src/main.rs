fn match_statement() {
  let country_code = 77;
  let country = match country_code {
    33 => "France",
    44 => "UK",
    46 => "Sweden",
    49 => "Germany",
    1...99 => "Unknown",  // 1 to 99 inclusive
    _ => "Invalid"
  };

  println!("country code {} is {}", country_code, country);
}

fn main() {
  match_statement();
}
