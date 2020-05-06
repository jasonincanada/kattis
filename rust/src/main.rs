use std::io::{self, Read};
use std::str::FromStr;

mod tiredterry;

fn main() {
  let mut input = String::new();

  io::stdin().read_to_string(&mut input).unwrap();

  let case   = tiredterry::TestCase::from_str(&input).unwrap();
  let result = tiredterry::do_case(case);

  println!("{}", result);
}

