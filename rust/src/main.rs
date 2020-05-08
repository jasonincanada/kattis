
use std::io::{self, BufRead};

mod lektira;

fn main() {

  // no parsing required, just read in a string from stdin
  for line in io::stdin().lock().lines() {

    let result = lektira::do_case(&line.unwrap());

    println!("{}", result);
  }

}

/*
use std::str::FromStr;

mod tiredterry;

fn main() {
  let mut input = String::new();

  io::stdin().read_to_string(&mut input).unwrap();

  let case   = tiredterry::TestCase::from_str(&input).unwrap();
  let result = tiredterry::do_case(case);

  println!("{}", result);
}
*/

