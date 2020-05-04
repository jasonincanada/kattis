
mod lektira;

use std::io::{self, BufRead};

fn main() {

  // no parsing required, just read in a string from stdin
  for line in io::stdin().lock().lines() {

    let result = lektira::do_case(&line.unwrap());

    println!("{}", result);
  }

}

