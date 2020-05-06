/* Tired Terry - https://open.kattis.com/problems/tiredterry

   An easy one I picked to figure out how to do parsing in Rust, but I ended up using the
   Recap crate with a regex annotation on the TestCase struct instead of doing anything
   with parser combinators. This passes the test cases locally, but kattis doesn't let you
   import external crates, so this didn't compile on their servers. With Rust's standard
   library being so slim I likely won't use it much for kattis challenges

*/

use recap::Recap;
use serde::Deserialize;

#[derive(Deserialize, Recap)]
#[recap(regex = r#"\d+\s(?P<p>\d+)\s(?P<d>\d+)\n(?P<pattern>\S+)"#)]
pub struct TestCase {
  p : usize,
  d : i32,
  pattern : String
}

pub fn do_case(case: TestCase) -> String {

  let mut running_z : i32 = 0;
  let mut tireds    : i32 = 0;

  let bytes = case.pattern.as_bytes();

  for i in 0 .. bytes.len() {
    
    if bytes[i] == b'Z' {
      running_z += 1;
    }

    // if we're losing a Z from it rolling out of our sliding window
    if i > case.p {
      if bytes[i - case.p + 1] == b'Z' {
        running_z -= 1;
      }
    }

    if running_z < case.d {
      tireds += 1;
    }
  }

  tireds.to_string()
}


/*- Tests -*/

#[cfg(test)]
mod tests {
  use super::{do_case, TestCase};

  #[test]
  fn do_case_1() {
    let case1 = TestCase { p: 1
                         , d: 1
                         , pattern: "WZ".to_string() };

    assert_eq!(do_case(case1), "1");
  }

  #[test]
  fn do_case_2() {
    let case2 = TestCase { p: 3
                         , d: 2
                         , pattern: "WZWWZ".to_string() };

    assert_eq!(do_case(case2), "4");
  }
}

