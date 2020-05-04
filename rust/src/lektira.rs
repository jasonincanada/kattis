/* Lektira - https://open.kattis.com/problems/lektira

   This is a Rust version of the Lektira kattis challenge. This is my first Rust program,
   to practice the nuances of strings vs string slices and, of course, the new memory
   ownership/borrowing model that makes rust so frustrating and so awesome at the same
   time

   This solution takes 0.00s (!) to complete on the kattis server

*/

pub fn do_case(case: &str) -> String {

  let mut first  = case.to_string();
  let     length = case.len();

  for   i in 1   .. length-1 {
    for j in i+1 .. length   {

      let (left,mid,right) = cut(case, i, j);
      let recombined =  reverse(left)
                     + &reverse(mid)
                     + &reverse(right);

      if recombined < first {
        first = recombined;
      }
    }
  }

  first
}

// cut a string into three substrings along the given indices
fn cut(s: &str, i: usize, j: usize) -> (&str,&str,&str) {
  
  ( &s[ ..i],
    &s[i..j],
    &s[j.. ] )
}

fn reverse(s: &str) -> String {
  s.chars().rev().collect::<String>()
}


/*- Tests -*/

#[cfg(test)]
mod tests {
  use super::{cut, do_case, reverse};

  #[test]
  fn cut_works() {
    let test = "dcbagfekjih";

    assert_eq!(cut(test, 1, 3), ("d", "cb", "agfekjih"));
  }


  #[test]
  fn do_case_works() {
    assert_eq!(do_case("dcbagfekjih"), "abcdefghijk");
    assert_eq!(do_case("mobitel")    , "bometil");
    assert_eq!(do_case("anakonda")   , "aanadnok");
  }


  #[test]
  fn reverse_works() {
    assert_eq!(reverse("Hello"), "olleH");
  }

}

