/* Lektira - https://open.kattis.com/problems/lektira

   This is a Rust version of the Lektira kattis challenge. This is my first Rust program,
   to practice the nuances of strings vs string slices and, of course, the new memory
   ownership/borrowing model that makes rust so frustrating and so awesome at the same
   time

   This solution takes 0.00s (!) to complete on the kattis server

   Revision: This revision should be a bit more performant (though it also takes 0.00s
   runtime on the server so it's hard to tell for sure). We now call reverse only once per
   change of the variable i to determine the left segment of the candidate string, instead
   of every time we compose a new one

*/

pub fn do_case(case: &str) -> String {

  let mut first  = case.to_string();
  let     length = case.len();

  for i in 1 .. length-1 {

    // reverse the left-most segment only once per change of i
    let left = reverse(&case[ ..i]);

    for j in i+1 .. length {

      let mid   = &case[i..j];
      let right = &case[j.. ];

      // this still copies left again to a new string before concatenating the mid and
      // right segments. it feels like this should be avoidable because we already
      // converted left to a string when we initialized it in the outer loop
      let recombined =  left.to_string()
                     + &reverse(mid)
                     + &reverse(right);

      if recombined < first {
        first = recombined;
      }
    }
  }

  first
}

fn reverse(s: &str) -> String {
  s.chars().rev().collect::<String>()
}


/*- Tests -*/

#[cfg(test)]
mod tests {
  use super::{do_case, reverse};


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

