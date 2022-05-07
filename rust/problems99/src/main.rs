/*  https://open.kattis.com/problems/99problems  */

use std::io::BufRead;

fn main() {
    // get first line of input and parse it into an integer
    let input = std::io::stdin().lock().lines().next().unwrap().unwrap();
    let n: i32 = input.parse().unwrap();

    println!("{}", do_case(n));
}

fn do_case(n: i32) -> i32 {

    // anything under 49 will round down to -1 without this check
    if n < 49 {
        return 99
    }

    // split 1234 into 1200 and 34
    let (div, rem) = (n / 100 * 100, n % 100);

    if rem < 49 {
        div - 1         // 1200 becomes 1199
    } else {
        div + 99        // 1200 becomes 1299
    }
}


#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_sample_1() { assert_eq!(do_case(10), 99); }

    #[test]    
    fn test_sample_2() { assert_eq!(do_case(249), 299); }

    #[test]
    fn test_sample_3() { assert_eq!(do_case(10000), 9999); }
}
