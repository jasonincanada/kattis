/*  https://open.kattis.com/problems/busskortet  */

use std::io::BufRead;

fn main() {

    let     stdin  = std::io::stdin();
    let mut lines  = stdin.lock().lines(); 

    let num : i32  = lines.next().unwrap().unwrap()
                          .parse().unwrap();

    println!("{}", do_case(num));
}

fn do_case(mut num: i32) -> i32 {

    // do the 500 multiples in one step
    let count = num / 500;

    num %= 500;

    // do the rest
    if num > 400 {            
        count + 1
    } else if num > 200 {
        count + 2
    } else {
        count + 1
    }
}


/* Tests */

#[cfg(test)]
mod tests {
    use super::*;

    #[test] fn test_sample_1() { assert_eq!(3, do_case(850)); }
    #[test] fn test_sample_2() { assert_eq!(5, do_case(1800)); }
}
