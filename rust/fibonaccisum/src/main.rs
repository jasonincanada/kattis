/*  https://open.kattis.com/problems/fibonaccisum  */

use std::io::BufRead;

fn main() {
    let     stdin  = std::io::stdin();
    let mut lines  = stdin.lock().lines(); 

    let sum : i32  = lines.next().unwrap().unwrap()
                          .parse().unwrap();
                            
    println!("{}", do_case(sum));
}

fn do_case(mut sum: i32) -> String {

    let sequence : Vec<i32> = vec![701408733,433494437,267914296,165580141,102334155,63245986,39088169,24157817,14930352,9227465,5702887,3524578,2178309,1346269,832040,514229,317811,196418,121393,75025,46368,28657,17711,10946,6765,4181,2584,1597,987,610,377,233,144,89,55,34,21,13,8,5,3,2,1];

    /*   λ> let fibs = 0 : 1 : zipWith (+) fibs (tail fibs)
         λ> reverse $ take 45 fibs
         [701408733,433494437,267914296,165580141,102334155,63245986,39088169,24157817,14930352,9227465,5702887,3524578,2178309,1346269,832040,514229,317811,196418,121393,75025,46368,28657,17711,10946,6765,4181,2584,1597,987,610,377,233,144,89,55,34,21,13,8,5,3,2,1,1,0]
    */
    
    let mut fibs : Vec<i32> = vec![];

    for fib in sequence {
        if fib <= sum {
            fibs.push(fib);
            sum -= fib;
        }
    }
    
    // join the fibs into a space-separated string
    fibs.iter()
        .rev()
        .map(|&v| v.to_string())
        .collect::<Vec<String>>()
        .join(" ")
}


/* Tests */

#[cfg(test)]
mod tests {
    use super::*;

    #[test] fn test_sample_1() { assert_eq!("2 5",   do_case(7)); }
    #[test] fn test_sample_2() { assert_eq!("1 3 8", do_case(12)); }
    #[test] fn test_sample_3() { assert_eq!("3",     do_case(3)); }
}
