/*  https://open.kattis.com/problems/addemup - Add 'Em Up!

    My first attempt at this was the obvious brute force O(n²) solution, and due to Rust's sheer
    speed it actually got onto the leaderboard at 0.12s.  I switched over to the HashMap solution
    below and brought the time down to 0.01s which ties it for first with the other Rust/C++
    solutions
*/

use std::io::{BufRead, Lines, StdinLock};
use std::collections::HashMap;

struct Input {
    sum     : i32,      // desired sum
    numbers : Vec<i32>  // card numbers
}

fn main() {

    // parse the problem input
    let stdin  = std::io::stdin();
    let lines  = stdin.lock().lines(); 
    let input  = parse(lines);

    let result = do_case(input);

    match result {
        Result::Yes => println!("YES"),
        Result::No  => println!("NO")
    }    
}

fn parse(mut lines: Lines<StdinLock>) -> Input {
    
    // get the second number from the first line
    let line = lines.next()
                    .unwrap()
                    .unwrap();

    let split : Vec<&str> = line.split_whitespace().skip(1).collect();
    let sum   : i32       = split[0].parse().unwrap();

    // parse the list of numbers in the second line
    let numbers = lines.next()
                       .unwrap()
                       .unwrap()
                       .split_whitespace()
                       .map(|s| s.parse().unwrap())
                       .collect();

    Input {
        sum,
        numbers
    }
}

#[derive(Debug, PartialEq)]
enum Result {
    Yes,
    No
}

fn do_case(input: Input) -> Result {
        
    // count how many of each right-side-up and rotated numbers we have
    let mut original : HashMap<i32, i32> = HashMap::with_capacity( input.numbers.len() );
    let mut rotated  : HashMap<i32, i32> = HashMap::with_capacity( input.numbers.len() );

    for number in input.numbers {
        add_or_increment(&mut original, &number);

        // if this number has a valid rotation, count it
        if let Some(rot) = rotate(number) {
            add_or_increment(&mut rotated, &rot);
        }
    }

    // first phase, see if we can make the sum out of 2 of the original (unrotated) cards
    // or 1 unrotated and 1 rotated card
    for (num, count) in &original {

        // here's our target, look for this in either set without using the same card twice
        let difference = input.sum - num;

        if difference < 0 {
            continue
        }

        // if the candidate num is half the sum, this is fine, we just need to have at least one
        // other card with this number, since we can't use the same card twice
        if difference == *num {
            if count > &1 {
                return Result::Yes
            }            
        } else if original.contains_key(&difference) {
            return Result::Yes
        }

        // try the rotated numbers
        if rotated.contains_key(&difference) {

            // one concern here, the second num can't be a rotation of the original card. so, check
            // whether our current candidate number can even rotate to this number. if so we need
            // to have had at least one other card that can (count > 1)
            if let Some(rot) = rotate(*num) {
                if rot == difference {
                    if let Some(count) = rotated.get(&rot) {
                        if count > &1 {
                            return Result::Yes
                        }
                    }
                } else {
                    return Result::Yes
                }
            } else {
                return Result::Yes
            }
        }
    }

    // second phase, can we make the sum out of two rotated values
    for (num, count) in &rotated {
        
        let difference = input.sum - num;

        if difference < 0 {
            continue
        }

        // if the candidate num is half the sum, make sure we have at least one other
        // card that rotates to this number, since we can't use the same card twice
        if difference == *num {
            if count > &1 {
                return Result::Yes
            }
        } else if rotated.get(&difference).is_some() {
            return Result::Yes
        }
    }

    Result::No
}

// add 1 to the value at this key, or start a new value of 1 at this key if it doesn't yet exist
fn add_or_increment(map: &mut HashMap<i32, i32>, key: &i32) {    
    map.entry(*key)
       .and_modify(|count| *count += 1)
       .or_insert(1);
}

// every string in the input is a valid integer, but not all of them can be rotated

// 12      -> Some(21)
// 123     -> None      (because 3 rotated 180° isn't a valid digit)
// 125     -> Some(521)
// 1256890 -> Some(689521)
fn rotate(mut num: i32) -> Option<i32> {
    
    // build the rotated number a digit at a time
    let mut rotated = 0;

    while num > 0 {
        let digit = num % 10;

        if digit == 3 || digit == 4 || digit == 7 {
            return None
        }

        let rotated_digit = match digit {
            6 => 9,
            9 => 6,

            // the other digits stay themselves under 180° rotation: 1 2 5 8 0
            x => x  
        };        

        rotated = rotated * 10 + rotated_digit;
        num /= 10;
    }

    Some(rotated)
}


#[cfg(test)]
mod tests {
    use super::*;

    #[test] fn test_rotate_12()      { assert_eq!(rotate(12), Some(21)); }
    #[test] fn test_rotate_123()     { assert_eq!(rotate(123), None); }
    #[test] fn test_rotate_1256890() { assert_eq!(rotate(1256890), Some(689521)); }
    #[test] fn test_rotate_1200()    { assert_eq!(rotate(1200), Some(21)); } // try trailing 00

    // sample inputs from the problem description
    #[test]
    fn test_sample_1() {
        let input = Input {
            sum: 66,
            numbers: vec![15, 21, 22]
        };

        assert_eq!(do_case(input), Result::No);
    }

    #[test]
    fn test_sample_2() {
        let input = Input {
            sum: 63,
            numbers: vec![15, 21, 22]
        };

        assert_eq!(do_case(input), Result::Yes);
    }

    // test the various paths through do_case()
    #[test]
    fn test_both_in_original_different() {
        let input = Input {
            sum: 4+6,
            numbers: vec![4, 6]
        };

        assert_eq!(do_case(input), Result::Yes);
    }

    #[test]
    fn test_both_in_original_same() {
        let input = Input {
            sum: 5+5,
            numbers: vec![5, 5]
        };

        assert_eq!(do_case(input), Result::Yes);
    }

    #[test]
    fn test_both_in_original_one_only() {

        // shouldn't be able to use the same card twice
        let input = Input {
            sum: 5+5,
            numbers: vec![5]
        };

        assert_eq!(do_case(input), Result::No);
    }

    #[test]
    fn test_one_in_original_one_in_rotated() {
        
        let input = Input {
            sum: 12+16,
            numbers: vec![12, 91]
        };

        assert_eq!(do_case(input), Result::Yes);
    }

    #[test]
    fn test_cant_be_rotation_of_original() {

        // can't be a card and its own rotation
        let input = Input {
            sum: 33,
            numbers: vec![12]
        };

        // we can't add 12 to its own rotation 21 to make 33
        assert_eq!(do_case(input), Result::No);
    }

    #[test]
    fn test_both_in_rotated() {
        
        let input = Input {
            sum: 21+91,
            numbers: vec![12, 16]
        };

        assert_eq!(do_case(input), Result::Yes);
    }
}
