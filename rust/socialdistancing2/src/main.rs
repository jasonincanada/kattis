/*  https://open.kattis.com/problems/socialdistancing2  */

use std::io::BufRead;

fn main() {
    let     stdin = std::io::stdin();
    let mut input = stdin.lock().lines();
    
    // extract the seat count from the first line
    let line               = input.next().unwrap().unwrap();
    let tokens : Vec<&str> = line.split(' ').collect();
    let capacity : i32     = tokens[0].parse().unwrap();
    
    // parse and collect all numbers on the next line, these are the occupied seat indices
    let line = input.next().unwrap().unwrap();
    let seated : Vec<i32> = line.split(' ')
                                .map(|s| s.parse().unwrap())
                                .collect();
    
    let extra = do_case(&seated, capacity);

    println!("{}", extra);
}

fn do_case(seated : &Vec<i32>, capacity : i32) -> i32 {
    
    // accumulate the extra seat count one interval at a time
    let mut extra = 0;

    for i in 0..seated.len()-1 {
        let distance = seated[i+1] - seated[i] - 1;

        extra += insertable(distance);
    }

    // emulate the table wrapping around by calculating where the first seat would be
    // if it were actually after the last one
    let virtual_seat = capacity + seated[0] - 1;

    extra += insertable(virtual_seat - seated.last().unwrap());

    extra    
}

// how many people can sit between two people with a given number of open seats between them
fn insertable(distance: i32) -> i32 {
    (distance + 1) / 2 - 1
}


/* Tests */

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_insertable() {
        assert_eq!(0, insertable(1));
        assert_eq!(0, insertable(2));
        assert_eq!(1, insertable(3));
        assert_eq!(1, insertable(4));
        assert_eq!(2, insertable(5));
        assert_eq!(2, insertable(6));
        assert_eq!(3, insertable(7));
    }

    /* sample inputs */

    #[test]
    fn test_sample_1() { assert_eq!(2,  do_case(&vec![2,6], 9)); }

    #[test]
    fn test_sample_2() { assert_eq!(1,  do_case(&vec![1,4,7], 10)); }

    #[test]
    fn test_sample_3() { assert_eq!(0,  do_case(&vec![2,5], 6)); }

    #[test]
    fn test_sample_4() { assert_eq!(43, do_case(&vec![7,14,47,78,99], 100)); }

    #[test]
    fn test_sample_5() { assert_eq!(2,  do_case(&vec![3], 6)); }
}
