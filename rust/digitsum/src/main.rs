/*  https://open.kattis.com/problems/digitsum  */

fn main() {
    let     stdin = stdin();    
    let mut lines = stdin.lock().lines();

    // pre-compute the table of digit sums up to every single-digit multiple of 10 up to 10^15
    let table = build_table();

    let count = parse_nums(&mut lines)[0];

    for _ in 0..count {
        let nums = parse_nums(&mut lines);
        let from = nums[0];
        let to   = nums[1];

        println!("{}", do_case(from, to, &table));
    }
}

fn do_case(from :  u128,
           to   :  u128,
           table: &Table) -> u128
{
    let from  = if   from == 0 { 0 }
                else           { digit_sum(from - 1, table) };
    
    let to    = digit_sum(to, table);

    to - from
}

fn digit_sum(number:  u128,
             table : &Table) -> u128
{                                               // for number = 623
    let exp = log10_floor(number);              // 2
    let power_of_10 = 10u128.pow(exp as u32);   // 100

    let leading   = number / power_of_10;       // 6
    let remainder = number % power_of_10;       // 23

    let main = table[exp as usize][leading as usize];

    if exp == 0 { return main }

    // TODO: explain what this is
    let extra = leading * remainder;

    main + extra + digit_sum(remainder, table)
}

// compute up to 10^15
const MAX_POWER : usize = 15;

type Table = [[u128; 10+1]; MAX_POWER+1];

// generate the table of sums up to single-digit multiples of powers of 10
fn build_table() -> Table {
    
    // initialize with all zeros
    let mut table : Table = [[0; 10+1]; MAX_POWER+1];

    for p in 0..=MAX_POWER {
    for d in 1..=10 {

        let blue     = table[p][d-1];

        let red      = if   p == 0 { 0              }
                       else        { table[p-1][10] };

        let purple   = 10u128.pow(p as u32) - 1;
        let green    = d as u128 - 1;
        let darkblue = if d == 10 { 0 } else { d };

        table[p][d] = blue + red + (purple*green) + darkblue as u128;
    }
    }

    table
}


/* Math */

// rust complains about its own log10 when run on integer types because it's undefined how to
// convert the result back to an integer: round up? round down? for this we need to round down
fn log10_floor(mut number: u128) -> u8 {
    let mut exponent = 0;

    while number >= 10 {
        exponent += 1;
        number /= 10;
    }

    exponent
}


/* Parsing */

use std::io::{BufRead, Lines, stdin, StdinLock};

fn parse_nums(lines: &mut Lines<StdinLock>) -> Vec<u128> {
    lines.next().unwrap().unwrap()
         .split_whitespace()
         .map(|s| s.parse().unwrap())
         .collect::<Vec<u128>>()
}


/* Tests */

#[cfg(test)]
mod tests {
    use super::*;

    #[test] fn test_sample_1() { assert_eq!(46,      do_case(0, 10, &build_table())); }
    #[test] fn test_sample_2() { assert_eq!(28,      do_case(28, 31, &build_table())); }
    #[test] fn test_sample_3() { assert_eq!(1128600, do_case(1234, 56789, &build_table())); }

    #[test]
    fn test_log10_floor() {
        assert_eq!(0, log10_floor(9));
        assert_eq!(1, log10_floor(10));
        assert_eq!(1, log10_floor(11));
        assert_eq!(1, log10_floor(99));
        assert_eq!(2, log10_floor(100));
    }
   
    #[test]
    fn test_from_table() {
        let table = build_table();

        assert_eq!(1, digit_sum(1, &table));
        assert_eq!(3, digit_sum(2, &table));
        assert_eq!(6, digit_sum(3, &table));
        assert_eq!(45, digit_sum(9, &table));

        assert_eq!(46, digit_sum(10, &table));
        assert_eq!(102, digit_sum(20, &table));

        assert_eq!(154, digit_sum(28, &table));
        assert_eq!(172, digit_sum(31, &table));

        assert_eq!(901, digit_sum(100, &table));
        assert_eq!(903, digit_sum(101, &table));

        assert_eq!(1902, digit_sum(200, &table));
    }
}
