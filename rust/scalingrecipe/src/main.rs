/*  https://open.kattis.com/problems/scalingrecipe  */

use std::io::{BufRead, StdinLock, Lines};

fn main() {
    let     stdin = std::io::stdin();
    let mut input = stdin.lock().lines();

    let first = parse_numbers_from_line(&mut input);

    // portions produced by the recipe and portions needed
    let produced = first[1];
    let needed   = first[2];

    // rest of lines
    while let Some(line) = input.next() {
        let amount : i32 = line.unwrap().parse().unwrap();

        println!("{}", amount * needed / produced);
    }
}

fn parse_numbers_from_line(input: &mut Lines<StdinLock>) -> Vec<i32> {
    let line  = input.next().unwrap().unwrap();
    let split = line.split(" ");

    split.map(|s| s.parse().unwrap())
         .collect()
}
