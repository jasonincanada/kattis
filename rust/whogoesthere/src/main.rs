/*  https://open.kattis.com/problems/whogoesthere  */

use std::io::{BufRead, StdinLock, Lines};

fn main() {    
    let     stdin = std::io::stdin();
    let mut input = stdin.lock().lines();

    // the first number on the first line is the site capacity
    let     capacity = parse_first_number(&mut input);
    let mut assigned = 0;

    // followed by team counts, one per line. keep track of each school's index because
    // we prune this list as the elements reach 0. we need to remember the original indices
    // because they will become out of alignment with the `accepted` list as we delete
    let mut teams = parse_lines_of_one_number_with_index(&mut input);

    // start off the list of accepted team counts with 0s for each school
    let mut accepted = vec![0; teams.len()];
    
    // note we don't need to track the identity of each individual team, we are just draining
    // integers (counts of how many teams per school) until they reach 0, then removing them
    // from the original list, while adding the same amounts to `accepted` and `assigned`
    
    while capacity > assigned {

        // pruning step, keep only schools with teams left
        teams.retain(|(_, count)| count > &0);

        if teams.is_empty() {
            break;
        }

        // assignment step
        for i in 0..teams.len() {
            let team = teams[i].0 as usize;

            // move a team from waitlist to roster
            accepted[team] += 1;
            teams[i].1     -= 1;

            // total assignments
            assigned       += 1;

            if assigned >= capacity {
                break;
            }
        }
    }  

    for i in accepted {
        println!("{}", i);
    }
}

// parse the first number from the next full line of input, discard the rest of the line
fn parse_first_number(input: &mut Lines<StdinLock>) -> i32 {
    let line = input.next().unwrap().unwrap();
    let split : Vec<&str> = line.split(' ').collect();
    
    split[0].parse().unwrap()
}

// parse the number from each of the remaining lines, tracking their index starting from 0
fn parse_lines_of_one_number_with_index(input: &mut Lines<StdinLock>) -> Vec<(usize, i32)> {
    let mut numbers = Vec::<(usize, i32)>::new();
    let mut idx : usize = 0;

    while let Some(line) = input.next() {
        let inner = line.unwrap();
        let number = inner.parse().unwrap();

        numbers.push((idx, number));

        idx += 1;
    }

    numbers
}

/*  C:\Users\Jason\Documents\GitHub\kattis\rust\whogoesthere> type sample.txt | cargo run
    5
    5
    1
    5
    4
*/
