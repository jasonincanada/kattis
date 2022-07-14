/*  https://open.kattis.com/problems/spiderman */

fn main() {
    let     stdin = stdin();    
    let mut lines = stdin.lock().lines();
      
    let count = parse_int(&mut lines);

    for _ in 0..count {        
        lines.next();

        // parse distances and add a blank entry at the beginning for 1-based indices
        let mut distances = parse_usizes(&mut lines);
        distances.insert(0, 0);

        println!("{}", do_case(distances));        
    }    
}

const UP   : u64 = 1;
const DOWN : u64 = 0;
const MAX_MOVES  : usize = 40;
const MAX_HEIGHT : usize = 1000;

// best path (encoded bitwise as an integer) and max height to here
type Cell = (u64, usize);

fn do_case(distances: Vec<usize>) -> String {

    // dp table storing best paths/heights, all cells initialized to None
    let mut table : [[Option<Cell>; MAX_HEIGHT+1]; MAX_MOVES+1] = [[None; MAX_HEIGHT+1]; MAX_MOVES+1];

    // movement 1 must always be up, and the max height we've been at so far is the first distance
    table[1][distances[1]] = Some((UP, distances[1]));

    // it's impossible to get anywhere else on the first move, so the rest of the heights
    // for column 1 of the table are left as None

    let n = distances.len() - 1;

    // remainder of the moves    
    for i in 2..=n {

        // now we're in the general step, where each cell represents the better of two choices:
        // move down from the prior cell d higher up, or move up from the prior cell d lower down

        let d = distances[i];
        
        // from ground level to the current distance we can get here only by climbing down
        for h in 0..d {
            if let Some((path, highest)) = table[i-1][h+d] {
                table[i][h] = Some((add_to_path(path, i, DOWN), highest));
            }
        }

        // the middle segment of heights can be arrived at by climbing down or up
        for h in d..MAX_HEIGHT-d {

            let from_above = table[i-1][h+d];
            let from_below = table[i-1][h-d];

            // if there's a path from above but not below
            if let (Some((path, highest)), None) = (from_above, from_below) {
                table[i][h] = Some((add_to_path(path, i, DOWN), highest));
            } 
            
            // if there's a path from below but not above
            else if let (None, Some((path, highest))) = (from_above, from_below) {
                table[i][h] = Some((add_to_path(path, i, UP), highest.max(h)));
            }

            // if we can get here from above or below
            else if let (Some((path_from_above, highest_from_above)), Some((path_from_below, highest_from_below)))
                      = (from_above, from_below)
            {
                // the optimization step
                // we have to choose which path to extend, so choose the lowest maximum height
                if highest_from_above < highest_from_below {
                    table[i][h] = Some((add_to_path(path_from_above, i, DOWN), highest_from_above));
                } else {
                    table[i][h] = Some((add_to_path(path_from_below, i, UP), highest_from_below.max(h)));
                }
            }
        }

        // from the top down by this distance, we can only get here by climbing up
        for h in MAX_HEIGHT-d..MAX_HEIGHT {
            if let Some((path, highest)) = table[i-1][h-d] {
                table[i][h] = Some((add_to_path(path, i, UP), highest.max(h)));
            }
        }
    }
        
    match table[n][0] {
        Some((path, _)) => path_to_string(path, n),
        None            => String::from("IMPOSSIBLE")
    }    
}

// set a bit on or off at a certain point in the path
fn add_to_path(path: u64, i: usize, updown: u64) -> u64 {
    path | (updown << (i-1))
}

// convert a path like 11001 to string "UDDUU"
fn path_to_string(path: u64, n: usize) -> String {
    let mut string = String::new();

    for i in 0..n {
        if path & (1 << i) != 0 {
            string.push('U');
        } else {
            string.push('D');
        }
    }

    string
}


/* Parsing */

use std::io::{BufRead, Lines, StdinLock, stdin};

fn parse_int(lines: &mut Lines<StdinLock>) -> u8 {
    lines.next().unwrap().unwrap()
         .parse().unwrap()
}

fn parse_usizes(lines: &mut Lines<StdinLock>) -> Vec<usize> {
    lines.next().unwrap().unwrap()
         .split_whitespace()
         .map(|num| num.parse().unwrap())
         .collect()
}


/* Tests */

#[cfg(test)]
mod tests {
    use super::*;

    #[test] fn test_sample_1() { assert_eq!("UDUD"      , do_case(vec![0, 20,20,20,20])); }
    #[test] fn test_sample_2() { assert_eq!("UUDUDD"    , do_case(vec![0, 3,2,5,3,1,2])); }
    #[test] fn test_sample_3() { assert_eq!("IMPOSSIBLE", do_case(vec![0, 3,4,2,1,6,4,5])); }

    #[test]
    fn test_add_to_path() {

        // UDUD
        assert_eq!(1, add_to_path(0, 1, UP));
        assert_eq!(1, add_to_path(1, 2, DOWN));
        assert_eq!(5, add_to_path(1, 3, UP));
        assert_eq!(5, add_to_path(5, 4, DOWN));
    }

    #[test]
    fn test_path_to_string() {
        assert_eq!("UDUD"  , path_to_string(5, 4));
        assert_eq!("UUDUDD", path_to_string(11, 6));
    }
}
