/*  https://open.kattis.com/problems/skolvagen  */

fn main() {
    let input = get_crossings_from_stdin();
    println!("{}", do_case(input))
}

// the first index into our 2-dimensional array
const TOP    : usize = 0;
const BOTTOM : usize = 1;

fn do_case(crossings: Vec<char>) -> u16 {
    
    let n = crossings.len();

    // allocate for the worst case scenario, 1000 intersections (plus 1 for base case)
    let mut table = [[0; (1000+1)]; 2];

    // base cases
    table[TOP]   [0] = 0;
    table[BOTTOM][0] = 1;   // crossing south from the starting position

    use std::cmp::min;

    // systematically fill in the recursive cases
    for i in 1..=n {
        table[TOP][i]    = min(table[TOP]   [i-1] + crossing_cost(i, TOP   , &crossings) + 0,
                               table[BOTTOM][i-1] + crossing_cost(i, BOTTOM, &crossings) + 1);
        
        table[BOTTOM][i] = min(table[BOTTOM][i-1] + crossing_cost(i, BOTTOM, &crossings) + 0,
                               table[TOP]   [i-1] + crossing_cost(i, TOP   , &crossings) + 1);
    }

    // return the minimum number of crossings from top left to top right
    table[TOP][n]
}

fn crossing_cost(i: usize,
                side: usize,
                crossings: &[char]) -> u16
{
    // if we're crossing eastwards on the north side, it costs 1 crossing only if
    // the intersection type is North or Both (free if there's no street to cross)
    if side == TOP {
        match crossings[i-1] {
            'N' => 1,
            'S' => 0,
            'B' => 1,
            _   => panic!("unknown crossing type")
        }
    } else {
        match crossings[i-1] {
            'N' => 0,
            'S' => 1,
            'B' => 1,
            _   => panic!("unknown crossing type")
        }
    }
}


/* Parsing */

fn get_crossings_from_stdin() -> Vec<char> {
    use std::io::BufRead;
    
    let     stdin  = std::io::stdin();
    let mut lines  = stdin.lock().lines(); 

    let line    = lines.next().unwrap().unwrap();
    let letters = line.chars().collect::<Vec<char>>();

    letters
}


/* Tests */

#[cfg(test)]
mod tests {
    use super::*;

    #[test] fn test_sample_1() { assert_eq!(4, do_case("SNBNNSB"         .chars().collect())); }
    #[test] fn test_sample_2() { assert_eq!(8, do_case("SBSNNBSNNSSSNNNB".chars().collect())); }
}
