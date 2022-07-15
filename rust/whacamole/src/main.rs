/*  https://open.kattis.com/problems/whacamole

    Passes the sample tests and my local tests but fails on the second (of 2) server tests
*/

fn main() {
    let     stdin = stdin();    
    let mut lines = stdin.lock().lines();

    loop {
        let (n,d,m) = parse_3_ints(&mut lines);

        if n == 0 { break }

        let mut moles = vec![];
        for _ in 0..m {
            let mole = parse_mole(&mut lines);
            moles.push(mole);
        }

        println!("{}", do_case(n as usize, d, moles));
    }
}


const MAX_SIZE : usize = 20;  // max side length of the whacamole board
const MAX_TIME : usize = 10;  // no more than 10 time steps

fn do_case(n: usize,
           d: i32,
           moles: Vec<Mole>) -> u32
{
    // cache the integer coordinates that are covered by segments of length d from the origin
    let segments = precompute_segments(d);

    // 3-dimensional dp table storing max moles at each (x,y) coordinate by each timestep (t)
    let mut table = [[[0u32; MAX_SIZE]; MAX_SIZE]; MAX_TIME+1];

    let last_t = moles.iter()
                      .map(|mole| mole.t)
                      .max().unwrap();

    // base case is t=0, with no moles the most moles we can whack is obviously 0
    // the array is already initialized with all 0s so the base case is done

    // general case
    for t in 1..=last_t {

        // prepare the (x,y) grid of moles popping up at this time step
        let mut board = [[false; MAX_SIZE]; MAX_SIZE];

        for mole in moles.iter().filter(|mole| mole.t == t) {
            board[mole.x][mole.y] = true;
        }

        // the recursive case, each cell represents the maximum number of moles possible
        // ending at that coordinate at this time step
        for x in 0..n {
        for y in 0..n {
            let mut most_moles : u32 = 0;

            // this could be cached on x/y/n as a key.  and it is stored as a HashMap which is
            // less efficient than a 2D array
            let neighbourhood = shift_segments(&segments, x, y, n);
            
            for (from, points) in neighbourhood.iter() {
                let moles_hit = points.iter()
                                      .filter(|p| board[p.x as usize][p.y as usize])
                                      .count() as u32;

                let prior_most = table[t-1][from.x as usize][from.y as usize];
                let candidate = prior_most + moles_hit;
                
                if candidate > most_moles {
                    most_moles = candidate;
                }
            }

            table[t][x][y] = most_moles;
        }
        }
    }    

    let mut most = 0;

    // return the largest value at the last time step
    for x in 0..n {
    for y in 0..n {
        most = most.max(table[last_t][x][y]);
    }
    }    

    most                 
}

// the pre-computed segment map is centered at the origin, so shift it onto the
// 0-based dp table (no negative indices) while removing out-of-range starting points
fn shift_segments(segments: &HashMap<Point, Vec<Point>>,
                  x: usize,
                  y: usize,
                  n: usize) -> Vec<(Point, Vec<Point>)>
{
    let x = x as i32;
    let y = y as i32;
    let n = n as i32;

    let mut list = vec![];

    for (from, points) in segments {

        let shifted_x = x + from.x;
        let shifted_y = y + from.y;

        // make sure the starting point is on the board
        if   shifted_x >= 0 && shifted_x < n
          && shifted_y >= 0 && shifted_y < n
        {
            let shifted = points.iter()
                                .map(|p| Point::new(p.x + x, p.y + y))
                                .collect();

            list.push((Point::new(shifted_x, shifted_y), shifted));
        }    
    }

    list
}


#[derive(Clone, Copy, Eq, Hash, PartialEq)]
struct Point {
    x : i32,
    y : i32
}

impl Point {
    fn new(x: i32, y: i32) -> Self {
        Point { x, y }
    }
}

// integer points around the origin within radius d
fn points_within(d: i32) -> Vec<Point> {

    assert!(d >= 1);

    let d    = d as i32;
    let from = -d;
    let to   = d;
    let dd   = d*d;

    let mut points = vec![];

    for x in from..=to {
    for y in from..=to {
        if x*x + y*y > dd { continue }        
        points.push(Point::new(x, y));
    }
    }

    points
}

// get all the gcd calculations and divisions out of the way ahead of time so the core loop is
// just additions and comparisons
fn precompute_segments(d: i32) -> HashMap<Point, Vec<Point>> {
    
    let mut map = HashMap::new();
    
    // for a point up to d steps away from the origin, consider the segment from it to the origin
    for point in points_within(d) {

        let rise = point.y;
        let run  = point.x;

        if rise == 0 && run == 0 {
            map.insert(point, vec![point.clone()]);
            continue
        }

        let divisor = gcd(rise.abs(), run.abs());
        
        // dy be negative if rise is negative, same for dx and run
        let dy = rise / divisor;
        let dx = run / divisor;

        let mut x = 0;
        let mut y = 0;
        let mut points = vec![];

        while x != point.x || y != point.y {
            //assert!(!points.contains(&Point::new(x, y)));

            points.push(Point::new(x, y));
            x += dx;
            y += dy;
        }

        // don't forget the point itself
        //assert!(!points.contains(&point));
        points.push(point.clone());

        map.insert(point, points);
    }

    map
}

struct Mole {
    x : usize,
    y : usize,
    t : usize,
}


/* Math */

// this is the algorithm from https://en.wikipedia.org/wiki/Binary_GCD_algorithm#Algorithm
fn gcd(u: i32, v: i32) -> i32 {
    
    assert!(u >= 0);
    assert!(v >= 0);
    assert!(u > 0 || v > 0);

    if u == 0 { return v }
    if v == 0 { return u }

    if u % 2 == 0 && v % 2 == 0 { return 2 * gcd(u / 2, v / 2) }
    if u % 2 == 0               { return     gcd(u / 2, v    ) }
    if               v % 2 == 0 { return     gcd(u    , v / 2) }
    
    gcd((u - v).abs(), u.min(v))
}


/* Parsing */

use std::io::{BufRead, Lines, StdinLock, stdin};
use std::collections::HashMap;

fn parse_3_ints(lines: &mut Lines<StdinLock>) -> (i32, i32, i32) {
    let nums : Vec<i32> = lines.next().unwrap().unwrap()
                               .split_whitespace()
                               .map(|num| num.parse().unwrap())
                               .collect();

    (nums[0], nums[1], nums[2])
}

fn parse_mole(lines: &mut Lines<StdinLock>) -> Mole {
    let (x,y,t) = parse_3_ints(lines);

    Mole {
        x: x as usize,
        y: y as usize,
        t: t as usize
    }
}


/* Tests */

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_sample_1_1() {
        let moles = vec![
            Mole {x: 0, y: 0, t: 1},
            Mole {x: 3, y: 1, t: 3},
            Mole {x: 0, y: 1, t: 2},
            Mole {x: 0, y: 2, t: 2},
            Mole {x: 1, y: 0, t: 2},
            Mole {x: 2, y: 0, t: 2},
        ];

         assert_eq!(4, do_case(4, 2, moles));
    }

    #[test]
    fn test_sample_1_2() {
        let moles = vec![
            Mole {x: 0, y: 0, t: 1},
            Mole {x: 1, y: 2, t: 1},
            Mole {x: 2, y: 4, t: 1},
        ];

        assert_eq!(2, do_case(5, 4, moles));
    }

    #[test]
    fn test_gcd() {
        assert_eq!(1, gcd(3, 5));        
        assert_eq!(2, gcd(6, 10));
        assert_eq!(12, gcd(36, 24));
        
        // compare our gcd with the one from num::integer (not available on the kattis servers but
        // these tests are only run locally)
        for x in 1..50 {
        for y in 1..50 {
            assert_eq!(gcd(x, y), num::integer::gcd(x, y))
        }
        }
    }

    #[test]
    fn test_precompute_segments() {
        let segments = precompute_segments(1);

        assert_eq!(5 , precompute_segments(1).len());
        assert_eq!(true, segments.contains_key(&Point::new(-1, 0)));
        assert_eq!(true, segments.contains_key(&Point::new(0, -1)));
        assert_eq!(true, segments.contains_key(&Point::new(0, 0)));
        assert_eq!(true, segments.contains_key(&Point::new(0, 1)));
        assert_eq!(true, segments.contains_key(&Point::new(1, 0)));

        assert_eq!(2,    segments[&Point::new(-1, 0)].len());
        assert_eq!(true, segments[&Point::new(-1, 0)].contains(&Point::new(-1, 0)));
        assert_eq!(true, segments[&Point::new(-1, 0)].contains(&Point::new(0 , 0)));

        assert_eq!(1,    segments[&Point::new(0, 0)].len());
        assert_eq!(true, segments[&Point::new(0, 0)].contains(&Point::new(0 , 0)));
        
        assert_eq!(13, precompute_segments(2).len());
    }

    #[test]
    fn test_shift_segments() {
        assert_eq!(1, shift_segments(&precompute_segments(1), 0, 0, 1).len());
        assert_eq!(0, shift_segments(&precompute_segments(1), 1, 1, 1).len());
        assert_eq!(4, shift_segments(&precompute_segments(2), 0, 0, 2).len());
        assert_eq!(4, shift_segments(&precompute_segments(2), 1, 1, 2).len());
    }
}
