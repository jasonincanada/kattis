/*  https://open.kattis.com/problems/targetpractice  */

use std::io::BufRead;

fn main() {
    let     stdin  = std::io::stdin();
    let mut lines  = stdin.lock().lines(); 

    // skip the first line
    lines.next();

    // gather the points from the rest of the lines
    let mut points : Vec<Point> = vec![];

    for line in lines {
        let point = parse_point(&line.unwrap());
        points.push(point);
    }

    match do_case(points) {
        Result::Success => println!("success"),
        Result::Failure => println!("failure")
    }
}

fn do_case(points: Vec<Point>) -> Result {

    // with 4 or fewer points we can always draw two lines that pass through all points
    if points.len() <= 4 {
        return Result::Success
    }

    // with our algo we can start with any two points so choose the first two
    let a = &points[0];
    let b = &points[1];

    let ab = Side::new(a, b);
    
    // find the first point not collinear with the line AB, to form a triangle ABC
    let c = points.iter()
                  .skip(2)
                  .find(|c| !is_collinear(&ab, c));

    if let Some(c) = c {

        let ac = Side::new(a, c);
        let bc = Side::new(b, c);

        // find a point that is collinear with one of the sides of the triangle
        let red = points.iter()
                        .skip(2)
                        .find(|&p| *p != *c && ( is_collinear(&ab, p)
                                              || is_collinear(&ac, p)
                                              || is_collinear(&bc, p)));

        // figure out which side that was, and the third vertex of the triangle
        if let Some(red) = red {
            
            let side   : &Side;
            let vertex : &Point;

            if is_collinear(&ab, red) {
                side = &ab;
                vertex = c;
            } else if is_collinear(&ac, red) {
                side = &ac;
                vertex = b;
            } else if is_collinear(&bc, red) {
                side = &bc;
                vertex = a;
            } else {
                panic!("shouldn't get here")
            }

            // find a point that is not collinear with our triangle side
            let blue = points.iter()
                             .skip(2)
                             .find(|&p| *p != *vertex && !is_collinear(side, p));

            if let Some(blue) = blue {
                let line = Side::new(blue, vertex);

                // at this point, if this is a successful test case, then every point must lie on
                // the line MN or the line from the blue point to the triangle vertex. try to find
                // a point not on either of these two lines
                let rogue = points.iter()
                                  .skip(2)
                                  .find(|&p| !is_collinear(side, p)
                                          && !is_collinear(&line, p));

                if rogue.is_none() {
                    return Result::Success
                } else {
                    return Result::Failure
                }
            } else {
                return Result::Success
            }
        }

        Result::Failure
    } else {    
        
        // all points were collinear with AB, meaning a single line can pass through all points
        Result::Success
    }
}

// "12 -34" -> (12, -34)
fn parse_point(line: &str) -> Point {
    let mut split = line.split_whitespace();

    let x = split.next().unwrap().parse().unwrap();
    let y = split.next().unwrap().parse().unwrap();

    Point { x, y }
}

#[derive(Clone, PartialEq)]
struct Point {
    x: i32,
    y: i32
}

struct Side {
    p:     Point,
    q:     Point,
    slope: Slope,
}

impl Side {
    fn new(p: &Point, q: &Point) -> Self {
        Side {
            p: (*p).clone(),
            q: (*q).clone(),
            slope: slope(p, q)
        }
    }
}

// store the slope as its rise/run so the equality comparison between two slopes is
// a simple cross-multiplication instead of having to compare floats after division
struct Slope {
    rise: i32,
    run:  i32
}

impl PartialEq for Slope {
    fn eq(&self, other: &Self) -> bool {
        self.rise * other.run == other.rise * self.run
    }
}

fn slope(p: &Point, q: &Point) -> Slope {
    Slope {
        rise: q.y - p.y,
        run:  q.x - p.x
    }
}

fn is_collinear(side: &Side,
                p:    &Point) -> bool
{    
    side.slope == slope(&side.q, p)
}


#[derive(Debug, PartialEq)]
enum Result {
    Failure,
    Success
}


#[cfg(test)]
mod tests {
    use super::*;
   
    #[test]
    fn test_do_case_failure()
    {
        // Sample Input 1
        let points = vec![
            Point { x: -1, y: 0 },
            Point { x:  0, y: 0 },
            Point { x:  1, y: 0 },
            Point { x: -1, y: 1 },
            Point { x:  0, y: 2 },
            Point { x:  1, y: 1 },
        ];

        assert_eq!(Result::Failure, do_case(points));
    }

    #[test]
    fn test_do_case_success()
    {
        // Sample Input 2
        let points = vec![
            Point { x: 1, y:  1 },
            Point { x: 3, y:  5 },
            Point { x: 0, y: -1 },
            Point { x: 1, y:  0 },
            Point { x: 5, y:  0 },
            Point { x: 0, y:  0 },
        ];

        assert_eq!(Result::Success, do_case(points));
    }

    #[test]
    fn test_are_collinear()
    {
        let p00 = Point { x: 0, y: 0 };
        let p11 = Point { x: 1, y: 1 };
        let p22 = Point { x: 2, y: 2 };
        let p23 = Point { x: 2, y: 3 };

        assert_eq!(true,  is_collinear(&Side::new(&p00, &p11), &p22));
        assert_eq!(false, is_collinear(&Side::new(&p00, &p11), &p23));
    }
}
