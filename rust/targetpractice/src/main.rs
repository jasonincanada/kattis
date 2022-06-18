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

    let ab = Line::new(a, b);
    
    // find the first point not collinear with the line AB, to form a triangle ABC
    let c = points.iter()
                  .skip(2)
                  .find(|c| !is_collinear(&ab, c));

    if let Some(c) = c {

        let ac = Line::new(a, c);
        let bc = Line::new(b, c);

        // find a point that is collinear with one of the sides of the triangle
        let red = points.iter()
                        .skip(2)
                        .find(|&p| p != c && ( is_collinear(&ab, p)
                                            || is_collinear(&ac, p)
                                            || is_collinear(&bc, p)));

        if let Some(red) = red {

            // figure out which side that was, and the third vertex of the triangle            
            let side   : &Line;
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
                             .find(|&p| p != vertex && !is_collinear(side, p));

            if let Some(blue) = blue {
                let line = Line::new(blue, vertex);

                // at this point, if this is a successful test case, then every point must lie on
                // the line MN or the line from the blue point to the triangle vertex. try to find
                // a point not on either of these two lines
                let rogue = points.iter()
                                  .skip(2)
                                  .find(|&p| !is_collinear(side, p)
                                          && !is_collinear(&line, p));

                if rogue.is_none() {
                    Result::Success
                } else {
                    Result::Failure
                }
            } else {
                Result::Success
            }
        }
        
        else {

            // there are no points collinear with any side of the triangle. it is still a success case
            // if all the other points are in one line containing one of the triangle's vertices
            let d = points.iter()
                          .skip(2)
                          .find(|&p| p != c)
                          .unwrap();

            for vertex in vec![a, b, c].iter() {
                let line = Line::new(vertex, d);

                // if all remaining points are collinear with this line it's a success case
                if points.iter()
                         .skip(2)
                         .all(|p| is_collinear(&line, p))
                {
                    return Result::Success
                }
            }
            
            Result::Failure
        }
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

struct Line<'a> {
    q:     &'a Point,
    slope: Slope,
}

// a line can be described either with two points or with one point and a slope
impl<'a> Line<'a> {
    fn new(p: &Point, q: &'a Point) -> Self {
        Line {
            q,
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

// this is the slope formula method from: https://www.vedantu.com/maths/collinear-points
fn is_collinear(line: &Line,
                p:    &Point) -> bool
{    
    line.slope == slope(line.q, p)
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

        assert_eq!(true,  is_collinear(&Line::new(&p00, &p11), &p22));
        assert_eq!(false, is_collinear(&Line::new(&p00, &p11), &p23));
    }

    #[test]
    fn test_cube_with_5th_point()
    {
        let points = vec![
            // line 1
            Point { x: 0, y: 0 },
            Point { x: 1, y: 0 },

            // line 2
            Point { x: 0, y: 1 },
            Point { x: 1, y: 1 },
            Point { x: 2, y: 1 },
        ];

        // this test now passes when the code doesn't find a point collinear to triangle MNO
        assert_eq!(Result::Success, do_case(points));
    }

    #[test]
    fn test_cube()
    {
        let points = vec![
            // line 1
            Point { x: 0, y: 0 },
            Point { x: 1, y: 0 },

            // line 2
            Point { x: 0, y: 1 },
            Point { x: 1, y: 1 },
        ];
        
        assert_eq!(Result::Success, do_case(points));
    }
}
