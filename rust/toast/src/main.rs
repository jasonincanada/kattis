/*  https://open.kattis.com/problems/toast

    The generalized binary search function `search` is ported from the Haskell on Brent Yorgey's blog:
    [1] https://byorgey.wordpress.com/2023/01/01/competitive-programming-in-haskell-better-binary-search/
*/

fn main() {
    let (n, d, t) = get_3_numbers_from_stdin();
    let answer    = toast(n, d as f32, t);

    println!("{} {}", answer.0, answer.1);
}

type Radius = f32;
type Clinks = u32;

fn toast(people    : u32,
         arm_length: Radius,
         clinks    : Clinks) -> (Radius, Radius)
{
    assert!(people >= 2);
    assert!(arm_length > 0.0);
    assert!(clinks > 0);
    assert!(clinks >= people / 2);
    assert!(clinks <= people * (people-1) / 2);

    let mut tiers: Vec<(Clinks, Radius)> = vec![];
    let mut total_clinks = 0;

    // as the table radius shrinks, more and more people become within reach of each other.
    // this occurs as the angle sweeps around the table from an arbitrary person
    for angle in angles_for(people) {
        let radius = get_radius_for(angle);

        // the radius is small enough now for the next set of "clinks" to occur simultaneously
        total_clinks += people;
        tiers.push((total_clinks, radius));
    }

    // for an even number of people, don't forget the last "clink set" where
    // everyone can finally reach the person at the opposite end of the table
    if is_even(people) {
        total_clinks += people / 2;
        tiers.push((total_clinks, 1.0));
    }

    tiers.push((total_clinks, 0.0));

    // find where we have exactly the number of clinks we're looking for. this can
    // fail if the input is malformed but the problem says there's always a radius
    let (from, to) = get_radius_range(&tiers, clinks);

    // we were working with an arm length of 1, scale it up to its actual length
    (from * arm_length, to * arm_length)
}

// binary search on the number of clinks and return that index and the next's radiuses.
// this forms the range of table radiuses that hear a certain number of clinks
fn get_radius_range(tiers : &[(Clinks, Radius)],
                    clinks: u32) -> (Radius, Radius)
{
    let predicate = |i: &i32| tiers[*i as usize].0 >= clinks;

    let index_pair = search(binary,
                            predicate,
                            -1,
                            tiers.len() as i32);

    let from = tiers[index_pair.1 as usize+1].1;
    let to   = tiers[index_pair.1 as usize  ].1;

    (from, to)
}

fn is_even(people: u32) -> bool {
    people % 2 == 0
}


/* AngleIterator - produce the angles we need to check for n people sitting around a table */

fn angles_for(people: u32) -> AngleIterator {
    if people < 2 {
        panic!("AngleIterator: expected people >= 2");
    }

    AngleIterator {
        people,

        // start with our immediate neighbour
        i: 1
    }
}

struct AngleIterator {
    people: u32,
    i     : u32
}

impl Iterator for AngleIterator {
    type Item = f32;

    // for n = 5 we need to check i=[1,2]
    // for n = 6 we need to check i=[1,2]
    // for n = 7 we need to check i=[1,2,3]
    fn next(&mut self) -> Option<Self::Item>
    {
        if self.i > (self.people-1)/2 {
            return None
        }

        // θ = 2πi/n
        let angle = 2.0 * std::f32::consts::PI
                        / self.people as f32
                        * self.i as f32;
        self.i += 1;
        Some(angle)
    }
}

fn get_radius_for(angle: f32) -> Radius {
    1.0 / (angle / 2.0).sin()
}


/* Binary Search */

// generic search function ported from Yorgey's blog [1]. notice no constraints on type A!
// if there are any, they're applied by the particular mid function we choose
fn search<F,P,A>(mid  : F,
                 pred : P,
                 left : A,
                 right: A) -> (A, A)
where
    F: Fn(&A, &A) -> Option<A>,
    P: Fn(&A)     -> bool
{
    match mid(&left, &right) {
        Some(m) if pred(&m) => search(mid, pred, left, m),
        Some(m)             => search(mid, pred, m, right),
        None                => (left, right)
    }
}

// custom "mid" function to pass as the first argument to search() when we need a binary search
fn binary(left : &i32,
          right: &i32) -> Option<i32>
{
    if right - left > 1 {
        Some((left+right) / 2)
    } else {
        None
    }
}


/* Parsing */

fn get_3_numbers_from_stdin() -> (u32, u32, u32) {
    use std::io::BufRead;

    let line = std::io::stdin().lock().lines().next().unwrap().unwrap();
    let numbers: Vec<u32> = line.split_whitespace()
                                .map(|num| num.parse().unwrap())
                                .collect();

    assert!(numbers.len() >= 3);

    (numbers[0],
     numbers[1],
     numbers[2])
}


/* Tests */

#[cfg(test)]
mod tests {
    use super::*;
    use std::f32::consts::PI;

    #[test] fn test_toast()
    {
        assert_eq!(toast(2, 1000.0, 1), (0.0, 1000.0));
        assert_eq!(toast(3, 1000.0, 3), (0.0, 1154.7006));
        assert_eq!(toast(4, 1000.0, 4), (1000.0, 1414.2135)); // sample input
        assert_eq!(toast(4, 1000.0, 6), (0.0, 1000.0));

        assert_eq!(toast(5, 1000.0, 5*(5-1)/2), (0.0, 1051.4622));
        assert_eq!(toast(5, 1000.0, 5        ), (1051.4622, 1701.3016));
    }

    #[test]
    #[should_panic]
    fn test_angles_for_1() {
        angles_for(1);
    }

    #[test] fn test_angles_for_2() {
        let mut angles = angles_for(2);
        assert_eq!(angles.next(), None);
    }

    #[test] fn test_angles_for_5() {
        let mut angles = angles_for(5);
        assert_eq!(angles.next(), Some(2.0*PI / 5.0 * 1.0));
        assert_eq!(angles.next(), Some(2.0*PI / 5.0 * 2.0));
        assert_eq!(angles.next(), None);
    }

    #[test] fn test_angles_for_6() {
        let mut angles = angles_for(6);
        assert_eq!(angles.next(), Some(2.0*PI / 6.0 * 1.0));
        assert_eq!(angles.next(), Some(2.0*PI / 6.0 * 2.0));
        assert_eq!(angles.next(), None);
    }
}
