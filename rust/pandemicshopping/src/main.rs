/*  https://open.kattis.com/problems/pandemicshopping  */

use std::collections::HashMap;

fn main() {
    let mall = get_input_from_stdin();
    println!("{}", mall.count_floor_plans());
}

struct Mall {
    // the directions of Aisle A and B if they were specified in the input
    aisle_a : Option<Direction>,
    aisle_b : Option<Direction>,

    // count of east-west product aisles including the top and bottom ones
    n : usize,

    // east-west product aisles that were specified in the input
    product_aisles : HashMap<usize, Direction>
}

type Direction = String;

enum Aisle {
    A,
    B,
    Product(usize)
}

impl Mall {

    fn count_floor_plans(&self) -> u32 {
        let mut count = 0;
        
        // try the 4 permutations of the side aisles
        let directions = vec!["S2N", "N2S"];

        for aisle_a in directions.iter() {
            // try this direction for Aisle A only if no direction was specified in the
            // input for it, or it was and it matches this direction. skip it otherwise
            if self.have_aisle_and_not_equal_to(Aisle::A, aisle_a) { continue }

            for aisle_b in directions.iter() {
                // same reasoning for trying or not trying this direction for Aisle B
                if self.have_aisle_and_not_equal_to(Aisle::B, aisle_b) { continue }
                
                // we have our pair of A and B aisle directions, now count valid floor plans
                // assuming the two side aisles were to be set as these
                count += self.count_with(aisle_a, aisle_b);
            }
        }

        count
    }

    // count valid floor plans if Aisles A and B were to be aisle_a/aisle_b
    fn count_with(&self,
                  aisle_a: &str,
                  aisle_b: &str) -> u32 {
        
        // the top and bottom product aisles are determined by the side aisles. if the
        // wrong direction has been specified, there can't be any valid floor plans
        if aisle_a == "S2N" {
            if self.have_aisle_and_not_equal_to(Aisle::Product(self.n), "W2E") { return 0 }
        } else {
            if self.have_aisle_and_not_equal_to(Aisle::Product(self.n), "E2W") { return 0 }
        }

        // same deal for Aisle B
        if aisle_b == "N2S" {
            if self.have_aisle_and_not_equal_to(Aisle::Product(1), "E2W") { return 0 }
        } else {
            if self.have_aisle_and_not_equal_to(Aisle::Product(1), "W2E") { return 0 }
        }

        // if the side aisles are the same direction, there can be only one valid floor plan:
        // the number of product aisles must be odd and the customer must zig-zag east/west as
        // they travel up the store (or down if the side aisles are pointed N2S)
        if aisle_a == "S2N" && aisle_b == "S2N" {

            if is_even(self.n) { return 0 }
            
            // product aisles must zig zag all the way up
            for (id, dir) in self.product_aisles.iter() {
                if  is_even(*id) && dir != "E2W" { return 0 }
                if !is_even(*id) && dir != "W2E" { return 0 }
            }

            // all specified product aisles are copacetic, and the unspecified ones are
            // uniquely determined by the zig zag pattern. so we can count 1 valid floor plan
            1

        } else if aisle_a == "N2S" && aisle_b == "N2S" {

            if is_even(self.n) { return 0 }
            
            // product aisles must zig zag all the way down
            for (id, dir) in self.product_aisles.iter() {
                if  is_even(*id) && dir != "W2E" { return 0 }
                if !is_even(*id) && dir != "E2W" { return 0 }
            }

            // same reasoning as for S2N aisles, we can count this as 1 valid floor plan
            1

        } else {

            // the side aisles are pointing in opposite directions. with the top and bottom
            // product aisles already checked, the interior aisle directions don't matter because
            // the shopper can loop around the perimeter as much as they want to get to the start
            // of any aisle. so we only have to count the number of unspecified interior aisles
            // and consider each one as 2 possible directions
            let unspec = self.n
                            - 2
                            - self.product_aisles.iter()
                                                 .filter(|(&k, _)| k > 1 && k < self.n)
                                                 .count();
            
            // 2^x
            2_u32.pow(unspec as u32)
        }
    }

    /*  5 4
        A S2N
        B S2N
        5 W2E
        2 E2W
    */
    fn from_input(lines: &[String]) -> Mall {
        
        // parse the first integer from the first line
        let n: usize = lines[0].split_whitespace()
                               .next()
                               .unwrap()
                               .parse::<usize>()
                               .unwrap();

        // the rest of the lines can be in any order
        let mut aisle_a : Option<Direction> = None;
        let mut aisle_b : Option<Direction> = None;
        let mut product_aisles : HashMap<usize, Direction> = HashMap::new();

        for line in &lines[1..] {
            let words : Vec<&str> = line.split_whitespace().collect();
            let direction = words[1].to_string();

            match words[0] {
                "A" => { aisle_a = Some(direction) },
                "B" => { aisle_b = Some(direction) },
                id  => {
                    let aisle = id.parse::<usize>().unwrap();                    
                    product_aisles.insert(aisle, direction);
                }
            }
        }
                     
        Mall {
            n,
            aisle_a,
            aisle_b,
            product_aisles
        }        
    }

    // do we have this aisle specified and it's not equal to the passed string
    fn have_aisle_and_not_equal_to(&self, aisle: Aisle, compare: &str) -> bool {

        match aisle {
            Aisle::A => if let Some(dir) = self.aisle_a.as_ref() {
                            if dir != compare { return true }
                        },

            Aisle::B => if let Some(dir) = self.aisle_b.as_ref() {
                            if dir != compare { return true }
                        },

            Aisle::Product(n) =>
                        if let Some(dir) = self.product_aisles.get(&n) {
                            if dir != compare { return true }
                        }
        }        
    
        false
    }
}

fn is_even(n: usize) -> bool {
    n % 2 == 0
}


/* Input */

fn get_input_from_stdin() -> Mall {
    use std::io::BufRead;

    let mut ls : Vec<String> = vec![];
    let stdin = std::io::stdin();
    let lines = stdin.lock().lines(); 
    
    for line in lines {
        ls.push(line.unwrap());
    }
    
    Mall::from_input(&ls)
}


/* Tests */

#[cfg(test)]
mod tests {
    use super::*;

    // this is sample input 1 from the problem description. both Aisle A and Aisle B are
    // specified and the number of east-west aisles is odd (5)
    #[test]
    fn test_both_specified_and_same_n_odd() {

        let lines = vec!["5 4".to_string(),
                         "A S2N".to_string(),
                         "B S2N".to_string(),
                         "5 W2E".to_string(),
                         "2 E2W".to_string() ];

        let mall = Mall::from_input(&lines);

        assert_eq!(mall.count_floor_plans(), 1);
    }

    #[test]
    fn test_minimal_both_s2n() {

        let lines = vec!["3 0".to_string(),
                         "A S2N".to_string(),
                         "B S2N".to_string() ];

        let mall = Mall::from_input(&lines);

        assert_eq!(mall.count_floor_plans(), 1);
    }

    #[test]
    fn test_minimal_both_s2n_bad_middle() {

        let lines = vec!["3 1".to_string(),
                         "A S2N".to_string(),
                         "B S2N".to_string(),
                         "2 W2E".to_string() ];

        let mall = Mall::from_input(&lines);

        assert_eq!(mall.count_floor_plans(), 0);
    }

    #[test]
    fn test_minimal_both_s2n_good_middle() {

        let lines = vec!["3 1".to_string(),
                         "A S2N".to_string(),
                         "B S2N".to_string(),
                         "2 E2W".to_string() ];

        let mall = Mall::from_input(&lines);

        assert_eq!(mall.count_floor_plans(), 1);
    }

    #[test]
    fn test_minimal_both_s2n_bad_top() {

        let lines = vec!["3 1".to_string(),
                         "A S2N".to_string(),
                         "B S2N".to_string(),
                         "3 E2W".to_string() ];

        let mall = Mall::from_input(&lines);

        assert_eq!(mall.count_floor_plans(), 0);
    }

    #[test]
    fn test_minimal_both_s2n_good_top() {

        let lines = vec!["3 1".to_string(),
                         "A S2N".to_string(),
                         "B S2N".to_string(),
                         "3 W2E".to_string() ];

        let mall = Mall::from_input(&lines);

        assert_eq!(mall.count_floor_plans(), 1);
    }

    #[test]
    fn test_minimal_both_n2s() {

        let lines = vec!["3 0".to_string(),
                         "A N2S".to_string(),
                         "B N2S".to_string() ];

        let mall = Mall::from_input(&lines);

        assert_eq!(mall.count_floor_plans(), 1);
    }

    #[test]
    fn test_minimal_both_n2s_bad_middle() {

        let lines = vec!["3 1".to_string(),
                         "A N2S".to_string(),
                         "B N2S".to_string(),
                         "2 E2W".to_string() ];

        let mall = Mall::from_input(&lines);

        assert_eq!(mall.count_floor_plans(), 0);
    }

    #[test]
    fn test_minimal_both_n2s_good_middle() {

        let lines = vec!["3 1".to_string(),
                         "A N2S".to_string(),
                         "B N2S".to_string(),
                         "2 W2E".to_string() ];

        let mall = Mall::from_input(&lines);

        assert_eq!(mall.count_floor_plans(), 1);
    }
    
    #[test]
    fn test_different_3_aisles_none_specified() {

        let lines = vec!["3 0".to_string(),
                         "A S2N".to_string(),
                         "B N2S".to_string() ];

        let mall = Mall::from_input(&lines);

        assert_eq!(mall.count_floor_plans(), 2);
    }
    
    #[test]
    fn test_different_4_aisles_none_specified() {

        let lines = vec!["4 0".to_string(),
                         "A S2N".to_string(),
                         "B N2S".to_string() ];

        let mall = Mall::from_input(&lines);

        assert_eq!(mall.count_floor_plans(), 4);
    }

    #[test]
    fn test_different_4_aisles_1_specified() {

        let lines = vec!["4 1".to_string(),
                         "A S2N".to_string(),
                         "B N2S".to_string(),
                         "2 E2W".to_string() ];

        let mall = Mall::from_input(&lines);

        assert_eq!(mall.count_floor_plans(), 2);
    }

    #[test]
    fn test_different_4_aisles_both_specified() {

        let lines = vec!["4 2".to_string(),
                         "A S2N".to_string(),
                         "B N2S".to_string(),
                         "2 E2W".to_string(),
                         "3 E2W".to_string() ];

        let mall = Mall::from_input(&lines);

        assert_eq!(mall.count_floor_plans(), 1);
    }

    #[test]
    fn test_different_4_aisles_top_bottom_specified() {

        let lines = vec!["4 2".to_string(),
                         "A S2N".to_string(),
                         "B N2S".to_string(),
                         "1 E2W".to_string(),
                         "4 W2E".to_string() ];

        let mall = Mall::from_input(&lines);

        assert_eq!(mall.count_floor_plans(), 4);
    }

    #[test]
    fn test_different_2_aisles() {

        let lines = vec!["2 2".to_string(),
                         "A S2N".to_string(),
                         "B N2S".to_string() ];

        let mall = Mall::from_input(&lines);

        assert_eq!(mall.count_floor_plans(), 1);
    }

    #[test]
    fn test_2_aisles() {
        let lines = vec!["2 0".to_string() ];
        let mall = Mall::from_input(&lines);

        assert_eq!(mall.count_floor_plans(), 2);
    }
    
    #[test]
    fn test_3_aisles() {
        let lines = vec!["3 0".to_string() ];
        let mall = Mall::from_input(&lines);

        assert_eq!(mall.count_floor_plans(), 6);
    }

    #[test]
    fn test_3_aisles_1_side() {
        let lines = vec!["3 1".to_string(),
                         "A S2N".to_string() ];
        let mall = Mall::from_input(&lines);

        assert_eq!(mall.count_floor_plans(), 3);
    }

    #[test]
    fn test_4_aisles() {
        let lines = vec!["4 0".to_string() ];
        let mall = Mall::from_input(&lines);

        assert_eq!(mall.count_floor_plans(), 8);
    }

    ////////

    #[test]
    fn test_parse() {
        let lines = vec!["5 4".to_string(),
                         "A S2N".to_string(),
                         "B S2N".to_string(),
                         "5 W2E".to_string(),
                         "2 E2W".to_string() ];

        let mall : Mall = Mall::from_input(&lines);

        assert_eq!(mall.n, 5);
        assert_eq!(mall.aisle_a.as_ref().unwrap(), "S2N");
        assert_eq!(mall.aisle_b.as_ref().unwrap(), "S2N");
        assert_eq!(mall.product_aisles.get(&2).unwrap(), "E2W");
        assert_eq!(mall.product_aisles.get(&5).unwrap(), "W2E");
        assert_eq!(mall.product_aisles.len(), 2);
    }

    #[test]
    fn test_is_even() {
        assert!(is_even(0));
        assert!(is_even(2));
        assert!(is_even(4));        
        assert!(! is_even(1));
        assert!(! is_even(3));
    }
}
