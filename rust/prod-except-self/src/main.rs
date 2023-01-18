/*  https://leetcode.com/problems/product-of-array-except-self/  */

fn main() {    
    println!("run `cargo test` instead");
}

struct Solution {}

impl Solution {
    pub fn product_except_self(nums: Vec<i32>) -> Vec<i32> {
        Solution::product_except_self_linear(nums)
    }

    pub fn product_except_self_linear(nums: Vec<i32>) -> Vec<i32> {
        get_factor_pairs(nums)
            .into_iter()
            .map(|(left, right)| left * right)
            .collect()
    }

    // naive brute-force solution that hides each element one at a time and
    // calculates the product of all other elements
    pub fn product_except_self_quadratic(nums: Vec<i32>) -> Vec<i32> {
        let mut vec: Vec<i32> = vec![0; nums.len()];

        for (index, _) in nums.iter().enumerate() {
            let product_others: i32 =
                nums.iter()
                    .enumerate()
                    .filter(|(i, _)| *i != index)
                    .map(|(_, num)| num)
                    .product();

            vec[index] = product_others;
        }

        vec
    }
}


/* FactorPair Iterator */

fn get_factor_pairs(vec: Vec<i32>) -> FactorPairs {
    let mut forwards  = vec.clone();
    let mut backwards = vec.into_iter().rev().collect::<Vec<i32>>();

    forwards .insert(0, 1); forwards.pop();
    backwards.insert(0, 1); backwards.pop();

    // calculate cummulative products from both sides 
    FactorPairs {
        forwards:  forwards .into_iter().scan(1, |accum, x| { *accum *= x; Some(*accum) }).collect(),
        backwards: backwards.into_iter().scan(1, |accum, x| { *accum *= x; Some(*accum) }).collect(),

        // start the iterator with the leftmost forwards element
        index: 0
    }
}

struct FactorPairs {
    forwards : Vec<i32>,
    backwards: Vec<i32>,
    index    : usize
}

impl Iterator for FactorPairs {
    type Item = (i32, i32);

    fn next(&mut self) -> Option<Self::Item> {
        if self.index == self.forwards.len() {
            return None
        }

        let pair = (self.forwards [self.index                           ],
                    self.backwards[self.backwards.len() - 1 - self.index]);

        self.index += 1;
        Some(pair)
    }
}


/* Tests */

#[cfg(test)]
mod tests {
    use super::*;

    #[test] fn test_case_quadratic() {
        assert_eq!(Solution::product_except_self_quadratic(vec![1,2,3,4]), vec![24,12,8,6]);
        assert_eq!(Solution::product_except_self_quadratic(vec![-1,1,0,-3,3]), vec![0,0,9,0,0]);
    }

    #[test] fn test_case_linear() {
        assert_eq!(Solution::product_except_self_linear(vec![1,2,3,4]), vec![24,12,8,6]);
        assert_eq!(Solution::product_except_self_linear(vec![-1,1,0,-3,3]), vec![0,0,9,0,0]);
    }

    #[test]
    fn test_get_pairs() {
        let mut iter = get_factor_pairs(vec![2,3,4]);

        assert_eq!(iter.next(), Some((1, 12)));
        assert_eq!(iter.next(), Some((2, 4)));
        assert_eq!(iter.next(), Some((6, 1)));
        assert_eq!(iter.next(), None);
    }
}
