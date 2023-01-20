/*  https://leetcode.com/problems/house-robber/  */

fn main() {
    println!("rob([1,2,3,1]): {}", Solution::rob(vec![1,2,3,1]));
}

impl Solution {
    pub fn rob(loots: Vec<i32>) -> i32
    {
        let houses = loots.len();
        let mut table: Vec<i32> = vec![0; loots.len()];

        for (l, loot) in loots.into_iter().enumerate()
        {
            // base case, always rob the first house
            if l == 0 {
                table[0] = loot;
                continue
            }

            // the first decision, should we rob only the first house or only this second one
            if l == 1 {
                table[1] = max(table[0], loot);
                continue
            }

            // calculate the overall value of robbing this house (skipping the prior one),
            // versus skipping this house and taking the maximum loot up to the prior one
            let rob  = table[l-2] + loot;
            let skip = table[l-1];
            table[l] = max(rob, skip)
        }

        table[houses-1]
    }
}

struct Solution {}

use std::cmp::max;


/* Tests */

#[cfg(test)]
mod tests {
    use super::*;

    #[test] fn test_example1() { assert_eq!(Solution::rob(vec![1,2,3,1]), 4); }
    #[test] fn test_example2() { assert_eq!(Solution::rob(vec![2,7,9,3,1]), 12); }
}
