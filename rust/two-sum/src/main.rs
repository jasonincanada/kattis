/*  https://leetcode.com/problems/two-sum/

    The generalized binary search function `search` is ported from the Haskell on Brent Yorgey's blog:
    [1] https://byorgey.wordpress.com/2023/01/01/competitive-programming-in-haskell-better-binary-search/
*/

fn main() {    
    println!("run `cargo test` instead");
}

impl Solution {
    // quadratic solution
    pub fn two_sum_quadratic(nums: Vec<i32>, target: i32) -> Vec<i32> {
        for (i, num1) in nums.iter().enumerate() {
        for (j, num2) in nums.iter().enumerate() {
            if i == j { continue }

            if num1 + num2 == target {
                return vec![i as i32, j as i32];
            }
        }}

        return vec![]
    }

    // logarithmic solution
    pub fn two_sum(nums: Vec<i32>, target: i32) -> Vec<i32>
    {
        let mut indexed: Vec<(i32, usize)> =
            nums.into_iter()
                .enumerate()
                .map(|(i, num)| (num, i))
                .collect();

        indexed.sort();

        for (num, i) in indexed.iter() {
            let difference = target - num;

            let predicate = |j: &i32| indexed[*j as usize].0 >= difference;
            let pair = search(binary,
                              predicate,
                              -1,
                              indexed.len() as i32);

            let j = pair.1 as usize;

            // didn't find the target number
            if j == indexed.len()         { continue }
            if indexed[j].0 != difference { continue }

            // if it's the same index, the very next element must be the target number
            // because the list is sorted
            if indexed[j].1 == *i {
                return vec![*i as i32, indexed[j+1].1 as i32]
            } else {
                return vec![*i as i32, indexed[j  ].1 as i32]
            }
        }

        panic!("shouldn't get here, problem assures us")
    }
}

struct Solution {}


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


/* Tests */

#[cfg(test)]
mod tests {
    use super::*;

    #[test] fn test_case_quadratic() {
        assert_eq!(Solution::two_sum_quadratic(vec![2,7,11,15], 9), vec![0,1]);
        assert_eq!(Solution::two_sum_quadratic(vec![3,2,4]    , 6), vec![1,2]);
        assert_eq!(Solution::two_sum_quadratic(vec![3,3]      , 6), vec![0,1]);
        assert_eq!(Solution::two_sum_quadratic(vec![3,2,3]    , 6), vec![0,2]); // test 7
        assert_eq!(Solution::two_sum_quadratic(vec![2,5,5,11] ,10), vec![1,2]); // test 10
        assert_eq!(Solution::two_sum_quadratic(vec![-3,4,3,90], 0), vec![0,2]); // test 12
    }

    #[test] fn test_case_logarithmic() {
        assert_eq!(Solution::two_sum(vec![2,7,11,15], 9), vec![0,1]);
        assert_eq!(Solution::two_sum(vec![3,2,4]    , 6), vec![1,2]);
        assert_eq!(Solution::two_sum(vec![3,3]      , 6), vec![0,1]);
        assert_eq!(Solution::two_sum(vec![3,2,3]    , 6), vec![0,2]); // test 7
        assert_eq!(Solution::two_sum(vec![2,5,5,11] ,10), vec![1,2]); // test 10
        assert_eq!(Solution::two_sum(vec![-3,4,3,90], 0), vec![0,2]); // test 12
    }
}
