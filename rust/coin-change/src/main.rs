/*  https://leetcode.com/problems/coin-change/  */

fn main() {
    println!("coin_change([1,2,5], 11): {}", Solution::coin_change(vec![1,2,5], 11));    
}

#[derive(Clone, Copy, Eq, Ord, PartialEq, PartialOrd)]
enum Counting {
    Count(i32),
    Infinity
}

// override the + operator for the Counting enum to account for the Infinity variant
impl std::ops::Add for Counting {
    type Output = Self;

    fn add(self, rhs: Self) -> Self::Output {
        match (self, rhs) {
            (Count(left), Count(right)) => Count(left+right),

            // the others involve infinity, adding anything to it is still infinity
            _                           => Infinity
        }
    }
}

impl Solution {
    pub fn coin_change(coins : Vec<i32>,
                       amount: i32) -> i32
    {
        let coin_count = coins.len();
        let mut table: Vec<Vec<Counting>> = vec![ vec![Infinity; amount as usize +1]; coins.len()];

        for (c, coin) in coins.into_iter().enumerate() {

            // consider only the first coin denomination in the list
            if c == 0 {
                for (a, amount) in (0..=amount).enumerate() {
                    
                    // making amount 0 takes 0 coins
                    if amount == 0 {
                        table[c][a] = Count(0);
                        continue
                    }

                    if amount % coin == 0 {
                        table[c][a] = Count(amount / coin);
                    }
                }
            }

            // c > 0
            else {
                for (a, amount) in (0..=amount).enumerate() {

                    // making amount 0 takes 0 of this coin denomination
                    if amount == 0 {
                        table[c][a] = Count(0);
                        continue
                    }

                    let mut candidates: Vec<Counting> = vec![];

                    // the general step, where the bulk of the computation takes place.
                    // consider taking more and more of this coin denomination
                    for (k, a) in candidate_indexes(coin, amount as usize) {
                        let coin_count = table[c-1][a] + Count(k as i32);
                        candidates.push(coin_count);
                    }

                    let optimal = candidates.into_iter()
                                            .min().unwrap();

                    table[c][a] = optimal;
                }
            }
        }

        match table[coin_count-1][amount as usize] {
            Count(count) => count as i32,
            Infinity     => -1
        }
    }
}

fn candidate_indexes(coin  : i32,
                     amount: usize) -> impl Iterator<Item=(usize,usize)>
{
    let max_coins = amount / coin as usize;

    (0..=max_coins)
        .map(move |k| (k, amount - k*coin as usize))
}

use Counting::*;

struct Solution {}


/* Tests */

#[cfg(test)]
mod tests {
    use super::*;

    #[test] fn test_example1() { assert_eq!(Solution::coin_change(vec![1,2,5], 11), 3); }
    #[test] fn test_example2() { assert_eq!(Solution::coin_change(vec![2], 3), -1); }
    #[test] fn test_example3() { assert_eq!(Solution::coin_change(vec![1], 0), 0); }
    #[test] fn test_case_33()  { assert_eq!(Solution::coin_change(vec![2,5,10,1], 27), 4); }

    #[test]
    fn test_counting_ord() {
        assert!(Counting::Count(1) == Counting::Count(1));
        assert!(Counting::Count(1)  < Counting::Count(2));

        // everything is less than Infinity
        assert!(Counting::Count(1)        < Counting::Infinity);
        assert!(Counting::Count(i32::MAX) < Counting::Infinity);
    }

    #[test]
    fn test_counting_add() {
        assert!(Counting::Count(2) + Counting::Count(3) == Counting::Count(5)); // 2 + 3 = 5
        assert!(Counting::Count(2) + Counting::Infinity == Counting::Infinity); // 2 + inf = inf
        assert!(Counting::Infinity + Counting::Infinity == Counting::Infinity); // inf + inf = inf
    }
}
