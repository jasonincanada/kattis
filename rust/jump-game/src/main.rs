/*  https://leetcode.com/problems/jump-game/  */

fn main() {}

pub fn can_jump(nums: Vec<i32>) -> bool
{
    let mut table = vec![false; nums.len()];

    // one past the end is always true. this avoids having to bounds-check the jump
    table.push(true);

    for (i, jump) in nums.iter()
                         .enumerate()
                         .rev()
    {
        // we can always arrive at the last position from the last position
        if i == nums.len() - 1 {
            table[i] = true;
            continue
        }

        for j in 1..=*jump as usize {
            if table[i+j] {
                table[i] = true;
                break
            }
        }
    }

    table[0]
}


/* Tests */

#[cfg(test)]
mod tests {
    use super::*;

    #[test] fn test_example_1() { assert_eq!(can_jump(vec![2,3,1,1,4]), true); }
    #[test] fn test_example_2() { assert_eq!(can_jump(vec![3,2,1,0,4]), false); }

    #[test] fn test_case_135() { assert_eq!(can_jump(vec![1,2,3]), true); }
}
