---
name: Two Sum
url: https://leetcode.com/problems/two-sum/
difficulty: Easy
---

# Two Sum

This is a LeetCode problem that I found on this list of problems: [Top FAANG Interview Questions From LeetCode](https://docs.google.com/spreadsheets/d/1hzP8j7matoUiJ15N-RhsL5Dmig8_E3aP/)

I might break out these LeetCode problems into their own repo at some point, but for now it makes sense to keep them in the kattis/rust folder and continue adding my write-ups to this notes folder.

## Original Solution

I wrote the obvious $O(n^2)$ solution first, just trying every element from every element:

```rust
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
```

This type of quadratic solution often has nearly identical inner and outer `for` loop structure, which I emphasize by aligning their columns (not indenting the inner loop) to show the commonality.

## Binary Search

The problem asks us if we can find a better time complexity than $O(n^2)$, and indeed we can. The key step is to sort the list, which, as I learned in [this online course](https://www.coursera.org/learn/algorithms-divide-conquer), is one of the first things to consider when solving a problem. At $O(n \log n)$ it's a "for free" primitive, and it does make a lot of things easier. In the case of this problem it lets us use binary search. Isn't it binary search week?

We need to sort the input list by number but keep around the original indices for reporting the final answer, so use `enumerate` to add the index to the list, then `map` to swap the tuple around, since `sort` operates on the first element of the pair first:

```rust
let mut indexed: Vec<(i32, usize)> =
    nums.into_iter()
        .enumerate()
        .map(|(i, num)| (num, i))
        .collect();

indexed.sort();
```

The main loop looks at each element, calculates the difference between it and the target number, and searches for the difference:

```rust
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
```

This has a better time complexity. We sort the list, incurring an $O(n \log n)$ cost, then for each element we calculate the difference between it and the target number (constant time $O(1)$), then do the $O(\log n)$ binary search for the difference. That gives an overall complexity of $O(n \log n + n \log n)$, which we can consider just $O(n \log n$).

Although it works, this code is starting to look a bit ugly. For the first time I'm considering what a binary search DSL (domain-specific language) might look like, and if it would make code like this any easier to understand. The `search` function itself is fairly minimal as it is, but we sometimes (as in this problem) have to check in the calling code whether our indices are still valid, and sometimes pick the next element instead of the one we have the index for, etc. These are going to be common concepts if we're using binary search all the time and the clarity of the code might benefit from using an embedded language to describe them.
