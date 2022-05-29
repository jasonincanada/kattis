---
name: Add ''Em Up!
slug: addemup
url: https://open.kattis.com/problems/addemup
difficulty: 4.9
---

# Add 'Em Up!

## Brute Force

My first attempt at this challenge was the obvious brute force $O(n^2)$ solution, and it actually managed to sneak into 10th place on the Rust leaderboard, at 0.12s runtime:

```rust
fn do_case(input: Input) -> String {

    // we need to retain the original identity of the card because we're not allowed to use
    // the same card twice. sometimes the sum we're looking for will be a card plus its own
    // rotation, which is not allowed because it's not using 2 different cards. so we need
    // to remember the card's original identity, which we'll track simply as its list index
    
    // allocate a BinaryHeap with enough capacity to store the input's original numbers
    // and any rotated ones we were able to derive from them (up to all of them)
    let mut heap : BinaryHeap<(i32,usize)> = BinaryHeap::with_capacity(input.numbers.len() * 2);

    for (i, number) in input.numbers.iter().enumerate() {
        heap.push((*number, i));

        if let Some(r) = rotate(*number) {

            // keeping the i around pairs up this rotated number with its original so we
            // don't inadvertently sum using the same card twice
            heap.push((r, i));
        }
    }

    let vec = heap.into_sorted_vec();

    for (num1, i1) in &vec {
    for (num2, i2) in &vec {

        if *num1 + *num2 == input.sum {

            // the problem says 2 cards so make sure this isn't the same one twice
            if i1 == i2 {
                continue
            }

            return String::from("YES")
        }
    }
    }

    String::from("NO")
}
```
