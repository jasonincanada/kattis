---
name: Short Sell
url: https://open.kattis.com/problems/shortsell
difficulty: 4.2
---

# Short Sell

Been stuck on this one since May 24. It feels like it's just outside my grasp. There's triangles, slopes, and points here but I need an insight to prevent timeouts from the brute force method.  Since $O(n^2)$ doesn't work, it'll need to be $O(log n)$ at the worst, and ideally $O(2n)$ if we can maintain a left index and right index with $right > left$ and they only increase as we traverse the prices

## Brute Force

```rust
// brute-force method, only gets the first 5 tests correct before timing out on the 6th (of 16)
fn do_case_brute_force(problem: Input) -> i32 {
    
    let days = problem.prices.len();

    let mut best_profit = 0;

    for short in 0 .. days-1 {
    for cover in short+1 .. days {
        
        let this_profit = calc_profit(problem.k,
                                      problem.prices[short],
                                      problem.prices[cover],
                                      cover as i32 - short as i32 + 1);

        if this_profit > best_profit {
            best_profit = this_profit;
        }
    }
    }

    best_profit
}
```

If three points A B C are in a line then the profit from A->B is the same as B->C and the same as A->C

if this can be proved then we just need to find the highest slope


## Algorithm Assumptions

The ideal trade starts at a local maximum and ends at a local minimum

If the input was 100 100 100 10 10 10 the best short is from the right-most 100 to the left-most 10. But these in general are not straight lines because of the daily cost of borrowing. Quantify this adjusted slope (adjusted off the horizontal)

Determine when it becomes more profitable to use a more-left value to short and a more-right value to cover in terms of $k$:

Given prices $p_1, p_2, ..., p_n$, if $p_i = p_{i+1}$ it makes more sense to short at index $i+1$ because you save a day's borrowing costs. When borrowing costs for the day outweigh the additional profit, then short at index $i+1$.

More generally, if $p_i \le f(p_i, p_{i+1})$ for some function $f$ we need to define, then go with $p_i$.


## Bounds

- $1 \le p_i \le 100000$
- $1 \le N \le 100000$
- $1 \le K \le 100$
- $Shares = 100$

Note $Shares$ is equal to the maximum possible value of $K$, which is $100$. This lets us simplify the profit equation further in the event $K = 100$:

$$\begin{aligned}
  P &= 100(open - close) - K(i_2 - i_1 + 1)   \\
    &= 100(open - close) - 100(i_2 - i_1 + 1) \\
    &= 100(open - close - (i_2 - i_1 + 1))
  \end{aligned}
$$
