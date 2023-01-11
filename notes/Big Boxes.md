---
name: Big Boxes
url: https://open.kattis.com/problems/bigboxes
difficulty: 4.0
---

# Big Boxes

| Input | Description            |
| ----- | ---------------------- |
| $k$   | Number of boxes        |
| $n$   | Number of items        |
| $w_i$ | The weight of item $i$ |

- $1 \le k \le n \le 10^5$

## Observations

- There will never be more boxes than items ($k \le n$)
- If $k = n$, every item has its own box and the answer is just $\max w_i$
- If $k = 1$, we have to put all items into one box, so the answer is the sum of all the weights $\sum_i w_i$

At $k = 2$, things start to get interesting. We can start with the first item in the first box and all the other items in the second box. Then start taking one item at a time from the right box (the left-most one, to preserve the overall order) and add it to the left box. Or we could see it as moving the divider between lists of items (this is likely closer to how it will be modeled in code, with references to items instead of moving ownership of items between containers). Since all the weights are positive, the overall weight of the left box is going to keep increasing as we add items to it, and the right box weight will keep decreasing as we take items from it. At some point (maybe right away before we even start moving items) the weight of the left box will exceed the right box.

This should remind us of the `search` function we've been looking at from [Brent's blog on binary search](https://byorgey.wordpress.com/2023/01/01/competitive-programming-in-haskell-better-binary-search/#a-better-binary-search). It finds the place in a list where the predicate function `p` switches from returning `false` to returning `true`, then returns the indices on both side of the change.

---

I considered starting with everything in its own box ($k = n$) and taking a box away one at a time until we get to the original $k$ from the input:

- If $k = n - 1$, one of the boxes will have two items in it.
	- At first I thought the doubled-up box will always contain the max weight and the other weight will be the smaller of its two neighbours
	- Not the case though, here's a counter-example:
		- You're given the arrangement {5}{5}{9}{4} but $k$ is now $3$, so you have to get rid of a box.
		- The arrangement {5}{5}{9,4} makes the solution $9+4=13$ where {5}{5,9}{4} makes it $5+9=14$
		- But actually {5,5}{9}{4} gives the optimal solution $10$
		- I was considering the idea of starting as though $n=k$ and work down to the real number of boxes $k$ by taking one away at a time. But as shown above this isn't workable because right away we can leave 9 in its own box again and put the two 5's together


## Complexity

We can't consider a quadratic solution because $k$ and $n$ can be up to $10^5$. The fastest time on the leaderboard is 10ms, meaning somebody has done it without going through all $10^{10}$ steps of considering every $n$ or $k$ from every $n$ or $k$.

So we need to look for something like $O(n \log n)$ or some mixture of $n$'s and $k$'s as long as we don't make it quadratic overall by multiplying them. Brent Yorgey's implementation of `search` in Haskell [on his blog entry](https://byorgey.wordpress.com/2023/01/01/competitive-programming-in-haskell-better-binary-search/#:~:text=at%20this%20point%3A-,search,-%3A%3A) gives us a starting point. We can look at the function signature for clues on how to proceed:

```haskell
search :: (a -> a -> Maybe a) -> (a -> Bool) -> a -> a -> (a,a)
       -- mid                    p              l    r    (a,a)
```

At first it seems weird to abstract away functions from the innards of binary search into separate functions `mid` and `p`, then pass them separately to a different function, only for it to combine them back together again.  But it makes it easier to reason separately about the complexity of `mid` and `p`--in particular what they *can't* be.

- What kinds of functions `mid` can be defined if we knew the complexity was $O(\log n)$? What about $O(\log k)$?
- What kinds of functions `p` can be defined if we knew the complexity was $O(\log n)$? What about $O(\log k)$?
- Can `mid` and `p` both be $O(\log n)$ or $O(\log k)$?

---

What's the complexity of the most naive but easiest to understand solution?

```haskell
bigboxes :: [Weight] -> Weight
bigboxes weights = -- ...
```

1) Generate all possible boxings
2) Find the boxing with the minimum maximum box weight $w$ and return $w$

...
