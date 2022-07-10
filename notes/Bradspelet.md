---
name: The Board Game
url: https://open.kattis.com/problems/bradspelet
difficulty: tbd
---

# The Board Game

We use a dynamic programming recurrence relation to avoid re-computing winners from board dimensions we've already seen.

## Recurrence

The recurrence for this solution is a function $f : \mathbb{N} \times \mathbb{N} \to \{0,1\}$ defined as:

$$
\begin{align}
    f(1, 1) &= false \\
    f(n, m) &= \bigvee_{(b_1, b_2) \in cuts(n,m)} \neg g(b_1) \land \neg g(b_2) \\
\end{align}
$$

where function $g : \text{Board} \to \{0,1\}$ destructures a board into its $n$ and $m$ fields before calling $f$ on them:

$$g(\text{board}) = f(\text{board.n}, \text{board.m})$$

and $cuts$ returns the set of all pairs of valid boards that can be constructed from the passed dimensions.
