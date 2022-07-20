---
name: Digit Sum
url: https://open.kattis.com/problems/digitsum
difficulty: 6.4
---

# Digit Sum

## Recurrence

For $p$ the power of 10 (table row) and $d$ the digit (table column), with each term on its own line (colors match the spreadsheet):

$$
\begin{align}
  A(p,d) &= \color{blue}{A(p, d-1)} \\
         &+ \color{red}{A(p-1, 10)} \\
         &+ \color{magenta}{(10^p - 1)}\color{green}{(d-1)} \\
         &+ \color{darkblue}{d}
\end{align}
$$

Screenshot of the spreadsheet with the formula for $A(3,4)$ expanded:

![[digit sum - 1.png]]
