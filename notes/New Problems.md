## 25 September 2022

[Mad Diamond](https://open.kattis.com/problems/maddiamond)
- Similar to the tilting labyrinth game
- Gravity component is just a vector that rotates, don't rotate the whole dataset


## 3 August 2022

[Guma](https://open.kattis.com/problems/guma)
- Surprised this has no accepted solutions as of September 25, it looks relatively easy

[Inheritance](https://open.kattis.com/problems/inheritance)
- Find the divisors of a number which consist only of digits 2 and 4


## 9 July 2022

[Palindrome-Free Numbers](https://open.kattis.com/problems/palindromefree)
- Detect palindromes in large ranges of numbers (up to $10^{18}$)
- No chance of brute force, need to find a pattern

[Going to School](https://open.kattis.com/problems/skolvagen)
- I think this has a dynamic programming solution

[MeTube](https://open.kattis.com/problems/dutub)
- Looks like a good optimization problem


## 8 July 2022

### Chocolate Division / Board Game
[Chocolate Division](https://open.kattis.com/problems/chocolatedivision) is almost the same as [The Board Game](https://open.kattis.com/problems/bradspelet) but in Chocolate Division the players can choose from any of the pieces that have ever been broken off; where in The Board Game, whenever a piece is broken, one of the pieces is chosen by the other player to be kept and the other piece is thrown away for the rest of the game.  The Board Game should be easier to code

### Grid Volleyball

[Grid Volleyball](https://open.kattis.com/problems/gridvolleyboll) looks like brute-force might work since the dimensions are so small, at least to start the game: since $X \le 4$ and $Y \le 3$, the number of starting game configurations (given that players on the same team can occupy the same position) is $(4 \times 3)^{3} = 1728$ (the board is symmetric along the horizontal axis so player A1 can always start in the top right, leaving only $3$ in the exponent).  However the gameplay tree could get way out of control, and remembering which states we've already seen so we can detect loops/ties might be performance intensive

Would dynamic programming work for this? What would be the optimal substructure?  How could a subproblem be stated as a function of other, already-completed subproblems?
