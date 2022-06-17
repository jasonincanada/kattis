---
name: Target Practice
url: https://open.kattis.com/problems/targetpractice
difficulty: 4.8
---

# Target Practice

This challenge involves some geometric deductive reasoning, using points, lines and a triangle. The solution below runs in 0.01s in Rust, which is fast enough to join a 4-way tie for 1st place with the C++ solutions

## Visualization

To get a visual sense of what's going on, let's render **Sample Input 2** using [Desmos](https://www.desmos.com/calculator/) and draw the obvious lines through it:

![[targetpractice-sample-2.png]]

This test case is a successful one because with at most two lines drawn (red and blue) we have crossed through all the points in the data set

Compare this to **Sample Input 1**, where we would need at least three lines to cross all points:

![[targetpractice-sample-1.png]]

Try drawing one line through the remaining blue points!


## Algorithm

**Summary**: Iterate over points until we have a triangle. Pick a fourth point, $R$. In the success case this point must be collinear with one of the sides $MN$ of the triangle. Then every other point must be collinear with side $MN$ or the line from a new point $B$ (which is not collinear with $MN$) to vertex $O$ of the triangle, the one not belonging to side $MN$. If so, it's a success case, otherwise a failure


### Notes

It took a bit of thinking to figure out the above algorithm. Here are some notes I took during my brainstorming:

- In the success case, there are at most two lines that together pass through all points.  Meaning if you have any 3 points, 2 are on one of the lines and 1 is on the other, or all 3 are on one line
- Said differently: any three points that don't form a line actually form a triangle. In the success case at least one of the sides of the triangle is on one of the lines

## General Case

Let's consider the general case by choosing a specific one that we know (by construction with two list comprehensions) can be covered by only two lines.

So we'll need two lines' worth of data for a test graph:

```bash
$ docker run --rm -it haskell
````

```haskell
ghci> [ (x, (-1)*x+3) | x <- [-5..5], odd x ]
[(-5,8),(-3,6),(-1,4),(1,2),(3,0),(5,-2)]

ghci> [ (x, 2*x-1) | x <- [-3..5], odd x ]
[(-3,-7),(-1,-3),(1,1),(3,5),(5,9)]
```

Paste the points into Desmos and toggle lines:

![[tp-example.png]]

Let's go through the five steps of our algorithm:

### Step 1 - Form Triangle

The algorithm is general enough to start with any two arbitrary points. Suppose they are the following two points on the red line, shown by the purple line segment connecting them:

![[tp-0.png]]

Next, find the first point that is not collinear to this line. If we don't find one, we're done and we have a successful case, because all points are on a single line that can be drawn through them all.

The more interesting case is when we do have a point not on our original line, thus forming a triangle:

![[tp-1.png]]

### Step 2 - Find Collinear Point

Next, find a point collinear with any one of the sides of the triangle. If none of the remaining points are collinear, we'll necessarily need to draw more than two lines to cover all the points, so it's a failure case and we can stop.

Say we did find a point $R$ (for red). The side the point is collinear with is identified as $MN$, and the other vertex of the triangle we identify as point $O$, as below:

![[tp-2.png]]

### Step 3 - Find Non-collinear Point

If there are any points left that aren't on the $MN$ line (the line containing the side $MN$), they had better all be on a line containing point $O$. So find the first unaccounted-for point that is not on the line $MN$ and call it $B$ (for blue). Suppose it happens to be the one circled in blue below:

![[tp-4.png]]

Now all points should be accounted for by our lines $MN$ and $OB$, so if we find even one point not on one of these two lines, it's a failure case. Otherwise, we've found the two lines and it's a success case!

## Pseudocode

Here's pseudocode for the above algorithm:

```pseudo
form triangle T

loop
  R <- next point
until R is collinear with side MN of triangle T

O <- the point of triangle T other than M and N

loop
  B <- next point
until B not collinear with MN

all points are collinear with MN or OB
```
