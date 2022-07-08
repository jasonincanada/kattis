---
name: Box of Mirrors
url: https://open.kattis.com/problems/boxofmirrors
difficulty: tbd
---

# Box of Mirrors

- No possible cycles, you'd need mirrors angled the other way
- As it is, beams from the left only go right or up or a combination of both
- Beams can only criss-cross each other at a cell--they cannot "share" some of a path and then diverge, or the other beam would have diverged at that same point on the same mirror too. This implies that when beam X outputs at location Y, then the beam emitting at location Y will output at X, every time; and the number of beams is even

We know these things for sure about the first row:
- the light either goes out the other end with no mirrors in the way (so, no mirrors anywhere on the first row) or
- it exits at one of the top points, so there must be a mirror at that column (no rows left to wiggle rightward along the way up)


## Visuals

This is a sketch of **Sample Input 1**. It can be solved two different ways, this is the one given in **Sample Output 1**:

![[boxofmirrors-1.png]]

This is the alternative way it could have been solved, with the bottom left mirror moved to the top right:

![[boxofmirrors-2.png]]

We cannot merge both solutions or we have both beams 2 and 4 reflecting 90 degrees off before they exit, and the solution isn't correct:

![[boxofmirrors-3.png]]

We can know a few things for sure, namely the first row is either going to be all non mirrors or there will be at least one in the way, reflecting the beam off its straight path. By similar reasoning we can know some things about the left-most and right-most columns and the bottom row. But this still leaves the "interior" mirrors unknown, those that are behind ones we know for sure are there:

![[boxofmirrors-4.png]]


### The General Mirror

This describes the general left-most mirror in the first mirrored row, reasoning from the top left:

![[boxofmirrors-5.png]]
