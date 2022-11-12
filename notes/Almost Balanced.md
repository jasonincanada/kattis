---
name: Almost Balanced
url: https://open.kattis.com/problems/almostbalanced
difficulty: 6.9
---

# Almost Balanced

The problem repeatedly reminds us the string length is **even** so that's probably important. Should we consider bits in pairs?
There are up to 300k chars in the string and up to 300k constraints on it. I think this rules out an $O(n^2)$ algorithm, and I think most naive algos I've already considered bits and pieces of are even worse, probably $O(n^3)$ so this will require something clever


## Count distinct constraints for an $n$-string

Simplest string and all possible constraints:

```text
2 4
0 1 2 0   -- at most zero more 0s
0 1 2 2   -- at most two  more 0s
1 1 2 0   -- at most zero more 1s
1 1 2 2   -- at most two  more 1s
```

The first two overlap and the last two overlap. In both cases the constraint with the lower $V_i$ value "wins".

The output string for this contrived input must be `01` to satisfy all constraints. `00` fails constraint 1, `11` fails constraint 3, and `10` satisfies all listed constraints but it's not the lexicographically smallest string, which is a standing constraint on all inputs.

*TODO*: formula for counting number of valid constraints for a bit string of length $n$


## Sample Inputs

### Input 1

```text
6 2      -- Best output evolution
0 1 6 2     000011  (set bits 5 and 6)
1 5 6 1     000101  (swap bits 4 and 5)
```

### Input 2

```text
6 3      -- Best output evolution
0 1 6 0     000111  (set bits 4,5,6)
0 1 4 0     001101  (swap bits 3 and 5)
1 3 4 0     010101  (swap bits 2 and 3)
```


## Notes

- The output string is insensitive to the order the constraints are considered. However, the algorithm complexity will probably depend on it. There is probably a smart way to sort the constraints before applying them
- Do the constraints form a monoid?
	- Can we combine two valid constraints into a valid constraint? Probably not because how do you merge two constraints whose ranges don't overlap? Can we lift this into some more complex monoid?
	- What would be the neutral constraint?
- Consider a DSL for operating on the string or spans of the string. Note two operations already from the Input notes above:
	- `SetBit i {0,1}`
	- `SwapBit i j`
	- Would we ever need to `SetBit i 0`, or can we just initially set as many `1`s as we need then swap with remaining `0`s?
- Sort constraints *ascending* by $T_i$ then $L_i$ then $R_i$, then *descending* by $V_i$

## Algorithm

Phase 0: Generate all required `1`s, exactly as many as needed and as far to the right as possible
Phase 1: Evict `1`s as needed by throwing them to the left while maintaining prior constraints
Phase 2: Render to string

```
    phase 0 generation 1 (all "0 1 r v" constraints, sorted r ASC, v DESC)

    new 16-chain:  0000000000000000   (conceptually, don't actually store a 16-bit vector)
    0 1 4 4        0000               no change
    0 1 4 2        0001               bring into compliance by filling 1s from the RIGHT
    0 1 4 0        0011               we could have ignored the first two constraints and ended
                                      up with the same chain by this point. for a set of constraints
                                      0 l r {v1,v2,...,vn} we need only consider 0 l r (min vi)
                                      
                 --1234567890123456-- (ruler)
    0 1 10 2       0011000011         catch up by adding two 1s on the right
    0 1 10 0       0011000111         one more 1.  if we had *only* this constraint we'd have
                                      0000011111 but we already have two 1s from earlier constraints
                                      so we only needed to add a total of three 1s to the right

    new example:   0000000000000000
    0 1 4 0        0011
    0 1 12 6       001100000001
    0 1 16 0       0011000000111111   new phenomenon: we have to add five 1s from the right but
                                      we have to JUMP a 1 already set from a prior constraint
                                      
                                      all of these ones can be shifted to the left as far as the
                                      start of the generation that created them (l=1 here). we'll
                                      need to do so in phase 1 wherever we have to evict 1s
    
    -- end of phase 0 generation 1

    -- start phase 0 generation 2 (l=2)
                   0011000000111111
    0 2 7 0        0011001000011111   new phenomenon: we can "tractor beam" any 1 that is right of
                         ^...v        r=7 instead of generating a new 1. all ones to the right were
                                      created in prior generations, meaning they can be moved
                                      without breaking any constraint from phase 0, because they
                                      were added by generation l < 2 (explain this better)
    .
    .
    . 

    -- start phase 1: evict 1s

    -- start phase 1 generation 13
	. .. .. .     0011001000011111
	1 13 16 0     0011001001110011   evict two 1s, throwing them to the left only as far as needed
		                   ^^.vv
									 (prove there will always be an "available" 0 to the left of l=13)
									 (prove there will always be enough "movable" 1s in 13..16)


    Sample Input 2
    --------------
    Input     String  Stash of (n/2) 1s for tractoring
    6 3       000000  111
    0 1 4 0   001100  001
    0 1 6 0   001101  000
    1 3 4 0   010101  000

```

## Reasoning

*Proposition.* The first bit will never be 1

*Proof.* ...


*Proposition.* The string `0101...01` satisfies all constraints.

*Proof.* There are an even number of elements within each range, and the length $n$ of the whole string is even. Because the bits in `0101...01` alternate `0` and `1` and the ranges are even-length, the count of `1` bits will equal the count of `0` bits--they will always balance out. The constraints are on the count of one of the bits being at most $V_i$ more than the other, with $0 \le V_i$. In addition, it is the lexicographically smallest such balanced string (the other being `1010...10`)

*Proposition.* The total number of `1`s will never exceed $n/2$.
*Proposition.* When throwing `1`s to the left, there will always be room for the `1`s to the left.
*Proposition.* When throwing `1`s to the left, there will always be enough evictable `1`s to do so.
