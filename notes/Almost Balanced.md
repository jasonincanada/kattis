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


## Reasoning

*Proposition.* The first bit will never be 1

*Proof.* ...


*Proposition.* The string `0101...01` satisfies all constraints.

*Proof.* There are an even number of elements within each range, and the length $n$ of the whole string is even. Because the bits in `0101...01` alternate `0` and `1` and the ranges are even-length, the count of `1` bits will equal the count of `0` bits--they will always balance out. The constraints are on the count of one of the bits being at most $V_i$ more than the other, with $0 \le V_i$. In addition, it is the lexicographically smallest such balanced string (the other being `1010...10`)

