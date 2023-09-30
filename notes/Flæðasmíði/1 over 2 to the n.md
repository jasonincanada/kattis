# $f(n) = \frac{1}{2^n}$

---
## (n = 1) 1 source to 2 customers


```mermaid
stateDiagram-v2
	1 --> 2: 1/2
	1 --> 3: 1/2

	classDef source fill:#C44
	classDef sink fill:#3CD
	class 1 source
	class 2 sink
	class 3 sink
```

## n = 2

```mermaid
stateDiagram-v2
	1 --> 6
	1 --> 7
	6 --> 2: 1/4
	6 --> 3
	7 --> 4
	7 --> 5

	classDef source fill:#C44
	classDef sink fill:#3CD
	class 1 source
	class 2 sink
	class 3 sink
	class 4 sink
	class 5 sink
```

## n = 3

```mermaid
stateDiagram-v2
	1 --> 10
	1 --> 11
	10 --> 12
	10 --> 13
	11 --> 14
	11 --> 15
	12 --> 2
	12 --> 3
	13 --> 4
	13 --> 5: 1/8
	14 --> 6: 1/8
	14 --> 7
	15 --> 8
	15 --> 9	

	classDef source fill:#C44
	classDef sink fill:#3CD
	class 1 source
	class 2 sink
	class 3 sink
	class 4 sink
	class 5 sink
	class 6 sink
	class 7 sink
	class 8 sink
	class 9 sink
```


Formula for number of pipes:

$$E(n) = \sum_{i=1}^n 2^i$$

Formula for number of joints (including source/customers):

$$\begin{align}
	V(n) &= 1 + \sum_{i=1}^{n-1} 2^i + 2^n \\
	     &= \sum_{i=0}^n 2^i
\end{align}$$
