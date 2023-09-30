# $f(n) = \frac{1}{2^n+1}$

---
## 1 source to 3 customers

$$f(1) = \frac{1}{3}$$


```mermaid
stateDiagram-v2
	
	1 --> 5: 2/3
	1 --> 6: 2/3
	5 --> 2
	5 --> 3
	6 --> 1: 1/3
	6 --> 4: 1/3

	classDef source fill:#C44
	classDef sink fill:#3CD
	class 1 source
	class 2 sink
	class 3 sink
	class 4 sink	
```

## 1 source to 5 customers

$$f(2) = \frac{1}{5}$$


```mermaid
stateDiagram-v2
	
	1 --> 7: 4/5
	1 --> 8: 4/5
	7 --> 1
	7 --> 9
	8 --> 10: 2/5
	8 --> 11: 2/5
	9 --> 1
	9 --> 2: 1/5
	10 --> 3
	10 --> 4
	11 --> 5
	11 --> 6

	classDef source fill:#C44
	classDef sink fill:#3CD
	class 1 source
	class 2 sink
	class 3 sink
	class 4 sink
	class 5 sink
	class 6 sink
```


## 1 source to 9 customers

$$f(3) = \frac{1}{9}$$


```mermaid
stateDiagram-v2
	S --> 1: 9/9
	1 --> 11: 8/9
	1 --> 18: 8/9
	18 --> 19: 4/9
	18 --> 22: 4/9
	19 --> 20: 2/9
	19 --> 21: 2/9
	20 --> 2: 1/9
	20 --> 21: 1/9
	21 --> 22: 3/9
	22 --> 1: 7/9
	
	11 --> 12: 4/9
	11 --> 13
	12 --> 14: 2/9
	12 --> 15
	13 --> 16
	13 --> 17
	14 --> 3: 1/9
	14 --> 4
	15 --> 5
	15 --> 6
	16 --> 7
	16 --> 8
	17 --> 9
	17 --> 10

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
	class 10 sink
```

## Observations

- Two subgraphs, one a 2-ary tree rooted at 11 containing $2^n$ leaves, the other a cyclic graph with 1 leaf (customer) that gathers and carries back to 1 all the excess supply