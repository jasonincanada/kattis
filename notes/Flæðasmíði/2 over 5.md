## 2 sources to 5 customers
- 15 edges (probably optimal)
- first time seeing a source feed another source

```mermaid
stateDiagram-v2
	S1 --> 1: 5/5
	S2 --> 2: 5/5
	1 --> 8: 4/5
	1 --> 9: 4/5
	8 --> 3: 2/5
	8 --> 4: 2/5
	9 --> 5: 2/5
	9 --> 6: 2/5
	2 --> 10: 4/5
	2 --> return: 4/5
	10 --> 7: 2/5
	10 --> return: 2/5
	return --> onetwo: 6/5
	onetwo --> 1: 3/5
	onetwo --> 2: 3/5
	3
	4
	5
	6
	7

	classDef source fill:#C44
	classDef sink fill:#3CD
	class 1 source
	class 2 source
	class 3 sink
	class 4 sink
	class 5 sink
	class 6 sink
	class 7 sink
```

## 1 source to 5 customers 2 times ($2\times\frac{1}{5}$)
- 24 edges (interesting idea but not optimal)

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

    a1 --> a7: 4/5
	a1 --> a8: 4/5
	a7 --> a1
	a7 --> a9
	a8 --> a10: 2/5
	a8 --> a11: 2/5
	a9 --> a1
	a9 --> 2: 1/5
	a10 --> 3
	a10 --> 4
	a11 --> 5
	a11 --> 6

	classDef source fill:#C44
	classDef sink fill:#3CD
	class 1 source
	class a1 source
	class 2 sink
	class 3 sink
	class 4 sink
	class 5 sink
	class 6 sink
```

