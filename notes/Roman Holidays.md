---
name: Roman Holidays
slug: romanholidays
url: https://open.kattis.com/problems/romanholidays
difficulty: 4.9
---

# Roman Holidays

Step 1: Write `roman()`

```rust
fn roman(mut decimal: u32) -> String {

    let table = vec![
       (1000, "M"),
       (900, "CM"),
       (500, "D"),
       (400, "CD"),
       (100, "C"),
       (90, "XC"),
       (50, "L"),
       (40, "XL"),
       (10, "X"),
       (9, "IX"),
       (5, "V"),
       (4, "IV"),
       (1, "I")
    ];

    let mut roman = String::new();

    while decimal > 0 {        
        for (level, numerals) in table.iter() {
            if decimal >= *level {
                roman.push_str(numerals);
                decimal -= level;                
                break;
            }
        }                
    }

    roman
}
```

Step 2: Generate all Roman numbers from 1..999 then sort into two separate lists: those occurring before "M" in the sorted list (if it had gone to 1,000) and those after:

```rust
fn print() {
    for number in 1..=999 {
        println!("{} {}", number, roman(number));
    }
}
```

```bash
$ cargo run > 1-999.txt

# check output
$ head 1-999.txt
1 I
2 II
3 III
4 IV
5 V
6 VI
7 VII
8 VIII
9 IX
10 X
```

Split into sorted numbers before and after M in lexicographic order:

```bash
$ cut -d' ' -f 2 1-999.txt | awk '/^[CDIL]/ { print }' | sort    > before-M.txt
$ cut -d' ' -f 2 1-999.txt | awk '/^[VX]/   { print }' | sort -r >  after-M.txt

# quick sanity check to make sure we still have all 999 numbers across the two files:
$ wc -l {before,after}-M.txt
 945 before-M.txt
  54 after-M.txt
 999 total
```

Get it into Rust as static lists:

```bash
$ ./generate_vecs.sh
```
