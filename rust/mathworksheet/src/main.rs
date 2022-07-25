/*  https://open.kattis.com/problems/mathworksheet  */

fn main() {
    let     stdin = std::io::stdin();
    let mut lines = stdin.lock().lines();
    let mut first = true;

    loop {
        let n = read_u8(&mut lines);
        if n == 0 { break }

        // this avoids printing a final newline
        if !first { println!(); } first = false;

        let input = read_n_strings(n, &mut lines);
        for line in do_case(input).iter() {
            println!("{}", line);
        }        
    }
}

fn do_case(expressions: Vec<String>) -> Vec<String> {

    let mut longest = 0;
    
    expressions
        .iter()
        .map(|expr| {
                        let result = evaluate(expr);

                        // keep track of the longest result so we can pad
                        // the others with spaces as needed
                        longest = result.len().max(longest);

                        result
                    })
        .collect::<Vec<String>>()
        .iter()

        // right-align, padding with spaces on the left
        .map(|result| format!("{res:>width$}", res=result, width=longest))
        .collect::<Vec<String>>()
        
        // split into lines no longer than 50 chars including spaces
        .chunks(50 / (longest + 1))
        .map(|chunk| chunk.join(" "))
        .collect()

}

fn evaluate(expr: &str) -> String {

    let split = expr.split_whitespace().collect::<Vec<_>>();
    let left  = split[0].parse::<i32>().unwrap();
    let right = split[2].parse::<i32>().unwrap();

    match split[1] {
        "+" => format!("{}", left + right),
        "-" => format!("{}", left - right),
        "*" => format!("{}", left * right),
         _  => panic!("unknown operation")
    }
}


/* Parsing */

use std::io::{BufRead, Lines, StdinLock};

fn read_u8(lines: &mut Lines<StdinLock>) -> u8 {
    lines.next().unwrap().unwrap()
         .parse().unwrap()
}

fn read_n_strings(n: u8, lines: &mut Lines<StdinLock>) -> Vec<String> {
    let mut strings = vec![];

    for _ in 0..n {
        strings.push(lines.next().unwrap().unwrap());
    }

    strings
}


/* Tests */

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_sample_1() {

        let input = vec![
            String::from("9999 * 2000"),
            String::from("10 - 5"),
            String::from("1 + 1"),
            String::from("3 - 3"),
            String::from("2 * -4"),
            String::from("3 * 3"),
            String::from("18 + 23")
        ];

        let expected = vec![
            String::from("19998000        5        2        0       -8"),
            String::from("       9       41")
        ];

        let actual = do_case(input);

        assert_eq!(expected, actual);
    }

    #[test]
    fn test_sample_2() {

        let input = vec![
            String::from("3 - 2"),
            String::from("1 * 1"),
            String::from("2 + -1"),
            String::from("4 * 4")
        ];

        let expected = vec![
            String::from(" 1  1  1 16")
        ];

        let actual = do_case(input);

        assert_eq!(expected, actual);
    }

    #[test] fn test_evaluate_multiply() { assert_eq!(String::from("19998000"), evaluate("9999 * 2000")); }
    #[test] fn test_evaluate_subtract() { assert_eq!(String::from("-5"), evaluate("5 - 10")); }
    #[test] fn test_evaluate_addition() { assert_eq!(String::from("3"), evaluate("1 + 2")); }
}
