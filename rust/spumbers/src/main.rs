/*  https://open.kattis.com/problems/noonerizedspumbers  */

use std::io::BufRead;

// An equation stored as its string components
#[derive(Clone)]
struct Equation {
    arg1: String,
    arg2: String,
    operation: char,
    result: String
}

impl Equation {

    // construct an Equation from a string like "92 + 2803 = 669495"
    fn new(input: &str) -> Self {
        let tokens : Vec<&str> = input.split(' ').collect();

        Equation {
            arg1:      tokens[0].to_string(),
            operation: tokens[1].chars().next().unwrap(),
            arg2:      tokens[2].to_string(),
            result:    tokens[4].to_string()
        }
    }

    // Return a particular component of the equation
    fn part(&self, component: &Component) -> &str {
        match component {
            Component::Arg1   => &self.arg1,
            Component::Arg2   => &self.arg2,
            Component::Result => &self.result,
        }
    }

    fn set_part(&mut self, component: &Component, val: String) {
        match component {
            Component::Arg1   => self.arg1   = val,
            Component::Arg2   => self.arg2   = val,
            Component::Result => self.result = val,
        }
    }

    fn valid(&self) -> bool {
        let arg1 = self.arg1.parse::<i64>().unwrap();
        let arg2 = self.arg2.parse::<i64>().unwrap();
        let res  = self.result.parse::<i64>().unwrap();

        match self.operation {
            '+' => arg1 + arg2 == res,
            '*' => arg1 * arg2 == res,
            _   => panic!("Unknown operation")
        }
    }

    // Convert back to string for the solution checker
    fn render(&self) -> String {
        [&self.arg1,
         &self.operation.to_string(),
         &self.arg2,
         "=",
         &self.result].join(" ")
    }
}

// The parts of an equation that can be edited (ie, the operation stays the same)
enum Component {
    Arg1,
    Arg2,
    Result
}


fn main() {

    // get first line of input and parse it into an Equation
    let input = std::io::stdin().lock().lines().next().unwrap().unwrap();
    let equation = Equation::new(&input);

    let result: Option<String> = do_case(&equation);

    match result {
        Some(output) => println!("{}", output),
        None => println!("no result")
    }    
}

fn do_case(equation: &Equation) -> Option<String> {

    // build all permutations of the equation and try them all. even with computing all these
    // first instead of on-demand, the server runtime is still rounding down to 0.00s
    let mut tries: Vec<Equation> = Vec::new();

    add_permutations(&mut tries, equation, &Component::Arg1, &Component::Arg2);
    add_permutations(&mut tries, equation, &Component::Arg1, &Component::Result);
    add_permutations(&mut tries, equation, &Component::Arg2, &Component::Result);

    // try the permutations
    for attempt in tries {
        if attempt.valid() {
            return Some(attempt.render());
        }
    }

    return None;
}

// Add all equations generated by permuting two particular components of a source equation
fn add_permutations(tries: &mut Vec<Equation>,
                    equation: &Equation,
                    comp1: &Component,
                    comp2: &Component) -> () {

    let num1 = equation.part(comp1);
    let num2 = equation.part(comp2);

    for i in 1..num1.len() {
    for j in 1..num2.len() {
        let (swapped1, swapped2) = swap(num1, num2, i, j);

        let mut equation = equation.clone();

        equation.set_part(comp1, swapped1);
        equation.set_part(comp2, swapped2);

        tries.push(equation);
    }
    }
}

// Swap the prefixes of two strings at a certain index in each
fn swap(s1: &str, s2: &str, idx1: usize, idx2: usize) -> (String, String) {
    
    let prefix1 = &s1[..idx1];
    let suffix1 = &s1[idx1..];
    let prefix2 = &s2[..idx2];
    let suffix2 = &s2[idx2..];

    let new1 = [prefix2, suffix1].join("");
    let new2 = [prefix1, suffix2].join("");

    (new1, new2)
}


/* Tests */

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_swap() {
        assert_eq!(("uvbcdef".to_string(), "awxyz".to_string()),
                   swap("abcdef", "uvwxyz", 1, 2));
    }

    #[test]
    fn test_valid() {        
        assert_eq!(true, Equation::new("2 + 3 = 5").valid());
        assert_eq!(true, Equation::new("2 * 3 = 6").valid());

        assert_eq!(false, Equation::new("2 + 3 = 50").valid());
        assert_eq!(false, Equation::new("2 * 3 = 60").valid());
    }

    #[test]
    fn test_do_case() {
        // sample inputs
        assert_eq!("6692 + 2803 = 9495"  , do_case(&Equation::new("92 + 2803 = 669495")).unwrap());
        assert_eq!("7291 * 683 = 4979753", do_case(&Equation::new("6891 * 723 = 4979753")).unwrap());

        // test 2-char numbers
        assert_eq!("12 * 34 = 408"       , do_case(&Equation::new("32 * 14 = 408")).unwrap());
    }
}
