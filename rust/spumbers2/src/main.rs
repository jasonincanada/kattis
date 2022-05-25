/*  https://open.kattis.com/problems/noonerizedspumbers  

    This is a rewrite of my first attempt at this challenge (../spumbers). The first attempt had a
    glaring inefficiency: it calls .clone() on the whole Equation object in the innermost loop,
    which allocates three new strings each time even though one (the "anchor") doesn't change at
    all from the original equation, and the other two get overwritten right away with permutations
    of each other

    In this new solution, the old Equation object is still here but it's used only once, to hold
    (and own) the original strings from the input equation. For each of the three parts of the
    input equation, an EquationBuilder object is constructed with a read-only reference to the term
    in the underlying equation that isn't changing. This requires the use of lifetime parameters
    (the 'a notation) to inform the Rust compiler that the referent value lives at least as long as
    the reference to it

    I've factored the prefix swapping into its own structure PairPrefixSwapper. This structure has
    the Iterator trait implemented on it, so rather than using two nested and indexed for loops for
    the core swapping loop, it is now a single foreach that tries each of the pairs. Holding
    references to the underlying terms that are being permuted requires us again to use lifetime
    parameters to convince Rust that the referents will last at least as long as the references

    The runtime is showing 0.00s on the server, but the original one did as well. We can only
    assume it runs even faster this time because there is no superfluous copying going on, though
    the kattis servers can't help us confirm this

    Revision: This update resolves the last obvious inefficiency. The anchor was being parsed
    repeatedly every time a new pair of terms was tried, even though it doesn't change its value
    like the other two terms do

    Differences to note between the old and new solutions:

      - #[derive(Clone)] is gone from Equation because .clone() is gone from the code!
      - render() now lives on EquationBuilder: we never have to re-render the original equation
        so that's not the best place to put render(). also it uses the much cleaner format!()
        macro rather than joining an array of Strings on " "
      - since we're using a proper iterator we can bail from the main loop as soon as we find a
        permutation that is valid, instead of constructing all possible objects first and then
        looping through them. the new way never has more than a few strings and a few references
        allocated at once, which is a much more sensical way to do this since it's not being
        parallelized
*/

use std::io::BufRead;

fn main() {

    // get first line of input and parse it into an Equation
    let input = std::io::stdin().lock().lines().next().unwrap().unwrap();
    let equation = Equation::new(&input);

    let result: Option<String> = do_case(equation);

    match result {
        Some(output) => println!("{}", output),
        None         => println!("no result")
    }    
}

fn do_case(equation: Equation) -> Option<String> {
    
    // hold one part of the equation constant at a time (the "anchor") while swapping the prefixes
    // of the other two terms
    let parts = vec![ Part::Term1,
                      Part::Term2,
                      Part::Result ];

    for anchor in parts {

        // try equations starting with a common base equation and anchor
        let builder = EquationBuilder::new(&equation, &anchor);

        // swap the prefixes of the two non-anchor terms and try them until one works
        let (term1, term2) = equation.parts_other_than(&anchor);
        let swapper        = PairPrefixSwapper::new(term1, term2);

        for (left, right) in swapper {
            if builder.is_valid_with(&left, &right) {
                return Some(builder.render_with(&left, &right))
            }
        }
    }

    None
}


// the equation from the input, stored as owned string components
struct Equation {
    term1:     String,
    term2:     String,
    operation: char,
    result:    String
}

impl Equation {

    // construct an Equation from a string like "92 + 2803 = 669495"
    fn new(input: &str) -> Self {
        let tokens : Vec<&str> = input.split(' ').collect();

        Equation {
            term1:     tokens[0].to_string(),
            operation: tokens[1].chars().next().unwrap(),
            term2:     tokens[2].to_string(),
            result:    tokens[4].to_string()
        }
    }

    // the anchor is the term that is not changing throughout the permutations.
    // return the other two parts that are
    fn parts_other_than(&self, anchor: &Part) -> (&str, &str) {
        match anchor {
            Part::Term1  => (&self.term2, &self.result),
            Part::Term2  => (&self.term1, &self.result),
            Part::Result => (&self.term1, &self.term2)
        }
    }
}

// The parts of an equation that can be edited (ie, the operation always stays the same)
enum Part {
    Term1,
    Term2,
    Result
}

struct EquationBuilder<'a> {
    equation:      &'a Equation,
    anchor:        &'a Part,

    // cache the anchor's integer value
    anchor_parsed: i64
}

impl<'a> EquationBuilder<'a> {

    fn new(equation: &'a Equation, anchor: &'a Part) -> Self {
        EquationBuilder {
            equation,
            anchor,

            // parse the anchor once during construction and keep it around since it doesn't change
            anchor_parsed: match anchor {
                Part::Term1  => parsei64(&equation.term1),
                Part::Term2  => parsei64(&equation.term2),
                Part::Result => parsei64(&equation.result),
            },
        }
    }

    fn is_valid_with(&self, left: &str, right: &str) -> bool {
       
        let term1 = match self.anchor {
             Part::Term1 => { self.anchor_parsed },
                       _ => { parsei64(&left) },
        };

        let term2 = match self.anchor {
            Part::Term1  => { parsei64(&left) },
            Part::Term2  => { self.anchor_parsed },
            Part::Result => { parsei64(&right) },
        };
        
        let res = match self.anchor {
            Part::Result => { self.anchor_parsed },
                       _ => { parsei64(&right) },
        };
        
        match self.equation.operation {
            '+' => term1 + term2 == res,
            '*' => term1 * term2 == res,
             _  => panic!("Unknown operation")
        }
    }

    // convert an equation to a string for the solution checker
    fn render_with(&self, left: &str, right: &str) -> String {
        match self.anchor {
            Part::Term1  => format!("{} {} {} = {}", self.equation.term1,
                                                     self.equation.operation,
                                                     left,
                                                     right),

            Part::Term2  => format!("{} {} {} = {}", left,
                                                     self.equation.operation,
                                                     self.equation.term2,
                                                     right),

            Part::Result => format!("{} {} {} = {}", left,
                                                     self.equation.operation,
                                                     right,
                                                     self.equation.result),
        }
    }
}

fn parsei64(number: &str) -> i64 {
    number.parse::<i64>().unwrap()
}

struct PairPrefixSwapper<'a> {
    left:  &'a str,
    right: &'a str,

    // remember where we are in the permutation space between calls to .next()
    l: usize,
    r: usize
}

impl<'a> PairPrefixSwapper<'a> {

    fn new(left: &'a str, right: &'a str) -> Self {
        PairPrefixSwapper {
            left,
            right,

            // initialize the iterator state variables
            l: 1, 
            r: 1 
        }
    }
    
    // swap the prefixes of two strings at a certain index in each
    fn swap(s1: &str, s2: &str, idx1: usize, idx2: usize) -> (String, String) {
        
        let prefix1 = &s1[..idx1];
        let suffix1 = &s1[idx1..];
        let prefix2 = &s2[..idx2];
        let suffix2 = &s2[idx2..];

        let new1 = [prefix2, suffix1].join("");
        let new2 = [prefix1, suffix2].join("");

        (new1, new2)
    }
}

impl Iterator for PairPrefixSwapper<'_> {

    // each iteration returns a pair of strings
    type Item = (String, String);

    fn next(&mut self) -> Option<Self::Item> {
        
        if self.l >= self.left.len() {
            return None
        }

        let (swapped1, swapped2) = PairPrefixSwapper::swap(self.left,
                                                           self.right,
                                                           self.l,
                                                           self.r);

        // update the internal iterator state, wrapping around as needed
        self.r += 1;
        
        if self.r >= self.right.len() {
            self.r = 1;
            self.l += 1;
        }

        // move these newly constructed owned strings into the caller of .next(), which is 
        // called implicitly by the for (left,right) loop in do_case()
        Some((swapped1, swapped2))
    }
}


#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_swapper_iterator() {
        let mut test = PairPrefixSwapper::new("abc", "123");

        // l = 1, r = [1..2]
        assert_eq!(Some(("1bc".to_owned(), "a23".to_owned())), test.next());
        assert_eq!(Some(("12bc".to_owned(), "a3".to_owned())), test.next());
        
        // l = 2, r = [1..2]
        assert_eq!(Some(("1c".to_owned(), "ab23".to_owned())), test.next());
        assert_eq!(Some(("12c".to_owned(), "ab3".to_owned())), test.next());

        // l = 3
        assert_eq!(None, test.next());
    }

    #[test]
    fn test_do_case() {
        // sample inputs
        assert_eq!("6692 + 2803 = 9495"  , do_case(Equation::new("92 + 2803 = 669495")).unwrap());
        assert_eq!("7291 * 683 = 4979753", do_case(Equation::new("6891 * 723 = 4979753")).unwrap());

        // test 2-char numbers
        assert_eq!("12 * 34 = 408"       , do_case(Equation::new("32 * 14 = 408")).unwrap());
    }
}
