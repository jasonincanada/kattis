/*  https://open.kattis.com/problems/noonerizedspumbers  

    This is a rewrite of my first attempt at this challenge (../spumbers). The first solution has a
    glaring inefficiency: it calls .clone() on the whole Equation object in the innermost loop,
    which allocates three new strings each time even though one (which I've called the "anchor")
    doesn't change at all from the original equation, and the other two get overwritten right away
    with permutations of those fields. In this new solution, the old Equation object is still here
    but it's only used one time, to hold (and own) the original strings from the input equation.
    For each attempt to permute this equation, a new EquationBuilder object is constructed whose
    anchor term is only a reference to the original term, and the two permuted fields are
    constructed (during the swapping operation) and kept around only long enough to test the newly
    built equation for validity. The net effect is that no strings are duplicated needlessly

    I've factored the prefix permuting into its own structure PairPrefixSwapper. Rather than using
    two nested for loops for the core loop, the iteration is a legit implementation of Iterator for
    this structure, so the main search loop is now a foreach instead of nested fors

    The runtime is showing 0.00s on the server, but the original one did as well. We can only
    assume it runs even faster this time because there is no superfluous copying going on, though
    the kattis servers can't help us confirm this

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


// An equation (invalid) from the input stored as owned string components
struct Equation {
    arg1:      String,
    arg2:      String,
    operation: char,
    result:    String
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

    // the anchor is the term that is not changing throughout the permutations.
    // return the other two parts that are
    fn parts_other_than(&self, anchor: &Part) -> (&str, &str) {
        match anchor {
            Part::Arg1   => (&self.arg2, &self.result),
            Part::Arg2   => (&self.arg1, &self.result),
            Part::Result => (&self.arg1, &self.arg2)
        }
    }
}

// The parts of an equation that can be edited (ie, the operation always stays the same)
enum Part {
    Arg1,
    Arg2,
    Result
}


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
    
    // the parts of the equation are just another list of things to iterate over
    let parts = vec![ Part::Arg1,
                      Part::Arg2,
                      Part::Result ];

    // the anchor is the term that doesn't change when going through permutations
    for anchor in parts {

        // get the two terms that aren't the anchor
        let (term1, term2) = equation.parts_other_than(&anchor);

        // permute the two terms' prefixes with each other
        let swapper = PairPrefixSwapper::new(term1, term2);

        for (left, right) in swapper {

            // build and try this particular prefix swapping
            let built = EquationBuilder::new(&anchor, &left, &right, &equation);

            if built.is_valid_equation() {
                return Some(built.render())
            }
        }
    }

    None
}

struct EquationBuilder<'a> {
    arg1:      &'a str,
    arg2:      &'a str,
    operation: char,
    result:    &'a str
}

impl<'a> EquationBuilder<'a> {

    fn new(anchor: &'a Part, left: &'a str, right: &'a str, equation: &'a Equation) -> Self {
        
        // it's a bit clumsy building the new equation this way (ie, without lenses) but it's
        // clear what it's doing: for the anchor term, it borrows the same term from the underlying
        // equation, then it uses the left/right permuted terms to fill in the other two fields.
        // the operation is copied from the underlying equation but it's a stack-based char so
        // we lose no efficiency
        match anchor {
            Part::Arg1 => EquationBuilder { arg1: &equation.arg1,
                                            arg2: left,
                                            result: right,
                                            operation: equation.operation
                                          },

            Part::Arg2 => EquationBuilder { arg1: left,
                                            arg2: &equation.arg2,
                                            result: right,
                                            operation: equation.operation
                                          },

            Part::Result => EquationBuilder { arg1: left,
                                              arg2: right,
                                              result: &equation.result,
                                              operation: equation.operation
                                            }
        }
    }
    
    fn is_valid_equation(&self) -> bool {

        let num1 = self.arg1.parse::<i64>().unwrap();
        let num2 = self.arg2.parse::<i64>().unwrap();
        let res  = self.result.parse::<i64>().unwrap();

        match self.operation {
            '+' => num1 + num2 == res,
            '*' => num1 * num2 == res,
             _  => panic!("Unknown operation")
        }
    }

    // Convert back to string for the solution checker
    fn render(&self) -> String {
        format!("{} {} {} = {}", self.arg1,
                                 self.operation,
                                 self.arg2,
                                 self.result)
    }
}

struct PairPrefixSwapper<'a> {
    left:  &'a str,
    right: &'a str,

    // remember where we are in the permutation space between calls to next()
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
    
    // Swap the prefixes of two strings at a certain index in each. This function is identical
    // to the one in the original solution
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

    // each iteration returns a pair of strings (with their prefixes freshly swapped)
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

        // move these newly constructed owned strings into the caller of .next()
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
