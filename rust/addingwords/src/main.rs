/*  https://open.kattis.com/problems/addingwords

    This has a 0.01s runtime which isn't enough to get onto the Rust leaderboard, which currently
    has five solutions submitted at 0.00s. Where to find those precious few cycles...
*/

use std::collections::HashMap;
use std::io::BufRead;

fn main() {
    let stdin = std::io::stdin();
    let mut input = stdin.lock().lines();

    // collect our variables and values in a hashmap
    let mut vars = HashMap::new();
    
    while let Some(line) = input.next() {
        let inner = line.unwrap();
        let split = inner.split(" ");
        let tokens : Vec<&str> = split.collect();

        match tokens[0] {
            "def"   => def(&mut vars,
                           tokens[1].to_owned(),
                           tokens[2].parse().unwrap()),

            "calc"  => println!("{} {}",
                                tokens[1..].join(" "),

                                match calc(&vars, &tokens[1..]) {
                                    Some(result) => result,
                                    None         => "unknown".to_owned()
                                }),

            "clear" => clear(&mut vars),
             _      => panic!("Unknown line type")
        };
    }
}

// define a variable
fn def(vars:  &mut HashMap<String, i32>,
       var:   String,
       value: i32)
{
    vars.insert(var, value);
}

// attempt a calculation
fn calc(vars:   &HashMap<String, i32>,
        tokens: &[&str]) -> Option<String>
{
    let mut accum : i32;

    // the first variable doesn't have an operation yet, start our accumulator with it
    match vars.get(tokens[0]) {
        Some(val) => accum = *val,
        None      => return None
    }

    let mut i = 1;

    loop {
        if tokens[i] == "=" {
            match find_var_for_value(&vars, accum) {
                Some(var) => return Some(var.to_owned()),
                None      => return None
            }
        }

        match vars.get(tokens[i+1]) {
            Some(val) => match tokens[i] {
                             "+" => accum += *val,
                             "-" => accum -= *val,
                              _  => panic!("calc(): Unknown operation")
                         },

                 None => return None
        }

        // done with this variable name and its operation
        i += 2;
    }
}

// clear the variable map
fn clear(vars: &mut HashMap<String, i32>) {
    vars.clear()
}

// reverse dictionary lookup from: https://stackoverflow.com/a/59401721/229717
fn find_var_for_value<'a>(map: &'a HashMap<String, i32>, value: i32) -> Option<&'a String> {
    map.iter()
       .find_map(|(key, &val)| if val == value { Some(key) } else { None })
}
