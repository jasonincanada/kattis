/*  https://open.kattis.com/problems/alphabetanimals  */

use std::io::BufRead;

fn main() {
    let     stdin = std::io::stdin();
    let mut input = stdin.lock().lines();

    let last_animal = input.next().unwrap().unwrap();
    let last_letter = last_char(&last_animal);

    // skip the next line which is the number of lines to follow
    input.next();

    // track which letters we've seen at the start of words
    // this list of bools is O(1) to work with instead of a HashSet at O(log n)
    let mut seen : Vec<bool> = vec![false; 'z' as usize + 1];

    // keep only the words that start with the last letter of the previous word
    let mut starters : Vec<String> = Vec::new();

    // one animal per line
    while let Some(line) = input.next() {
        let animal = line.unwrap();
        let first_letter = first_char(&animal);

        seen[first_letter as usize] = true;

        if first_letter == last_letter {
            starters.push(animal);
        }
    }

    if starters.is_empty() {
        println!("?");
        return
    }

    if starters.len() == 1 {

        let starter = starters.first().unwrap();

        // special case where we have one candidate word that ends with the letter it starts with,
        // this word leaves no options for the next player after removing itself
        if first_char(starter) == last_char(starter) {
            println!("{}!", starter);
            return
        }
    }

    // pass through our candidate list to find the first candidate word that ends
    // with a letter not seen at the beginning of any word
    for test in starters.iter() {
        let last = last_char(test);

        if !seen[last as usize] {
            println!("{}!", test);
            return
        }
    }

    // no such luck, just output the first starter word we saw
    println!("{}", starters.first().unwrap());
}

fn first_char(string: &str) -> char {
    string.chars().nth(0).unwrap()
}

fn last_char(string: &str) -> char {
    string.chars().rev().nth(0).unwrap()
}
