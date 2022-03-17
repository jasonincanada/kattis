/*  https://open.kattis.com/problems/sacredtexts  */

use std::io::{BufRead, StdinLock, Lines};

fn main() {
    let     stdin = std::io::stdin();
    let mut input = stdin.lock().lines();

    // first line
    let header = split_line(&mut input);
    let rune   = &header[0];
    let letter = header[1].chars().next().unwrap();

    let rune_score = score_rune(&rune) as u8;

    // 0 <= offset < 26
    let offset = rune_score - (letter as u8 - 'a' as u8 + 1);

    // rest of lines
    while let Some(line) = input.next() {
        let inner = line.unwrap();
        let runes : Vec<&str> = inner.split(' ').collect();

        for rune in runes {

            match rune {
                "0" => print!(" "),
                "<" => print!(","),
                ">" => print!("."),

                 r  => {
                    let letter = decode_rune(&r, offset);
                    print!("{}", letter);
                 }
            }
        }

        println!("");
    }
}

fn score_rune(rune: &str) -> u8 {
    let     first = '!' as u8;
    let mut score = 0;

    for ch in rune.chars() {
        score += ch as u8 - first + 1;
    }

    score
}

fn decode_rune(rune: &str, offset: u8) -> char {
    let     score = score_rune(rune);
    let mut code  = 'a' as u8 - offset - 1 + score;

    if code < 'a' as u8 {
        code += 26;
    } else if code > 'z' as u8 {
        code -= 26;
    }

    code as char
}

// expect another line from stdin and split it on whitespace
fn split_line(input: &mut Lines<StdinLock>) -> Vec<String> {
    let line = input.next().unwrap().unwrap();
    
    line.split(' ')
        .map(|s| s.to_string())
        .collect()
}


/* Tests */

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_score_rune() {
        assert_eq!(1,      score_rune("!"));
        assert_eq!(1+14+9, score_rune("!.)"));
    }

    #[test]
    fn test_decode_rune() {
        assert_eq!('a', decode_rune("!", 0));
        assert_eq!('z', decode_rune("!!!!!!!!!!!!!!!!!!!!!!!!!!", 0));

        assert_eq!('a', decode_rune("#",  2));
        assert_eq!('n', decode_rune("/!", 2));
        assert_eq!('y', decode_rune("!",  2));
        assert_eq!('z', decode_rune("\"", 2));
    }
}

/*  C:\Users\Jason\Documents\GitHub\kattis\rust\sacredtexts> type sample-2.txt | cargo run
    never gonna give you up.
    never gonna let you down.
    never gonna run around,
    and desert you.
    never gonna make you cry,
    never gonna say goodbye.
    never gonna tell a lie,
    and hurt you.
*/
