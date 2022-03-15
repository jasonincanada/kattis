/*  https://open.kattis.com/problems/riseandfall

    This solution passes some tests (the sample input and the first hidden server test) but fails
    on the second server test. My logic is described in the comments throughout the file, though I
    must have made an incorrect assumption somewhere. The 0.00s solutions tell me there's a really
    quick, probably linear pass through this. In fact, the below solution is streaming one byte at
    a time, so despite the scary sounding bounds in the problem description, this actually runs in
    constant memory (2 bytes! the current and prior bytes from the input). I have probably over-
    simplified somewhere in my reasoning
*/

use std::io::Read;

#[derive(PartialEq)]
enum Mode {
    FirstLine,          // passing through the first line of the input, which we ignore
    NewLine,            // the prior byte encountered was a newline \n
    AscendingFrom(u8),  // we're on the ascent, with the prior digit carried along
                        // so we can compare it to the current one
    RestOfLine(u8)      // the rest of the integer will be this digit
}

fn main() {    
    let stdin = std::io::stdin();
    let input = stdin.lock();
    
    let mut mode = Mode::FirstLine;

    // go through the input stream one byte at a time
    for byte in input.bytes() {
        let ch = byte.unwrap();

        // ignore windows line endings
        if ch == b'\r' {
            continue;
        }
        
        // handle unix line endings
        if ch == b'\n' {
            if mode != Mode::FirstLine {
                println!("");
            }

            mode = Mode::NewLine;
            continue;
        } 

        // we have a character that's not a newline, so it must be a digit
        match mode {

            // the first line is the header line noting how many lines are to follow,
            // but we're streaming the file one byte at a time, which will end at EOF.
            // so let these bytes be read in without doing anything
            Mode::FirstLine => (),

            // we have a character in NewLine mode meaning it's the first digit of a new
            // test case. start the potentially long trek upwards (or straightwards)
            Mode::NewLine => {
                print!("{}", ch as char);
                mode = Mode::AscendingFrom(ch);
            },

            Mode::AscendingFrom(prior) => {

                print!("{}", ch as char);

                // if this digit is greater than or equal to the prior one, we're
                // still ascending (or staying level)
                if prior <= ch {   

                    mode = Mode::AscendingFrom(ch);

                } else {

                    // important moment in this test case! we've encountered the first digit
                    // that is less than the prior one. the best we can do from here is to
                    // repeat this digit for the rest of the line. this will make the remainder
                    // of this integer as high as it can be without starting a new trek upwards
                    mode = Mode::RestOfLine(ch);
                }
            },

            // we're in this mode until a newline breaks us out of it
            Mode::RestOfLine(level) => {
                print!("{}", level as char)
            }
        }
    }
}

/*  C:\Users\Jason\Documents\GitHub\kattis\rust\riseandfall> type sample.txt | cargo run    
    29000
    56555
*/
