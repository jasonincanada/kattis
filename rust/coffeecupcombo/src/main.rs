/*  https://open.kattis.com/problems/coffeecupcombo  */

fn main() {    

    // read the bitstring from the second line of stdin
    let machines = {
        use std::io::BufRead;

        let     stdin = std::io::stdin();
        let mut lines = stdin.lock().lines();

        // skip the first line, return the second
        lines.next();    
        lines.next().unwrap().unwrap()
    };

    println!("{}", do_case(&machines));   
}

fn do_case(machines: &str) -> i32 {

    let mut lectures = 0;
    let mut holding  = 0;

    machines
        .chars()
        .for_each(|machine| {
            if machine == '1' {
                lectures += 1;
                holding = 2;
            } else if holding > 0 {
                lectures += 1;
                holding -= 1;
            }
        });    
  
    lectures
}


/* Tests */

#[cfg(test)]
mod tests {
    use super::*;

    #[test] fn test_sample_1() { assert_eq!(do_case("0100010100"), 8); }
    #[test] fn test_sample_2() { assert_eq!(do_case("1100000000"), 4); }
    #[test] fn test_sample_3() { assert_eq!(do_case("0"), 0); }
}
