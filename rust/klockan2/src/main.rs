/*  https://open.kattis.com/problems/klockan2  */

use std::io::BufRead;

fn main() {
    let     stdin  = std::io::stdin();
    let mut lines  = stdin.lock().lines(); 

    let angle : i32  = lines.next().unwrap().unwrap()
                            .parse().unwrap();

    println!("{}", do_case(angle));
}

fn do_case(mut angle: i32) -> String {
    
    let mods = vec![0, 30, 5, 35, 10, 40, 15, 45, 20, 50, 25, 35];
    
    let index = mods.iter()
                    .position(|&m| m == angle % 55)
                    .unwrap();
        
    angle += 3600 * index as i32;
    
    let total_minutes = angle / 55;
    
    let hours = total_minutes / 60;
    let mins  = total_minutes % 60;
    
    format!("{:02}:{:02}", hours, mins)
}


/* Tests */

#[cfg(test)]
mod tests {
    use super::*;

    #[test] fn test_sample_1() { assert_eq!("01:21", do_case(855)); }
    #[test] fn test_sample_2() { assert_eq!("03:08", do_case(3140)); }

    #[test] fn test_30() { assert_eq!("01:06", do_case(30)); }
    #[test] fn test_0()  { assert_eq!("00:00", do_case(0)); }
}
