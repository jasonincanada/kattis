/*  https://open.kattis.com/problems/romanholidays

*/

#![allow(dead_code, unused_variables)]

use std::io::BufRead;

fn main() {

      // parse the problem input
      let stdin  = std::io::stdin();
      let mut lines  = stdin.lock().lines(); 
      
      lines.next();

      for line in lines {
          let number = line.unwrap().parse().unwrap();

          match do_case(number) {
            Some(ordinal) => println!("{}", ordinal),
            None          => println!("-1")
        }
      }
}

fn do_case(number: u32) -> Option<u32> {
  
    None
}

fn roman(mut decimal: u32) -> String {

    let mut roman = String::new();

    while decimal > 0 {

        if decimal >= 1000 {
            roman.push('M');
            decimal -= 1000;
        }

        else if decimal >= 900 {
            roman.push_str("CM");
            decimal -= 900;
        }

        else if decimal >= 500 {
            roman.push_str("D");
            decimal -= 500;
        }

        else if decimal >= 400 {
            roman.push_str("CD");
            decimal -= 400;
        }

        else if decimal >= 100 {
            roman.push_str("C");
            decimal -= 100;
        }

        else if decimal >= 90 {
            roman.push_str("XC");
            decimal -= 90;
        }

        else if decimal >= 50 {
            roman.push_str("L");
            decimal -= 50;
        }
        
        else if decimal >= 10 {
            roman.push_str("X");
            decimal -= 10;
        }

        else if decimal >= 9 {
            roman.push_str("IX");
            decimal -= 9;
        }

        else if decimal >= 5 {
            roman.push_str("V");
            decimal -= 5;
        }

        else if decimal >= 4 {
            roman.push_str("IV");
            decimal -= 4;
        }
        
        else if decimal >= 1 {
            roman.push('I');
            decimal -= 1;
        }
      
        else {
            return "no-value".to_owned()
        }    
    }

    roman
}


#[cfg(test)]
mod tests {
    use super::*;
/*
    #[test] fn test_sample_1() { assert_eq!(do_case(100), Some(1)); }
    #[test] fn test_sample_2() { assert_eq!(do_case(101), Some(302)); }
    #[test] fn test_sample_3() { assert_eq!(do_case(38), None); }
 */

    #[test]
    fn test_roman() {
        assert_eq!(roman(1), "I");
        assert_eq!(roman(2), "II");
        assert_eq!(roman(3), "III");
        assert_eq!(roman(4), "IV");
        assert_eq!(roman(5), "V");
        assert_eq!(roman(6), "VI");
        assert_eq!(roman(7), "VII");
        assert_eq!(roman(8), "VIII");
        assert_eq!(roman(9), "IX");
        assert_eq!(roman(10), "X");

        assert_eq!(roman(11), "XI");
        assert_eq!(roman(12), "XII");
        assert_eq!(roman(13), "XIII");
        assert_eq!(roman(14), "XIV");
        assert_eq!(roman(15), "XV");
        assert_eq!(roman(16), "XVI");
        assert_eq!(roman(17), "XVII");
        assert_eq!(roman(18), "XVIII");
        assert_eq!(roman(19), "XIX");
        assert_eq!(roman(20), "XX");

        assert_eq!(roman(50), "L");
        assert_eq!(roman(60), "LX");

        assert_eq!(roman(89), "LXXXIX");
        assert_eq!(roman(99), "XCIX");

        assert_eq!(roman(100), "C");
        assert_eq!(roman(200), "CC");
        assert_eq!(roman(300), "CCC");
        assert_eq!(roman(400), "CD");
        assert_eq!(roman(499), "CDXCIX");
        assert_eq!(roman(500), "D");

        assert_eq!(roman(900), "CM");
        assert_eq!(roman(1000), "M");
    }
}
