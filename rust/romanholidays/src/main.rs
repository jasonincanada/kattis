/*  https://open.kattis.com/problems/romanholidays  */

use std::io::BufRead;

use roman_numbers::{get_before_m, get_after_m};
mod roman_numbers;

fn main() {

    let     stdin  = std::io::stdin();
    let mut lines  = stdin.lock().lines(); 

    // skip the first line
    lines.next();

    let before_m = get_before_m();
    let after_m  = get_after_m();

    for line in lines {
        let number = line.unwrap().parse().unwrap();

        match do_case(number, &before_m, &after_m) {
            Result::FromStart(offset) => println!( "{}", offset),
            Result::FromEnd(offset)   => println!("-{}", offset)
        }
    }
}

fn do_case(number:    u32,
           before_m: &[&'static str],
           after_m:  &[&'static str]) -> Result {

    let thousands = (number / 1000) as usize;
    let remainder = (number % 1000) as u32;

    // if we have an even multiple of 1000, a basic calculation suffices instead of a lookup
    if remainder == 0 {
        return Result::FromStart(before_m.len() * thousands
                                                + thousands)
    }
    
    let roman = roman(remainder);
    let first = roman.chars().next().unwrap();

    // if the first non-M letter is a letter "less" than M (alphabetically less)
    if "CDIL".contains(first) {
        let offset = before_m.binary_search_by(|r| (*r).cmp(&roman)).unwrap();

        Result::FromStart(thousands * (before_m.len() + 1) + offset + 1)
    } 
    
    // the first non-M letter is greater than M
    else {
        let offset = after_m.binary_search_by(|r| (*r).cmp(&roman).reverse()).unwrap();

        Result::FromEnd(thousands * after_m.len() + offset + 1)
    }

}

#[derive(Debug, PartialEq)]
enum Result {
    FromStart(usize),
    FromEnd(usize)
}

fn roman(mut decimal: u32) -> String {

    let table = vec![
       (1000, "M"),
       (900, "CM"),
       (500, "D"),
       (400, "CD"),
       (100, "C"),
       (90, "XC"),
       (50, "L"),
       (40, "XL"),
       (10, "X"),
       (9, "IX"),
       (5, "V"),
       (4, "IV"),
       (1, "I")
    ];

    let mut roman = String::new();

    while decimal > 0 {        
        for (level, numerals) in table.iter() {
            if decimal >= *level {
                roman.push_str(numerals);
                decimal -= level;                
                break;
            }
        }                
    }

    roman
}


#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_do_case()
    {
        let before_m = get_before_m();
        let after_m  = get_after_m();

        // offset from start of ordering
        assert_eq!(do_case(100, &before_m, &after_m), Result::FromStart(1));        // C
        assert_eq!(do_case(200, &before_m, &after_m), Result::FromStart(2));        // CC
        assert_eq!(do_case(101, &before_m, &after_m), Result::FromStart(302));      // CI
        assert_eq!(do_case(102, &before_m, &after_m), Result::FromStart(303));      // CII
        assert_eq!(do_case(188, &before_m, &after_m), Result::FromStart(346));      // CLXXXVIII
        assert_eq!(do_case(900, &before_m, &after_m), Result::FromStart(346+1));    // CM
        assert_eq!(do_case(901, &before_m, &after_m), Result::FromStart(346+2));    // CMI
        assert_eq!(do_case(500, &before_m, &after_m), Result::FromStart(501));      // D
        assert_eq!(do_case(1  , &before_m, &after_m), Result::FromStart(901));      // I
        assert_eq!(do_case(2  , &before_m, &after_m), Result::FromStart(902));      // II
        assert_eq!(do_case(87 , &before_m, &after_m), Result::FromStart(944));      // LXXXVII
        assert_eq!(do_case(88 , &before_m, &after_m), Result::FromStart(945));      // LXXXVIII

        // M
        assert_eq!(do_case(1000, &before_m, &after_m), Result::FromStart(before_m.len() + 1));
        assert_eq!(do_case(1100, &before_m, &after_m), Result::FromStart(before_m.len() + 2)); // MC

        // MM
        assert_eq!(do_case(2000, &before_m, &after_m), Result::FromStart(before_m.len() + before_m.len() + 1 + 1));

        // offset from end of ordering
        assert_eq!(do_case(1037, &before_m, &after_m), Result::FromEnd(after_m.len() + 2));  // MXXXVII
        assert_eq!(do_case(1038, &before_m, &after_m), Result::FromEnd(after_m.len() + 1));  // MXXXVIII
        assert_eq!(do_case(5   , &before_m, &after_m), Result::FromEnd(after_m.len()    ));  // V
        assert_eq!(do_case(37  , &before_m, &after_m), Result::FromEnd(2));                  // XXXVII
        assert_eq!(do_case(38  , &before_m, &after_m), Result::FromEnd(1));                  // XXXVIII    
    }

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
