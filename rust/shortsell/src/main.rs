/*  https://open.kattis.com/problems/shortsell */

use std::io::{BufRead, Lines, StdinLock};

struct Input {
    k      : i32,      // cost per day of borrowing
    prices : Vec<i32>  // daily price of currency
}

struct Result {
    from   : usize,
    to     : usize,
    open   : i32,
    close  : i32,
    profit : i32,
    days   : usize
}

fn main() {

    // parse the problem input
    let stdin    = std::io::stdin();
    let lines    = stdin.lock().lines();
    let problem  = parse(lines);

    let result: Result = do_case(&problem);
    
    println!("{}", result.profit);
}

fn calc_profit(k : i32, short_price: i32, cover_price: i32, days_borrowed: usize) -> i32 {

    // from the problem description, we're always borrowing 100 shares
    const SHARES: i32 = 100;

    (SHARES*short_price) - (SHARES*cover_price) - (k * days_borrowed as i32)
}

fn do_case(problem: &Input) -> Result {

    let days = problem.prices.len();

    let mut result = Result {
        from: 0,
        to: 0,
        open: 0,
        close: 0, 
        profit: 0,
        days: 0
    };

    for short in 0 .. days-1 {
    for cover in short+1 .. days {
    
        let short_price = problem.prices[short];
        let cover_price = problem.prices[cover];
        let days_borrowed = cover - short + 1;

        if short_price < cover_price {
            continue
        }

        let this_profit = calc_profit(problem.k,
                                      short_price,
                                      cover_price,
                                      days_borrowed);

        if this_profit > result.profit {
            result.profit = this_profit;
            result.open = short_price;
            result.close = cover_price;
            result.from = short;
            result.to = cover;
            result.days = days_borrowed;
        }
    }
    }

    result
}


fn parse(mut lines: Lines<StdinLock>) -> Input {

    let line = lines.next()
                    .unwrap()
                    .unwrap();

    let split : Vec<&str> = line.split_whitespace().skip(1).collect();
    let k     : i32       = split[0].parse().unwrap();

    let prices = lines.next()
                      .unwrap()
                      .unwrap()
                      .split_whitespace()
                      .map(|s| s.parse().unwrap())
                      .collect();

    Input {
        k,
        prices
    }
}


#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_sample_1() {
        let input = Input {
            k: 10,
            prices: vec![1000, 980, 960, 940, 10]
        };

        assert_eq!(98950, do_case(&input).profit);
    }

    #[test]
    fn test_sample_2() {
        let input = Input {
            k: 100,
            prices: vec![100, 100, 100, 103, 100]
        };

        assert_eq!(100, do_case(&input).profit);
    }

    /*
        λ> let f k  open close d = (100*open) - (100*close) - (d*k)
        λ>     f 10 1000 10    5
        98950
    */

    #[test]
    fn test_calc_profit() {
        assert_eq!(98950, calc_profit(10, 1000, 10, 5));
    }

    #[test]
    fn test_assumption_1() {

        // assumption 1: the best trade starts at a local maximum and ends at a local minimum
        let k = 966;

        let result = do_case(&(Input {
            k,
            prices: vec![30, 20, 1]
        }));

        assert_eq!(100*(30-1) - 3*k, result.profit);
    }

    #[test]
    fn test_sample_1_changed() {
        let delta = 20;
        let input = Input {
            k: 10,
            prices: vec![1000,
                         1000-delta*1,
                         1000-delta*2,
                         1000-delta*3,
                         10]
        };

        //assert_eq!(98950, do_case(&input).profit);
        assert_eq!(0, do_case(&input).from);
        assert_eq!(4, do_case(&input).to);
    }      
}
