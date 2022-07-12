/*  https://open.kattis.com/problems/bradspelet  */

fn main() {
    let input = get_board_from_stdin();
                
    if is_winner(input) {
        println!("A")
    } else {
        println!("B")
    }
}

fn is_winner(board: Board) -> bool {

    // allocate the worst-case array, with +1 so we can emulate 1-based indices
    let mut table = [[false; 100+1]; 100+1];
       
    for n in 1..=board.n {
    for m in n..=board.m {
        
        if n == 1 {
            if m == 1 {
                
                // base case
                table[1][1] = false;

            } else {
                for i in 1..=(m/2) {
                    if table[1][i] == false && table[1][m-i] == false {
                        table[n][m] = true;
                        table[m][n] = true;
                        break
                    }
                }
            }
        } else {

            // horizontal cuts
            for i in 1..=(n/2) {
                if table[i][m] == false && table[n-i][m] == false {
                    table[n][m] = true;
                    table[m][n] = true;
                    break
                }
            }

            if n == m { continue }

            // vertical cuts
            for i in 1..=(m/2) {
                if table[n][i] == false && table[n][m-i] == false {
                    table[n][m] = true;
                    table[m][n] = true;
                    break
                }
            }
        }
    }
    }

    table[board.n][board.m]
}

struct Board {
    n : usize,
    m : usize,
}

impl Board {
    fn new(n: usize, m: usize) -> Self {
        // make sure n <= m during construction
        if n > m {
            Board { n: m, m: n }
        } else {
            Board { n, m }
        }
    }
}


/* Parsing */

fn get_board_from_stdin() -> Board {
    use std::io::BufRead;
    
    let     stdin  = std::io::stdin();
    let mut lines  = stdin.lock().lines(); 

    let line = lines.next().unwrap().unwrap();
    let ints = line.split_whitespace()
                   .map(|s| s.parse().unwrap())
                   .collect::<Vec<usize>>();

    Board::new(ints[0], ints[1])
}


/* Tests */

#[cfg(test)]
mod tests {
    use super::*;

    #[test] fn test_sample_1() { assert_eq!(true , is_winner(Board::new(2, 3))); }
    #[test] fn test_sample_2() { assert_eq!(false, is_winner(Board::new(2, 6))); }
    #[test] fn test_sample_3() { assert_eq!(true , is_winner(Board::new(6, 8))); }  

    // probing inputs
    #[test] fn test_1_1() { assert_eq!(false, is_winner(Board::new(1, 1))); }
    #[test] fn test_1_2() { assert_eq!(true , is_winner(Board::new(1, 2))); }
    #[test] fn test_2_2() { assert_eq!(false, is_winner(Board::new(2, 2))); }
    #[test] fn test_1_3() { assert_eq!(false, is_winner(Board::new(1, 3))); }
    #[test] fn test_3_3() { assert_eq!(false, is_winner(Board::new(3, 3))); }
    #[test] fn test_3_2() { assert_eq!(true , is_winner(Board::new(3, 2))); }
}
