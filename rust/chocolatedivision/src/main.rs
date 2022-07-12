/*  https://open.kattis.com/problems/chocolatedivision  */

fn main() {
    let input = get_board_from_stdin();
                
    if is_winner(input) {
        println!("Alf")
    } else {
        println!("Beata")
    }
}

fn is_winner(board: Board) -> bool {
    board.total_cuts() % 2 == 1
}

struct Board {
    n : u16,
    m : u16,
}

impl Board {
    fn new(n: u16, m: u16) -> Self {
        Board { n, m }        
    }

    fn total_cuts(&self) -> u16 {
        self.n * self.m - 1
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
                   .collect::<Vec<u16>>();

    Board::new(ints[0], ints[1])
}


/* Tests */

#[cfg(test)]
mod tests {
    use super::*;

    #[test] fn test_sample_1() { assert_eq!(false, is_winner(Board::new(1, 1))); }
    #[test] fn test_sample_2() { assert_eq!(true , is_winner(Board::new(1, 2))); }
    #[test] fn test_sample_3() { assert_eq!(true , is_winner(Board::new(2, 2))); }   
}
