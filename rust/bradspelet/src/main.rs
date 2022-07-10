/*  https://open.kattis.com/problems/bradspelet  */

fn main() {
    let input = get_board_from_stdin();
                
    if is_winner(input) {
        println!("A")
    } else {
        println!("B")
    }
}

struct Board {
    n : i32,
    m : i32,
}

impl Board {
    fn new(n: i32, m: i32) -> Self {
        // make sure n < m during construction
        if n > m {
            Board { n: m, m: n }
        } else {
            Board { n, m }
        }
    }
}

fn is_winner(board: Board) -> bool {

    // we know 1 <= n, m <= 100 so we can allocate the worst-case array ahead of time
    let mut cache : [Option<bool>; 101*101] = [None; 101*101];

    winsfrom(board, &mut cache)
}

fn winsfrom(board: Board,
            cache: &mut [Option<bool>]) -> bool {

    if board.n == 1 && board.m == 1 {
        return false
    }

    let cuts = get_cuts(board);

    // the player wins with a block if they can cut it in such a way the other player can't win
    // with either of the remaining two blocks
    for (b1, b2) in cuts {
        if losesfrom(b1, cache) && losesfrom(b2, cache) {
            return true
        }
    }

    false
}

fn losesfrom(board: Board,
             cache: &mut [Option<bool>]) -> bool {
    
    let index = (100*board.n + board.m) as usize;

    match cache[index] {
        Some(result) => result,
        None => {
            let result = !winsfrom(board, cache);
            cache[index] = Some(result);
            result
        }
    }
}

// this could be cached as well, we're doing a lot of repeat calls to get_cuts for the same n,m
fn get_cuts(board: Board) -> Vec<(Board, Board)> {
    
    let mut cuts = vec![];

    // horizontal cuts
    for across in 1..=(board.n / 2) {
        cuts.push((Board::new(across, board.m),
                   Board::new(board.n - across, board.m)));
    }

    // vertical cuts if we have a rectangle
    if board.n != board.m {
        for down in 1..=(board.m / 2) {
            cuts.push((Board::new(board.n, down),
                       Board::new(board.n, board.m - down)));
        }
    }

    cuts
}


/* Parsing */

fn get_board_from_stdin() -> Board {
    use std::io::BufRead;
    
    let     stdin  = std::io::stdin();
    let mut lines  = stdin.lock().lines(); 

    let line = lines.next().unwrap().unwrap();
    let ints = line.split_whitespace()
                   .map(|s| s.parse().unwrap())
                   .collect::<Vec<i32>>();

    Board::new(ints[0], ints[1])
}


/* Tests */

#[cfg(test)]
mod tests {
    use super::*;

    #[test] fn test_sample_1() { assert_eq!(true , is_winner(Board::new(2, 3))); }
    #[test] fn test_sample_2() { assert_eq!(false, is_winner(Board::new(2, 6))); }
    #[test] fn test_sample_3() { assert_eq!(true , is_winner(Board::new(6, 8))); }

    #[test] fn test_cuts_12() { assert_eq!(vec![((1,1), (1,1))], boards_to_tuples(get_cuts(Board::new(1, 2)))); }
    #[test] fn test_cuts_22() { assert_eq!(vec![((1,2), (1,2))], boards_to_tuples(get_cuts(Board::new(2, 2)))); }
    #[test] fn test_cuts_23() { assert_eq!(vec![((1,3), (1,3)), ((1,2),(2,2))], boards_to_tuples(get_cuts(Board::new(2, 3)))); }
    #[test] fn test_cuts_32() { assert_eq!(vec![((1,3), (1,3)), ((1,2),(2,2))], boards_to_tuples(get_cuts(Board::new(3, 2)))); }

    // put this here for the test asserts instead of cluttering the Board struct
    fn boards_to_tuples(bs: Vec<(Board, Board)>) -> Vec<((i32,i32), (i32,i32))> {
        bs.iter()
          .map(|(b1, b2)| ((b1.n, b1.m), (b2.n, b2.m)))
          .collect()
    }
}
