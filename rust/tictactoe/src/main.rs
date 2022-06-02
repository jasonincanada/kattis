/*  https://open.kattis.com/problems/tictactoecounting - Tic Tac Toe Counting

    This solution returns the wrong answer for test 3 (of 15) on the kattis servers. My first
    attempt was too slow to complete the 2nd test even with 3 seconds to run. I overcame this by
    caching the results of the gameplay simulations, after noticing there were up to 100,000 boards
    in a single test, but only 19,683 possible starting configurations (3^9). However there is
    still some error somewhere in my reasoning since it fails on the 3rd test. I've spent a few
    days on this already so I'm moving on to another challenge for now

*/

#![allow(clippy::match_like_matches_macro)]

use std::io::BufRead;
use std::collections::HashMap;

fn main() {

    // parse the problem input
    let     stdin  = std::io::stdin();
    let mut lines  = stdin.lock().lines();

    lines.next();   // skip the first line

    let mut cache : HashMap<String, Result> = HashMap::new();

    for line in lines {
        let board = line.unwrap();
        
        let result = match cache.get(&board) {
            Some(r) => r.clone(),
            None => {
                let r = do_case(board.clone());
                cache.insert(board, r.clone());
                r
            }
        };

        match result {
            Result::Wins(x, o)  => println!("{} {}", x, o),
            Result::Unreachable => println!("-1 -1")
        }
    }
}

fn do_case(board: String) -> Result {

    let mut board = TicTacToe::new(&board);

    match board.get_board_state() {

        // the game board they gave us is already in a winning/drawn state
        GameState::Winner(Player::X) => Result::Wins(1, 0),
        GameState::Winner(Player::O) => Result::Wins(0, 1),
        GameState::Draw              => Result::Wins(0, 0),
        
        // start playing all possible games from here
        GameState::ToMove(player)    => {
            let (wins_x, wins_y) = play_games(&mut board, player);

            Result::Wins(wins_x, wins_y)
        },

        // the board specified is in an unreachable state
        GameState::Invalid => Result::Unreachable,
    }
}

fn play_games(board: &mut TicTacToe, player: Player) -> (i32, i32) {
    
    let mut wins_x = 0;
    let mut wins_o = 0;

    let cells = board.get_empty_cells();

    // try each empty cell in turn, first marking it, recursing on the rest of the board
    // for the other player's turn, then unmarking the cell when the recursive call returns
    for cell in cells {
        board.mark(cell, player);

        if board.is_winning_mark(cell) {
            match board.cells[cell] {
                Cell::MarkedBy(Player::X) => wins_x += 1,
                Cell::MarkedBy(Player::O) => wins_o += 1,
                Cell::Empty => panic!("shouldn't get here"),
            }
        } else {
            // recursive call to start the process over again, with the other player to move,
            // with one less available square
            let (sub_wins_x, sub_wins_o) = play_games(board, next_player(player));

            wins_x += sub_wins_x;
            wins_o += sub_wins_o;
        }

        // undo this move so we can try the next cell
        board.unmark(cell);
    }

    (wins_x, wins_o)
}

fn next_player(player: Player) -> Player {
    match player {
        Player::X => Player::O,
        Player::O => Player::X
    }
}


#[derive(Clone, Debug, PartialEq)]
enum Result {
    Wins(i32, i32),
    Unreachable
}

#[derive(Clone, Copy, Debug, PartialEq)]
enum Player {
    X,
    O
}

#[derive(Clone, Copy, PartialEq)]
enum Cell {
    MarkedBy(Player),
    Empty
}

#[derive(Debug, PartialEq)]
enum GameState {
    Invalid,
    Winner(Player),
    ToMove(Player),
    Draw,
}

struct TicTacToe {
    cells      : Vec<Cell>,

    // static data, computed once during construction
    paths      : Vec<Vec<usize>>,
    relevant   : Vec<Vec<usize>>
}

impl TicTacToe {

    // construct a new, not necessarily valid, board from an input file line. the order
    // the moves were played to get to this configuration is not known
    fn new(line : &str) -> Self {

        // these are the 8 possible lines to draw through the tic tac toe board checking
        // for a win. not all of them need to be searched for any given mark
        let paths = vec![
            // three across
            vec![0,1,2],        // index 0
            vec![3,4,5],        // 1
            vec![6,7,8],        // 2

            // three down
            vec![0,3,6],        // 3
            vec![1,4,7],        // 4
            vec![2,5,8],        // 5

            // diagonals
            vec![0,4,8],        // 6
            vec![2,4,6]         // 7
        ];

        // these numbers index into the paths vector above. the first row indicates that if
        // the top-left (cell 0) cell was marked last, we only need to check paths 0, 3, and 6
        // for a win (the lines with cell 0 somewhere in them)
        let relevant = vec![
            vec![0,3,6],        // cell 0
            vec![0,4],          // 1
            vec![0,5,7],        // 2

            vec![1,3],          // 3
            vec![1,4,6,7],      // 4
            vec![1,5],          // 5

            vec![2,3,7],        // 6
            vec![2,4],          // 7
            vec![2,5,6]         // 8
        ];

        TicTacToe {
            cells: line.chars()
                       .map(|c| match c {
                                    'X' => Cell::MarkedBy(Player::X),
                                    'O' => Cell::MarkedBy(Player::O),
                                     _  => Cell::Empty
                                })
                       .collect(),

            paths,
            relevant
        }
    }

    // called once after the board is constructed to determine our first action for this board,
    // but not called after each move (only the relevant paths are checked for a winning line)
    fn get_board_state(&self) -> GameState {
        let mut lines : Vec<Vec<Cell>> = vec![];

        for cs in &self.paths {
            let line = vec![
                self.cells[cs[0] as usize],
                self.cells[cs[1] as usize],
                self.cells[cs[2] as usize]
            ];

            lines.push(line);
        }
     
        let judged : Vec<Option<Player>> = lines.into_iter()
                                                .map(TicTacToe::judge)
                                                .collect();

        let num_wins_x = judged.iter()
                               .filter(|c| match c {            // the matches![] macro would
                                   Some(Player::X) => true,     // make this cleaner but the
                                   _ => false                   // servers don't support it
                               })
                               .count();

        let num_wins_o = judged.iter()
                               .filter(|c| match c {
                                   Some(Player::O) => true,
                                   _ => false
                               })
                               .count();

        let num_x_cells = self.cells
                              .iter()
                              .filter(|c| match c {
                                  Cell::MarkedBy(Player::X) => true,
                                  _ => false
                              })
                              .count();

        let num_o_cells = self.cells
                              .iter()
                              .filter(|c| match c {
                                  Cell::MarkedBy(Player::O) => true,
                                  _ => false
                              })
                              .count();

        // players can't both have winning lines
        if num_wins_x > 0 && num_wins_o > 0 {
             return GameState::Invalid
        }

        if (num_x_cells as i64 - num_o_cells as i64).abs() > 1 {
            return GameState::Invalid
        }

        if num_wins_x >= 1 {            
            return GameState::Winner(Player::X)
        }
       
        if num_wins_o >= 1 {
            return GameState::Winner(Player::O)
        }

        if num_x_cells + num_o_cells == 9 {
            return GameState::Draw
        }

        if num_x_cells == num_o_cells + 1 {
            return GameState::ToMove(Player::O)
        }

        if num_x_cells == num_o_cells {
            return GameState::ToMove(Player::X)
        }

        GameState::Invalid
    }

    fn judge(cells : Vec<Cell>) -> Option<Player> {

        if cells[0] == Cell::MarkedBy(Player::X)
        && cells[1] == Cell::MarkedBy(Player::X)
        && cells[2] == Cell::MarkedBy(Player::X) {
            return Some(Player::X)
        }

        if cells[0] == Cell::MarkedBy(Player::O)
        && cells[1] == Cell::MarkedBy(Player::O)
        && cells[2] == Cell::MarkedBy(Player::O) {
            return Some(Player::O)
        }

        None
    }

    fn get_empty_cells(&self) -> Vec<usize> {
        self.cells.iter()
                  .enumerate()
                  .filter(|(_, cell)| match cell { Cell::Empty => true, _ => false })
                  .map(|(i, _)| i)
                  .collect()
    }
  
    fn mark(&mut self, cell: usize, player: Player) {
        self.cells[cell] = Cell::MarkedBy(player);
    }

    // if the given cell was the latest mark placed on the board, did that player just win?
    fn is_winning_mark(&self, cell: usize) -> bool {
        let player = self.cells[cell];

        for line in self.relevant[cell].iter() {
            let mut complete = true;

            for cell in self.paths[*line].iter() {
                if self.cells[*cell] != player {
                    complete = false;
                    break;
                }
            }

            if complete {
                return true
            }
        }
            
        false
    }

    fn unmark(&mut self, cell: usize) {
        self.cells[cell] = Cell::Empty;
    }

}


#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_get_board_state_winner_x() {
        assert_eq!(TicTacToe::new("XOOX.OXO.").get_board_state(), GameState::Winner(Player::X))
    }

    #[test]
    fn test_get_board_state_o_to_move() {
        assert_eq!(TicTacToe::new("XX..O....").get_board_state(), GameState::ToMove(Player::O))
    }

    #[test]
    fn test_get_board_state_x_to_move() {
        assert_eq!(TicTacToe::new("X...O....").get_board_state(), GameState::ToMove(Player::X))
    }

    #[test]
    fn test_get_board_state_winner_o() {
        assert_eq!(TicTacToe::new("OOOX.X.X.").get_board_state(), GameState::Winner(Player::O))
    }

    #[test]
    fn test_get_board_state_invalid() {
        // can't have two players with winning lines at the same time
        assert_eq!(TicTacToe::new("OOOXXX...").get_board_state(), GameState::Invalid)
    }

    #[test]
    fn test_get_board_state_draw() {
        assert_eq!(TicTacToe::new("OXOOXXXOX").get_board_state(), GameState::Draw)
    }

    #[test]
    fn test_sample_1() {
        assert_eq!(do_case("XX..O....".to_owned()), Result::Wins(191, 194))
    }

    #[test]
    fn test_sample_2() {
        assert_eq!(do_case("X...OX...".to_owned()), Result::Wins(232, 200))
    }
        
    #[test]
    fn test_sample_3() {
        assert_eq!(do_case("OOOX.X.X.".to_owned()), Result::Wins(0, 1))
    }    
    
    #[test]
    fn test_sample_4() {
        assert_eq!(do_case("OOOXXX...".to_owned()), Result::Unreachable)
    }
    
    #[test]
    fn test_get_empty_cells() {
        let empties = TicTacToe::new("XOOX.OXO.").get_empty_cells();
        assert_eq!(vec![4,8], empties)
    }

    #[test]
    fn test_empty_board() {
        // numbers from: https://www.quora.com/What-is-the-probability-of-the-first-player-winning-in-Tic-Tac-Toe-as-well-as-the-second-one-winning/answer/Kshitij-Rastogi
        assert_eq!(do_case(".........".to_owned()), Result::Wins(131184, 77904))
    }

}
