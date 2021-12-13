use std::collections::HashMap;
use std::io::{self, BufRead, StdinLock};

#[derive(Debug)]
struct Tile {
    row: u32,
    col: u32,
    called: bool,
}

type Board = HashMap<u8, Tile>;
// map from bingo value to tile for quick lookups
type Boards = Vec<Board>;

fn get_boards(lines: std::io::Lines<StdinLock<'_>>) -> Boards {
    let mut board_length: Option<(usize, usize)> = None;
    #[derive(Debug)]
    struct FoldState {
        cur_board: Board,
        all_boards: Boards,
        cur_row: u32,
    }
    let starting_state = FoldState {
        cur_board: Board::new(),
        all_boards: Boards::new(),
        cur_row: 0,
    };
    let boards: Boards = lines
        .fold(starting_state, |mut state, line| {
            if let Ok(ref line) = line {
                if line == "" {
                    return state;
                }
                line.split_whitespace()
                    .enumerate()
                    .map(|(y, c)| {
                        let datum = c.parse().unwrap();
                        (datum, y)
                    })
                    .for_each(|(datum, y)| {
                        let new_tile = Tile {
                            row: state.cur_row,
                            col: y as u32,
                            called: false,
                        };
                        state.cur_board.insert(datum, new_tile);
                    });
                state.cur_row += 1;
                if let None = board_length {
                    let len = state.cur_board.len();
                    board_length = Some((len, len * len));
                }
                if state.cur_board.len() == board_length.unwrap().1 {
                    let mut new_state = FoldState {
                        cur_row: 0,
                        cur_board: Board::new(),
                        all_boards: state.all_boards,
                    };
                    new_state.all_boards.push(state.cur_board);
                    new_state
                } else {
                    state
                }
            } else {
                state
            }
        })
        .all_boards;
    return boards;
}

fn apply_move<'a>(num_called: &'a u8, boards: &'a mut Boards) -> Option<&'a Board> {
    boards
        .iter_mut()
        .enumerate()
        .fold(None, |winning_board, (idx, board)| {
            if let Some(ref w) = winning_board {
                return winning_board;
            }
            if let Some(ent) = board.get_mut(num_called) {
                ent.called = true;
            }
            if let Some(ent) = board.get(num_called) {
                let same_row_bingo = board
                    .values()
                    .filter(|tile| tile.row == ent.row)
                    .all(|t| t.called);
                let same_col_bingo = board
                    .values()
                    .filter(|tile| tile.col == ent.col)
                    .all(|t| t.called);
                if same_col_bingo || same_row_bingo {
                    return Some(board);
                }
            }
            winning_board
        })
}

fn apply_moves(nums_called: Vec<u8>, mut boards: Boards) -> (u32, u32) {
    let ans: Option<(u32, u32)> = nums_called.iter().fold(None, |cur_ans, num_called| {
        if let Some(_) = cur_ans {
            return cur_ans;
        }
        let winning_board = apply_move(num_called, &mut boards);
        if let Some(wb) = winning_board {
            let unmarked_sum: u32 = wb
                .iter()
                .filter(|(_, v)| !v.called)
                .map(|tup| *tup.0 as u32)
                .sum();
            Some((unmarked_sum, *num_called as u32))
        } else {
            cur_ans
        }
    });
    ans.unwrap()
}

fn main() -> Result<(), ()> {
    let stdin = io::stdin();
    let mut lines = stdin.lock().lines();
    let nums_called: Vec<u8> = lines
        .next()
        .unwrap()
        .unwrap()
        .split(",")
        .map(|num_str| {
            let num: u8 = num_str.parse().unwrap();
            num
        })
        .collect();
    let boards = get_boards(lines);
    let (unmarked, winning_num) = apply_moves(nums_called, boards);
    println!(
        "ans = unmarked * winning_num = {} * {} = {}",
        unmarked,
        winning_num,
        unmarked * winning_num
    );
    Ok(())
}
