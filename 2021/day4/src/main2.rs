use std::collections::HashMap;
use std::io::{self, BufRead, StdinLock};

#[derive(Debug)]
struct Tile {
    row: u32,
    col: u32,
    called: bool,
}

// map from bingo value to tile for quick lookups
type TilesMap = HashMap<u8, Tile>;
enum Board {
    Unsolved(TilesMap),
    Done,
}
type Boards = Vec<Board>;

fn get_boards(lines: std::io::Lines<StdinLock<'_>>) -> Boards {
    let mut board_length: Option<usize> = None;
    struct FoldState {
        cur_board: Board,
        all_boards: Boards,
        cur_row: u32,
    }
    let starting_state = FoldState {
        cur_board: Board::Unsolved(TilesMap::new()),
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
                        if let Board::Unsolved(ref mut b) = state.cur_board {
                            let new_tile = Tile {
                                row: state.cur_row,
                                col: y as u32,
                                called: false,
                            };
                            b.insert(datum, new_tile);
                        }
                    });
                state.cur_row += 1;
                if let None = board_length {
                    if let Board::Unsolved(ref b) = state.cur_board {
                        let len = b.len();
                        board_length = Some(len * len);
                    }
                }
                let done = match state.cur_board {
                    Board::Unsolved(ref b) => b.len() == board_length.unwrap(),
                    _ => false,
                };
                if done {
                    let mut new_state = FoldState {
                        cur_row: 0,
                        cur_board: Board::Unsolved(TilesMap::new()),
                        all_boards: state.all_boards,
                    };
                    new_state.all_boards.push(state.cur_board);
                    return new_state;
                }
            }
            return state;
        })
        .all_boards;
    return boards;
}

fn apply_move<'a>(num_called: &'a u8, boards: &'a mut Boards) -> Vec<&'a mut Board> {
    boards
        .iter_mut()
        .fold(Vec::new(), |mut winning_board, board| match board {
            Board::Unsolved(b) => {
                if let Some(ent) = b.get_mut(num_called) {
                    ent.called = true;
                }
                if let Some(ent) = b.get(num_called) {
                    let same_row_bingo = b
                        .values()
                        .filter(|tile| tile.row == ent.row)
                        .all(|t| t.called);
                    let same_col_bingo = b
                        .values()
                        .filter(|tile| tile.col == ent.col)
                        .all(|t| t.called);
                    if same_col_bingo || same_row_bingo {
                        winning_board.push(board);
                    }
                }
                winning_board
            }
            _ => winning_board,
        })
}

fn apply_moves(nums_called: Vec<u8>, mut boards: Boards) -> (u32, u32) {
    let mut finished_boards: usize = 0;
    let ans: Option<(u32, u32)> = nums_called.iter().fold(None, |cur_ans, num_called| {
        if cur_ans.is_some() && finished_boards == boards.len() {
            return cur_ans;
        }
        let mut winning_boards = apply_move(num_called, &mut boards);
        let mut ret: Option<(u32, u32)> = cur_ans;
        winning_boards.iter_mut().for_each(|wb| {
            if let Board::Unsolved(b) = *wb {
                finished_boards += 1;
                let unmarked_sum: u32 = b
                    .iter()
                    .filter(|(_, v)| !v.called)
                    .map(|tup| *tup.0 as u32)
                    .sum();
                ret = Some((unmarked_sum, *num_called as u32));
            }
            **wb = Board::Done;
        });
        ret
    });
    println!("{} {} {:?}", boards.len(), finished_boards, ans);
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
