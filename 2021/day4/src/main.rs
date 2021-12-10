use std::io::{self, BufRead, StdinLock};

#[derive(Debug)]
struct Tile {
    datum: u8,
    called: bool,
}

struct Coord {
    x: u32,
    y: u32,
}

type Board = Vec<Vec<Tile>>;
type Boards = Vec<Board>;

fn get_boards(lines: std::io::Lines<StdinLock<'_>>) -> Boards {
    let mut board_length: Option<usize> = None;
    struct FoldState {
        cur_board: Board,
        all_boards: Boards,
    }
    let starting_state = FoldState {
        cur_board: Board::new(),
        all_boards: Boards::new(),
    };
    let boards: Boards = lines
        .fold(starting_state, |mut state, line| {
            if let Ok(ref line) = line {
                if line == "" {
                    return state;
                }
                if let None = board_length {
                    board_length = Some(line.len());
                }
                let tile_vec: Vec<Tile> = line
                    .split_whitespace()
                    .map(|c| Tile {
                        datum: c.parse().unwrap(),
                        called: false,
                    })
                    .collect();
                state.cur_board.push(tile_vec);
                if state.cur_board.len() == board_length.unwrap() {
                    let mut new_state = FoldState {
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

fn apply_moves(nums_called: Vec<u8>, boards: &mut Boards) -> (u32, u32) {
    let (unmarked, winning_num) = (0, 0);
    let new_boards = nums_called.iter().fold(boards, |cur_boards, num_called| {
        cur_boards[0][0][0].called = true;
        cur_boards
    });
    (unmarked, winning_num)
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
    let mut boards = get_boards(lines);
    let (unmarked, winning_num) = apply_moves(nums_called, &mut boards);
    println!("ans = unmarked * winning_num = {} * {} = {}", unmarked, winning_num,
             unmarked * winning_num);
    Ok(())
}
