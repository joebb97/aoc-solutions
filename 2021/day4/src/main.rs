use std::io::{self, BufRead, StdinLock};
#[derive(Debug)]
struct Tile {
    datum: u8,
    called: bool,
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
    println!("{:?}", boards[0]);
    return boards;
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
    println!("{:?}", nums_called);
    let boards = get_boards(lines);
    // let mut num_lines = 0;
    // let mut len: Option<usize> = None;
    // let counts: Option<Vec<u32>> = lines
    // .fold(None, |counts,  line| {
    //     let line = line.unwrap();
    //     num_lines += 1;
    //     if let None = len {
    //         len = Some(line.len());
    //     }
    //     line.chars().enumerate().fold(counts, |mut counts, (idx, c)| {
    //         if let Some(ref mut counts) = counts {
    //             counts[idx] += c.to_digit(2).unwrap();
    //         } else {
    //             counts = Some(vec![0; len.unwrap()]);
    //         }
    //         counts
    //     })
    // });
    // if let Some(counts) = counts {
    //     let half = num_lines / 2;
    //     let bit_map = counts.iter().map(|count| {
    //         count > &half
    //     });
    //     let gamma: u64 = bit_map.clone().fold(0, |gamma, bit| {
    //         (gamma << 1) ^ u64::from(bit)
    //     });
    //     let epsilon: u64 = bit_map.fold(0, |gamma, bit| {
    //         (gamma << 1) ^ u64::from(!bit)
    //     });
    //     println!("ans = gamma * epsilon = {} * {} = {}",
    //              gamma, epsilon, gamma * epsilon);
    // }
    Ok(())
}
