use std::io::{self, BufRead};

#[derive(Debug)]
struct Grid {
    grid: Vec<Vec<u32>>,
}

impl Grid {
    fn tree_score(&self, row: usize, col: usize) -> u32 {
        let cell = self.grid[row][col];
        let mut right_score = 0;
        let mut left_score = 0;
        let mut top_score = 0;
        let mut bot_score = 0;
        for row_idx in row + 1..self.grid.len() {
            let sightline_cell = self.grid[row_idx][col];
            bot_score += 1;
            if sightline_cell >= cell {
                break;
            }
        }
        for row_idx in (0..row).rev() {
            let sightline_cell = self.grid[row_idx][col];
            top_score += 1;
            if sightline_cell >= cell {
                break;
            }
        }
        for col_idx in col + 1..self.grid[0].len() {
            let sightline_cell = self.grid[row][col_idx];
            right_score += 1;
            if sightline_cell >= cell {
                break;
            }
        }
        for col_idx in (0..col).rev() {
            let sightline_cell = self.grid[row][col_idx];
            left_score += 1;
            if sightline_cell >= cell {
                break;
            }
        }
        right_score * left_score * bot_score * top_score
    }
}

fn main() {
    let grid = io::stdin()
        .lock()
        .lines()
        .map(|line| {
            line.unwrap()
                .chars()
                .map(|c| c.to_digit(10).unwrap())
                .collect::<Vec<u32>>()
        })
        .collect::<Vec<Vec<u32>>>();
    let grid = Grid { grid };
    let mut total = 0;
    for (row_idx, row) in grid.grid.iter().enumerate() {
        for (col_idx, _) in row.iter().enumerate() {
            if (row_idx == 0 || row_idx == grid.grid.len() - 1)
                || (col_idx == 0 || col_idx == grid.grid[0].len() - 1)
            {
                continue;
            }
            let tree_score = grid.tree_score(row_idx, col_idx);
            if tree_score > total {
                // println!("({row_idx}, {col_idx}), {cell}");
                total = tree_score;
            }
        }
    }
    println!("ans = {total}");
}
