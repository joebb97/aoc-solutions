use std::io::{self, BufRead};

#[derive(Debug)]
struct Grid {
    grid: Vec<Vec<u32>>,
}

impl Grid {
    fn visible(&self, row: usize, col: usize) -> bool {
        let cell = self.grid[row][col];
        let mut visible_from_right = true;
        let mut visible_from_left = true;
        let mut visible_from_top = true;
        let mut visible_from_bot = true;
        for row_idx in row + 1..self.grid.len() {
            let sightline_cell = self.grid[row_idx][col];
            if sightline_cell >= cell {
                visible_from_bot = false;
            }
        }
        for row_idx in (0..row).rev() {
            let sightline_cell = self.grid[row_idx][col];
            if sightline_cell >= cell {
                visible_from_top = false;
            }
        }
        for col_idx in col + 1..self.grid[0].len() {
            let sightline_cell = self.grid[row][col_idx];
            if sightline_cell >= cell {
                visible_from_right = false;
            }
        }
        for col_idx in (0..col).rev() {
            let sightline_cell = self.grid[row][col_idx];
            if sightline_cell >= cell {
                visible_from_left = false;
            }
        }
        visible_from_bot || visible_from_top || visible_from_left || visible_from_right
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
                total += 1;
                continue;
            }
            let cell = grid.grid[row_idx][col_idx];
            let visible = grid.visible(row_idx, col_idx);
            if visible {
                // println!("({row_idx}, {col_idx}), {cell}");
                total += 1;
            }
        }
    }
    println!("ans = {total}");
}
