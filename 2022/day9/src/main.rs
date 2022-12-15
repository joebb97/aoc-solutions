use std::io::{self, BufRead};
use std::collections::HashSet;


fn main() {
    let head_moves = io::stdin()
        .lock()
        .lines()
        .map(|line| {
            let line = line.unwrap();
            let split: Vec<_> = line.split_whitespace().collect();
            let dir = split[0].as_bytes()[0] as char;
            let amt = split[1].parse().unwrap();
            (dir, amt)
        }).collect::<Vec<(char, i32)>>();
    let mut tail_visited: HashSet<(i32, i32)> = HashSet::new();
    let (mut tail_x, mut tail_y) = (0, 0);
    tail_visited.insert((tail_x, tail_y));
    let (mut head_x, mut head_y) = (0, 0);
    for (dir, amt) in head_moves.iter() {
        match dir {
            'R' => {
                for _ in 0..*amt {
                    head_x += 1;
                    if head_x == tail_x + 2 {
                        if tail_y == head_y + 1 || tail_y == head_y - 1 {
                            tail_y = head_y;
                        }
                        tail_x += 1;
                        tail_visited.insert((tail_x, tail_y));
                    }
                }
            },
            'U' => {
                for _ in 0..*amt {
                    head_y += 1;
                    if head_y == tail_y + 2 {
                        if tail_x == head_x + 1 || tail_x == head_x - 1 {
                            tail_x = head_x;
                        }
                        tail_y += 1;
                        tail_visited.insert((tail_x, tail_y));
                    }
                }
            },
            'L' => {
                for _ in 0..*amt {
                    head_x -= 1;
                    if head_x == tail_x - 2 {
                        if tail_y == head_y + 1 || tail_y == head_y - 1 {
                            tail_y = head_y;
                        }
                        tail_x -= 1;
                        tail_visited.insert((tail_x, tail_y));
                    }
                }
            },
            'D' => {
                for _ in 0..*amt {
                    head_y -= 1;
                    if head_y == tail_y - 2 {
                        if tail_x == head_x + 1 || tail_x == head_x - 1 {
                            tail_x = head_x;
                        }
                        tail_y -= 1;
                        tail_visited.insert((tail_x, tail_y));
                    }
                }
            },
            _ => {}
        }
    }
    println!("ans = {}", tail_visited.len());
}
