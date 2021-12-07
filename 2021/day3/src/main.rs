use std::io::{self, BufRead};
use bitvec::prelude::*;

fn main() -> Result<(), ()>{
    let stdin = io::stdin();
    let lines = stdin.lock().lines();
    let mut num_lines = 0;
    let mut len: Option<usize> = None;
    let counts: Option<Vec<u32>> = lines
    .fold(None, |counts,  line| {
        let line = line.unwrap();
        num_lines += 1;
        if let None = len {
            len = Some(line.len());
        }
        line.chars().enumerate().fold(counts, |mut counts, (idx, c)| {
            if let Some(ref mut counts) = counts {
                counts[idx] += c.to_digit(10).unwrap();
            } else {
                counts = Some(vec![0; len.unwrap()]);
            }
            counts
        })
    });
    println!("{:?} {:?} {}", counts, len, num_lines);
    // println!("ans = gamma * epsilon = {} * {} = {}", 
    //          gamma, epsilon, gamma * epsilon);
    Ok(())
}
