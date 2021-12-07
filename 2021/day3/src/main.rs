use std::io::{self, BufRead};

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
                counts[idx] += c.to_digit(2).unwrap();
            } else {
                counts = Some(vec![0; len.unwrap()]);
            }
            counts
        })
    });
    if let Some(counts) = counts {
        let half = num_lines / 2;
        let bit_map = counts.iter().map(|count| {
            count > &half
        });
        let gamma: u64 = bit_map.clone().fold(0, |gamma, bit| {
            (gamma << 1) ^ u64::from(bit)
        });
        let epsilon: u64 = bit_map.fold(0, |gamma, bit| {
            (gamma << 1) ^ u64::from(!bit)
        });
        println!("ans = gamma * epsilon = {} * {} = {}", 
                 gamma, epsilon, gamma * epsilon);
    }
    Ok(())
}
