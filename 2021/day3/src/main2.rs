use std::io::{self, BufRead};
use std::error::Error;

fn main() -> Result<(), Box<dyn Error>>{
    let stdin = io::stdin();
    let lines: Vec<_> = stdin.lock().lines().collect();
    let mut num_lines = 0;
    let mut len: Option<usize> = None;
    Ok(())
}

fn get_counts(lines: Vec<Result<String, std::io::Error>>) -> Vec<u32> {
    let counts: Option<Vec<u32>> = lines
    .iter().fold(None, |counts,  line| {
        let line = line.as_ref().unwrap();
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
}

fn handle_counts(counts: Vec<u32>) {
    if let Some(counts) = counts {
        let half = num_lines / 2;
        let bit_map = counts.iter().map(|count| {
            (count > &half) as u32
        });
        println!("{:?}", bit_map);
        let co2: Vec<&String> = lines.iter().map(|res| res.as_ref().unwrap()).collect();
        let co2 = bit_map.clone().enumerate().fold(co2, |mut cur, (idx, bit)|{
            if cur.len() == 1 {
                return cur
            }
            cur.retain(|line| {
                let c = line.as_bytes()[idx] as char;
                return c.to_digit(10).unwrap() == (bit as u32)
            });
            cur
        });
        println!("{:?}", co2);
        let co2: &String = &co2[0];
        let co2: u64 = co2.chars().fold(0, |co2, c| {
            (co2 << 1) ^ (c.to_digit(2).unwrap() as u64)
        });
        let oxygen: Vec<&String> = lines.iter().map(|res| res.as_ref().unwrap()).collect();
        let oxygen = bit_map.enumerate().fold(oxygen, |mut cur, (idx, bit)|{
            if cur.len() == 1 {
                return cur
            }
            cur.retain(|line| {
                let c = line.as_bytes()[idx] as char;
                return c.to_digit(10).unwrap() != (bit as u32)
            });
            cur
        });
        println!("{:?}", oxygen);
        let oxygen: &String = &oxygen[0];
        let oxygen: u64 = oxygen.chars().fold(0, |oxygen, c| {
            (oxygen << 1) ^ (c.to_digit(2).unwrap() as u64)
        });
        // let gamma: u64 = bit_map.clone().fold(0, |gamma, bit| {
        //     (gamma << 1) ^ u64::from(bit)
        // });
        // let epsilon: u64 = bit_map.fold(0, |gamma, bit| {
        //     (gamma << 1) ^ u64::from(!bit)
        // });
        println!("ans = oxygen * co2 = {} * {} = {}", 
                 oxygen, co2, oxygen * co2);
    }

}
