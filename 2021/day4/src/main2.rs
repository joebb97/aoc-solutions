use std::cmp::Ordering;
use std::error::Error;
use std::io::{self, BufRead};
type IoLines = Vec<Result<String, std::io::Error>>;

fn main() -> Result<(), Box<dyn Error>> {
    let stdin = io::stdin();
    let lines: IoLines = stdin.lock().lines().collect();
    let oxygen = prune_lines(&lines, |o: Ordering| o.is_eq(), '1');
    let co2 = prune_lines(&lines, |o: Ordering| o.is_ne(), '0');
    println!("oxygen {:?}, co2 {:?}", oxygen, co2);
    let oxygen = ascii_bin_string_to_int(&oxygen[0]);
    let co2 = ascii_bin_string_to_int(&co2[0]);
    println!("oxygen = {}, co2 = {}, prod = {}", oxygen, co2, oxygen * co2);
    Ok(())
}

fn ascii_bin_string_to_int(ascii_string: &String) -> u32 {
    ascii_string
        .chars()
        .fold(0, |acc, c| (acc << 1) ^ (c.to_digit(2).unwrap() as u32))
}

fn get_ones_at_bit_idx(lines: &Vec<&String>, idx: usize) -> u32 {
    let ascii_one_as_byte = '1' as u8;
    let counts: u32 = lines.iter().fold(0, |counts, line| {
        let byte = line.as_bytes()[idx];
        let is_one = (byte == ascii_one_as_byte) as u32;
        counts + is_one
    });
    counts
}

fn prune_lines<'a>(
    lines: &'a IoLines,
    cmp_fn: fn(Ordering) -> bool,
    tiebreaker: char,
) -> Vec<&'a String> {
    let lines_copy: Vec<&String> = lines.iter().map(|res| res.as_ref().unwrap()).collect();
    let num_bits = lines_copy[0].len();
    (0..num_bits)
        .fold(lines_copy, |mut cur, bit_idx| {
            if cur.len() == 1 {
                return cur;
            }
            let num_ones = get_ones_at_bit_idx(&cur, bit_idx);
            let num_zeros = cur.len() as u32 - num_ones;
            cur.retain(|line| {
                let c = line.as_bytes()[bit_idx] as char;
                let ord = num_ones.cmp(&num_zeros);
                let target = match ord {
                    Ordering::Less => '0',
                    Ordering::Equal => {return c == tiebreaker},
                    Ordering::Greater => '1',
                };
                cmp_fn(c.cmp(&target))
            });
            // println!("bit_idx {}, ones {}, zeros {}, cur {:?}", bit_idx, num_ones, num_zeros, cur);
            cur
        })
}
