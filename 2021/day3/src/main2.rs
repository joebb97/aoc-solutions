use std::cmp::Ordering;
use std::error::Error;
use std::io::{self, BufRead};
type IoLines = Vec<Result<String, std::io::Error>>;

fn main() -> Result<(), Box<dyn Error>> {
    let stdin = io::stdin();
    let lines: IoLines = stdin.lock().lines().collect();
    let counts = get_counts(&lines);
    let half = (lines.len() / 2) as u32;
    println!("counts {:?}, half {}", counts, half);
    let ideal: String = counts.iter().map(|count| {
        if count >= &half {
            '1' 
        } else {
            '0'
        }
    }).collect();
    println!("ideal {} = {}", ideal, ascii_bin_string_to_int(&ideal));
    let oxygen = prune_lines(&counts, &lines, |o: Ordering| o.is_eq(), '1');
    let co2 = prune_lines(&counts, &lines, |o: Ordering| o.is_ne(), '0');
    println!("oxygen {:?}, co2 {:?}", oxygen, co2);
    let oxygen = ascii_bin_string_to_int(&oxygen[0]);
    let co2 = ascii_bin_string_to_int(&co2[0]);
    println!("oxygen = {}, co2 = {}, prod = {}", oxygen, co2, oxygen * co2);
    Ok(())
}

fn get_counts(lines: &IoLines) -> Vec<u32> {
    let mut len: Option<usize> = None;
    let counts: Option<Vec<u32>> = lines.iter().fold(None, |counts, line| {
        let line = line.as_ref().unwrap();
        if let None = len {
            len = Some(line.len());
        }
        line.chars()
            .enumerate()
            .fold(counts, |mut counts, (idx, c)| {
                if let None = counts {
                    counts = Some(vec![0; len.unwrap()]);
                }
                if let Some(ref mut counts) = counts {
                    counts[idx] += c.to_digit(10).unwrap();
                }
                counts
            })
    });
    counts.unwrap()
}

fn ascii_bin_string_to_int(ascii_string: &String) -> u32 {
    ascii_string
        .chars()
        .fold(0, |acc, c| (acc << 1) ^ (c.to_digit(2).unwrap() as u32))
}

fn prune_lines<'a>(
    counts: &'a Vec<u32>,
    lines: &'a IoLines,
    cmp_fn: fn(Ordering) -> bool,
    tiebreaker: char,
) -> Vec<&'a String> {
    let half = (lines.len() / 2) as u32;
    let bit_map = counts.iter().map(|count| count.cmp(&half));
    let lines_copy: Vec<&String> = lines.iter().map(|res| res.as_ref().unwrap()).collect();
    bit_map
        .clone()
        .enumerate()
        .fold(lines_copy, |mut cur, (idx, ord)| {
            if cur.len() == 1 {
                return cur;
            }
            cur.retain(|line| {
                let target = match ord {
                    Ordering::Less => '0',
                    Ordering::Equal => tiebreaker,
                    Ordering::Greater => '1',
                };
                let line_char = line.as_bytes()[idx] as char;
                return cmp_fn(line_char.cmp(&target));
            });
            cur
        })
}
