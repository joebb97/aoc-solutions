use std::io::{self, BufRead};

fn main() -> Result<(), ()>{
    let (depth, horiz) = (0, 0);
    let (depth, horiz) = io::stdin().lock().lines()
    .fold((depth, horiz), |(depth, horiz), line| {
        let line = line.unwrap();
        let line: Vec<&str> = line.split_whitespace().collect();
        let (instr, amt): (&str, i32) = (line[0], line[1].parse().unwrap());
        let new_depth = match instr {
            "up" => depth - amt,
            "down" => depth + amt,
            _ => depth
        };
        let new_horiz = match instr {
            "forward" => horiz + amt,
            _ => horiz
        };
        (new_depth, new_horiz)
    });
    println!("ans = depth * horiz = {} * {} = {}", 
             depth, horiz, depth * horiz);
    Ok(())
}
