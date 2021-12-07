use std::io::{self, BufRead};

fn main() -> Result<(), ()>{
    let (depth, horiz, aim) = (0, 0, 0);
    let (depth, horiz, _) = io::stdin().lock().lines()
    .fold((depth, horiz, aim), |(depth, horiz,  aim), line| {
        let line = line.unwrap();
        let line: Vec<&str> = line.split_whitespace().collect();
        let (instr, amt): (&str, i32) = (line[0], line[1].parse().unwrap());
        let new_aim = match instr {
            "up" => aim - amt,
            "down" => aim + amt,
            _ => aim 
        };
        let new_horiz = match instr {
            "forward" => horiz + amt,
            _ => horiz
        };
        let new_depth = match instr {
            "forward" => depth + (aim * amt),
            _ => depth
        };
        (new_depth, new_horiz, new_aim)
    });
    println!("ans = depth * horiz = {} * {} = {}", 
             depth, horiz, depth * horiz);
    Ok(())
}
