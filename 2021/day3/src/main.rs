use std::io::{self, BufRead};

fn main() -> Result<(), ()>{
    let (gamma, epsilon): (u32, u32) = io::stdin().lock().lines()
    .reduce(|(depth, horiz), line| {
        let line = line.unwrap();
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
    println!("ans = gamma * epsilon = {} * {} = {}", 
             gamma, epsilon, gamma * epsilon);
    Ok(())
}
