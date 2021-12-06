use std::io::{self, BufRead};

fn main() -> Result<(), ()> {
    let (prev, start): (Option<i32>, i32) = (None, 0);
    let stdin = io::stdin();
    let lines = stdin.lock().lines();
    let lines: Vec<io::Result<String>> = lines.collect();
    let windows = lines.windows(3);
    let sums = windows.map(|trip| {
        let sum: i32 = trip.iter().map(|single_str| {
            let single_str = single_str.as_ref().unwrap();
            single_str.parse::<i32>().unwrap()
        }).sum();
        sum
    });
    let res = sums
    .fold((prev, start), |(prev, res), cur_sweep| {
        let new_prev = Some(cur_sweep);
        match prev {
            None => (new_prev, res),
            Some(prev) => {
                if cur_sweep > prev {
                    return (new_prev, res + 1)
                }
                (new_prev, res)
            }
        }
    }).1;
    println!("{}", res);
    Ok(())
}
