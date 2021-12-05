use std::io::{self, BufRead};

fn main() -> Result<(), ()>{
    let (prev, start): (Option<u32>, Result<u32, ()>) = (None, Ok(0));
    let res = io::stdin().lock().lines()
    .fold((prev, start), |(prev, res), cur_sweep| {
        let cur_sweep = cur_sweep.unwrap().parse().ok();
        match prev {
            None => (cur_sweep, res),
            Some(prev) => {
                let new_prev = cur_sweep;
                if let (Some(cur_sweep), Ok(res)) = (cur_sweep, res) {
                    if cur_sweep > prev {
                        return (new_prev, Ok(res + 1))
                    }
                }
                (new_prev, res)
            }
        }
    }).1?;
    println!("{}", res);
    Ok(())
}
