use std::io::{self, BufRead};
use std::collections::HashMap;

fn main() {
    io::stdin()
        .lock()
        .lines()
        .map(|line| {
            let line = line.unwrap();
            let len = line.len();
            let bytes = line.as_bytes();
            let mut counts = HashMap::new();
            for (idx, ch) in bytes.iter().enumerate() {
                let ch = *ch as char;
                counts.entry(ch).and_modify(|counter| *counter += 1).or_insert(1);
                if idx < 4 {
                    continue
                }
                let to_remove = bytes[idx - 4] as char;
                if let Some(count) = counts.get_mut(&to_remove) {
                    *count -= 1;
                    if *count == 0  {
                        counts.remove(&to_remove);
                    }
                }
                if counts.len() == 4 {
                    let ans = idx + 1;
                    return (line, ans);
                }
            }
            (line, len)
        })
        .for_each(|res| println!("{res:?}"))
}
