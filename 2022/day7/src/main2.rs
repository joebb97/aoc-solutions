use std::io::{self, BufRead};

#[derive(Debug)]
struct File {
    name: String,
    size: u32,
}

#[derive(Debug)]
struct Directory {
    name: String,
    files: Vec<File>,
    children: Vec<u32>,
    parent: Option<u32>,
}

#[derive(Debug)]
enum Command {
    LS,
    CD(String),
    File(File),
    Directory(Directory),
}

fn parse_line(line: String) -> Command {
    let split = line.split_whitespace().collect::<Vec<_>>();
    if split[0] == "$" {
        if split[1] == "cd" {
            let dirname = split[2];
            return Command::CD(dirname.to_string());
        }
        if split[1] == "ls" {
            return Command::LS;
        }
    }
    if split[0] == "dir" {
        let dirname = split[1];
        let directory = Directory {
            name: dirname.to_string(),
            children: Vec::new(),
            files: Vec::new(),
            parent: None,
        };
        return Command::Directory(directory);
    }
    let file_size = split[0].parse().unwrap();
    let file_name = split[1];
    let file = File {
        name: file_name.to_string(),
        size: file_size,
    };
    Command::File(file)
}

fn traverse_fs(
    fs: &Vec<Directory>,
    root: &Directory,
    padding: u32,
    running_total: &mut u32,
    target: u32,
) -> u32 {
    let padding_string = (0..padding).map(|_| ' ').collect::<String>();
    let mut dir_total = 0;
    // println!("{}{}", padding_string, root.name);
    for file in root.files.iter() {
        // println!("  {padding_string}{file:?} ");
        dir_total += file.size;
    }
    // println!();
    for dir_idx in root.children.iter() {
        dir_total += traverse_fs(fs, &fs[*dir_idx as usize], padding + 4, running_total, target);
    }
    // println!("{dir_total}");
    if dir_total >= target && (*running_total == 0 || dir_total < *running_total) {
        *running_total = dir_total;
    }
    dir_total
}

fn main() {
    let mut lines_iter = io::stdin().lock().lines().map(|line| line.unwrap());
    // skip cd /
    lines_iter.next();
    let mut fs = vec![Directory {
        name: "/".to_string(),
        children: Vec::new(),
        files: Vec::new(),
        parent: None,
    }];
    let mut next_idx = 0;
    let mut cd = 0;
    for line in lines_iter {
        let command = parse_line(line);
        // println!("{command:?}");
        match command {
            Command::LS => {}
            Command::File(file) => {
                fs[cd].files.push(file);
            }
            Command::CD(dir_name) => {
                if dir_name == ".." {
                    // println!("{:?}", fs[cd]);
                    let parent_idx = fs[cd].parent;
                    cd = parent_idx.unwrap() as usize;
                    continue;
                }
                for child_idx in fs[cd].children.iter() {
                    if fs[*child_idx as usize].name == dir_name {
                        cd = *child_idx as usize;
                        break;
                    }
                }
            }
            Command::Directory(mut dir) => {
                next_idx += 1;
                fs[cd].children.push(next_idx);
                dir.parent = Some(cd as u32);
                fs.push(dir);
            }
        }
    }
    let mut running_total = 0;
    let root_space = traverse_fs(&fs, &fs[0], 0, &mut running_total, 0);
    let target = root_space - 40000000;
    let mut ans = 0;
    traverse_fs(&fs, &fs[0], 0, &mut ans, target);
    println!("target = {target} , ans = {ans}");
}
