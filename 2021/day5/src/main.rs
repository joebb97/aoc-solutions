use itertools::Itertools;
use std::cmp::{max, min};
use std::collections::HashMap;
use std::io::{self, BufRead, StdinLock};
use std::ops::{Mul, Sub};

#[derive(Debug, Copy, Clone, Eq, PartialEq, Hash)]
struct Coord(i32, i32);

type VentLine = (Coord, Coord);
type VentLines = Vec<VentLine>;

impl Sub for Coord {
    type Output = Self;

    fn sub(self, other: Self) -> Self::Output {
        Self(self.0 - other.0, self.1 - other.1)
    }
}

impl Mul for Coord {
    type Output = i32;
    fn mul(self, rhs: Self) -> Self::Output {
        (self.0 * rhs.1) - (rhs.0 * self.1)
    }
}

fn direction(pi: Coord, pj: Coord, pk: Coord) -> i32 {
    (pk - pi) * (pj - pi)
}

fn on_segment(Coord(xi, yi): Coord, Coord(xj, yj): Coord, Coord(xk, yk): Coord) -> bool {
    let min_xi_xj = min(xi, xj);
    let max_xi_xj = max(xi, xj);
    let min_yi_yj = min(yi, yj);
    let max_yi_yj = max(yi, yj);
    (min_xi_xj <= xk && xk <= max_xi_xj) && (min_yi_yj <= yk && yk <= max_yi_yj)
}

fn segments_intersect(p1: Coord, p2: Coord, p3: Coord, p4: Coord) -> bool {
    let d1 = direction(p3, p4, p1);
    let d2 = direction(p3, p4, p2);
    let d3 = direction(p1, p2, p3);
    let d4 = direction(p1, p2, p4);
    if ((d1 > 0 && d2 < 0) || (d1 < 0 && d2 > 0)) && ((d3 > 0 && d4 < 0) || (d3 < 0 && d4 > 0)) {
        true
    } else if d1 == 0 && on_segment(p3, p4, p1) {
        true
    } else if d2 == 0 && on_segment(p3, p4, p2) {
        true
    } else if d3 == 0 && on_segment(p1, p2, p3) {
        true
    } else if d4 == 0 && on_segment(p1, p2, p4) {
        true
    } else {
        false
    }
}

fn all_points_on_line(l: &VentLine) -> Vec<Coord> {
    let l_type = get_line_type(&l);
    let (p1, p2) = l;
    let (Coord(x1, y1), Coord(x2, y2)) = (p1, p2);
    let y_max = max(y1, y2);
    let y_min = min(y1, y2);
    let x_max = max(x1, x2);
    let x_min = min(x1, x2);
    match l_type {
        LineType::Vert => {
            let l_diff = y_max - y_min;
            (0..l_diff + 1)
                .map(|offset| Coord(p1.0, y_min + offset))
                .collect()
        }
        LineType::Horiz => {
            let l_diff = x_max - x_min;
            (0..l_diff + 1)
                .map(|offset| Coord(x_min + offset, p1.1))
                .collect()
        }
        LineType::Diag => {
            let steps = x_max - x_min;
            let x_dir = if x2 > x1 {1} else {-1};
            let y_dir = if y2 > y1 {1} else {-1};
            let slope = 1;
            let ret = (0..steps + 1)
                .map(|step| {
                    let x_offset = slope * step * x_dir;
                    let y_offset = slope * step * y_dir;
                    Coord(x1 + x_offset, y1 + y_offset)
                })
                .collect();
            // println!("Diag line {:?} points are {:?}", l, ret);
            ret
        }
        LineType::Unknown => {
            vec![*p1, *p2]
        }
    }
}

#[derive(Clone, Copy)]
struct PointInfo(u32, bool);

fn update_counts_impl(
    mut cur_counts: HashMap<Coord, PointInfo>,
    (coord, point_info): (&Coord, &PointInfo),
) -> HashMap<Coord, PointInfo> {
    match cur_counts.get_mut(coord) {
        Some(&mut existing_point_info) => {
            let new_point_info = match (point_info, existing_point_info) {
                (PointInfo(new_marks, false), PointInfo(old_marks, true)) => {
                    PointInfo(old_marks + new_marks, true)
                }
                (PointInfo(new_marks, true), PointInfo(old_marks, false)) => {
                    PointInfo(new_marks + old_marks, true)
                },
                (_, PointInfo(_, true)) => existing_point_info,
                _ => existing_point_info,
            };
            cur_counts.insert(*coord, new_point_info);
        }
        None => {
            cur_counts.insert(*coord, *point_info);
        }
    }
    cur_counts
}

fn num_intersections(lines: VentLines) -> usize {
    let counts: HashMap<Coord, PointInfo> = HashMap::new();
    lines
        .iter()
        .enumerate()
        .flat_map(|(idx, coord)| std::iter::repeat(coord).zip(lines.iter().skip(idx + 1)))
        .fold(counts, |counts, (&l1, &l2)| {
            let (p1, p2) = l1;
            let (p3, p4) = l2;
            if segments_intersect(p1, p2, p3, p4) {
                let point_intersections: HashMap<Coord, PointInfo> = HashMap::new();
                let point_intersections = all_points_on_line(&l1)
                    .iter()
                    .chain(all_points_on_line(&l2).iter())
                    .fold(point_intersections, |mut cur_dict, point| {
                        let entry = cur_dict.entry(*point).or_insert(PointInfo(0, false));
                        let PointInfo(marks, is_cross) = entry;
                        *marks += 1;
                        if *marks >= 2 {
                            *is_cross = true
                        }
                        cur_dict
                    });
                return point_intersections.iter().fold(counts, update_counts_impl);
            }
            counts
        })
        .values()
        .filter(|&count| count.0 >= 2 && count.1)
        .count()
}

#[derive(PartialEq, Eq)]
enum LineType {
    Vert,
    Horiz,
    Diag,
    Unknown,
}

fn get_line_type(l: &VentLine) -> LineType {
    let Coord(x1, y1) = l.0;
    let Coord(x2, y2) = l.1;
    if x1 == x2 {
        LineType::Vert
    } else if y1 == y2 {
        LineType::Horiz
    } else if (y2 - y1).abs() == (x2 - x1).abs() {
        LineType::Diag
    } else {
        LineType::Unknown
    }
}

fn line_to_coords(std_line: &String) -> Option<VentLine> {
    std_line
        .split_whitespace()
        .take(3)
        .map(|l| l.to_string())
        .enumerate()
        .filter_map(|(idx, chunk)| {
            if idx != 1 {
                // -> in the middle of the line
                let (x, y) = chunk
                    .split(",")
                    .map(|c| c.parse::<i32>().unwrap())
                    .next_tuple::<(i32, i32)>()
                    .unwrap();
                Some(Coord(x, y))
            } else {
                None
            }
        })
        .next_tuple()
}

fn get_vent_lines(std_lines: std::io::Lines<StdinLock<'_>>) -> VentLines {
    std_lines.fold(VentLines::new(), |mut state, line| {
        if let Ok(ref line) = line {
            if line == "" {
                return state;
            }
            if let Some(coords) = line_to_coords(&line) {
                let lt = get_line_type(&coords);
                match lt {
                    LineType::Diag | LineType::Vert | LineType::Horiz => state.push(coords),
                    _ => (),
                };
            }
        }
        state
    })
}

fn main() -> Result<(), ()> {
    let stdin = io::stdin();
    let stdin_lines = stdin.lock().lines();
    let vent_lines = get_vent_lines(stdin_lines);
    println!("ans = {}", num_intersections(vent_lines));
    Ok(())
}
