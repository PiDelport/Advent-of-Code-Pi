//! <https://adventofcode.com/2021/day/1>

pub type Depth = u32;

use crate::parse_lines;

pub fn example() -> Vec<Depth> {
    parse_lines(include_str!(
        "../../../../aoc-input-files/2021/day-1/example.txt"
    ))
}

pub fn input() -> Vec<Depth> {
    parse_lines(include_str!(
        "../../../../aoc-input-files/2021/day-1/input.txt"
    ))
}
