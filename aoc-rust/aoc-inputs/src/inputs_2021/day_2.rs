//! <https://adventofcode.com/2021/day/2>

use std::ops::Deref;
use std::str::FromStr;

use anyhow::anyhow;

use crate::parse_lines;

pub type Unit = i32;

pub enum Command {
    Forward(Unit),
    Down(Unit),
    Up(Unit),
}

impl FromStr for Command {
    type Err = anyhow::Error;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        use crate::inputs_2021::day_2::Command::*;

        let parts: Box<[&str]> = s.split_whitespace().collect();
        match parts.deref() {
            ["forward", units] => Ok(Forward(units.parse()?)),
            ["down", units] => Ok(Down(units.parse()?)),
            ["up", units] => Ok(Up(units.parse()?)),
            _ => Err(anyhow!("Invalid command: {s}")),
        }
    }
}

pub fn example() -> Vec<Command> {
    parse_lines(include_str!(
        "../../../../aoc-input-files/2021/day-2/example.txt"
    ))
}

pub fn input() -> Vec<Command> {
    parse_lines(include_str!(
        "../../../../aoc-input-files/2021/day-2/input.txt"
    ))
}
