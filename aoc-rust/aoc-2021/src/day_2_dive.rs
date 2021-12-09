//! Day 2: Dive!
//!
//! <https://adventofcode.com/2021/day/2>

use std::iter::Sum;
use std::ops::Add;

use aoc_inputs::inputs_2021::day_2::{Command, Unit};

#[derive(Eq, PartialEq, Default)]
pub struct SubmarineVector {
    horizontal_position: Unit,
    depth: Unit,
}

impl SubmarineVector {
    pub fn from_horizontal_position(horizontal_position: Unit) -> Self {
        Self {
            horizontal_position,
            depth: 0,
        }
    }
    pub fn from_depth(depth: Unit) -> Self {
        Self {
            horizontal_position: 0,
            depth,
        }
    }
}

impl From<&Command> for SubmarineVector {
    fn from(command: &Command) -> Self {
        match *command {
            Command::Forward(units) => Self::from_horizontal_position(units),
            Command::Down(units) => Self::from_depth(units),
            Command::Up(units) => Self::from_depth(-units),
        }
    }
}

impl Add for SubmarineVector {
    type Output = Self;

    fn add(self, rhs: Self) -> Self::Output {
        Self {
            horizontal_position: self.horizontal_position + rhs.horizontal_position,
            depth: self.depth + rhs.depth,
        }
    }
}

impl Sum for SubmarineVector {
    fn sum<I>(iter: I) -> Self
    where
        I: Iterator<Item = Self>,
    {
        iter.fold(SubmarineVector::default(), SubmarineVector::add)
    }
}

pub fn interpret(commands: &[Command]) -> SubmarineVector {
    commands.iter().map(SubmarineVector::from).sum()
}

pub fn part_1(commands: &[Command]) -> Unit {
    let final_vector = interpret(commands);
    final_vector.horizontal_position * final_vector.depth
}

#[derive(Eq, PartialEq, Default)]
pub struct SubmarineState {
    vector: SubmarineVector,
    aim: Unit,
}

impl SubmarineState {
    pub fn follow_command(self, command: &Command) -> Self {
        use Command::*;

        match *command {
            Forward(units) => Self {
                vector: self.vector
                    + SubmarineVector {
                        horizontal_position: units,
                        depth: self.aim * units,
                    },
                ..self
            },
            Down(units) => Self {
                aim: self.aim + units,
                ..self
            },
            Up(units) => Self {
                aim: self.aim - units,
                ..self
            },
        }
    }

    pub fn follow_commands(self, commands: &[Command]) -> Self {
        commands.iter().fold(self, Self::follow_command)
    }
}

pub fn part_2(commands: &[Command]) -> Unit {
    let final_state = SubmarineState::default().follow_commands(commands);
    let final_vector = final_state.vector;
    final_vector.horizontal_position * final_vector.depth
}

#[cfg(test)]
mod tests {
    use aoc_inputs::inputs_2021::day_2::{example, input};

    use super::*;

    #[test]
    fn test_part_1() {
        assert_eq!(part_1(&example()), 150);
        assert_eq!(part_1(&input()), 1990000);
    }

    #[test]
    fn test_part_2() {
        assert_eq!(part_2(&example()), 900);
        assert_eq!(part_2(&input()), 1975421260);
    }
}
