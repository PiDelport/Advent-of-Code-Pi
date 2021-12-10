//! Day 2: Dive!
//!
//! <https://adventofcode.com/2021/day/2>

pub mod input {
    use std::str::FromStr;

    use anyhow::anyhow;

    pub type Unit = i32;

    pub enum Command {
        Forward(Unit),
        Down(Unit),
        Up(Unit),
    }

    impl FromStr for Command {
        type Err = anyhow::Error;

        fn from_str(s: &str) -> Result<Self, Self::Err> {
            use Command::*;

            let parts: Box<[&str]> = s.split_whitespace().collect();
            match *parts {
                ["forward", units] => Ok(Forward(units.parse()?)),
                ["down", units] => Ok(Down(units.parse()?)),
                ["up", units] => Ok(Up(units.parse()?)),
                _ => Err(anyhow!("Invalid command: {s}")),
            }
        }
    }
}

pub mod part_1 {
    use std::iter::Sum;
    use std::ops::Add;

    use super::input::{Command, Unit};

    #[derive(Eq, PartialEq, Default)]
    pub struct SubmarineVector {
        pub(crate) horizontal_position: Unit,
        pub(crate) depth: Unit,
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
}

pub mod part_2 {
    use super::input::{Command, Unit};
    use super::part_1::SubmarineVector;

    #[derive(Eq, PartialEq, Default)]
    pub struct SubmarineState {
        pub(crate) vector: SubmarineVector,
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
}

#[cfg(test)]
mod tests {
    use aoc_inputs::inputs_2021::day_2::{EXAMPLE, INPUT};
    use aoc_inputs::parse_lines;

    use super::input::*;
    use super::part_1::*;
    use super::part_2::*;

    pub fn part_1(commands: &[Command]) -> Unit {
        let final_vector = interpret(commands);
        final_vector.horizontal_position * final_vector.depth
    }

    pub fn part_2(commands: &[Command]) -> Unit {
        let final_state = SubmarineState::default().follow_commands(commands);
        let final_vector = final_state.vector;
        final_vector.horizontal_position * final_vector.depth
    }

    #[test]
    fn test_part_1() {
        assert_eq!(part_1(&parse_lines(EXAMPLE)), 150);
        assert_eq!(part_1(&parse_lines(INPUT)), 1990000);
    }

    #[test]
    fn test_part_2() {
        assert_eq!(part_2(&parse_lines(EXAMPLE)), 900);
        assert_eq!(part_2(&parse_lines(INPUT)), 1975421260);
    }
}
