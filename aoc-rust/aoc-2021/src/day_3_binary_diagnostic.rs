//! Day 3: Binary Diagnostic
//!
//! <https://adventofcode.com/2021/day/3>

pub mod input_1 {
    //! For part 1, interpret each line as a list of weights.
    //!
    //! TODO: Look at changing this to a matrix representation,
    //!       to simplify summing the columns.

    use std::ops::Deref;
    use std::str::FromStr;

    use anyhow::anyhow;

    pub type Weight = i32;

    #[derive(Debug)]
    pub struct Weights(pub Box<[Weight]>);

    impl Weights {
        pub fn new(len: usize) -> Self {
            Self(vec![0; len].into_boxed_slice())
        }
    }

    impl Deref for Weights {
        type Target = [Weight];

        fn deref(&self) -> &Self::Target {
            self.0.deref()
        }
    }

    impl FromStr for Weights {
        type Err = anyhow::Error;

        fn from_str(s: &str) -> Result<Self, Self::Err> {
            let slice = s
                .chars()
                .map(|c| match c {
                    '0' => Ok(-1),
                    '1' => Ok(1),
                    invalid => Err(anyhow!("Invalid bit value: {invalid:?}")),
                })
                .collect::<Result<Box<[Weight]>, Self::Err>>()?;
            Ok(Self(slice))
        }
    }
}

pub mod part_1 {
    use std::iter::{zip, Sum};
    use std::ops::Add;

    use anyhow::{anyhow, Context};

    use super::input_1::*;

    /// The gamma / epsilon rate.
    pub type Rate = u64;

    impl Add for &Weights {
        type Output = Weights;

        fn add(self, rhs: Self) -> Self::Output {
            let lhs = self.as_ref();
            let rhs = rhs.as_ref();
            if lhs.len() == rhs.len() {
                Weights(
                    zip(lhs, rhs)
                        .map(|(&l, &r)| Weight::checked_add(l, r).unwrap())
                        .collect(),
                )
            } else {
                panic!("cannot add mismatched lengths: {lhs:?}, {rhs:?}")
            }
        }
    }

    impl<'a> Sum<&'a Weights> for Weights {
        fn sum<I: Iterator<Item = &'a Weights>>(iter: I) -> Self {
            let mut iter = iter.peekable();
            let first = iter
                .peek()
                .expect("summing weights requires at least one item");
            let init = Weights::new(first.0.len());
            iter.fold(init, |acc, r| &acc + r)
        }
    }

    /// Sum a collection of report weights to their combined value.
    pub fn combine_weights(report: &[Weights]) -> Weights {
        report.iter().sum()
    }

    /// Determine the gamma rate bit value for a single weight.
    fn gamma_bit_value(weight: Weight) -> anyhow::Result<Rate> {
        match weight.signum() {
            -1 => Ok(0),
            1 => Ok(1),
            0 => Err(anyhow!("ambiguous digit: {weight:?}")),
            otherwise => panic!("unexpected signum: {otherwise}"),
        }
    }

    /// Determine the rates.
    pub fn gamma_epsilon_rates(weights: &Weights) -> anyhow::Result<(Rate, Rate)> {
        let init: anyhow::Result<Rate> = Ok(0);
        let gamma = weights
            .iter()
            .map(|&w| gamma_bit_value(w))
            .fold(init, |acc, bit| {
                let acc = acc?;
                let acc = acc
                    .checked_shl(1)
                    .ok_or_else(|| anyhow!("unexpected overflow: {acc}"))?;
                Ok(acc + bit?)
            })?;

        // Assuming the gamma rate is defined, the epsilon rate should be the bitwise inversion
        let mask = 1_u64
            .checked_shl(weights.len() as u32)
            .context("overflow")?
            - 1;
        let epsilon = gamma ^ mask;

        Ok((gamma, epsilon))
    }
}

pub mod part_2 {
    // use super::input::*;
}

#[cfg(test)]
mod tests {
    use aoc_inputs::inputs_2021::day_3::{EXAMPLE, INPUT};
    use aoc_inputs::parse_lines;

    use super::input_1::*;
    use super::part_1::*;
    // use super::part_2::*;

    fn part_1(report: &[Weights]) -> Rate {
        let combined = &combine_weights(report);
        let (gamma, epsilon) = gamma_epsilon_rates(combined).unwrap();
        Rate::checked_mul(gamma, epsilon).unwrap_or_else(|| panic!("overflow: {gamma} * {epsilon}"))
    }

    fn part_2(_: &[Weights]) -> u32 {
        todo!()
    }

    #[test]
    fn test_part_1() {
        assert_eq!(part_1(&parse_lines(EXAMPLE)), 198);
        assert_eq!(part_1(&parse_lines(INPUT)), 3882564);
    }

    #[test]
    fn test_part_2() {
        assert_eq!(part_2(&parse_lines(EXAMPLE)), 230);
        assert_eq!(part_2(&parse_lines(INPUT)), 0);
    }
}
