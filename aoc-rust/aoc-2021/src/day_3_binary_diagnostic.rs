//! Day 3: Binary Diagnostic
//!
//! <https://adventofcode.com/2021/day/3>

pub mod input {
    //! Parse input lines to [`bitvec`] values.

    use std::str::FromStr;

    use anyhow::anyhow;
    use bitvec::prelude::{BitBox, BitVec};

    #[repr(transparent)]
    pub struct ParseBits(pub BitBox);

    /// ```
    /// # use aoc_2021::day_3_binary_diagnostic::input::ParseBits;
    ///
    /// let ParseBits(bits) = "10101".parse().unwrap();
    /// assert_eq!(bits.to_string(), "[10101]")
    /// ```
    impl FromStr for ParseBits {
        type Err = anyhow::Error;

        fn from_str(s: &str) -> Result<Self, Self::Err> {
            s.chars()
                .map(|c| match c {
                    '0' => Ok(false),
                    '1' => Ok(true),
                    invalid => Err(anyhow!("Invalid bit: {invalid:?}")),
                })
                // Unlike Box<[T]>, BitBox doesn't implement FromIterator,
                // so we collect as BitVec and convert to BitBox.
                .collect::<Result<BitVec, Self::Err>>()
                .map(BitBox::from)
                .map(Self)
        }
    }
}

pub mod part_1_weights {
    //! For part 1, interpret bits as weights to sum, column-wise.
    //!
    //! TODO: Look at changing this to a matrix representation, for simpler code?

    use std::iter::{zip, Sum};
    use std::ops::{Add, Deref};

    use bitvec::prelude::BitSlice;

    pub type Weight = i32;

    #[repr(transparent)]
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

    /// ```
    /// # use aoc_2021::day_3_binary_diagnostic::part_1_weights::Weights;
    /// use bitvec::prelude::*;
    ///
    /// let Weights(weights) = bits![1,0,1,0,1].into();
    /// assert_eq!(format!("{:?}", weights), "[1, -1, 1, -1, 1]")
    /// ```
    impl From<&BitSlice> for Weights {
        fn from(bits: &BitSlice) -> Self {
            Self(
                bits.iter()
                    .by_val()
                    .map(|bit| match bit {
                        false => (-1),
                        true => (1),
                    })
                    .collect::<Box<[Weight]>>(),
            )
        }
    }

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
}

pub mod part_1 {

    use anyhow::{anyhow, Context};
    use bitvec::boxed::BitBox;

    use super::part_1_weights::{Weight, Weights};

    pub type GammaRate = u64;
    pub type EpsilonRate = u64;

    /// Sum a collection of report weights column-wise, to a combined value.
    pub fn combine_weights(report: &[Weights]) -> Weights {
        report.iter().sum()
    }

    /// Determine the gamma rate bit value for a single combined weight.
    fn gamma_bit_value(weight: Weight) -> anyhow::Result<GammaRate> {
        match weight.signum() {
            -1 => Ok(0),
            1 => Ok(1),
            0 => Err(anyhow!("ambiguous digit: {weight:?}")),
            otherwise => panic!("unexpected signum: {otherwise}"),
        }
    }

    /// Determine the rates.
    pub fn gamma_epsilon_rates(weights: &Weights) -> anyhow::Result<(GammaRate, EpsilonRate)> {
        let init: anyhow::Result<GammaRate> = Ok(0);
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

    /// Decode a full report into the two rate values.
    pub fn decode_report(report: &[BitBox]) -> anyhow::Result<(GammaRate, EpsilonRate)> {
        let weights = report
            .iter()
            .map(|bits| Weights::from(bits.as_bitslice()))
            .collect::<Box<[Weights]>>();

        let combined = &combine_weights(&weights);
        gamma_epsilon_rates(combined)
    }
}

pub mod part_2 {
    // use super::input::*;
}

#[cfg(test)]
mod tests {
    use aoc_inputs::inputs_2021::day_3::{EXAMPLE, INPUT};
    use aoc_inputs::parse_lines;
    use bitvec::boxed::BitBox;

    use super::input::*;
    use super::part_1::*;
    // use super::part_2::*;

    pub fn part_1(report: &[BitBox]) -> u64 {
        let (gamma, epsilon) = decode_report(report).unwrap();
        u64::checked_mul(gamma, epsilon).unwrap_or_else(|| panic!("overflow: {gamma} * {epsilon}"))
    }

    fn part_2(_: &[BitBox]) -> u32 {
        todo!()
    }

    #[test]
    fn test_part_1() {
        assert_eq!(part_1(&unwrap_bits(parse_lines(EXAMPLE))), 198);
        assert_eq!(part_1(&unwrap_bits(parse_lines(INPUT))), 3882564);
    }

    #[test]
    #[ignore]
    fn test_part_2() {
        assert_eq!(part_2(&unwrap_bits(parse_lines(EXAMPLE))), 230);
        assert_eq!(part_2(&unwrap_bits(parse_lines(INPUT))), 0);
    }

    /// XXX: Unwrap the [`ParseBits`] newtype.
    fn unwrap_bits(report: Vec<ParseBits>) -> Vec<BitBox> {
        report.into_iter().map(|ParseBits(bits)| bits).collect()
    }
}
