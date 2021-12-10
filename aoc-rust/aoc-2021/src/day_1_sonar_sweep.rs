//! Day 1: Sonar Sweep
//!
//! <https://adventofcode.com/2021/day/1>

pub mod input {
    pub type Depth = u32;
}

pub mod part_1 {
    use super::input::Depth;

    /// Count the number of increasing pairs of depths.
    pub fn count_increases(depths: &[Depth]) -> usize {
        depths.array_windows().filter(|[a, b]| a < b).count()
    }
}

pub mod part_2 {
    use super::input::Depth;

    /// Sum each window of `N` measurements.
    ///
    /// Return a slice with with a size _N - 1_ measurements shorter than the input slice.
    pub fn sum_windows<const N: usize>(depths: &[Depth]) -> Box<[Depth]> {
        depths
            .array_windows::<N>()
            .map(|window| window.iter().sum())
            .collect()
    }
}

#[cfg(test)]
mod tests {
    use aoc_inputs::inputs_2021::day_1::{EXAMPLE, INPUT};
    use aoc_inputs::parse_lines;

    use super::input::*;
    use super::part_1::*;
    use super::part_2::*;

    fn part_1(depths: &[Depth]) -> usize {
        count_increases(depths)
    }

    fn part_2(depths: &[Depth]) -> usize {
        let windowed = sum_windows::<3>(depths);
        count_increases(&windowed)
    }

    #[test]
    fn test_part_1() {
        assert_eq!(part_1(&parse_lines(EXAMPLE)), 7);
        assert_eq!(part_1(&parse_lines(INPUT)), 1400);
    }

    #[test]
    fn test_part_2() {
        assert_eq!(part_2(&parse_lines(EXAMPLE)), 5);
        assert_eq!(part_2(&parse_lines(INPUT)), 1429);
    }
}
