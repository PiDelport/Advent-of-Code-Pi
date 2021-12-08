//! Day 1: Sonar Sweep
//!
//! <https://adventofcode.com/2021/day/1>

use aoc_inputs::inputs_2021::day_1::Depth;

/// Count the number of increasing pairs of depths.
pub fn count_increases(depths: &[Depth]) -> usize {
    depths.array_windows().filter(|[a, b]| a < b).count()
}

pub fn part_1(depths: &[Depth]) -> usize {
    count_increases(depths)
}

/// Sum each window of `N` measurements.
///
/// Return a slice with with a size _N - 1_ measurements shorter than the input slice.
pub fn sum_windows<const N: usize>(depths: &[Depth]) -> Box<[Depth]> {
    depths
        .array_windows::<N>()
        .map(|window| window.iter().sum())
        .collect()
}

pub fn part_2(depths: &[Depth]) -> usize {
    let windowed = sum_windows::<3>(depths);
    count_increases(&windowed)
}

#[cfg(test)]
mod tests {
    use aoc_inputs::inputs_2021::day_1::{example, input};

    use super::*;

    #[test]
    fn test_part_1() {
        assert_eq!(part_1(&example()), 7);
        assert_eq!(part_1(&input()), 1400);
    }

    #[test]
    fn test_part_2() {
        assert_eq!(part_2(&example()), 5);
        assert_eq!(part_2(&input()), 1429);
    }
}
