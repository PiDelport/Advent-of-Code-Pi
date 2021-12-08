//! Helper library for conveniently accessing the Advent of Code input in Rust.

use std::fmt::Display;
use std::str::FromStr;

pub mod inputs_2021;

/// Parse each line of a string to type `F`.
///
/// Intended for use with [`include_str`].
///
/// # Panics
///
/// If parsing any line fails.
fn parse_lines<F>(s: impl AsRef<str>) -> Vec<F>
where
    F: FromStr,
    F::Err: Display,
{
    s.as_ref()
        .lines()
        .map(|line| {
            line.parse()
                .unwrap_or_else(|err| panic!("parsing {line:?} failed: {err}"))
        })
        .collect()
}
