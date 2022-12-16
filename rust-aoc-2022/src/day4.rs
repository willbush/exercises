/// https://adventofcode.com/2022/day/4
use std::{fs::File, io::BufReader};

use crate::utils::parse_lines;

pub fn run() -> std::io::Result<()> {
    let file = File::open("../inputs/aoc/2022/day4.txt")?;
    let mut reader = BufReader::new(file);
    let pattern = [',', '-'];

    // The number of assignment pairs that fully contain the other.
    let mut total_fully_contain_ranges = 0;
    let mut total_overlaps = 0;

    parse_lines(&mut reader, 20, |line| {
        let mut xs = line.split(&pattern);
        let numbers = (
            xs.next().map(|x| x.parse::<usize>()).and_then(Result::ok),
            xs.next().map(|x| x.parse::<usize>()).and_then(Result::ok),
            xs.next().map(|x| x.parse::<usize>()).and_then(Result::ok),
            xs.next().map(|x| x.parse::<usize>()).and_then(Result::ok),
        );

        match numbers {
            (Some(a), Some(b), Some(x), Some(y)) => {
                if (a <= x && b >= y) || (x <= a && y >= b) {
                    total_fully_contain_ranges += 1;
                    total_overlaps += 1;
                } else if (a <= x && b >= x) || (a <= y && b >= y) {
                    total_overlaps += 1;
                }
            }
            _ => panic!("Invalid input"),
        }
    });

    println!("Day 4");
    println!("- Part 1: {}", total_fully_contain_ranges);
    println!("- Part 2: {}", total_overlaps);

    Ok(())
}
