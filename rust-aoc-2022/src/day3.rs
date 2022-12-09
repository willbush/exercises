/// https://adventofcode.com/2022/day/3
use std::{
    collections::HashSet,
    fs::File,
    io::{BufReader, Seek, SeekFrom},
};

use crate::utils::{parse_lines, parse_lines_by3};

pub fn run() -> std::io::Result<()> {
    let file = File::open("../inputs/aoc/2022/day3.txt")?;
    let mut reader = BufReader::new(file);

    let mut part1_result = 0;

    parse_lines(&mut reader, 100, |line| {
        // part 1
        let items = line.chars().collect::<Vec<_>>();
        let mid = items.len() / 2;

        let fst: HashSet<&char> = HashSet::from_iter(items[..mid].iter());
        let snd: HashSet<&char> = HashSet::from_iter(items[mid..].iter());

        for &item in fst.intersection(&snd) {
            part1_result += priority(*item);
        }
    });

    println!("Day 3");
    println!("part 1: {}", part1_result);

    let mut part2_result = 0;

    reader.seek(SeekFrom::Start(0))?;

    parse_lines_by3(&mut reader, 100, |line1, line2, line3| {
        for item in intersect(line1, line2, line3) {
            part2_result += priority(item);
        }
    });

    println!("part 2: {}", part2_result);

    Ok(())
}

fn intersect(line1: &str, line2: &str, line3: &str) -> HashSet<char> {
    let xs: HashSet<char> = HashSet::from_iter(line1.chars());
    let ys: HashSet<char> = HashSet::from_iter(line2.chars());
    let zs: HashSet<char> = HashSet::from_iter(line3.chars());

    xs.iter()
        .filter(|x| ys.contains(x) && zs.contains(x))
        .cloned()
        .collect()
}

// Calculate the priority for a given char (item).
fn priority(c: char) -> usize {
    if c.is_lowercase() {
        // a..=z have priority 1..=26
        c as usize - ('a' as usize) + 1
    } else {
        // A..=Z have priority 27..=52
        c as usize - ('A' as usize) + 27
    }
}

#[cfg(test)]
mod day1_tests {
    use super::*;

    #[test]
    fn priority_test() {
        assert_eq!(priority('a'), 1);
        assert_eq!(priority('b'), 2);
        assert_eq!(priority('z'), 26);
        assert_eq!(priority('A'), 27);
        assert_eq!(priority('Z'), 52);
    }

    #[test]
    fn intersect_test() {
        let line1 = "vJrwpWtwJgWrhcsFMMfFFhFp";
        let line2 = "jqHRNqRjqzjGDLGLrsFMfFZSrLrFZsSL";
        let line3 = "PmmdzqPrVvPwwTWBwg";

        assert_eq!(intersect(line1, line2, line3), HashSet::from(['r']));
    }
}
