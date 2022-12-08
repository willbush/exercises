/// https://adventofcode.com/2022/day/3
use std::{
    collections::HashSet,
    fs::File,
    io::{BufRead, BufReader},
};

pub fn run() -> std::io::Result<()> {
    let file = File::open("../inputs/aoc/2022/day3.txt")?;
    let mut reader = BufReader::new(file);

    let result = parse(&mut reader);

    println!("Day 3");
    println!("part 1: {}", result);

    Ok(())
}

fn parse<R>(reader: &mut R) -> usize
where
    R: BufRead,
{
    let mut result = 0;
    let mut line = String::with_capacity(100);

    // Read the file line by line.
    while let Ok(bytes_read) = reader.read_line(&mut line) {
        if bytes_read == 0 {
            break; // EOF reached
        }
        let items = line.trim_end().chars().collect::<Vec<_>>();
        let mid = items.len() / 2;

        let fst: HashSet<&char> = HashSet::from_iter(items[..mid].iter());
        let snd: HashSet<&char> = HashSet::from_iter(items[mid..].iter());

        for &item in fst.intersection(&snd) {
            result += priority(*item);
        }

        line.clear();
    }
    result
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
        assert_eq!(priority('z'), 26);
        assert_eq!(priority('A'), 27);
        assert_eq!(priority('Z'), 52);
    }
}
