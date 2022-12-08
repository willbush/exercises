/// https://adventofcode.com/2022/day/1
use std::{
    fs::File,
    io::{BufRead, BufReader},
};

pub fn run() -> std::io::Result<()> {
    let file = File::open("../inputs/aoc/2022/day3.txt")?;
    let mut reader = BufReader::new(file);

    let mut sacks = parse(&mut reader);

    Ok(())
}

fn parse<R>(reader: &mut R) -> Vec<RuckSack>
where
    R: BufRead,
{
    let mut sacks = Vec::new();
    let mut line = String::with_capacity(100);

    // Read the file line by line.
    while let Ok(bytes_read) = reader.read_line(&mut line) {
        if bytes_read == 0 {
            break; // EOF reached
        }
        let items = line.trim_end().chars().collect::<Vec<_>>();
        let mid = items.len() / 2;
        let first = items[..mid].to_vec();
        let second = items[mid..].to_vec();

        sacks.push(RuckSack::new(first, second));

        line.clear();
    }
    sacks
}

struct RuckSack {
    fst_compartment: Vec<char>,
    snd_compartment: Vec<char>,
}

impl RuckSack {
    fn new(fst_compartment: Vec<char>, snd_compartment: Vec<char>) -> Self {
        Self {
            fst_compartment,
            snd_compartment,
        }
    }
}

// Calculate the priority for a given char (item).
fn priority(c: char) -> u8 {
    if c.is_lowercase() {
        // a..=z have priority 1..=26
        c as u8 - ('a' as u8) + 1
    } else {
        // A..=Z have priority 27..=52
        c as u8 - ('A' as u8) + 27
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
