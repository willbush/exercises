/// https://adventofcode.com/2022/day/1
use std::{
    fs::File,
    io::{BufRead, BufReader},
};

fn main() -> std::io::Result<()> {
    let file = File::open("../inputs/aoc/2022/day1.txt")?;
    let mut reader = BufReader::new(file);
    let mut elves = parse(&mut reader);

    // reverse sort.
    elves.sort_by(|a, b| b.total_calories.cmp(&a.total_calories));

    if let Some(max) = elves.first() {
        println!("part 1: {}", max.total_calories);
    }

    let total_top_three: u32 = elves.iter().map(|e| e.total_calories).take(3).sum();

    println!("part 2: {}", total_top_three);

    Ok(())
}

#[derive(Debug, PartialEq)]
struct Elf {
    total_calories: u32,
}

impl Elf {
    fn new(total_calories: u32) -> Self {
        Self { total_calories }
    }
}

/// Parse a lines into a vector of elves.
fn parse<R>(reader: &mut R) -> Vec<Elf>
where
    R: BufRead,
{
    let mut elves = Vec::new();
    let mut total_calories = 0;
    let mut line = String::with_capacity(20);

    // Read the file line by line.
    while let Ok(bytes_read) = reader.read_line(&mut line) {
        if bytes_read == 0 {
            break; // EOF reached
        }

        if let Ok(calories) = line.trim_end().parse::<u32>() {
            total_calories += calories;
        } else if total_calories > 0 {
            elves.push(Elf::new(total_calories));
            total_calories = 0;
        }
        line.clear();
    }
    if total_calories > 0 {
        elves.push(Elf::new(total_calories));
    }
    elves
}

#[cfg(test)]
mod day1_tests {
    use super::*;

    #[test]
    fn can_parse() {
        let input = "1000
2000
3000

4000

5000
6000

7000
8000
9000

10000";
        let expected = vec![
            Elf::new(6000),
            Elf::new(4000),
            Elf::new(11000),
            Elf::new(24000),
            Elf::new(10000),
        ];

        let mut reader = BufReader::new(input.as_bytes());
        let actual = parse(&mut reader);
        assert_eq!(expected, actual);
    }
}
