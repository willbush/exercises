/// https://adventofcode.com/2022/day/5
use std::{collections::VecDeque, fs::File, io::BufReader};

use crate::utils::parse_lines;

pub fn run() -> std::io::Result<()> {
    let file = File::open("../inputs/aoc/2022/day5.txt")?;
    let mut reader = BufReader::new(file);

    // [N]     [Q]         [N]
    // [R]     [F] [Q]     [G] [M]
    // [J]     [Z] [T]     [R] [H] [J]
    // [T] [H] [G] [R]     [B] [N] [T]
    // [Z] [J] [J] [G] [F] [Z] [S] [M]
    // [B] [N] [N] [N] [Q] [W] [L] [Q] [S]
    // [D] [S] [R] [V] [T] [C] [C] [N] [G]
    // [F] [R] [C] [F] [L] [Q] [F] [D] [P]
    //  1   2   3   4   5   6   7   8   9

    let crate_stacks = vec![
        VecDeque::from(vec!['F', 'D', 'B', 'Z', 'T', 'J', 'R', 'N']),
        VecDeque::from(vec!['R', 'S', 'N', 'J', 'H']),
        VecDeque::from(vec!['C', 'R', 'N', 'J', 'G', 'Z', 'F', 'Q']),
        VecDeque::from(vec!['F', 'V', 'N', 'G', 'R', 'T', 'Q']),
        VecDeque::from(vec!['L', 'T', 'Q', 'F']),
        VecDeque::from(vec!['Q', 'C', 'W', 'Z', 'B', 'R', 'G', 'N']),
        VecDeque::from(vec!['F', 'C', 'L', 'S', 'N', 'H', 'M']),
        VecDeque::from(vec!['D', 'N', 'Q', 'M', 'T', 'J']),
        VecDeque::from(vec!['P', 'G', 'S']),
    ];

    let commands = parse_commands(reader);

    println!("Day 5");
    // println!("part 1: {}", part1_result);

    Ok(())
}

fn parse_commands(mut reader: BufReader<File>) -> Vec<Command> {
    let mut commands = Vec::new();

    parse_lines(&mut reader, 25, |line| {
        let mut inputs = line.split(' ');
        inputs.next();
        let quantity = inputs
            .next()
            .map(|x| x.parse::<usize>())
            .and_then(Result::ok);
        inputs.next();
        let from = inputs
            .next()
            .map(|x| x.parse::<usize>())
            .and_then(Result::ok);
        inputs.next();
        let to = inputs
            .next()
            .map(|x| x.parse::<usize>())
            .and_then(Result::ok);

        match (quantity, from, to) {
            (Some(quantity), Some(from), Some(to)) => {
                commands.push(Command::new(quantity, from, to))
            }
            _ => panic!("Invalid input"),
        }
    });
    commands
}

#[derive(Debug)]
struct Command {
    quantity: usize,
    source: usize,
    destination: usize,
}

impl Command {
    fn new(quantity: usize, source: usize, destination: usize) -> Self {
        Self {
            quantity,
            source,
            destination,
        }
    }
}

#[cfg(test)]
mod day5_tests {
    use super::*;
}
