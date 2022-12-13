/// https://adventofcode.com/2022/day/5
use std::{collections::VecDeque, fs::File, io::BufReader};

use crate::utils::parse_lines;

pub fn run() -> std::io::Result<()> {
    let file = File::open("../inputs/aoc/2022/day5.txt")?;
    let reader = BufReader::new(file);

    // [N]     [Q]         [N]
    // [R]     [F] [Q]     [G] [M]
    // [J]     [Z] [T]     [R] [H] [J]
    // [T] [H] [G] [R]     [B] [N] [T]
    // [Z] [J] [J] [G] [F] [Z] [S] [M]
    // [B] [N] [N] [N] [Q] [W] [L] [Q] [S]
    // [D] [S] [R] [V] [T] [C] [C] [N] [G]
    // [F] [R] [C] [F] [L] [Q] [F] [D] [P]
    //  1   2   3   4   5   6   7   8   9

    let mut crate_stacks = vec![
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

    run_commands(&commands, &mut crate_stacks);

    println!("Day 5");

    println!("part 1: {}", String::from_iter(top_each_stack(&crate_stacks)));

    Ok(())
}

fn run_commands(commands: &Vec<Command>, crate_stacks: &mut Vec<VecDeque<char>>) {
    for c in commands {
        for _ in 0..c.quantity {
            if let Some(x) = crate_stacks[c.source - 1].pop_back() {
                crate_stacks[c.destination - 1].push_back(x);
            }
        }
    }
}

fn top_each_stack(crate_stacks: &Vec<VecDeque<char>>) -> Vec<char> {
    crate_stacks
        .iter()
        .map(|s| s.back().unwrap_or(&' '))
        .cloned()
        .collect::<Vec<char>>()
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

    #[test]
    fn simple_crane() {
        // [A] [B]
        //  1   2
        let mut crate_stacks = vec![VecDeque::from(vec!['A']), VecDeque::from(vec!['B'])];
        // move 1 from 1 to 2
        let commands = vec![Command::new(1, 1, 2)];
        run_commands(&commands, &mut crate_stacks);

        let expected_stacks = vec![VecDeque::from(vec![]), VecDeque::from(vec!['B', 'A'])];

        assert_eq!(expected_stacks, crate_stacks);
    }

    #[test]
    fn test_crane() {
        //     [D]
        // [N] [C]
        // [Z] [M] [P]
        //  1   2   3

        let mut crate_stacks = vec![
            VecDeque::from(vec!['Z', 'N']),
            VecDeque::from(vec!['M', 'C', 'D']),
            VecDeque::from(vec!['P']),
        ];

        // move 1 from 2 to 1
        // move 3 from 1 to 3
        // move 2 from 2 to 1
        // move 1 from 1 to 2
        let commands = vec![
            Command::new(1, 2, 1),
            Command::new(3, 1, 3),
            Command::new(2, 2, 1),
            Command::new(1, 1, 2),
        ];
        run_commands(&commands, &mut crate_stacks);

        let expected_stacks = vec![
            VecDeque::from(vec!['C']),
            VecDeque::from(vec!['M']),
            VecDeque::from(vec!['P', 'D', 'N', 'Z']),
        ];

        assert_eq!(expected_stacks, crate_stacks);
        assert_eq!(vec!['C', 'M', 'Z'], top_each_stack(&crate_stacks));
    }
}
