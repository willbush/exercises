/// https://adventofcode.com/2022/day/10
use std::{
    fs::File,
    io::{BufRead, BufReader},
};

use crate::utils::parse_lines;

pub fn run() -> std::io::Result<()> {
    let file = File::open("../inputs/aoc/2022/day10.txt")?;
    let mut reader = BufReader::new(file);
    let instructions = parse_instructions(&mut reader);
    let samples_sum: i32 = simulate_cpu(&instructions, |cycle| match cycle {
        20 | 60 | 100 | 140 | 180 | 220 => true,
        _ => false,
    })
    .iter()
    .sum();

    println!("Day 10");
    println!("- Part 1: {}", samples_sum);

    Ok(())
}

#[derive(Debug, PartialEq, Eq)]
enum Inst {
    Noop,
    Addx(i32),
}

fn parse_instructions<R>(reader: &mut R) -> Vec<Inst>
where
    R: BufRead,
{
    let mut instructions = Vec::new();

    parse_lines(reader, 10, |line| {
        if line.is_empty() {
            return;
        }
        let mut tokens = line.split(' ');

        let cmd = (
            tokens.next(),
            tokens.next().map(|x| x.parse::<i32>()).and_then(Result::ok),
        );

        match cmd {
            (Some("noop"), None) => instructions.push(Inst::Noop),
            (Some("addx"), Some(x)) => instructions.push(Inst::Addx(x)),
            _ => panic!("Invalid command"),
        }
    });
    instructions
}

type SignalStrength = i32;

fn simulate_cpu<F>(instructions: &[Inst], sample_now: F) -> Vec<SignalStrength>
where
    F: Fn(&i32) -> bool,
{
    let mut signal_strengths = Vec::new();
    let mut x = 1;
    let mut cycle = 1;
    let mut i = 0;
    let mut running_inst_time = 0;
    let mut running_inst = None;

    while i < instructions.len() {
        if running_inst_time == 0 {
            let inst = &instructions[i];
            i += 1;
            running_inst = Some(inst);
            match inst {
                Inst::Noop => {}
                Inst::Addx(_) => running_inst_time = 2,
            }
        }
        if sample_now(&cycle) {
            signal_strengths.push(x * cycle);
        }
        match running_inst {
            Some(Inst::Addx(v)) => {
                running_inst_time -= 1;
                if running_inst_time == 0 {
                    x += v;
                }
            }
            _ => {}
        }
        cycle += 1;
    }
    signal_strengths
}

#[cfg(test)]
mod day9_tests {
    use super::*;

    #[test]
    fn test_parse() {
        let input = "noop
addx 1
addx 2
noop
";
        let mut reader = BufReader::new(input.as_bytes());
        let actual = parse_instructions(&mut reader);
        let expected = vec![Inst::Noop, Inst::Addx(1), Inst::Addx(2), Inst::Noop];
        assert_eq!(expected, actual);
    }

    #[test]
    fn test_simulate_cpu() {
        let input = "
addx 15
addx -11
addx 6
addx -3
addx 5
addx -1
addx -8
addx 13
addx 4
noop
addx -1
addx 5
addx -1
addx 5
addx -1
addx 5
addx -1
addx 5
addx -1
addx -35
addx 1
addx 24
addx -19
addx 1
addx 16
addx -11
noop
noop
addx 21
addx -15
noop
noop
addx -3
addx 9
addx 1
addx -3
addx 8
addx 1
addx 5
noop
noop
noop
noop
noop
addx -36
noop
addx 1
addx 7
noop
noop
noop
addx 2
addx 6
noop
noop
noop
noop
noop
addx 1
noop
noop
addx 7
addx 1
noop
addx -13
addx 13
addx 7
noop
addx 1
addx -33
noop
noop
noop
addx 2
noop
noop
noop
addx 8
noop
addx -1
addx 2
addx 1
noop
addx 17
addx -9
addx 1
addx 1
addx -3
addx 11
noop
noop
addx 1
noop
addx 1
noop
noop
addx -13
addx -19
addx 1
addx 3
addx 26
addx -30
addx 12
addx -1
addx 3
addx 1
noop
noop
noop
addx -9
addx 18
addx 1
addx 2
noop
noop
addx 9
noop
noop
noop
addx -1
addx 2
addx -37
addx 1
addx 3
noop
addx 15
addx -21
addx 22
addx -6
addx 1
noop
addx 2
addx 1
noop
addx -10
noop
noop
addx 20
addx 1
addx 2
addx 2
addx -6
addx -11
noop
noop
noop
";
        let mut reader = BufReader::new(input.as_bytes());
        let instructions = parse_instructions(&mut reader);

        let actual = simulate_cpu(&instructions, |cycle| match cycle {
            20 | 60 | 100 | 140 | 180 | 220 => true,
            _ => false,
        });
        let expected: Vec<i32> = vec![420, 1140, 1800, 2940, 2880, 3960];

        assert_eq!(expected, actual);
    }
}
