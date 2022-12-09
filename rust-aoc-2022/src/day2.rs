/// https://adventofcode.com/2022/day/1
use std::{
    fs::File,
    io::{BufRead, BufReader},
};

use crate::utils::parse_lines;

pub fn run() -> std::io::Result<()> {
    let file = File::open("../inputs/aoc/2022/day2.txt")?;
    let mut reader = BufReader::new(file);
    let my_total_score: Score = parse_to_my_scores(&mut reader).iter().sum();

    println!("Day 2");
    println!("part 1: {}", my_total_score);

    Ok(())
}

type Score = usize;

enum RPS {
    Rock,
    Paper,
    Scissors,
}

fn parse_to_my_scores<R>(reader: &mut R) -> Vec<Score>
where
    R: BufRead,
{
    let mut scores = Vec::new();

    parse_lines(reader, 5, |line| {
        let cs = line.chars().collect::<Vec<_>>();

        if let (Some(&theirs), Some(&my)) = (cs.first(), cs.last()) {
            if let (Some(their_rps), Some(my_rps)) = (char_to_rps(theirs), char_to_rps(my)) {
                scores.push(calc_my_score(&their_rps, &my_rps));
            }
        }
    });

    scores
}

fn char_to_rps(c: char) -> Option<RPS> {
    match c {
        'A' | 'X' => Some(RPS::Rock),
        'B' | 'Y' => Some(RPS::Paper),
        'C' | 'Z' => Some(RPS::Scissors),
        _ => None,
    }
}

fn calc_my_score(their_rps: &RPS, my_rps: &RPS) -> Score {
    match (their_rps, my_rps) {
        (RPS::Rock, RPS::Rock) => 1 + 3,
        (RPS::Rock, RPS::Paper) => 2 + 6,
        (RPS::Rock, RPS::Scissors) => 3 + 0,
        (RPS::Paper, RPS::Rock) => 1 + 0,
        (RPS::Paper, RPS::Paper) => 2 + 3,
        (RPS::Paper, RPS::Scissors) => 3 + 6,
        (RPS::Scissors, RPS::Rock) => 1 + 6,
        (RPS::Scissors, RPS::Paper) => 2 + 0,
        (RPS::Scissors, RPS::Scissors) => 3 + 3,
    }
}

#[cfg(test)]
mod day1_tests {
    use super::*;

    #[test]
    fn can_calc_score() {
        let input = "A Y
B X
C Z";

        let mut reader = BufReader::new(input.as_bytes());
        let scores = parse_to_my_scores(&mut reader);
        assert_eq!(vec![8, 1, 6], scores);
    }

    #[test]
    fn can_calc_score2() {
        let input = "B X
A Z
A Y ";

        let mut reader = BufReader::new(input.as_bytes());
        let scores = parse_to_my_scores(&mut reader);
        assert_eq!(vec![1, 3, 8], scores);
    }

    #[test]
    fn can_calc_score3() {
        let input = "C Y
C Z
B Y
B Y
B Y
A Y
C Y
B Y
C Y
C Y
B Y";

        let mut reader = BufReader::new(input.as_bytes());
        let scores = parse_to_my_scores(&mut reader);
        assert_eq!(vec![2, 6, 5, 5, 5, 8, 2, 5, 2, 2, 5], scores);
    }
}
