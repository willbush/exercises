/// https://adventofcode.com/2022/day/1
use std::{
    fs::File,
    io::{BufRead, BufReader},
};

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
    let mut line = String::with_capacity(5);

    // Read the file line by line.
    while let Ok(bytes_read) = reader.read_line(&mut line) {
        if bytes_read == 0 {
            break; // EOF reached
        }
        let cs = line.trim_end().chars().collect::<Vec<_>>();

        if let (Some(&my), Some(&theirs)) = (cs.first(), cs.last()) {
            if let (Some(my_rps), Some(their_rps)) = (char_to_rps(my), char_to_rps(theirs)) {
                scores.push(calc_my_score(&my_rps, &their_rps));
            }
        }
        line.clear();
    }
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

fn calc_my_score(my_rps: &RPS, their_rps: &RPS) -> Score {
    match (my_rps, their_rps) {
        (RPS::Rock, RPS::Scissors) => 1 + 6,
        (RPS::Rock, RPS::Rock) => 1 + 3,
        (RPS::Rock, RPS::Paper) => 1 + 0,
        (RPS::Paper, RPS::Rock) => 2 + 6,
        (RPS::Paper, RPS::Paper) => 2 + 3,
        (RPS::Paper, RPS::Scissors) => 2 + 0,
        (RPS::Scissors, RPS::Paper) => 3 + 6,
        (RPS::Scissors, RPS::Scissors) => 3 + 3,
        (RPS::Scissors, RPS::Rock) => 3 + 0,
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
        assert_eq!(vec![1, 8, 6], scores);
    }

    #[test]
    fn can_calc_score2() {
        let input = "B X
A Z
A Y ";

        let mut reader = BufReader::new(input.as_bytes());
        let scores = parse_to_my_scores(&mut reader);
        assert_eq!(vec![8, 7, 1], scores);
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
        assert_eq!(vec![9, 6, 5, 5, 5, 1, 9, 5, 9, 9, 5], scores);
    }
}
