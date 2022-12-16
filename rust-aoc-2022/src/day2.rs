/// https://adventofcode.com/2022/day/2
use std::{
    fs::File,
    io::{BufRead, BufReader},
};

use crate::utils::parse_lines;

pub fn run() -> std::io::Result<()> {
    let file = File::open("../inputs/aoc/2022/day2.txt")?;
    let mut reader = BufReader::new(file);

    let scores = parse_to_my_scores(&mut reader);
    let fst_strat_total_score: Score = scores.iter().map(|x| x.0).sum();
    let snd_strat_total_score: Score = scores.iter().map(|x| x.1).sum();

    println!("Day 2");
    println!("- Part 1: {}", fst_strat_total_score);
    println!("- Part 2: {}", snd_strat_total_score);

    Ok(())
}

type Score = usize;

enum RPS {
    Rock,
    Paper,
    Scissors,
}

enum Outcome {
    Win,
    Lose,
    Draw,
}

fn parse_to_my_scores<R>(reader: &mut R) -> Vec<(Score, Score)>
where
    R: BufRead,
{
    let mut scores = Vec::new();

    parse_lines(reader, 5, |line| {
        let cs = line.chars().collect::<Vec<_>>();

        if let (Some(&theirs), Some(&last)) = (cs.first(), cs.last()) {
            // Part 1 strategy
            if let (Some(their_rps), Some(my_rps), Some(outcome)) = (
                char_to_rps(theirs),
                char_to_rps(last),
                char_to_outcome(last),
            ) {
                // Part 1 strategy
                let fst_strat_score = calc_my_score(&their_rps, &my_rps);
                // Part 2 strategy
                let snd_strat_score = calc_my_score(&their_rps, &get_my_move(&their_rps, &outcome));

                scores.push((fst_strat_score, snd_strat_score));
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

fn char_to_outcome(c: char) -> Option<Outcome> {
    match c {
        'X' => Some(Outcome::Lose),
        'Y' => Some(Outcome::Draw),
        'Z' => Some(Outcome::Win),
        _ => None,
    }
}

fn get_my_move(their: &RPS, outcome: &Outcome) -> RPS {
    match (their, outcome) {
        (RPS::Rock, Outcome::Win) => RPS::Paper,
        (RPS::Rock, Outcome::Draw) => RPS::Rock,
        (RPS::Rock, Outcome::Lose) => RPS::Scissors,

        (RPS::Paper, Outcome::Win) => RPS::Scissors,
        (RPS::Paper, Outcome::Draw) => RPS::Paper,
        (RPS::Paper, Outcome::Lose) => RPS::Rock,

        (RPS::Scissors, Outcome::Win) => RPS::Rock,
        (RPS::Scissors, Outcome::Draw) => RPS::Scissors,
        (RPS::Scissors, Outcome::Lose) => RPS::Paper,
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
mod day2_tests {
    use super::*;

    #[test]
    fn can_calc_score_part1() {
        let input = "A Y
B X
C Z";

        let mut reader = BufReader::new(input.as_bytes());
        let scores = parse_to_my_scores(&mut reader)
            .iter()
            .map(|x| x.0)
            .collect::<Vec<_>>();
        assert_eq!(vec![8, 1, 6], scores);
    }

    #[test]
    fn can_calc_score_part2() {
        let input = "A Y
B X
C Z";

        let mut reader = BufReader::new(input.as_bytes());
        let scores = parse_to_my_scores(&mut reader)
            .iter()
            .map(|x| x.1)
            .collect::<Vec<_>>();
        assert_eq!(vec![4, 1, 7], scores);
    }

    #[test]
    fn can_calc_score2() {
        let input = "B X
A Z
A Y ";

        let mut reader = BufReader::new(input.as_bytes());
        let scores = parse_to_my_scores(&mut reader)
            .iter()
            .map(|x| x.0)
            .collect::<Vec<_>>();
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
        let scores = parse_to_my_scores(&mut reader)
            .iter()
            .map(|x| x.0)
            .collect::<Vec<_>>();
        assert_eq!(vec![2, 6, 5, 5, 5, 8, 2, 5, 2, 2, 5], scores);
    }
}
