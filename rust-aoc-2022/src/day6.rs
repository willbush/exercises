/// https://adventofcode.com/2022/day/6
use std::{collections::HashSet, fs::read_to_string};

pub fn run() -> std::io::Result<()> {
    let input = read_to_string("../inputs/aoc/2022/day6.txt")?;

    println!("Day 6");
    println!("Part 1: {:?}", find_marker(&input, 4));
    println!("Part 2: {:?}", find_marker(&input, 14));

    Ok(())
}

/// Find the packet marker
fn find_marker(input: &str, n: usize) -> Option<usize> {
    if input.len() < n || n == 0 {
        return None;
    }
    let mut marker = HashSet::with_capacity(n);

    return input
        .chars()
        .collect::<Vec<_>>()
        .windows(n)
        .position(|ws| {
            for c in ws {
                if !marker.insert(*c) {
                    marker.clear();
                    return false;
                }
            }
            return true;
        })
        .and_then(|i| Some(i + n));
}

#[cfg(test)]
mod day6_tests {
    use super::*;

    #[test]
    fn test_find_packet_marker() {
        assert_eq!(find_marker("", 4), None);
        assert_eq!(find_marker("abc", 4), None);
        assert_eq!(find_marker("abcd", 4), Some(4));
        assert_eq!(find_marker("abcad", 4), Some(5));
        assert_eq!(find_marker("mjqjpqmgbljsphdztnvjfqwrcgsmlb", 4), Some(7));
        assert_eq!(find_marker("bvwbjplbgvbhsrlpgdmjqwftvncz", 4), Some(5));
        assert_eq!(find_marker("nppdvjthqldpwncqszvftbrmjlhg", 4), Some(6));
        assert_eq!(
            find_marker("nznrnfrfntjfmvfwmzdfjlvtqnbhcprsg", 4),
            Some(10)
        );
        assert_eq!(find_marker("zcfzfwzzqfrljwzlrfnpqdbhtmscgvjw", 4), Some(11));
    }

    #[test]
    fn test_find_message_marker() {
        assert_eq!(find_marker("mjqjpqmgbljsphdztnvjfqwrcgsmlb", 14), Some(19));
        assert_eq!(find_marker("nppdvjthqldpwncqszvftbrmjlhg", 14), Some(23));
        assert_eq!(
            find_marker("nznrnfrfntjfmvfwmzdfjlvtqnbhcprsg", 14),
            Some(29)
        );
        assert_eq!(
            find_marker("zcfzfwzzqfrljwzlrfnpqdbhtmscgvjw", 14),
            Some(26)
        );
    }
}
