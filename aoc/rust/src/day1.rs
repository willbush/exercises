use std::fs::File;
use std::io::prelude::Read;
use std::io::BufReader;
use std::iter;

type Mass = i32;
type Fuel = i32;

pub fn run() -> std::io::Result<()> {
    let file = File::open("../inputs/input-day1.txt")?;
    let mut buf_reader = BufReader::new(file);
    let mut inputs = String::new();
    buf_reader.read_to_string(&mut inputs)?;

    let masses: Vec<_> = inputs
        .split_whitespace()
        .map(|s| s.parse::<i32>())
        .filter_map(Result::ok)
        .collect();

    println!("Part 1:");
    let part_1_sum: i32 = masses.iter().map(|&m| calc_fuel(m)).sum();
    println!("{}", part_1_sum);

    println!("Part 2:");
    let part_2_sum: i32 = masses.into_iter().map(calc_module_fuel).sum();
    println!("{}", part_2_sum);

    Ok(())
}

fn calc_module_fuel(m: Mass) -> Fuel {
    let mut current = m;
    // Note that the first mass value does not end up in the iterator.
    iter::repeat_with(|| {
        current = calc_fuel(current);
        current
    })
    .take_while(|x| x.is_positive())
    .sum()
}

fn calc_fuel(m: Mass) -> Fuel {
    m / 3 - 2
}

#[cfg(test)]
mod day1_tests {
    use super::*;

    #[test]
    fn test_calc_fuel() {
        assert_eq!(calc_fuel(12), 2);
        assert_eq!(calc_fuel(14), 2);
        assert_eq!(calc_fuel(1969), 654);
        assert_eq!(calc_fuel(100756), 33583);
    }

    #[test]
    fn test_calc_module_fuel() {
        assert_eq!(calc_module_fuel(12), 2);
        assert_eq!(calc_module_fuel(14), 2);
        assert_eq!(calc_module_fuel(1969), 966);
        assert_eq!(calc_module_fuel(100756), 50346);
    }
}
