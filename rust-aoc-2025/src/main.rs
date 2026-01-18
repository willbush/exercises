use std::{
    fs::File,
    io::{BufRead, BufReader},
    str::FromStr,
};

use anyhow::{Context, Error, anyhow};

// How many ticks are on the dial? Or the base / radix of the dial.
const DIAL_SIZE: u16 = 100;

// Starting point of the dial for part 1 and 2
const DIAL_START: u8 = 50;

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
enum Rotation {
    /// Rotate left
    L(u16),
    /// Rotate right
    R(u16),
}

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
struct Dial(u8);

impl Dial {
    pub fn rotate(&self, r: Rotation) -> Self {
        let d = i32::from(self.0);
        let n = match r {
            Rotation::L(x) => d - i32::from(x),
            Rotation::R(x) => d + i32::from(x),
        };
        Dial(n.rem_euclid(DIAL_SIZE.into()) as u8)
    }

    /// How many zeros does a rotation have us cross? Not counting if we start on 0.
    /// Only counting rotations that have us to or across zero.
    fn count_zeros(&self, rotation: Rotation) -> u32 {
        let (amount, is_right) = match rotation {
            Rotation::L(r) => (r, false),
            Rotation::R(r) => (r, true),
        };
        let full_rotations = (amount / DIAL_SIZE) as u32;
        let remainder = amount % DIAL_SIZE;
        let dial = self.0;

        // When dial is 0, any partial movement can't wrap back around to 0.
        if remainder == 0 || dial == 0 {
            return full_rotations;
        }

        let crosses_partial = if is_right {
            u16::from(dial) + remainder >= DIAL_SIZE
        } else {
            //    dial - remainder <= 0
            // => dial <= remainder
            u16::from(dial) <= remainder
        };

        full_rotations + u32::from(crosses_partial)
    }
}

impl FromStr for Rotation {
    type Err = Error;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let (dir, n) = s
            .trim()
            .split_at_checked(1)
            .ok_or_else(|| anyhow!("String too short to split"))?;
        let amount = n.parse::<u16>().context("failed parse amount number")?;

        match dir {
            "L" => Ok(Rotation::L(amount)),
            "R" => Ok(Rotation::R(amount)),
            _ => Err(anyhow!("Invalid direction {dir}")),
        }
    }
}

fn main() -> anyhow::Result<()> {
    let file = File::open("input.txt")?;
    let reader = BufReader::new(file);

    let mut rotations = Vec::with_capacity(5000);
    for line in reader.lines() {
        let rotation = line?.parse::<Rotation>()?;
        rotations.push(rotation);
    }
    println!("part1 {}", part1(&rotations));
    println!("part2 {}", part2(&rotations));

    Ok(())
}

fn part1(rotations: &[Rotation]) -> u32 {
    let mut zero_count = 0;
    let mut dial = Dial(DIAL_START);
    for r in rotations {
        dial = dial.rotate(*r);
        if dial.0 == 0 {
            zero_count += 1;
        }
    }
    zero_count
}

fn part2(rotations: &[Rotation]) -> u32 {
    let mut zero_count = 0;
    let mut dial = Dial(DIAL_START);
    for r in rotations {
        zero_count += dial.count_zeros(*r);
        dial = dial.rotate(*r);
    }
    zero_count
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn can_parse() -> anyhow::Result<()> {
        assert!("xxx".parse::<Rotation>().is_err());
        assert_eq!("L12".parse::<Rotation>()?, Rotation::L(12));
        assert_eq!("R128".parse::<Rotation>()?, Rotation::R(128));
        Ok(())
    }

    #[test]
    fn simple_rotation_right() {
        use Dial as D;
        use Rotation::R;
        assert_eq!(D(1), D(0).rotate(R(1)));
        assert_eq!(D(2), D(0).rotate(R(2)));
        assert_eq!(D(99), D(0).rotate(R(99)));
        assert_eq!(D(0), D(0).rotate(R(100)));
        assert_eq!(D(10), D(0).rotate(R(110)));
        assert_eq!(D(0), D(0).rotate(R(500)));
    }

    #[test]
    fn simple_rotation_left() {
        use Dial as D;
        use Rotation::L;
        assert_eq!(D(99), D(0).rotate(L(1)));
        assert_eq!(D(98), D(0).rotate(L(2)));
        assert_eq!(D(0), D(0).rotate(L(500)));
        assert_eq!(D(99), D(0).rotate(L(501)));

        assert_eq!(D(49), D(50).rotate(L(1)));
        assert_eq!(D(0), D(50).rotate(L(50)));
        assert_eq!(D(98), D(50).rotate(L(52)));
    }

    #[test]
    fn count_zero_zeros() {
        use Dial as D;
        use Rotation::{L, R};
        assert_eq!(0, D(0).count_zeros(L(0)));
        assert_eq!(0, D(0).count_zeros(R(0)));
        assert_eq!(0, D(0).count_zeros(L(1)));
        assert_eq!(0, D(0).count_zeros(R(1)));

        assert_eq!(0, D(0).count_zeros(R(99)));
        assert_eq!(0, D(1).count_zeros(R(98)));

        assert_eq!(0, D(0).count_zeros(L(99)));
        assert_eq!(0, D(99).count_zeros(L(98)));
    }

    #[test]
    fn count_full_rotation_zeros() {
        use Dial as D;
        use Rotation::{L, R};
        // 1, 2, 3 full rotations
        for n in 1..=3 {
            let r = n as u16 * DIAL_SIZE;
            assert_eq!(n, D(1).count_zeros(L(r)));
            assert_eq!(n, D(50).count_zeros(L(r)));
            assert_eq!(n, D(99).count_zeros(L(r)));
            assert_eq!(n, D(1).count_zeros(R(r)));
            assert_eq!(n, D(50).count_zeros(R(r)));
            assert_eq!(n, D(99).count_zeros(R(r)));
        }
    }

    #[test]
    fn count_zeros_simple() {
        use Dial as D;
        use Rotation::{L, R};
        assert_eq!(1, D(1).count_zeros(L(1)));
        assert_eq!(1, D(99).count_zeros(R(1)));

        assert_eq!(2, D(1).count_zeros(L(101)));
        assert_eq!(2, D(99).count_zeros(R(101)));
    }

    #[test]
    fn given_examples() {
        use Rotation::{L, R};
        let rotations = [
            L(68),
            L(30),
            R(48),
            L(5),
            R(60),
            L(55),
            L(1),
            L(99),
            R(14),
            L(82),
        ];
        assert_eq!(3, part1(&rotations), "part1");
        assert_eq!(6, part2(&rotations), "part2");
    }
}
