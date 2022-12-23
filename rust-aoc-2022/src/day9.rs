/// https://adventofcode.com/2022/day/9
use std::{
    collections::HashSet,
    fs::File,
    io::{BufRead, BufReader},
};

use crate::utils::parse_lines;

pub fn run() -> std::io::Result<()> {
    let file = File::open("../inputs/aoc/2022/day9.txt")?;
    let mut reader = BufReader::new(file);
    let cmds = parse_commands(&mut reader);
    let ropes = simulate_rope(&cmds);

    let tail_positions: HashSet<Point> = HashSet::from_iter(ropes.iter().map(|r| r.tail));

    println!("Day 9");
    println!("- Part 1: {}", tail_positions.len());

    Ok(())
}

#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash)]
struct Point {
    x: i32,
    y: i32,
}

impl Point {
    fn new(x: i32, y: i32) -> Self {
        Self { x, y }
    }
}

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
struct Rope {
    head: Point,
    tail: Point,
}

impl Rope {
    fn new(head: Point, tail: Point) -> Self {
        Self { head, tail }
    }
}

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
struct Command {
    dir: Direction,
    distance: u32,
}

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
enum Direction {
    Up,
    Down,
    Left,
    Right,
}

impl Command {
    fn new(dir: Direction, distance: u32) -> Self {
        Self { dir, distance }
    }
}

fn parse_commands<R>(reader: &mut R) -> Vec<Command>
where
    R: BufRead,
{
    let mut commands = Vec::new();

    parse_lines(reader, 10, |line| {
        if line.is_empty() {
            return;
        }
        let mut tokens = line.split(' ');

        let cmd = (
            tokens.next(),
            tokens.next().map(|x| x.parse::<u32>()).and_then(Result::ok),
        );

        match cmd {
            (Some("U"), Some(d)) => commands.push(Command::new(Direction::Up, d)),
            (Some("D"), Some(d)) => commands.push(Command::new(Direction::Down, d)),
            (Some("L"), Some(d)) => commands.push(Command::new(Direction::Left, d)),
            (Some("R"), Some(d)) => commands.push(Command::new(Direction::Right, d)),
            _ => panic!("Invalid command"),
        }
    });
    commands
}

fn simulate_rope(cmds: &Vec<Command>) -> Vec<Rope> {
    let mut rope = Rope {
        head: Point::new(0, 0),
        tail: Point::new(0, 0),
    };
    let mut actual = Vec::new();
    actual.push(rope);
    for c in cmds {
        for _ in 0..c.distance {
            rope = increment_head(&c.dir, &rope);
            if let Some(r) = drag_tail(&rope) {
                rope = r;
            }
            actual.push(rope);
        }
    }
    actual
}

/// Increment the head in the given direction.
fn increment_head(dir: &Direction, r: &Rope) -> Rope {
    match dir {
        Direction::Up => Rope::new(Point::new(r.head.x, r.head.y + 1), r.tail),
        Direction::Down => Rope::new(Point::new(r.head.x, r.head.y - 1), r.tail),
        Direction::Left => Rope::new(Point::new(r.head.x - 1, r.head.y), r.tail),
        Direction::Right => Rope::new(Point::new(r.head.x + 1, r.head.y), r.tail),
    }
}

/// If the head is ever two steps directly up, down, left, or right from the
/// tail, the tail must also move one step in that direction so it remains close
/// enough:
///
/// ```
/// .....    .....    .....
/// .TH.. -> .T.H. -> ..TH.
/// .....    .....    .....

/// ...    ...    ...
/// .T.    .T.    ...
/// .H. -> ... -> .T.
/// ...    .H.    .H.
/// ...    ...    ...
/// ```
///
/// Otherwise, if the head and tail aren't touching and aren't in the same row or
/// column, the tail always moves one step diagonally to keep up:
///
///
/// ```
/// .....    .....    .....
/// .....    ..H..    ..H..
/// ..H.. -> ..... -> ..T..
/// .T...    .T...    .....
/// .....    .....    .....
///
/// .....    .....    .....
/// .....    .....    .....
/// ..H.. -> ...H. -> ..TH.
/// .T...    .T...    .....
/// .....    .....    .....
/// ```
fn drag_tail(current: &Rope) -> Option<Rope> {
    let h = current.head;
    let t = current.tail;

    let (dx, dy) = (h.x - t.x, h.y - t.y);

    match (dx.abs(), dy.abs()) {
        // head and tail are touching, so do nothing
        (1, 1) | (1, 0) | (0, 1) | (0, 0) => None,
        // Head is two or more steps away from tail, so move tail in the same
        // direction as the head until its one step away.
        (dx_abs, 0) => Some(Rope::new(
            h,
            Point::new(t.x + dx.signum() * (dx_abs - 1), t.y),
        )),
        (0, dy_abs) => Some(Rope::new(
            h,
            Point::new(t.x, t.y + dy.signum() * (dy_abs - 1)),
        )),
        // Head and tail are not on the same x or y axis. They must be 2 or more
        // away from each other on either x or y axis due to the pattern
        // matching above. So move the tail one step diagonally to keep up.
        (dx_abs, 1) => Some(Rope::new(
            h,
            Point::new(t.x + dx.signum() * (dx_abs - 1), h.y),
        )),
        (1, dy_abs) => Some(Rope::new(
            h,
            Point::new(h.x, t.y + dy.signum() * (dy_abs - 1)),
        )),
        _ => None, // everything else is undefined.
    }
}

#[cfg(test)]
mod day9_tests {

    use super::*;

    #[test]
    fn test_drag_tail_nowhere() {
        assert_eq!(
            drag_tail(&Rope::new(Point::new(0, 0), Point::new(0, 0))),
            None
        );
        assert_eq!(
            drag_tail(&Rope::new(Point::new(1, 0), Point::new(0, 0))),
            None
        );
        assert_eq!(
            drag_tail(&Rope::new(Point::new(0, 1), Point::new(0, 0))),
            None
        );
        assert_eq!(
            drag_tail(&Rope::new(Point::new(1, 1), Point::new(0, 0))),
            None
        );
        assert_eq!(
            drag_tail(&Rope::new(Point::new(-1, -1), Point::new(0, 0))),
            None
        );
    }

    #[test]
    fn test_drag_tail_straight_by_1() {
        // drag rope straight up.
        // ....    H...    .H..
        // H... -> .... -> .T..
        // T...    T...    ....
        assert_eq!(
            drag_tail(&Rope::new(Point::new(0, 2), Point::new(0, 0))).unwrap(),
            Rope::new(Point::new(0, 2), Point::new(0, 1))
        );
        // drag rope straight down.
        // T...    T...    ....
        // H... -> .... -> .T..
        // ....    H...    .H..
        assert_eq!(
            drag_tail(&Rope::new(Point::new(0, 0), Point::new(0, 2))).unwrap(),
            Rope::new(Point::new(0, 0), Point::new(0, 1))
        );
        // drag rope straight left.
        // .HT -> H.T -> HT.
        assert_eq!(
            drag_tail(&Rope::new(Point::new(-2, 0), Point::new(0, 0))).unwrap(),
            Rope::new(Point::new(-2, 0), Point::new(-1, 0))
        );

        // drag rope straight right.
        // TH. -> T.H -> .TH
        assert_eq!(
            drag_tail(&Rope::new(Point::new(2, 0), Point::new(0, 0))).unwrap(),
            Rope::new(Point::new(2, 0), Point::new(1, 0))
        );
    }

    #[test]
    fn test_drag_tail_straight_by_many() {
        // drag rope straight up.
        // ....    H...    .H..
        // ....    ....    .T..
        // H... -> .... -> ....
        // T...    T...    ....
        assert_eq!(
            drag_tail(&Rope::new(Point::new(0, 3), Point::new(0, 0))).unwrap(),
            Rope::new(Point::new(0, 3), Point::new(0, 2))
        );
        // drag rope straight down.
        // T...    T...    ....
        // H... -> .... -> ....
        // ....    ....    .T..
        // ....    H...    .H..
        assert_eq!(
            drag_tail(&Rope::new(Point::new(0, -3), Point::new(0, 0))).unwrap(),
            Rope::new(Point::new(0, -3), Point::new(0, -2))
        );
        // drag rope straight left.
        // ..HT -> H..T -> HT..
        assert_eq!(
            drag_tail(&Rope::new(Point::new(-3, 0), Point::new(0, 0))).unwrap(),
            Rope::new(Point::new(-3, 0), Point::new(-2, 0))
        );

        // drag rope straight right.
        // TH... -> T...H -> ...TH
        assert_eq!(
            drag_tail(&Rope::new(Point::new(4, 0), Point::new(0, 0))).unwrap(),
            Rope::new(Point::new(4, 0), Point::new(3, 0))
        );
    }

    #[test]
    fn test_drag_tail_diagonal_by_1() {
        // For each assertion below tail (T) starts at (0, 0) before being dragged:

        // ....    ....    ....
        // .H.. -> ..H. -> .TH.
        // T...    T...    ....
        assert_eq!(
            drag_tail(&Rope::new(Point::new(2, 1), Point::new(0, 0))).unwrap(),
            Rope::new(Point::new(2, 1), Point::new(1, 1))
        );
        // ....    ....    ....
        // ....    .H..    .H..
        // .H.. -> .... -> .T..
        // T...    T...    ....
        assert_eq!(
            drag_tail(&Rope::new(Point::new(1, 2), Point::new(0, 0))).unwrap(),
            Rope::new(Point::new(1, 2), Point::new(1, 1))
        );

        // ...T    ...T    ....
        // ..H.    ....    ..T.
        // .... -> ..H. -> ..H.
        // ....    ....    ....
        assert_eq!(
            drag_tail(&Rope::new(Point::new(-1, -2), Point::new(0, 0))).unwrap(),
            Rope::new(Point::new(-1, -2), Point::new(-1, -1))
        );
    }

    #[test]
    fn test_drag_tail_diagonal_by_many() {
        // For each assertion below tail (T) starts at (0, 0) before being dragged:

        // ....    ......    ......
        // .H.. -> ....H. -> ...TH.
        // T...    T.....    ......
        assert_eq!(
            drag_tail(&Rope::new(Point::new(4, 1), Point::new(0, 0))).unwrap(),
            Rope::new(Point::new(4, 1), Point::new(3, 1))
        );
        // ....    ....    ....
        // ....    .H..    .H..
        // .H.. -> .... -> .T..
        // T...    T...    ....
        assert_eq!(
            drag_tail(&Rope::new(Point::new(1, 2), Point::new(0, 0))).unwrap(),
            Rope::new(Point::new(1, 2), Point::new(1, 1))
        );

        // ...T    ...T    ....
        // ..H.    ....    ..T.
        // .... -> ..H. -> ..H.
        // ....    ....    ....
        assert_eq!(
            drag_tail(&Rope::new(Point::new(-1, -2), Point::new(0, 0))).unwrap(),
            Rope::new(Point::new(-1, -2), Point::new(-1, -1))
        );
    }

    #[test]
    fn test_parse_input() {
        let input = "
R 4
U 4
L 3
D 1
R 4
D 1
L 5
R 2
";
        let mut reader = BufReader::new(input.as_bytes());
        let expected = vec![
            Command::new(Direction::Right, 4),
            Command::new(Direction::Up, 4),
            Command::new(Direction::Left, 3),
            Command::new(Direction::Down, 1),
            Command::new(Direction::Right, 4),
            Command::new(Direction::Down, 1),
            Command::new(Direction::Left, 5),
            Command::new(Direction::Right, 2),
        ];

        assert_eq!(parse_commands(&mut reader), expected);
    }

    #[test]
    fn test_simulating_rope() {
        let input = "
R 4
U 4
L 3
D 1
R 4
D 1
L 5
R 2
";
        let mut reader = BufReader::new(input.as_bytes());
        let cmds = parse_commands(&mut reader);

        let actual = simulate_rope(&cmds);
        let expected = vec![
            Rope {
                head: Point { x: 0, y: 0 },
                tail: Point { x: 0, y: 0 },
            },
            Rope {
                head: Point { x: 1, y: 0 },
                tail: Point { x: 0, y: 0 },
            },
            Rope {
                head: Point { x: 2, y: 0 },
                tail: Point { x: 1, y: 0 },
            },
            Rope {
                head: Point { x: 3, y: 0 },
                tail: Point { x: 2, y: 0 },
            },
            Rope {
                head: Point { x: 4, y: 0 },
                tail: Point { x: 3, y: 0 },
            },
            Rope {
                head: Point { x: 4, y: 1 },
                tail: Point { x: 3, y: 0 },
            },
            Rope {
                head: Point { x: 4, y: 2 },
                tail: Point { x: 4, y: 1 },
            },
            Rope {
                head: Point { x: 4, y: 3 },
                tail: Point { x: 4, y: 2 },
            },
            Rope {
                head: Point { x: 4, y: 4 },
                tail: Point { x: 4, y: 3 },
            },
            Rope {
                head: Point { x: 3, y: 4 },
                tail: Point { x: 4, y: 3 },
            },
            Rope {
                head: Point { x: 2, y: 4 },
                tail: Point { x: 3, y: 4 },
            },
            Rope {
                head: Point { x: 1, y: 4 },
                tail: Point { x: 2, y: 4 },
            },
            Rope {
                head: Point { x: 1, y: 3 },
                tail: Point { x: 2, y: 4 },
            },
            Rope {
                head: Point { x: 2, y: 3 },
                tail: Point { x: 2, y: 4 },
            },
            Rope {
                head: Point { x: 3, y: 3 },
                tail: Point { x: 2, y: 4 },
            },
            Rope {
                head: Point { x: 4, y: 3 },
                tail: Point { x: 3, y: 3 },
            },
            Rope {
                head: Point { x: 5, y: 3 },
                tail: Point { x: 4, y: 3 },
            },
            Rope {
                head: Point { x: 5, y: 2 },
                tail: Point { x: 4, y: 3 },
            },
            Rope {
                head: Point { x: 4, y: 2 },
                tail: Point { x: 4, y: 3 },
            },
            Rope {
                head: Point { x: 3, y: 2 },
                tail: Point { x: 4, y: 3 },
            },
            Rope {
                head: Point { x: 2, y: 2 },
                tail: Point { x: 3, y: 2 },
            },
            Rope {
                head: Point { x: 1, y: 2 },
                tail: Point { x: 2, y: 2 },
            },
            Rope {
                head: Point { x: 0, y: 2 },
                tail: Point { x: 1, y: 2 },
            },
            Rope {
                head: Point { x: 1, y: 2 },
                tail: Point { x: 1, y: 2 },
            },
            Rope {
                head: Point { x: 2, y: 2 },
                tail: Point { x: 1, y: 2 },
            },
        ];
        assert_eq!(actual, expected);
    }
}
