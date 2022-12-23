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

    println!("Day 9");
    println!("- Part 1: {}", simulate_rope(&cmds, 0).len());
    println!("- Part 2: {}", simulate_rope(&cmds, 8).len());

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

    /// Increment the point in the given direction.
    fn increment(&mut self, d: Direction) {
        match d {
            Direction::Up => self.y += 1,
            Direction::Down => self.y -= 1,
            Direction::Left => self.x -= 1,
            Direction::Right => self.x += 1,
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
    fn drag_along(&mut self, p: &Point) {
        let dx = p.x - self.x;
        let dy = p.y - self.y;

        match (dx.abs(), dy.abs()) {
            // head and tail are touching, so do nothing
            (1, 1) | (1, 0) | (0, 1) | (0, 0) => {}
            // Head is two or more steps away from tail, so move tail in the same
            // direction as the head until its one step away.
            (dx_abs, 0) => {
                self.x += dx.signum() * (dx_abs - 1);
            }
            (0, dy_abs) => {
                self.y += dy.signum() * (dy_abs - 1);
            }
            // Head and tail are not on the same x or y axis. They must be 2 or more
            // away from each other on either x or y axis due to the pattern
            // matching above. So move the tail one step diagonally to keep up.
            (dx_abs, 1) => {
                self.x += dx.signum() * (dx_abs - 1);
                self.y = p.y;
            }
            (1, dy_abs) => {
                self.x = p.x;
                self.y += dy.signum() * (dy_abs - 1);
            }
            (dx_abs, dy_abs) => {
                self.x += dx.signum() * (dx_abs - 1);
                self.y += dy.signum() * (dy_abs - 1);
            }
        };
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

fn simulate_rope(cmds: &Vec<Command>, num_inner_knots: u8) -> HashSet<Point> {
    let mut rope = Vec::with_capacity(num_inner_knots as usize + 2);
    for _ in 0..(num_inner_knots + 2) {
        rope.push(Point::new(0, 0));
    }
    let mut tail_positions = HashSet::new();

    tail_positions.insert(rope[rope.len() - 1]);

    for c in cmds {
        for _ in 0..c.distance {
            for i in 0..rope.len() {
                let mut curr = rope[i];
                if i == 0 {
                    curr.increment(c.dir);
                } else {
                    let prev = rope[i - 1];
                    curr.drag_along(&prev);
                }
                if i == rope.len() - 1 {
                    tail_positions.insert(curr);
                }
                rope[i] = curr;
            }
        }
    }
    tail_positions
}

#[cfg(test)]
mod day9_tests {
    use super::*;

    #[test]
    fn test_drag_tail_nowhere() {
        let mut tail = Point::new(0, 0);
        let expected = Point::new(0, 0);

        tail.drag_along(&Point::new(0, 0));
        assert_eq!(expected, tail);

        tail.drag_along(&Point::new(1, 0));
        assert_eq!(expected, tail);

        tail.drag_along(&Point::new(1, 1));
        assert_eq!(expected, tail);

        tail.drag_along(&Point::new(-1, -1));
        assert_eq!(expected, tail);
    }

    #[test]
    fn test_drag_tail_straight_by_1() {
        let mut tail = Point::new(0, 0);
        // drag rope straight up.
        // H...    .H..
        // .... -> .T..
        // T...    ....
        tail.drag_along(&Point::new(0, 2));
        assert_eq!(Point::new(0, 1), tail);

        // drag rope straight down.
        // T...    ....
        // .... -> .T..
        // H...    .H..
        tail.drag_along(&Point::new(0, -1));
        assert_eq!(Point::new(0, 0), tail);

        // drag rope straight left.
        // .HT -> H.T -> HT.
        tail.drag_along(&Point::new(-2, 0));
        assert_eq!(Point::new(-1, 0), tail);

        // drag rope straight right.
        // .HT -> H.T -> HT.
        tail.drag_along(&Point::new(1, 0));
        assert_eq!(Point::new(0, 0), tail);
    }

    #[test]
    fn test_drag_tail_diagonal_by_1() {
        let mut tail = Point::new(0, 0);

        // ....    ....
        // ..H. -> .TH.
        // T...    ....
        // assert_eq!(
        //     drag_tail(&Rope::new(Point::new(2, 1), Point::new(0, 0))).unwrap(),
        //     Rope::new(Point::new(2, 1), Point::new(1, 1))
        // );
        tail.drag_along(&Point::new(2, 1));
        assert_eq!(Point::new(1, 1), tail);

        // ....    ....
        // .H..    .H..
        // .... -> .T..
        // T...    ....
        tail = Point::new(0, 0);
        tail.drag_along(&Point::new(1, 2));
        assert_eq!(Point::new(1, 1), tail);

        // ...T    ....
        // ....    ..T.
        // ..H. -> ..H.
        // ....    ....
        tail = Point::new(0, 0);
        tail.drag_along(&Point::new(-1, -2));
        assert_eq!(Point::new(-1, -1), tail);
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
R 2";
        let mut reader = BufReader::new(input.as_bytes());
        let cmds = parse_commands(&mut reader);

        let actual = simulate_rope(&cmds, 0);
        let expected = HashSet::from_iter(vec![
            Point { x: 0, y: 0 },
            Point { x: 2, y: 2 },
            Point { x: 4, y: 2 },
            Point { x: 4, y: 3 },
            Point { x: 1, y: 2 },
            Point { x: 4, y: 1 },
            Point { x: 1, y: 0 },
            Point { x: 2, y: 4 },
            Point { x: 3, y: 4 },
            Point { x: 3, y: 2 },
            Point { x: 3, y: 0 },
            Point { x: 2, y: 0 },
            Point { x: 3, y: 3 },
        ]);
        assert_eq!(actual, expected);
        // Tail should never move in a 10 knot rope.
        assert_eq!(1, simulate_rope(&cmds, 8).len());
    }

    #[test]
    fn test_simulating_10_knot_rope() {
        let input = "
R 5
U 8
L 8
D 3
R 17
D 10
L 25
U 20";
        let mut reader = BufReader::new(input.as_bytes());
        let cmds = parse_commands(&mut reader);
        let actual = simulate_rope(&cmds, 8);

        let expected = HashSet::from_iter(vec![
            Point { x: -2, y: -5 },
            Point { x: 4, y: 5 },
            Point { x: -5, y: -2 },
            Point { x: 0, y: -5 },
            Point { x: 9, y: -1 },
            Point { x: 2, y: 4 },
            Point { x: 6, y: 4 },
            Point { x: 8, y: 2 },
            Point { x: 3, y: -5 },
            Point { x: 7, y: 3 },
            Point { x: 4, y: -5 },
            Point { x: -7, y: 0 },
            Point { x: 0, y: 0 },
            Point { x: 1, y: 1 },
            Point { x: -6, y: -1 },
            Point { x: 2, y: 2 },
            Point { x: -4, y: -3 },
            Point { x: -3, y: -4 },
            Point { x: -11, y: 6 },
            Point { x: 1, y: 3 },
            Point { x: 2, y: -5 },
            Point { x: 1, y: -5 },
            Point { x: 5, y: 5 },
            Point { x: 10, y: 0 },
            Point { x: -10, y: 3 },
            Point { x: 9, y: 1 },
            Point { x: 6, y: -4 },
            Point { x: -11, y: 4 },
            Point { x: -1, y: -5 },
            Point { x: -11, y: 5 },
            Point { x: -8, y: 1 },
            Point { x: 7, y: -3 },
            Point { x: 5, y: -5 },
            Point { x: -9, y: 2 },
            Point { x: 3, y: 5 },
            Point { x: 8, y: -2 },
        ]);
        assert_eq!(36, actual.len());
        assert_eq!(actual, expected);
    }
}
