/// https://adventofcode.com/2022/day/8
use ndarray::Array2;
use std::{
    fs::File,
    io::{BufReader, Seek},
};

use crate::utils::{count_rows_cols, parse_lines};

pub fn run() -> std::io::Result<()> {
    let file = File::open("../inputs/aoc/2022/day8.txt")?;
    let mut reader = BufReader::new(file);
    let (rows, cols) = count_rows_cols(&mut reader);

    reader.seek(std::io::SeekFrom::Start(0))?;

    let mut grid = Array2::<u8>::default((rows, cols));
    let mut row = 0;

    parse_lines(&mut reader, 100, |line| {
        for (col, c) in line.chars().enumerate() {
            grid[[row, col]] = c as u8;
        }
        row += 1;
    });

    println!("Day 8");
    println!("Part 1: {:?}", count_visible_trees(&grid));

    Ok(())
}

fn count_visible_trees(grid: &Array2<u8>) -> usize {
    let mut visible = Array2::<bool>::default(grid.dim());

    for r in 0..grid.nrows() {
        let mut max_height_left = None;

        for c in 0..grid.ncols() {
            let tree_height = grid[[r, c]];

            match max_height_left {
                Some(max) => {
                    if tree_height > max {
                        max_height_left = Some(tree_height);
                        visible[[r, c]] = true;
                    }
                }
                None => {
                    max_height_left = Some(tree_height);
                    visible[[r, c]] = true;
                }
            }
        }

        let mut max_height_right = None;
        for c in (0..grid.ncols()).rev() {
            let tree_height = grid[[r, c]];

            match max_height_right {
                Some(max) => {
                    if tree_height > max {
                        max_height_right = Some(tree_height);
                        visible[[r, c]] = true;
                    }
                }
                None => {
                    max_height_right = Some(tree_height);
                    visible[[r, c]] = true;
                }
            }
        }
    }

    for c in 0..grid.ncols() {
        let mut max_height_top = None;

        for r in 0..grid.nrows() {
            let tree_height = grid[[r, c]];

            match max_height_top {
                Some(max) => {
                    if tree_height > max {
                        max_height_top = Some(tree_height);
                        visible[[r, c]] = true;
                    }
                }
                None => {
                    max_height_top = Some(tree_height);
                    visible[[r, c]] = true;
                }
            }
        }

        let mut max_height_bottom = None;
        for r in (0..grid.nrows()).rev() {
            let tree_height = grid[[r, c]];

            match max_height_bottom {
                Some(max) => {
                    if tree_height > max {
                        max_height_bottom = Some(tree_height);
                        visible[[r, c]] = true;
                    }
                }
                None => {
                    max_height_bottom = Some(tree_height);
                    visible[[r, c]] = true;
                }
            }
        }
    }
    visible.iter().filter(|&&is_visible| is_visible).count()
}

#[cfg(test)]
mod day8_tests {
    use super::*;
    use ndarray::arr2;

    #[test]
    fn test_count_visible_trees() {
        let grid = arr2(&[
            [3, 0, 3, 7, 3],
            [2, 5, 5, 1, 2],
            [6, 5, 3, 3, 2],
            [3, 3, 5, 4, 9],
            [3, 5, 3, 9, 0],
        ]);

        assert_eq!(21, count_visible_trees(&grid));
    }
}
