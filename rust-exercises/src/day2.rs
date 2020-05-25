// Advent of Code 2019 day 2 solution.

use std::fs::File;
use std::io::prelude::Read;
use std::io::BufReader;

pub fn run() -> std::io::Result<()> {
    let file = File::open("../inputs/aoc/2019/input-day2.txt")?;
    let mut buf_reader = BufReader::new(file);
    let mut inputs = String::new();
    buf_reader.read_to_string(&mut inputs)?;

    let program: Vec<_> = inputs
        .split(',')
        .map(|s| s.parse::<usize>())
        .filter_map(Result::ok)
        .collect();

    if program.len() < 3 {
        panic!("The program input should have more values!");
    }

    println!("== Day 2 ==");
    println!("Part 1:");
    let mut memory = program.to_vec();
    memory[1] = 12;
    memory[2] = 2;
    run_program(&mut memory);
    println!("{}", memory[0]);

    println!("Part 2:");

    for n in 0..100 {
        for v in 0..100 {
            let mut memory = program.to_vec();
            memory[1] = n;
            memory[2] = v;
            run_program(&mut memory);
            if memory[0] == 19_690_720 {
                println!("{}", 100 * n + v);
                return Ok(());
            }
        }
    }
    Ok(())
}

fn run_program(mut program: &mut Vec<usize>) {
    for op_code_addr in (0..program.len()).step_by(4) {
        let op_code = program[op_code_addr];
        match op_code {
            1 => apply_op(|a, b| a + b, op_code_addr, &mut program),
            2 => apply_op(|a, b| a * b, op_code_addr, &mut program),
            99 => return,
            _ => panic!("unknown opcode: {}", op_code),
        }
    }
}

fn apply_op<F>(op: F, op_code_addr: usize, program: &mut Vec<usize>)
where
    F: FnOnce(usize, usize) -> usize,
{
    let a_addr = program[op_code_addr + 1];
    let b_addr = program[op_code_addr + 2];
    let result_addr = program[op_code_addr + 3];
    let a = program[a_addr];
    let b = program[b_addr];
    program[result_addr] = op(a, b);
}

#[cfg(test)]
mod day2_tests {
    use super::*;

    #[test]
    fn test_run_program1() {
        let mut program = vec![1, 0, 0, 0, 99];
        run_program(&mut program);
        assert_eq!(program, vec![2, 0, 0, 0, 99]);
    }

    #[test]
    fn test_run_program2() {
        let mut program = vec![2, 3, 0, 3, 99];
        run_program(&mut program);
        assert_eq!(program, vec![2, 3, 0, 6, 99]);
    }

    #[test]
    fn test_run_program3() {
        let mut program = vec![2, 4, 4, 5, 99, 0];
        run_program(&mut program);
        assert_eq!(program, vec![2, 4, 4, 5, 99, 9801]);
    }

    #[test]
    fn test_run_program4() {
        let mut program = vec![1, 1, 1, 4, 99, 5, 6, 0, 99];
        run_program(&mut program);
        assert_eq!(program, vec![30, 1, 1, 4, 2, 5, 6, 0, 99]);
    }

    #[test]
    fn run_program_should_halt() {
        let mut program = vec![99, 0, 0, 0, 1, 0, 0, 0];
        // should halt immediately and not mutate the program.
        run_program(&mut program);
        assert_eq!(program, program);
    }
}
