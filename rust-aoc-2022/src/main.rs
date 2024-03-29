#![feature(map_try_insert)]

extern crate ndarray;

mod day1;
mod day2;
mod day3;
mod day4;
mod day5;
mod day6;
mod day7;
mod day8;
mod day9;
mod day10;
mod utils;

fn main() -> std::io::Result<()> {
    day1::run()?;
    day2::run()?;
    day3::run()?;
    day4::run()?;
    day5::run()?;
    day6::run()?;
    day7::run()?;
    day8::run()?;
    day9::run()?;
    day10::run()?;

    Ok(())
}
