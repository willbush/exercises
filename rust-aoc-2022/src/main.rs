mod day1;
mod day2;
mod day3;
mod utils;

fn main() -> std::io::Result<()> {
    day1::run()?;
    day2::run()?;
    day3::run()?;
    Ok(())
}
