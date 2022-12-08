mod day1;
mod day2;
mod day3;

fn main() -> std::io::Result<()> {
    day1::run()?;
    day2::run()?;
    day3::run()?;
    Ok(())
}
