mod day1;
mod day3;

fn main() -> std::io::Result<()> {
    day1::run()?;
    day3::run()?;
    Ok(())
}
