use std::io::BufRead;

/// Apply the given parse function to each line of the reader until EOF. Line
/// endings are trimmed off. Uses a string buffer of the given capacity to avoid
/// string allocation for each line.
///
/// # Examples
///
/// Basic usage:
///
/// ```
/// let file = File::open("your-file.txt")?;
/// let mut reader = BufReader::new(file);
///
/// parse_lines(&mut reader, 10, |line| {
///     println!("{}", line);
/// });
/// ```
pub fn parse_lines<R, F>(reader: &mut R, capacity: usize, mut parser: F)
where
    R: BufRead,
    F: FnMut(&str),
{
    let mut line = String::with_capacity(capacity);

    // Read the file line by line.
    while let Ok(bytes_read) = reader.read_line(&mut line) {
        if bytes_read == 0 {
            break; // EOF reached
        }
        parser(&line.trim_end());
        line.clear();
    }
}
