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

/// Apply the given parse function to 3 lines at a time from the reader until
/// EOF. Line endings are trimmed off. Uses a string buffer of the given
/// capacity to avoid string allocation for each line.
///
/// Note that lines at the end of the file that are now a multiple of 3 are
/// dropped.
///
/// # Examples
///
/// Basic usage:
///
/// ```
/// parse_lines_by3(&mut reader, 100, |line1, line2, line3| {
///     println!("{}, {}, {}", line1, line2, line3);
/// });
/// ```
pub fn parse_lines_by3<R, F>(reader: &mut R, capacity: usize, mut parser: F)
where
    R: BufRead,
    F: FnMut(&str, &str, &str),
{
    let mut a = String::with_capacity(capacity);
    let mut b = String::with_capacity(capacity);
    let mut c = String::with_capacity(capacity);

    loop {
        let bytes_read_tuple = (
            reader.read_line(&mut a),
            reader.read_line(&mut b),
            reader.read_line(&mut c),
        );

        match bytes_read_tuple {
            (Ok(0), Ok(_), Ok(_)) => break, // EOF reached
            (Ok(_), Ok(0), Ok(_)) => break, // EOF reached
            (Ok(_), Ok(_), Ok(0)) => break, // EOF reached
            (Ok(_), Ok(_), Ok(_)) => {
                parser(&a.trim_end(), &b.trim_end(), &c.trim_end());
                a.clear();
                b.clear();
                c.clear();
            }
            _ => break,
        }
    }
}
