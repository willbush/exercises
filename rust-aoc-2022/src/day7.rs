/// https://adventofcode.com/2022/day/7
use std::{
    collections::BTreeMap,
    fs::File,
    io::{BufRead, BufReader},
    path::{Path, PathBuf},
};

use crate::utils::parse_lines;

pub fn run() -> std::io::Result<()> {
    let file = File::open("../inputs/aoc/2022/day7.txt")?;
    let mut reader = BufReader::new(file);

    let cmds = parse(&mut reader);
    let dir_to_total_size = discover_dir_sizes(&cmds);

    println!("Day 7");
    println!(
        "- Part 1: {}",
        dir_to_total_size
            .values()
            .filter(|&&size| size <= 100_000)
            .sum::<usize>()
    );

    if let Some(total_used_space) = dir_to_total_size.get(&PathBuf::from("/")) {
        let total_unused_space = 70_000_000 - total_used_space;
        let space_needed_for_update = 30_000_000 - total_unused_space;

        let size_dir_to_delete = dir_to_total_size
            .values()
            .filter(|&&size| size >= space_needed_for_update)
            .min();

        println!("- Part 2: {:?}", size_dir_to_delete);
    }

    Ok(())
}

#[derive(Debug, Clone, PartialEq)]
enum Cmd {
    Cd { path: String },
    Ls { output: Vec<LsOutput> },
}

#[derive(Debug, Clone, PartialEq)]
enum LsOutput {
    Dir { name: String },
    File { size: usize },
}

fn parse<R>(reader: &mut R) -> Vec<Cmd>
where
    R: BufRead,
{
    let mut result = Vec::new();
    let mut ls_output: Vec<LsOutput> = Vec::new();
    let mut is_ls_output = false;

    parse_lines(reader, 50, |line| {
        let cs: Vec<&str> = line.split(' ').collect();

        if line.starts_with('$') {
            if is_ls_output {
                result.push(Cmd::Ls {
                    output: ls_output.to_owned(),
                });
                ls_output.clear();
            }
            is_ls_output = false;

            match (cs.get(1), cs.get(2)) {
                (Some(&"cd"), Some(&path)) => result.push(Cmd::Cd {
                    path: path.to_string(),
                }),
                (Some(&"ls"), None) => is_ls_output = true,
                _ => {}
            }
        } else {
            match (cs.get(0), cs.get(1)) {
                (Some(&fst), Some(&snd)) => {
                    if !is_ls_output {
                        panic!("Expected ls output. Got: {}", line);
                    }

                    if fst == "dir" {
                        ls_output.push(LsOutput::Dir {
                            name: snd.to_string(),
                        });
                    } else if let Ok(size) = fst.parse() {
                        ls_output.push(LsOutput::File {
                            size
                        });
                    }
                }
                _ => {}
            }
        }
    });
    if is_ls_output {
        result.push(Cmd::Ls {
            output: ls_output.to_owned(),
        });
        ls_output.clear();
    }

    result
}

fn discover_dir_sizes(cmds: &[Cmd]) -> BTreeMap<PathBuf, usize> {
    let mut pwd = Vec::new();
    let mut result = BTreeMap::new();

    for cmd in cmds {
        match cmd {
            Cmd::Cd { path } => {
                if path == ".." {
                    pwd.pop();
                } else {
                    pwd.push(Path::new(path));
                    let path: PathBuf = pwd.iter().collect();
                    _ = result.try_insert(path, 0);
                }
            }
            Cmd::Ls { output } => {
                for o in output {
                    match o {
                        LsOutput::Dir { name } => {
                            let mut path: PathBuf = pwd.iter().collect();
                            path.push(name);
                            _ = result.try_insert(path, 0);
                        }
                        LsOutput::File { size } => {
                            for i in 0..pwd.len() {
                                let path: PathBuf = pwd[..=i].iter().collect();

                                if let Some(total) = result.get(&path) {
                                    result.insert(path, total + *size);
                                }
                            }
                        }
                    }
                }
            }
        }
    }

    result
}

#[cfg(test)]
mod day7_tests {
    use super::*;

    #[test]
    fn test_parse() {
        let input = "$ cd /
$ ls
dir a
14848514 b.txt
8504156 c.dat
dir d
$ cd a
$ ls
dir e
29116 f
2557 g
62596 h.lst
$ cd e
$ ls
584 i
$ cd ..
$ cd ..
$ cd d
$ ls
4060174 j
8033020 d.log
5626152 d.ext
7214296 k
";

        let mut reader = BufReader::new(input.as_bytes());

        let actual = parse(&mut reader);

        assert_eq!(actual.len(), 10);

        let expected = vec![
            Cmd::Cd {
                path: "/".to_string(),
            },
            Cmd::Ls {
                output: vec![
                    LsOutput::Dir {
                        name: "a".to_string(),
                    },
                    LsOutput::File {
                        size: 14848514,
                    },
                    LsOutput::File {
                        size: 8504156,
                    },
                    LsOutput::Dir {
                        name: "d".to_string(),
                    },
                ],
            },
            Cmd::Cd {
                path: "a".to_string(),
            },
            Cmd::Ls {
                output: vec![
                    LsOutput::Dir {
                        name: "e".to_string(),
                    },
                    LsOutput::File {
                        size: 29116,
                    },
                    LsOutput::File {
                        size: 2557,
                    },
                    LsOutput::File {
                        size: 62596,
                    },
                ],
            },
            Cmd::Cd {
                path: "e".to_string(),
            },
            Cmd::Ls {
                output: vec![LsOutput::File {
                    size: 584,
                }],
            },
            Cmd::Cd {
                path: "..".to_string(),
            },
            Cmd::Cd {
                path: "..".to_string(),
            },
            Cmd::Cd {
                path: "d".to_string(),
            },
            Cmd::Ls {
                output: vec![
                    LsOutput::File {
                        size: 4060174,
                    },
                    LsOutput::File {
                        size: 8033020,
                    },
                    LsOutput::File {
                        size: 5626152,
                    },
                    LsOutput::File {
                        size: 7214296,
                    },
                ],
            },
        ];

        assert_eq!(expected, actual);
    }

    #[test]
    fn test_discover_dir_sizes() {
        let cmds: Vec<Cmd> = vec![
            Cmd::Cd {
                path: "/".to_string(),
            },
            Cmd::Ls {
                output: vec![
                    LsOutput::Dir {
                        name: "a".to_string(),
                    },
                    LsOutput::File {
                        size: 14848514,
                    },
                    LsOutput::File {
                        size: 8504156,
                    },
                    LsOutput::Dir {
                        name: "d".to_string(),
                    },
                ],
            },
            Cmd::Cd {
                path: "a".to_string(),
            },
            Cmd::Ls {
                output: vec![
                    LsOutput::Dir {
                        name: "e".to_string(),
                    },
                    LsOutput::File {
                        size: 29116,
                    },
                    LsOutput::File {
                        size: 2557,
                    },
                    LsOutput::File {
                        size: 62596,
                    },
                ],
            },
            Cmd::Cd {
                path: "e".to_string(),
            },
            Cmd::Ls {
                output: vec![LsOutput::File {
                    size: 584,
                }],
            },
            Cmd::Cd {
                path: "..".to_string(),
            },
            Cmd::Cd {
                path: "..".to_string(),
            },
            Cmd::Cd {
                path: "d".to_string(),
            },
            Cmd::Ls {
                output: vec![
                    LsOutput::File {
                        size: 4060174,
                    },
                    LsOutput::File {
                        size: 8033020,
                    },
                    LsOutput::File {
                        size: 5626152,
                    },
                    LsOutput::File {
                        size: 7214296,
                    },
                ],
            },
        ];

        let actual = discover_dir_sizes(&cmds);
        let mut expected = BTreeMap::new();

        expected.insert(PathBuf::from("/"), 48381165);
        expected.insert(PathBuf::from("/a"), 94853);
        expected.insert(PathBuf::from("/a/e"), 584);
        expected.insert(PathBuf::from("/d"), 24933642);

        assert_eq!(expected, actual);
    }
}
