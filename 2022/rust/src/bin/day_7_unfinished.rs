use std::char;
use std::collections::{HashMap, HashSet};
use std::fs::read_to_string;
use std::path::Path;

const INPUT_PATH_REL: &str = "../../../inputs/day_7.txt";
const SRC_PATH: &str = concat!(env!("CARGO_MANIFEST_DIR"), "/", file!());

#[derive(Debug)]
enum Node<'a> {
    Directory {
        name: &'a str,
        files: HashMap<&'a str, Node<'a>>,
    },
    File {
        name: &'a str,
    },
}

impl<'a> Node<'a> {
    fn new_directory(name: &'a str) -> Node<'a> {
        Node::Directory {
            name,
            files: HashMap::new(),
        }
    }
}

fn parse_fs<'a>(lines: &'a [&str]) -> Node<'a> {
    let mut fs = Node::new_directory("/");
    let mut current_root = &mut fs;
    let mut idx = 0;
    let mut stack = vec![];
    while idx < lines.len() {
        let inst = lines[idx].split(" ").collect::<Vec<_>>();
        if inst[0] != "$" {
            panic!()
        }
        match inst[1] {
            "cd" => {
                match inst[2] {
                    "/" => {
                        current_root = &mut fs;
                        stack.clear();
                    }
                    ".." => {
                        current_root = stack.pop().unwrap();
                    }
                    dir => {
                        if ! dir in current_root.
                    }
                };
            }
            "ls" => {}
            _ => panic!(),
        }
        idx += 1;
    }
    fs
}

fn main() {
    let input_path = Path::new(SRC_PATH).parent().unwrap().join(INPUT_PATH_REL);
    let input = read_to_string(input_path).unwrap();
    let input_lines = input.lines().collect::<Vec<_>>();

    let fs = parse_fs(&input_lines);
    //let sizes = compute_sizes(&fs);
}
