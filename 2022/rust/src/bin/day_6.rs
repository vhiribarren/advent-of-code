use std::char;
use std::collections::HashSet;
use std::fs::read_to_string;
use std::path::Path;

const INPUT_PATH_REL: &str = "../../../inputs/day_6.txt";
const SRC_PATH: &str = concat!(env!("CARGO_MANIFEST_DIR"), "/", file!());

fn look_for_n_distinct_chars(n: usize, message: &[char]) -> Option<usize> {
    for (idx, window) in message.windows(n).enumerate() {
        if HashSet::<&char>::from_iter(window).len() == n {
            return Some(idx + n);
        }
    }
    None
}

fn main() {
    let input_path = Path::new(SRC_PATH).parent().unwrap().join(INPUT_PATH_REL);
    let input = read_to_string(input_path).unwrap();

    let chars = input.chars().collect::<Vec<_>>();
    println!(
        "Position for 4 chars is {}",
        look_for_n_distinct_chars(4, &chars).unwrap()
    );
    println!(
        "Position for 14 chars is {}",
        look_for_n_distinct_chars(14, &chars).unwrap()
    );
}
