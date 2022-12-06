use std::char;
use std::collections::HashSet;
use std::fs::read_to_string;
use std::path::Path;

const INPUT_PATH_REL: &str = "../../../inputs/day_6.txt";
const SRC_PATH: &str = concat!(env!("CARGO_MANIFEST_DIR"), "/", file!());

fn look_for_n_distinct_chars(n: usize, message: &[char]) -> usize {
    for (idx, window) in message.windows(n).enumerate() {
        let set = window.iter().collect::<HashSet<_>>();
        if set.len() == n {
            return idx + n;
        }
    }
    unreachable!()
}

fn main() {
    let input_path = Path::new(SRC_PATH).parent().unwrap().join(INPUT_PATH_REL);
    let input = read_to_string(input_path).unwrap();

    let chars = input.chars().collect::<Vec<_>>();
    println!(
        "Position for 4 chars is {}",
        look_for_n_distinct_chars(4, chars.as_slice())
    );
    println!(
        "Position for 14 chars is {}",
        look_for_n_distinct_chars(14, chars.as_slice())
    );
}
