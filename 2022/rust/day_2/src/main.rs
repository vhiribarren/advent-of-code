use std::path::Path;
use std::{collections::HashMap, fs::read_to_string};

const INPUT_PATH_REL: &str = "../../../inputs/day_2.txt";
const SRC_PATH: &str = concat!(env!("CARGO_MANIFEST_DIR"), "/", file!());

fn main() {
    #[allow(clippy::identity_op)]
    let battle_points_1 = HashMap::from([
        (("A", "X"), 3 + 1),
        (("A", "Y"), 6 + 2),
        (("A", "Z"), 0 + 3),
        (("B", "X"), 0 + 1),
        (("B", "Y"), 3 + 2),
        (("B", "Z"), 6 + 3),
        (("C", "X"), 6 + 1),
        (("C", "Y"), 0 + 2),
        (("C", "Z"), 3 + 3),
    ]);

    #[allow(clippy::identity_op)]
    let battle_points_2 = HashMap::from([
        (("A", "X"), 0 + 3),
        (("A", "Y"), 3 + 1),
        (("A", "Z"), 6 + 2),
        (("B", "X"), 0 + 1),
        (("B", "Y"), 3 + 2),
        (("B", "Z"), 6 + 3),
        (("C", "X"), 0 + 2),
        (("C", "Y"), 3 + 3),
        (("C", "Z"), 6 + 1),
    ]);

    let input_path = Path::new(SRC_PATH).parent().unwrap().join(INPUT_PATH_REL);
    let input = read_to_string(input_path).unwrap();
    let rounds: Vec<_> = input
        .lines()
        .map(|s| s.trim())
        .map(|s| s.split_whitespace().collect::<Vec<_>>())
        .map(|v| (v[0], v[1]))
        .collect();

    let score_1 = rounds.iter().map(|r| battle_points_1[r]).sum::<i32>();
    let score_2 = rounds.iter().map(|r| battle_points_2[r]).sum::<i32>();

    println!("{score_1}");
    println!("{score_2}");
}
