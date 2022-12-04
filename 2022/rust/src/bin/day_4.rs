use std::collections::HashSet;
use std::fs::read_to_string;
use std::path::Path;

const INPUT_PATH_REL: &str = "../../../inputs/day_4.txt";
const SRC_PATH: &str = concat!(env!("CARGO_MANIFEST_DIR"), "/", file!());

fn main() {
    let input_path = Path::new(SRC_PATH).parent().unwrap().join(INPUT_PATH_REL);
    let input = read_to_string(input_path).unwrap();

    let input_set = input
        .lines()
        .map(|s| {
            s.trim()
                .split(',')
                .map(|s| {
                    let range_values = s.split('-').map(|v| v.parse::<u32>().unwrap()).collect::<Vec<_>>();
                    let range = range_values[0]..=range_values[1];
                    range.into_iter().collect::<HashSet<_>>()
                })
                .collect::<Vec<_>>()
        })
        .collect::<Vec<_>>();

    let count_included_ranges = input_set
        .iter()
        .filter(|l| l[0].is_subset(&l[1]) || l[1].is_subset(&l[0]))
        .count();
    println!("{count_included_ranges}");

    let count_overlap_ranges = input_set
        .iter()
        .filter(|l| !l[0].is_disjoint(&l[1]))
        .count();
    println!("{count_overlap_ranges}");
}
