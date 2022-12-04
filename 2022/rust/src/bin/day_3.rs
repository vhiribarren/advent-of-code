use std::collections::HashSet;
use std::fs::read_to_string;
use std::path::Path;

const INPUT_PATH_REL: &str = "../../../inputs/day_3.txt";
const SRC_PATH: &str = concat!(env!("CARGO_MANIFEST_DIR"), "/", file!());

fn item_score(item: char) -> u32 {
    let ord_value = item as u32;
    match item {
        'a'..='z' => ord_value - 'a' as u32 + 1,
        'A'..='Z' => ord_value - 'A' as u32 + 27,
        _ => panic!(),
    }
}

fn main() {
    let input_path = Path::new(SRC_PATH).parent().unwrap().join(INPUT_PATH_REL);
    let input = read_to_string(input_path).unwrap();

    let items = input.lines().map(|s| s.trim()).collect::<Vec<_>>();

    let mut priority_1 = 0;
    let compartments = items
        .iter()
        .map(|s| s.split_at(s.len() / 2))
        .map(|(l, r)| [l, r].map(|i| i.chars().collect::<HashSet<_>>()))
        .collect::<Vec<_>>();
    for [l, r] in compartments {
        let overlaping_items = l.intersection(&r).collect::<Vec<_>>();
        if let Some(item) = overlaping_items.first() {
            priority_1 += item_score(**item);
        }
    }
    println!("{priority_1}");

    let mut priority_2 = 0;
    let mut item_iter = items.iter();
    loop {
        let common_element_set = item_iter
            .by_ref()
            .take(3)
            .map(|s| s.to_string().chars().collect::<HashSet<_>>())
            .reduce(|accum, item| accum.intersection(&item).copied().collect());
        match common_element_set {
            None => break,
            Some(item_set) => {
                let item = item_set.into_iter().next().unwrap();
                priority_2 += item_score(item);
            }
        };
    }
    println!("{priority_2}");
}
