use std::{
    error::Error,
    fs,
    path::{Path, PathBuf},
    sync::LazyLock,
};

use itertools::Itertools;

const INPUT_FILENAME: &str = "day_08.txt";

static INPUT_FILE: LazyLock<PathBuf> = LazyLock::new(|| {
    Path::new(env!("CARGO_MANIFEST_DIR"))
        .join("../inputs")
        .join(INPUT_FILENAME)
});

fn main() -> Result<(), Box<dyn Error>> {
    let input = fs::read_to_string(&*INPUT_FILE)?;
    println!("Problem 1: {}", problem_1(&input)?);
    problem_2(&input)?;
    Ok(())
}

const WIDTH: usize = 25;
const HEIGHT: usize = 6;

fn problem_1(input: &str) -> Result<String, Box<dyn Error>> {
    let min_layer = input
        .chars()
        .chunks(WIDTH * HEIGHT)
        .into_iter()
        .map(|chunk| chunk.collect::<Vec<_>>())
        .min_by_key(|x| x.iter().filter(|&&c| c == '0').count())
        .unwrap();

    let count = min_layer.iter().filter(|c| **c == '1').count()
        * min_layer.iter().filter(|c| **c == '2').count();
    Ok(count.to_string())
}

fn problem_2(input: &str) -> Result<(), Box<dyn Error>> {
    input
        .chars()
        .chunks(WIDTH * HEIGHT)
        .into_iter()
        .map(|layer| layer.collect::<Vec<_>>())
        .reduce(|acc, layer| {
            acc.into_iter()
                .zip(layer)
                .map(|(l, r)| if l == '2' { r } else { l })
                .collect::<Vec<_>>()
        })
        .unwrap()
        .chunks(WIDTH)
        .map(|arr| arr.iter().collect::<String>())
        .for_each(|chunk| println!("{chunk}"));
    Ok(())
}
