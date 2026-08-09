use std::{
    collections::{HashMap, HashSet}, error::Error, fs, path::{Path, PathBuf}, sync::LazyLock, unimplemented,
};

use num_integer::gcd;

const INPUT_FILENAME: &str = "day_10.txt";

static INPUT_FILE: LazyLock<PathBuf> = LazyLock::new(|| {
    Path::new(env!("CARGO_MANIFEST_DIR"))
        .join("../inputs")
        .join(INPUT_FILENAME)
});

fn main() -> Result<(), Box<dyn Error>> {
    let input = fs::read_to_string(&*INPUT_FILE)?;
    println!("Problem 1: {}", problem_1(&input)?);
    println!("Problem 2: {}", problem_2(&input)?);
    Ok(())
}

type NormalizedVector = (isize, isize);
type Count = usize;

fn problem_1(input: &str) -> Result<String, Box<dyn Error>> {
    let map = parse_input(input);
    let mut results = HashMap::<Coords, Count>::new();
    for candidate in &map {
        let mut nvects = HashSet::<NormalizedVector>::new();
        for asteroid in &map {
            if candidate == asteroid {
                continue;
            }
            let v = (asteroid.0 - candidate.0, asteroid.1 - candidate.1);
            let gcd = gcd(v.0.abs(), v.1.abs());
            nvects.insert((
                v.0 / gcd,
                v.1 / gcd
            ));
        }
        results.insert(*candidate, nvects.len());
    }
    Ok(results.values().max().unwrap().to_string())
}

fn problem_2(input: &str) -> Result<String, Box<dyn Error>> {
    let map = parse_input(input);
    unimplemented!()
}

type Coords = (isize, isize);

fn parse_input(input: &str) -> HashSet<Coords> {
    let mut map = HashSet::new();
    for (y, line) in input.lines().enumerate() {
        for (x, c) in line.chars().enumerate() {
            if c == '#' {
                map.insert((x as isize, y as isize));
            }
        }
    }
    map
}