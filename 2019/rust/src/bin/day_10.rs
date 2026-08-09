use std::{
    collections::{HashMap, HashSet},
    error::Error,
    f64::consts::PI,
    fs,
    path::{Path, PathBuf},
    sync::LazyLock,
};

use itertools::Itertools;
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
type Coords = (isize, isize);

fn problem_1(input: &str) -> Result<String, Box<dyn Error>> {
    let map = parse_input(input);
    let (_, count, _) = get_best_station(&map);
    Ok(count.to_string())
}

fn problem_2(input: &str) -> Result<String, Box<dyn Error>> {
    let map = parse_input(input);
    let (_, _, rays) = get_best_station(&map);
    let result = rays
        .into_iter()
        .map(|(k, v)| ((k.0 as f64).atan2(-(k.1 as f64)).rem_euclid(2.0 * PI), v))
        .sorted_by(|l, r| l.0.total_cmp(&r.0))
        .map(|(_, v)| v.into_iter().sorted_by_key(|v| v.0 * v.0 + v.1 * v.1))
        .cycle()
        .filter_map(|mut r| r.next())
        .nth(199)
        .unwrap();
    Ok((result.0 * 100 + result.1).to_string())
}

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

fn get_best_station(
    map: &HashSet<Coords>,
) -> (Coords, Count, HashMap<NormalizedVector, Vec<Coords>>) {
    aligned_asteroids(map)
        .into_iter()
        .map(|(candidate, v)| (candidate, v.keys().len(), v))
        .max_by_key(|(_, l, _)| *l)
        .unwrap()
}

fn aligned_asteroids(
    map: &HashSet<Coords>,
) -> HashMap<Coords, HashMap<NormalizedVector, Vec<Coords>>> {
    let mut results = HashMap::new();
    for candidate in map {
        let mut same_ray = HashMap::<NormalizedVector, Vec<Coords>>::new();
        for asteroid in map {
            if candidate == asteroid {
                continue;
            }
            let v = (asteroid.0 - candidate.0, asteroid.1 - candidate.1);
            let gcd = gcd(v.0.abs(), v.1.abs());
            let normalized_v = (v.0 / gcd, v.1 / gcd);
            same_ray.entry(normalized_v).or_default().push(*asteroid);
        }
        results.insert(*candidate, same_ray);
    }
    results
}
