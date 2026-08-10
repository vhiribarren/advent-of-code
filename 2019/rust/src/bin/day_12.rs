use std::{
    dbg,
    error::Error,
    fs,
    path::{Path, PathBuf},
    sync::LazyLock,
    unimplemented, vec,
};

use regex::Regex;

const INPUT_FILENAME: &str = "day_12.txt";

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

type Triple = [isize; 3];

fn problem_1(input: &str) -> Result<String, Box<dyn Error>> {
    let mut moons_pos = parse_input(input);
    let mut moons_speed = vec![[0; 3]; moons_pos.len()];
    let moon_count = moons_pos.len();
    for _ in 0..1000 {
        let mut moons_gravity = vec![[0; 3]; moons_pos.len()];
        for i in 0..moon_count {
            let curr_moon_pos = &moons_pos[i];
            for j in 0..moon_count {
                if i == j {
                    continue;
                }
                let adj_moon_pos = &moons_pos[j];
                for k in 0..3 {
                    moons_gravity[i][k] += if curr_moon_pos[k] < adj_moon_pos[k] {
                        1
                    } else if curr_moon_pos[k] > adj_moon_pos[k] {
                        -1
                    } else {
                        0
                    };
                }
            }
        }
        for i in 0..moon_count {
            for k in 0..3 {
                moons_speed[i][k] += moons_gravity[i][k];
                moons_pos[i][k] += moons_speed[i][k];
            }
        }
    }
    let pot = moons_pos
        .into_iter()
        .map(|p| p.into_iter().fold(0, |acc, v| acc + v.abs()))
        .collect::<Vec<_>>();
    let kin = moons_speed
        .into_iter()
        .map(|p| p.into_iter().fold(0, |acc, v| acc + v.abs()))
        .collect::<Vec<_>>();
    let result = pot
        .into_iter()
        .zip(kin.into_iter())
        .fold(0, |acc, v| acc + v.0 * v.1);
    Ok(result.to_string())
}

fn problem_2(input: &str) -> Result<String, Box<dyn Error>> {
    unimplemented!()
}

fn parse_input(input: &str) -> Vec<Triple> {
    let re = Regex::new(r"<x=(.*), y=(.*), z=(.*)>").unwrap();
    let mut results = vec![];
    for (_, [x, y, z]) in re.captures_iter(input).map(|c| c.extract()) {
        results.push([x.parse().unwrap(), y.parse().unwrap(), z.parse().unwrap()]);
    }
    results
}
