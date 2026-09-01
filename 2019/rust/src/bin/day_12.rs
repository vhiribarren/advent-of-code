use std::{
    error::Error,
    fs,
    path::{Path, PathBuf},
    sync::LazyLock,
    vec,
};

use num_integer::lcm;
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

type Triple = [i128; 3];

fn next_state(moons_pos: &mut [Triple], moons_speed: &mut [Triple]) {
    let moon_count = moons_pos.len();
    let mut moons_gravity = vec![[0; 3]; moons_pos.len()];
    for i in 0..moon_count {
        let curr_moon_pos = &moons_pos[i];
        #[allow(clippy::needless_range_loop)]
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

fn problem_1(input: &str) -> Result<String, Box<dyn Error>> {
    let mut moons_pos = parse_input(input);
    let mut moons_speed = vec![[0; 3]; moons_pos.len()];
    for _ in 0..1000 {
        next_state(&mut moons_pos, &mut moons_speed);
    }
    let pot = moons_pos
        .into_iter()
        .map(|p| p.into_iter().fold(0, |acc, v| acc + v.abs()))
        .collect::<Vec<_>>();
    let kin = moons_speed
        .into_iter()
        .map(|p| p.into_iter().fold(0, |acc, v| acc + v.abs()))
        .collect::<Vec<_>>();
    let result = pot.into_iter().zip(kin).fold(0, |acc, v| acc + v.0 * v.1);
    Ok(result.to_string())
}

fn problem_2(input: &str) -> Result<String, Box<dyn Error>> {
    let mut moons_pos = parse_input(input);
    let mut moons_speed = vec![[0; 3]; moons_pos.len()];
    let init_moons_pos = moons_pos.clone();
    let init_moons_speed = moons_speed.clone();
    let moon_count = moons_pos.len();
    let mut moon_cycles: [Option<i128>; 3] = [None; 3];
    for loop_idx in 1.. {
        next_state(&mut moons_pos, &mut moons_speed);
        for axis_idx in 0..3 {
            if moon_cycles[axis_idx].is_none()
                && (0..moon_count).all(|moon_idx| {
                    moons_pos[moon_idx][axis_idx] == init_moons_pos[moon_idx][axis_idx]
                        && moons_speed[moon_idx][axis_idx] == init_moons_speed[moon_idx][axis_idx]
                })
            {
                moon_cycles[axis_idx] = Some(loop_idx);
            }
        }
        if moon_cycles.iter().all(Option::is_some) {
            break;
        }
    }
    let state_lcm = moon_cycles
        .into_iter()
        .map(Option::unwrap)
        .reduce(lcm)
        .unwrap();
    Ok(state_lcm.to_string())
}

fn parse_input(input: &str) -> Vec<Triple> {
    let re = Regex::new(r"<x=(.*), y=(.*), z=(.*)>").unwrap();
    let mut results = vec![];
    for (_, [x, y, z]) in re.captures_iter(input).map(|c| c.extract()) {
        results.push([x.parse().unwrap(), y.parse().unwrap(), z.parse().unwrap()]);
    }
    results
}
