use std::{error::Error, fs, path::Path};

const INPUT_PATH_REL: &str = "../../../inputs/day_01.txt";
const SRC_PATH: &str = concat!(env!("CARGO_MANIFEST_DIR"), "/", file!());

fn main() -> Result<(), Box<dyn Error>> {
    let input_path = Path::new(SRC_PATH).parent().unwrap().join(INPUT_PATH_REL);
    let input = fs::read_to_string(input_path)?;
    println!("Problem 1: {}", problem_1(&input));
    println!("Problem 2: {}", problem_2(&input));
    Ok(())
}

fn problem_1(input: &str) -> String {
    input
        .lines()
        .map(|s| s.parse::<isize>().unwrap())
        .map(|v| v / 3 - 2)
        .sum::<isize>()
        .to_string()
}

fn problem_2(input: &str) -> String {
    input
        .lines()
        .map(|s| s.parse::<isize>().unwrap())
        .map(compute_fuel)
        .sum::<isize>()
        .to_string()
}

fn compute_fuel(mass: isize) -> isize {
    let mut total_fuel = 0;
    let mut current_mass = mass;
    loop {
        current_mass = current_mass / 3 - 2;
        if current_mass <= 0 {
            break;
        }
        total_fuel += current_mass;
    }
    total_fuel
}