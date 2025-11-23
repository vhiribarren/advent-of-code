use std::{
    collections::BTreeSet,
    error::Error,
    fs,
    path::{Path, PathBuf},
    sync::LazyLock,
};

const INPUT_FILENAME: &str = "day_03.txt";

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

struct Instruction {
    dir: char,
    dist: usize,
}
type Coords = (isize, isize);

fn parse_line(line: &str) -> Vec<Instruction> {
    line.split(',')
        .map(|s| s.split_at(1))
        .map(|v| Instruction {
            dir: v.0.chars().next().unwrap(),
            dist: v.1.parse().unwrap(),
        })
        .collect()
}

fn map_direction(c: char) -> (isize, isize) {
    match c {
        'U' => (0, 1),
        'D' => (0, -1),
        'L' => (-1, 0),
        'R' => (1, 0),
        _ => unreachable!(),
    }
}

fn problem_1(input: &str) -> Result<String, Box<dyn Error>> {
    let all_instructions = input.lines().map(parse_line).collect::<Vec<_>>();
    let mut all_visited = Vec::<_>::new();
    for instructions in all_instructions {
        let mut visited = BTreeSet::<Coords>::new();
        let mut current_coord = (0, 0);
        for instruction in instructions {
            let d = map_direction(instruction.dir);
            for _ in 1..=instruction.dist {
                current_coord.0 += d.0;
                current_coord.1 += d.1;
                visited.insert(current_coord);
            }
        }
        all_visited.push(visited);
    }
    Ok(all_visited[0]
        .intersection(&all_visited[1])
        .map(|(x, y)| x.abs() + y.abs())
        .min()
        .unwrap()
        .to_string())
}

fn problem_2(input: &str) -> Result<String, Box<dyn Error>> {
    let all_instructions = input.lines().map(parse_line).collect::<Vec<_>>();
    let mut all_visited = Vec::<_>::new();
    for instructions in &all_instructions {
        let mut visited = BTreeSet::<Coords>::new();
        let mut current_coord = (0, 0);
        for instruction in instructions {
            let d = map_direction(instruction.dir);
            for _ in 1..=instruction.dist {
                current_coord.0 += d.0;
                current_coord.1 += d.1;
                visited.insert(current_coord);
            }
        }
        all_visited.push(visited);
    }
    let result = all_visited[0]
        .intersection(&all_visited[1])
        .map(|intersect_coords| {
            let mut step_count = 0;
            for instructions in &all_instructions {
                let mut current_coord = (0, 0);
                'exit: for instruction in instructions {
                    let d = map_direction(instruction.dir);
                    for _ in 1..=instruction.dist {
                        current_coord.0 += d.0;
                        current_coord.1 += d.1;
                        step_count += 1;
                        if current_coord.0 == intersect_coords.0
                            && current_coord.1 == intersect_coords.1
                        {
                            break 'exit;
                        }
                    }
                }
            }
            step_count
        })
        .min()
        .unwrap()
        .to_string();
    Ok(result)
}
