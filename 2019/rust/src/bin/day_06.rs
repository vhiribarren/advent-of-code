use std::{
    collections::{HashMap, VecDeque},
    error::Error,
    fs,
    path::{Path, PathBuf},
    sync::LazyLock,
};

const INPUT_FILENAME: &str = "day_06.txt";

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

const START_OBJECT: &str = "COM";

#[derive(Debug)]
struct Orbit<'a> {
    distance: usize,
    object: &'a str,
}

fn problem_1(input: &str) -> Result<String, Box<dyn Error>> {
    let orbit_map = &parse_input(input);
    let bfs_orbits = &mut VecDeque::from([Orbit {
        distance: 0,
        object: START_OBJECT,
    }]);
    let distances = &mut Vec::<Orbit>::new();
    while let Some(candidate) = bfs_orbits.pop_back() {
        if let Some(next_orbits) = orbit_map.get(candidate.object) {
            next_orbits.iter().for_each(|object| {
                bfs_orbits.push_front(Orbit {
                    distance: candidate.distance + 1,
                    object,
                })
            });
        }
        distances.push(candidate);
    }
    let result = distances.iter().map(|o| o.distance).sum::<usize>();
    Ok(result.to_string())
}

fn problem_2(input: &str) -> Result<String, Box<dyn Error>> {
    unimplemented!()
}

fn parse_input(input: &str) -> HashMap<&str, Vec<&str>> {
    let mut orbits = HashMap::<&str, Vec<&str>>::new();
    input
        .lines()
        .map(|s| s.split(")").collect::<Vec<_>>())
        .for_each(|v| {
            orbits.entry(v[0]).or_default().push(v[1]);
        });
    orbits
}
