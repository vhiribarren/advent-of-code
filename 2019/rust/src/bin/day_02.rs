use std::{error::Error, cmp, fs, path::Path};

const INPUT_PATH_REL: &str = "../../../inputs/day_02.txt";
const SRC_PATH: &str = concat!(env!("CARGO_MANIFEST_DIR"), "/", file!());

fn main() -> Result<(), Box<dyn Error>> {
    let input_path = Path::new(SRC_PATH).parent().unwrap().join(INPUT_PATH_REL);
    let input = fs::read_to_string(input_path)?;
    println!("Problem 1: {}", problem_1(&input));
    println!("Problem 2: {}", problem_2(&input));
    Ok(())
}

fn problem_1(input: &str) -> String {
    let mut program = parse_input(input);
    program[1] = 12;
    program[2] = 2;
    compute_result(program).to_string()
}

fn problem_2(input: &str) -> String {
    const TARGET: usize = 19690720;
    let max_val = cmp::min(99, input.len() - 1);
    let init_program = parse_input(input);
    for noun in 0..=max_val {
        for verb in 0..=max_val {
            let mut program = init_program.clone();
            program[1] = noun;
            program[2] = verb;
            if compute_result(program) == TARGET {
                return (100*noun + verb).to_string();
            }
        }
    }
    unreachable!()
}

fn parse_input(input: &str) -> Vec<usize> {
    input.split(',').map(|s| s.parse().unwrap()).collect()
}

fn compute_result(mut program: Vec<usize>) -> usize {
    let mut inst_ptr = 0;
    loop {
        match program[inst_ptr] {
            1 => {
                let dest_idx = program[inst_ptr+3];
                program[dest_idx] = program[program[inst_ptr+1]] + program[program[inst_ptr+2]];
                inst_ptr += 4
            }
            2 => {
                let dest_idx = program[inst_ptr+3];
                program[dest_idx] = program[program[inst_ptr+1]] * program[program[inst_ptr+2]];
                inst_ptr += 4
            }
            99 => break,
            _ => unimplemented!(),
        }
    }
    program[0]
}