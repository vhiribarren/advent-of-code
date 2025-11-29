use std::{
    error::Error,
    fs,
    path::{Path, PathBuf},
    sync::LazyLock,
};

const INPUT_FILENAME: &str = "day_05.txt";

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

fn problem_1(input: &str) -> Result<String, Box<dyn Error>> {
    let computer = &mut intcode::IntCodeComputer::from_input(input);
    let result = computer.run_program(vec![1]);
    Ok(result.last().unwrap().to_string())
}

fn problem_2(input: &str) -> Result<String, Box<dyn Error>> {
    unimplemented!()
}

pub mod intcode {
    use std::collections::VecDeque;

    type IntCode = isize;

    #[derive(Debug)]
    enum Mode {
        Position,
        Immediate,
    }
    impl From<usize> for Mode {
        fn from(num_mode: usize) -> Self {
            match num_mode {
                0 => Mode::Position,
                1 => Mode::Immediate,
                _ => unreachable!("Invalid parameter mode"),
            }
        }
    }

    pub struct IntCodeComputer {
        pub program: Vec<IntCode>,
    }

    impl IntCodeComputer {
        pub fn from_input(input: &str) -> Self {
            let program = input
                .split(',')
                .map(|s| s.parse().expect("intcode parse error"))
                .collect();
            Self { program }
        }

        pub fn run_program(&mut self, input: Vec<isize>) -> Vec<isize> {
            let mut input = VecDeque::from(input);
            let mut output = Vec::new();
            let mut inst_ptr = 0;
            loop {
                let (opcode, mut modes) = Self::decode_inst(self.program[inst_ptr] as usize);
                match opcode {
                    1 => {  // add
                        let params_ptr: usize = inst_ptr + 1;
                        let val_left = self.fetch(params_ptr, modes.pop());
                        let val_right = self.fetch(params_ptr + 1, modes.pop());
                        let dest_idx = self.program[params_ptr + 2] as usize;
                        self.program[dest_idx] = val_left + val_right;
                        inst_ptr += 4
                    }
                    2 => {  // multiply
                        let params_ptr: usize = inst_ptr + 1;
                        let val_left = self.fetch(params_ptr, modes.pop());
                        let val_right = self.fetch(params_ptr + 1, modes.pop());
                        let dest_idx = self.program[params_ptr + 2] as usize;
                        self.program[dest_idx] = val_left * val_right;
                        inst_ptr += 4
                    }
                    3 => {  // get input
                        let dest_idx = self.program[inst_ptr + 1] as usize;
                        self.program[dest_idx] = input.pop_front().unwrap();
                        inst_ptr += 2
                    }
                    4 => {  // write output
                        let val = self.fetch(inst_ptr + 1, modes.pop());
                        output.push(val);
                        inst_ptr += 2
                    }
                    99 => break,
                    _ => unimplemented!(),
                }
            }
            output
        }

        fn decode_inst(instruction: usize) -> (isize, Vec<Mode>) {
            let opcode = instruction % 100;
            let mut modes = Vec::new();
            let mut encoded_modes = instruction / 100;
            while encoded_modes != 0 {
                modes.push((encoded_modes % 10).into());
                encoded_modes = encoded_modes / 10;
            }
            modes.reverse();
            (opcode as isize, modes)
        }

        fn fetch(&self, param_ptr: usize, mode: Option<Mode>) -> isize {
            let param_value = self.program[param_ptr];
            match mode.unwrap_or(Mode::Position) {
                Mode::Position => self.program[param_value as usize] as isize,
                Mode::Immediate => param_value as isize,
            }
        }
    }
}
