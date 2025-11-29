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
    let computer = &mut intcode::IntCodeComputer::from_input(input);
    let result = computer.run_program(vec![5]);
    Ok(result.first().unwrap().to_string())
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

    const OP_ADD: isize = 1;
    const OP_MULTIPLY: isize = 2;
    const OP_INPUT: isize = 3;
    const OP_OUTPUT: isize = 4;
    const OP_JUMP_IF_TRUE: isize = 5;
    const OP_JUMP_IF_FALSE: isize = 6;
    const OP_LESS_THAN: isize = 7;
    const OP_EQUALS: isize = 8;
    const OP_HALT: isize = 99;
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
                let params_ptr: usize = inst_ptr + 1;
                match opcode {
                    OP_ADD => {
                        let val_left = self.fetch(params_ptr, modes.pop());
                        let val_right = self.fetch(params_ptr + 1, modes.pop());
                        let dest_idx = self.program[params_ptr + 2] as usize;
                        self.program[dest_idx] = val_left + val_right;
                        inst_ptr += 4
                    }
                    OP_MULTIPLY => {
                        let val_left = self.fetch(params_ptr, modes.pop());
                        let val_right = self.fetch(params_ptr + 1, modes.pop());
                        let dest_idx = self.program[params_ptr + 2] as usize;
                        self.program[dest_idx] = val_left * val_right;
                        inst_ptr += 4
                    }
                    OP_INPUT => {
                        let dest_idx = self.program[params_ptr] as usize;
                        self.program[dest_idx] = input.pop_front().unwrap();
                        inst_ptr += 2
                    }
                    OP_OUTPUT => {
                        let val = self.fetch(params_ptr, modes.pop());
                        output.push(val);
                        inst_ptr += 2
                    }
                    OP_JUMP_IF_TRUE => {
                        let test = self.fetch(params_ptr, modes.pop());
                        let new_inst_ptr = self.fetch(params_ptr + 1, modes.pop()) as usize;
                        inst_ptr = if test != 0 {
                            new_inst_ptr
                        } else {
                            inst_ptr + 3
                        }
                    }
                    OP_JUMP_IF_FALSE => {
                        let test = self.fetch(params_ptr, modes.pop());
                        let new_inst_ptr = self.fetch(params_ptr + 1, modes.pop()) as usize;
                        inst_ptr = if test == 0 {
                            new_inst_ptr
                        } else {
                            inst_ptr + 3
                        }
                    }
                    OP_LESS_THAN => {
                        let val_left = self.fetch(params_ptr, modes.pop());
                        let val_right = self.fetch(params_ptr + 1, modes.pop());
                        let dest_idx = self.program[params_ptr + 2] as usize;
                        self.program[dest_idx] = if val_left < val_right { 1 } else { 0 };
                        inst_ptr += 4
                    }
                    OP_EQUALS => {
                        let val_left = self.fetch(params_ptr, modes.pop());
                        let val_right = self.fetch(params_ptr + 1, modes.pop());
                        let dest_idx = self.program[params_ptr + 2] as usize;
                        self.program[dest_idx] = if val_left == val_right { 1 } else { 0 };
                        inst_ptr += 4
                    }
                    OP_HALT => break,
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
                encoded_modes /= 10;
            }
            modes.reverse();
            (opcode as isize, modes)
        }

        fn fetch(&self, param_ptr: usize, mode: Option<Mode>) -> isize {
            let param_value = self.program[param_ptr];
            match mode.unwrap_or(Mode::Position) {
                Mode::Position => self.program[param_value as usize],
                Mode::Immediate => param_value,
            }
        }
    }
}
