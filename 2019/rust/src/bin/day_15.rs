use std::{
    collections::HashMap,
    error::Error,
    fs,
    path::{Path, PathBuf},
    println,
    sync::LazyLock,
    unimplemented, unreachable, vec,
};

const INPUT_FILENAME: &str = "day_15.txt";

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

const OUT_WALL: i128 = 0;
const OUT_EMPTY: i128 = 1;
const OUT_OXYGEN: i128 = 2;
const COMMANDS: [i128; 4] = [1, 2, 3, 4];

#[derive(Clone, Copy, Eq, PartialEq, Hash)]
struct Coords {
    x: isize,
    y: isize,
}
type Predecessors = HashMap<Coords, Option<Coords>>;

fn next_hop(coords: Coords, command: i128) -> Coords {
    match command {
        1 => Coords {
            x: coords.x,
            y: coords.y - 1,
        },
        2 => Coords {
            x: coords.x,
            y: coords.y + 1,
        },
        3 => Coords {
            x: coords.x - 1,
            y: coords.y,
        },
        4 => Coords {
            x: coords.x + 1,
            y: coords.y,
        },
        _ => unreachable!(),
    }
}

fn revert_command(command: i128) -> i128 {
    match command {
        1 => 2,
        2 => 1,
        3 => 4,
        4 => 3,
        _ => unreachable!(),
    }
}

fn chain_len(coords: Coords, predecessors: &Predecessors) -> isize {
    let mut current = coords;
    let mut count = 0;
    while let Some(next) = predecessors[&current] {
        count += 1;
        current = next;
    }
    count
}

fn scan_grid(
    coords: Coords,
    computer: &mut intcode::IntCodeComputer,
    predecessors: &mut Predecessors,
) -> Option<Coords> {
    for command in COMMANDS {
        let next_hop = next_hop(coords, command);
        if predecessors.contains_key(&next_hop) {
            if chain_len(coords, predecessors) + 1 < chain_len(next_hop, predecessors) {
                predecessors.insert(next_hop, Some(coords));
            }
            continue;
        }
        let intcode::IntCode(next_grid) = computer.run_program(vec![command])[0];
        match next_grid {
            OUT_WALL => {
                continue;
            }
            OUT_EMPTY => {
                predecessors.insert(next_hop, Some(coords));
                let scan_result = scan_grid(next_hop, computer, predecessors);
                if scan_result.is_some() {
                    return scan_result;
                } else {
                    computer.run_program(vec![revert_command(command)]);
                }
            }
            OUT_OXYGEN => {
                predecessors.insert(next_hop, Some(coords));
                return Some(next_hop);
            }
            _ => unreachable!(),
        }
    }
    None
}

fn problem_1(input: &str) -> Result<String, Box<dyn Error>> {
    let init_coords = Coords { x: 0, y: 0 };
    let computer = &mut intcode::IntCodeComputer::from_input(input);
    let predecessors = &mut HashMap::from([(init_coords, None)]);
    let found_oxygen = scan_grid(init_coords, computer, predecessors).unwrap();
    let count = chain_len(found_oxygen, predecessors);
    Ok(count.to_string())
}

fn problem_2(input: &str) -> Result<String, Box<dyn Error>> {
    unimplemented!()
}

// IntCode computer
///////////////////

pub mod intcode {
    use std::{
        collections::{HashMap, VecDeque},
        ops::{Add, AddAssign},
        unreachable, vec,
    };

    pub type Value = i128;
    #[derive(Clone, Eq, PartialEq, PartialOrd, Hash, Copy, Debug)]
    pub struct IntCode(pub Value);
    #[derive(Clone, Eq, PartialEq, PartialOrd, Hash, Copy, Debug)]
    pub struct Addr(pub u128);
    #[derive(Clone, Eq, PartialEq, PartialOrd, Hash, Copy, Debug)]
    pub struct OpCode(pub u8);

    impl Add<usize> for Addr {
        type Output = Addr;
        fn add(self, rhs: usize) -> Self::Output {
            Self(self.0 + rhs as u128)
        }
    }
    impl AddAssign<usize> for Addr {
        fn add_assign(&mut self, rhs: usize) {
            *self = Self(self.0 + rhs as u128);
        }
    }

    #[derive(Debug, Clone)]
    enum Mode {
        Position,
        Immediate,
        Relative,
    }
    impl From<usize> for Mode {
        fn from(num_mode: usize) -> Self {
            match num_mode {
                0 => Mode::Position,
                1 => Mode::Immediate,
                2 => Mode::Relative,
                _ => unreachable!("Invalid parameter mode"),
            }
        }
    }

    const OP_ADD: OpCode = OpCode(1);
    const OP_MULTIPLY: OpCode = OpCode(2);
    const OP_INPUT: OpCode = OpCode(3);
    const OP_OUTPUT: OpCode = OpCode(4);
    const OP_JUMP_IF_TRUE: OpCode = OpCode(5);
    const OP_JUMP_IF_FALSE: OpCode = OpCode(6);
    const OP_LESS_THAN: OpCode = OpCode(7);
    const OP_EQUALS: OpCode = OpCode(8);
    const OP_BASE_ADJUST: OpCode = OpCode(9);
    const OP_HALT: OpCode = OpCode(99);

    #[derive(Clone)]
    pub struct IntCodeComputer {
        pub program: HashMap<Addr, IntCode>,
        pub inst_ptr: Addr,
        pub rel_base: Value,
        pub halted: bool,
    }

    impl IntCodeComputer {
        pub fn from_input(input: &str) -> Self {
            let program = input
                .split(',')
                .map(|s| s.parse().expect("intcode parse error"))
                .zip(0..)
                .map(|(v, i)| (Addr(i), IntCode(v)))
                .collect();
            let inst_ptr = Addr(0);
            let halted = false;
            let rel_base = 0;
            Self {
                program,
                inst_ptr,
                halted,
                rel_base,
            }
        }

        pub fn is_halted(&self) -> bool {
            self.halted
        }

        fn get_intcode(&self, addr: Addr) -> IntCode {
            *self.program.get(&addr).unwrap_or(&IntCode(0))
        }

        pub fn run_program(&mut self, input: Vec<Value>) -> Vec<IntCode> {
            let mut input = VecDeque::from(input);
            let mut output = Vec::new();
            loop {
                let (opcode, mut modes) = Self::decode_inst(self.get_intcode(self.inst_ptr));
                let params_ptr: Addr = self.inst_ptr + 1;
                match opcode {
                    OP_ADD => {
                        let val_left = self.fetch_resolved_intcode(params_ptr, modes.pop());
                        let val_right = self.fetch_resolved_intcode(params_ptr + 1, modes.pop());
                        let dest_idx = self.fetch_resolved_addr(params_ptr + 2, modes.pop());
                        self.program
                            .insert(dest_idx, IntCode(val_left.0 + val_right.0));
                        self.inst_ptr += 4;
                    }
                    OP_MULTIPLY => {
                        let val_left = self.fetch_resolved_intcode(params_ptr, modes.pop());
                        let val_right = self.fetch_resolved_intcode(params_ptr + 1, modes.pop());
                        let dest_idx = self.fetch_resolved_addr(params_ptr + 2, modes.pop());
                        self.program
                            .insert(dest_idx, IntCode(val_left.0 * val_right.0));
                        self.inst_ptr += 4;
                    }
                    OP_INPUT => {
                        if input.is_empty() {
                            break;
                        }
                        let dest_idx = self.fetch_resolved_addr(params_ptr, modes.pop());
                        self.program
                            .insert(dest_idx, IntCode(input.pop_front().unwrap()));
                        self.inst_ptr += 2;
                    }
                    OP_OUTPUT => {
                        let val = self.fetch_resolved_intcode(params_ptr, modes.pop());
                        output.push(val);
                        self.inst_ptr += 2;
                    }
                    OP_JUMP_IF_TRUE => {
                        let test = self.fetch_resolved_intcode(params_ptr, modes.pop());
                        let new_inst_ptr =
                            self.fetch_resolved_intcode(params_ptr + 1, modes.pop()).0;
                        self.inst_ptr = if test.0 != 0 {
                            Addr(new_inst_ptr as u128)
                        } else {
                            self.inst_ptr + 3
                        }
                    }
                    OP_JUMP_IF_FALSE => {
                        let test = self.fetch_resolved_intcode(params_ptr, modes.pop());
                        let new_inst_ptr =
                            self.fetch_resolved_intcode(params_ptr + 1, modes.pop()).0;
                        self.inst_ptr = if test.0 == 0 {
                            Addr(new_inst_ptr as u128)
                        } else {
                            self.inst_ptr + 3
                        }
                    }
                    OP_LESS_THAN => {
                        let val_left = self.fetch_resolved_intcode(params_ptr, modes.pop());
                        let val_right = self.fetch_resolved_intcode(params_ptr + 1, modes.pop());
                        let dest_idx = self.fetch_resolved_addr(params_ptr + 2, modes.pop());
                        self.program.insert(
                            dest_idx,
                            if val_left < val_right {
                                IntCode(1)
                            } else {
                                IntCode(0)
                            },
                        );
                        self.inst_ptr += 4
                    }
                    OP_EQUALS => {
                        let val_left = self.fetch_resolved_intcode(params_ptr, modes.pop());
                        let val_right = self.fetch_resolved_intcode(params_ptr + 1, modes.pop());
                        let dest_idx = self.fetch_resolved_addr(params_ptr + 2, modes.pop());
                        self.program.insert(
                            dest_idx,
                            if val_left == val_right {
                                IntCode(1)
                            } else {
                                IntCode(0)
                            },
                        );
                        self.inst_ptr += 4
                    }
                    OP_BASE_ADJUST => {
                        let val = self.fetch_resolved_intcode(params_ptr, modes.pop());
                        self.rel_base += val.0;
                        self.inst_ptr += 2
                    }
                    OP_HALT => {
                        self.halted = true;
                        break;
                    }
                    _ => unimplemented!(),
                }
            }
            output
        }

        fn decode_inst(instruction: IntCode) -> (OpCode, Vec<Mode>) {
            let opcode = OpCode((instruction.0 % 100) as u8);
            let mut modes = vec![Mode::Position; 3];
            let mut encoded_modes = (instruction.0 / 100) as usize;
            for mode in modes.iter_mut() {
                *mode = (encoded_modes % 10).into();
                encoded_modes /= 10;
            }
            modes.reverse();
            (opcode, modes)
        }

        fn fetch_resolved_intcode(&self, param_ptr: Addr, mode: Option<Mode>) -> IntCode {
            let param_value = self.get_intcode(param_ptr);
            match mode.unwrap() {
                Mode::Position => self.get_intcode(Addr(param_value.0 as u128)),
                Mode::Immediate => param_value,
                Mode::Relative => self.get_intcode(Addr((param_value.0 + self.rel_base) as u128)),
            }
        }

        fn fetch_resolved_addr(&self, addr: Addr, mode: Option<Mode>) -> Addr {
            let candidate_addr = self.get_intcode(addr);
            match mode.unwrap() {
                Mode::Position => Addr(candidate_addr.0 as u128),
                Mode::Relative => Addr((candidate_addr.0 + self.rel_base) as u128),
                _ => unreachable!(),
            }
        }
    }
}
