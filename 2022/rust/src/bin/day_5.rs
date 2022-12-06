use std::fs::read_to_string;
use std::path::Path;

const INPUT_PATH_REL: &str = "../../../inputs/day_5.txt";
const SRC_PATH: &str = concat!(env!("CARGO_MANIFEST_DIR"), "/", file!());

fn parse_start_position(input: Vec<&str>) -> Vec<Vec<char>> {
    let mut stacks: Vec<Vec<char>> = vec![];
    for line in input.iter().rev().skip(1) {
        let mut pos = 1;
        let mut idx = 0;
        let chars = line.chars().collect::<Vec<_>>();
        loop {
            let element = match chars.get(pos) {
                None => break,
                Some(val) => *val,
            };
            if stacks.len() <= idx {
                stacks.push(vec![]);
            }
            if element != ' ' {
                stacks[idx].push(element);
            }
            pos += 4;
            idx += 1;
        }
    }
    stacks
}

fn parser_instructions(input: Vec<&str>) -> Vec<Instruction> {
    input
        .iter()
        .map(|s| s.split_whitespace().collect::<Vec<_>>())
        .map(|i| Instruction {
            count: i[1].parse().unwrap(),
            from: i[3].parse::<usize>().unwrap() - 1,
            to: i[5].parse::<usize>().unwrap() - 1,
        })
        .collect()
}

fn compute_result(stacks: &Vec<Vec<char>>) -> String {
    let mut result = String::new();
    for stack in stacks {
        let v = *stack.last().unwrap();
        result.push_str(&v.to_string());
    }
    result
}

struct Instruction {
    count: usize,
    from: usize,
    to: usize,
}

fn main() {
    let input_path = Path::new(SRC_PATH).parent().unwrap().join(INPUT_PATH_REL);
    let input = read_to_string(input_path).unwrap();

    let mut input_lines = input.lines();
    let input_lines_iter = input_lines.by_ref();
    let start_position_iter = input_lines_iter.take_while(|p| !p.is_empty());

    let orig_stacks = parse_start_position(start_position_iter.collect());
    let instructions = parser_instructions(input_lines_iter.collect());

    let mut stacks = orig_stacks.clone();
    for instruction in &instructions {
        for _ in 0..instruction.count {
            let elem = stacks[instruction.from].pop().unwrap();
            stacks[instruction.to].push(elem);
        }
    }
    println!("{}", compute_result(&stacks));

    stacks = orig_stacks;
    for instruction in &instructions {
        let mut elems = vec![];
        for _ in 0..instruction.count {
            let elem = stacks[instruction.from].pop().unwrap();
            elems.push(elem);
        }
        elems.reverse();
        stacks[instruction.to].extend(elems);
    }
    println!("{}", compute_result(&stacks));
}
