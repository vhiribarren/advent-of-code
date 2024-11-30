#!/usr/bin/env python3

import os.path as op

INPUT_FILEPATH = op.join(op.dirname(__file__), "..", "inputs", f"{op.splitext(op.basename(__file__))[0]}.txt")

def eval_string(input: str) -> int:
    escape = False
    count = 0
    input_iter = iter(input)
    for c in input_iter:
        if escape:
            match c:
                case "x":
                    next(input_iter)
                    next(input_iter)
            count += 1
            escape = False
        else:
            match c:
                case "\\": escape = True
                case "\"": continue
                case _: count += 1 
    return count

def part_1(input: str):
    size_memory = []
    size_code = []
    for line in input.splitlines():
        size_memory.append(len(line))
        size_code.append(eval_string(line))
    print("Part 1:", sum(size_memory) - sum(size_code))

def encode_string(input: str) -> int:
    count = 2
    input_iter = iter(input)
    for c in input_iter:
        match c:
            case "\"" | "\\":
                count += 2
                continue
            case _:
                count += 1
    return count

def part_2(input: str):
    size_memory = []
    size_code = []
    for line in input.splitlines():
        size_memory.append(len(line))
        size_code.append(encode_string(line))
    print("Part 2:", sum(size_code) - sum(size_memory))


if __name__ == "__main__":
    with open(INPUT_FILEPATH) as input:
        all_input = input.read()
        part_1(all_input)
        part_2(all_input)