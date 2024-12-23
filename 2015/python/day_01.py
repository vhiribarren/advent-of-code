#!/usr/bin/env python3

import os

INPUT_FILE = "day_01.txt"
INPUT_FILEPATH = os.path.join(os.path.dirname(__file__), "..", "inputs", INPUT_FILE)


def part_1(input: str):
    result = input.count("(") - input.count(")")
    print("Part 1:", result)


def part_2(input: str):
    floor = 0
    for idx, instruction in enumerate(input):
        match instruction:
            case "(": floor += 1
            case ")": floor -= 1
        if floor == -1:
            break
    print("Part 2:", idx + 1)


if __name__ == "__main__":
    with open(INPUT_FILEPATH) as input:
        all_input = input.read()
        part_1(all_input)
        part_2(all_input)