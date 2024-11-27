#!/usr/bin/env python3

import os

INPUT_FILE = "day_03.txt"
INPUT_FILEPATH = os.path.join(os.path.dirname(__file__), "..", "inputs", INPUT_FILE)


def part_1(input: str):
    current_pos = complex(0, 0)
    houses = set([current_pos])
    for dir in input:
        match dir:
            case ">": current_pos += 1
            case "<": current_pos += -1
            case "^": current_pos += 1j
            case "v": current_pos += -1j
        houses.add(current_pos)
    print("Part 1:", len(houses))


def part_2(input: str):
    current_pos = complex(0, 0)
    current_pos_next = complex(0, 0)
    houses = set([current_pos])
    for dir in input:
        match dir:
            case ">": current_pos += 1
            case "<": current_pos += -1
            case "^": current_pos += 1j
            case "v": current_pos += -1j
        houses.add(current_pos)
        current_pos, current_pos_next = current_pos_next, current_pos
    print("Part 2:", len(houses))


if __name__ == "__main__":
    with open(INPUT_FILEPATH) as input:
        all_input = input.read()
        part_1(all_input)
        part_2(all_input)