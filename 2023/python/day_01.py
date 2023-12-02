#!/usr/bin/env python3

import os

INPUT_FILE = "day_01.txt"
INPUT_FILEPATH = os.path.join(os.path.dirname(__file__), "..", "inputs", INPUT_FILE)

DIGIT_MAP = {
    "one": 1,
    "two": 2,
    "three": 3,
    "four": 4,
    "five": 5,
    "six": 6,
    "seven": 7,
    "eight": 8,
    "nine": 9,
}

def part_1(input: str):
    lines = input.splitlines()
    calibrations = []
    for line in lines:
        digits = [char for char in line if char.isdigit()]
        calibrations.append(int(digits[0]+digits[-1]))
    print("Result Part 1:", sum(calibrations))

def part_2(input: str):
    lines = input.splitlines()
    calibrations = []
    for line in lines:
        digits = []
        for idx in range(len(line)):
            if (c := line[idx]).isdigit():
                digits.append(c)
                continue
            for alpha, num in DIGIT_MAP.items():
                if line[idx:].startswith(alpha):
                    digits.append(str(num))
                    break
        calibrations.append(int(digits[0]+digits[-1]))
    print("Result Part 2:", sum(calibrations))


if __name__ == "__main__":
    with open(INPUT_FILEPATH) as input:
        all_input = input.read()
        part_1(all_input)
        part_2(all_input)