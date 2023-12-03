#!/usr/bin/env python3

import os
import re
from math import prod

INPUT_FILE = "day_03.txt"
INPUT_FILEPATH = os.path.join(os.path.dirname(__file__), "..", "inputs", INPUT_FILE)

# "type" here is new in Python 3.12
type Coords = tuple[int, int]
type Symbols = dict[Coords, str]
type Numbers = list[tuple[Coords, str]]

def parse_input(input: str) -> (Symbols, Numbers):
    symbols: Symbols = dict()
    numbers: Numbers = []
    y_pos = 0
    for line in input.splitlines():
        for m in re.finditer(r"[^\.\d]", line):
            symbols[(m.start(), y_pos)] = m[0]
        for m in re.finditer(r"\d+", line):
            numbers.append(((m.start(), y_pos), m[0]))
        y_pos += 1
    return (symbols, numbers)

def part_1(input: str):
    (symbols, numbers) = parse_input(input)
    parts = []
    for ((x, y), num) in numbers:
        for x_scan in range(x-1, x+len(num)+1):
            if (x_scan, y-1) in symbols or (x_scan, y+1) in symbols :
                parts.append(int(num))
                break
        if (x-1, y) in symbols or (x+len(num), y) in symbols :
            parts.append(int(num))
    print("Result Part 1:", sum(parts))

def part_2(input: str):
    (symbols, numbers) = parse_input(input)
    adjacents_numbers: dict[Coords, list[int]] = dict()
    def update_adj_num(coords: Coords, num: int):
        if adjacents_numbers.get(coords) is None:
            adjacents_numbers[coords] = []
        adjacents_numbers[coords].append(int(num))
    for ((x, y), num) in numbers:
        for x_scan in range(x-1, x+len(num)+1):
            for y_scan in [y-1, y+1]:
                if (x_scan, y_scan) in symbols:
                    update_adj_num((x_scan, y_scan), int(num))
        for x_scan in [x-1, x+len(num)]:
            if (x_scan, y) in symbols:
                update_adj_num((x_scan, y), int(num))
    valid_gears = [prod(parts) for _, parts in adjacents_numbers.items() if len(parts) == 2]
    print("Result Part 2:", sum(valid_gears))


if __name__ == "__main__":
    with open(INPUT_FILEPATH) as f:
        input = f.read()
        part_1(input)
        part_2(input)