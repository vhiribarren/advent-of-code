#!/usr/bin/env python3

from itertools import product
from pathlib import Path

INPUT_FILEPATH = (p := Path(__file__)).parent/".."/"inputs"/f"{p.stem}.txt"

NEIGHBORS = [
    complex(dx, dy)
    for (dx, dy) in product([-1, 0, 1], repeat=2)
    if not (dx == dy == 0)
]

def input_parser(input_data: str) -> set[complex]:
    rolls = set()
    for (y, line) in enumerate(input_data.splitlines()):
        for (x, elem) in enumerate(line):
            if elem == '@':
                rolls.add(complex(x, y))
    return rolls


def part_1(input_data: str):
    rolls = input_parser(input_data)
    selected = set[complex]()
    for roll in rolls:
        count = sum([1 for delta in NEIGHBORS if roll + delta in rolls])
        if count < 4:
            selected.add(roll)
    result = len(selected)
    print("Part 1:", result)


def part_2(input_data: str):
    rolls = input_parser(input_data)
    removed = set[complex]()
    while True:
        selected = set[complex]()
        for roll in rolls:
            count = sum([1 for delta in NEIGHBORS if roll + delta in rolls])
            if count < 4:
                selected.add(roll)
        removed |= selected
        rolls -= selected
        if len(selected) == 0:
            break
    result = len(removed)
    print("Part 2:", result)


if __name__ == "__main__":
    with INPUT_FILEPATH.open() as file:
        file_data = file.read()
        part_1(file_data)
        part_2(file_data)
