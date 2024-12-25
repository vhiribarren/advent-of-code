#!/usr/bin/env python3

from pathlib import Path
from itertools import product

INPUT_FILEPATH = (p := Path(__file__)).parent / ".." / "inputs" / f"{p.stem}.txt"

COMBINATION_WIDTH = 5


def part_1(input_data: str):
    combinations = input_data.split("\n\n")
    keys = set[tuple[int, ...]]()
    locks = set[tuple[int, ...]]()
    for combination in combinations:
        pins = [0 for _ in range(COMBINATION_WIDTH)]
        for line in combination.splitlines()[1:-1]:
            for idx, c in enumerate(line):
                if c == "#":
                    pins[idx] += 1
        if combination.startswith("#"):
            locks.add(tuple(pins))
        else:
            keys.add(tuple(pins))
    result = 0
    for key, lock in product(keys, locks):
        if all(key[i] + lock[i] <= COMBINATION_WIDTH for i in range(COMBINATION_WIDTH)):
            result += 1

    print("Part 1:", result)


def part_2(input_data: str):
    result = None
    print("Part 2:", result)


if __name__ == "__main__":
    with INPUT_FILEPATH.open() as file:
        file_data = file.read()
        part_1(file_data)
        part_2(file_data)
