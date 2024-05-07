#!/usr/bin/env python3

import os
from itertools import combinations
from collections import Counter

INPUT_FILE = "day_11.txt"
INPUT_FILEPATH = os.path.join(os.path.dirname(__file__), "..", "inputs", INPUT_FILE)


empty_lines: list[int] = []
empty_cols: list[int] = []
orig_galaxies: list[complex] = []


def init_common(input: str) -> (list[complex]):
    global empty_lines, empty_cols
    line_stats = Counter()
    col_stats =  Counter()
    for y, line in enumerate(input.splitlines()):
        for x, c in enumerate(line):
            if c != "#":
                continue
            orig_galaxies.append(complex(x, y))
            line_stats[y] += 1
            col_stats[x] += 1
    x_max, y_max = x, y
    empty_lines = [idx for idx in range(y_max) if line_stats[idx] == 0]
    empty_cols = [idx for idx in range(x_max) if col_stats[idx] == 0]


def part_1():
    exp_galaxies: list[complex] = []
    for galaxy in orig_galaxies:
        exp_galaxy = complex(
            galaxy.real + len([col for col in empty_cols if col < galaxy.real]),
            galaxy.imag + len([line for line in empty_lines if line < galaxy.imag])
        )
        exp_galaxies.append(exp_galaxy)
 
    distances: list[int] = []
    for galaxies in combinations(exp_galaxies, 2):
        distances.append(
            abs(galaxies[1].real - galaxies[0].real) + abs(galaxies[1].imag - galaxies[0].imag)
        )

    print("Result part 1: ", sum(distances))


def part_2():
    exp_galaxies: list[complex] = []
    for galaxy in orig_galaxies:
        empty_cols_before = len([col for col in empty_cols if col < galaxy.real])
        empty_lines_before = len([line for line in empty_lines if line < galaxy.imag])
        exp_galaxy = complex(
            galaxy.real + 1_000_000 * empty_cols_before - empty_cols_before,
            galaxy.imag + 1_000_000 * empty_lines_before - empty_lines_before,
        )
        exp_galaxies.append(exp_galaxy)
 
    distances: list[int] = []
    for galaxies in combinations(exp_galaxies, 2):
        distances.append(
            abs(galaxies[1].real - galaxies[0].real) + abs(galaxies[1].imag - galaxies[0].imag)
        )

    print("Result part 2: ", sum(distances))


if __name__ == "__main__":
    with open(INPUT_FILEPATH) as f:
        input = f.read()
        init_common(input)
        part_1()
        part_2()