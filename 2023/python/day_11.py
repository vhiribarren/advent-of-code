#!/usr/bin/env python3

import os
from itertools import combinations
from collections import Counter

INPUT_FILE = "day_11.txt"
INPUT_FILEPATH = os.path.join(os.path.dirname(__file__), "..", "inputs", INPUT_FILE)


def part_1(input: str):
    line_stats = Counter()
    col_stats =  Counter()
    orig_galaxies:  list[complex] = []
    exp_galaxies: list[complex] = []

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


if __name__ == "__main__":
    with open(INPUT_FILEPATH) as f:
        input = f.read()
        part_1(input)