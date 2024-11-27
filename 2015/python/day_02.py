#!/usr/bin/env python3

import os

INPUT_FILE = "day_02.txt"
INPUT_FILEPATH = os.path.join(os.path.dirname(__file__), "..", "inputs", INPUT_FILE)


def solver(input: str):
    results_1 = []
    results_2 = []
    for line in input.splitlines():
        dimensions = list(map(int, line.split("x")))
        [l, w, h] = dimensions
        min_dims = sorted(dimensions)
        results_1.append(2*l*w + 2*w*h + 2*h*l + min_dims[0]*min_dims[1])
        results_2.append(2*min_dims[0] + 2*min_dims[1] + l*w*h)
    print("Part 1:", sum(results_1))
    print("Part 2:", sum(results_2))


if __name__ == "__main__":
    with open(INPUT_FILEPATH) as input:
        all_input = input.read()
        solver(all_input)