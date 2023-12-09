#!/usr/bin/env python3

import os
import re
from functools import reduce

INPUT_FILE = "day_09.txt"
INPUT_FILEPATH = os.path.join(os.path.dirname(__file__), "..", "inputs", INPUT_FILE)

NODE_RE = re.compile(r"(\w+) = \((\w+), (\w+)\)")

def part_1_and_2(input: str):
    histories = [[int(v) for v in l.split()] for l in input.splitlines()]
    extrapolated_val_max = []
    extrapolated_val_min = []
    for history in histories:
        seq = history[:]
        next_seq = []
        last_nb = []
        first_nb = []
        while True:
            last_nb.append(seq[-1])
            first_nb.append(seq[0])
            for [l, r] in zip(seq, seq[1:]):
                next_seq.append(r-l)
            if all([v == 0 for v in next_seq]):
                break
            seq = next_seq
            next_seq = []
        last_nb.reverse()
        first_nb.reverse()
        extrapolated_val_max.append(reduce(lambda x, y: x + y, last_nb))
        extrapolated_val_min.append(reduce(lambda x, y: y - x, first_nb))
    print("Result Part 1:", sum(extrapolated_val_max))
    print("Result Part 2:", sum(extrapolated_val_min))


if __name__ == "__main__":
    with open(INPUT_FILEPATH) as f:
        input = f.read()
        part_1_and_2(input)