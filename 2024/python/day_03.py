#!/usr/bin/env python3

import os.path as op
import re

INPUT_FILEPATH = op.join(op.dirname(__file__), "..", "inputs", f"{op.splitext(op.basename(__file__))[0]}.txt")


def part_1(input: str):
    multiples = [int(mul.group(1)) * int(mul.group(2)) for mul in re.finditer(r"mul\((\d{1,3}),(\d{1,3})\)", input)]
    print("Part 1:", sum(multiples))


def part_2(input: str):
    enabled = True
    result = 0
    for inst in re.finditer(r"mul\((\d{1,3}),(\d{1,3})\)|do\(\)|don't\(\)", input):
        v = inst.group(0)
        if v.startswith("mul"):
            if enabled:
                result += int(inst.group(1)) * int(inst.group(2))
        elif v.startswith("don't"):
            enabled = False
        elif v.startswith("do"):
            enabled = True
    print("Part 2:", result)


if __name__ == "__main__":
    with open(INPUT_FILEPATH) as input:
        all_input = input.read()
        part_1(all_input)
        part_2(all_input)
