#!/usr/bin/env python3

import os.path as op
from itertools import pairwise

INPUT_FILEPATH = op.join(op.dirname(__file__), "..", "inputs", f"{op.splitext(op.basename(__file__))[0]}.txt")


def part_1(input: str):
    reports = [map(int, l.split()) for l in input.splitlines()]
    count = 0
    for report in reports:
        diffs = [l2 - l1 for l1, l2 in pairwise(report)]
        if all(1 <= abs(d) <= 3 for d in diffs)\
              and (all(d > 0 for d in diffs)\
              or all(d < 0 for d in diffs)):
            count += 1
    print("Part 1:", count)


def part_2(input: str):
    count = 0
    for line in input.splitlines():
        report_init = [int(l) for l in line.split()]
        for idx in range(0, len(report_init)):
            report_candidate = report_init[0:idx] + report_init[idx+1:]
            diffs = [l2 - l1 for l1, l2 in pairwise(report_candidate)]
            if all(1 <= abs(d) <= 3 for d in diffs)\
                  and (all(d > 0 for d in diffs)\
                  or all(d < 0 for d in diffs)):
                count += 1
                break
    print("Part 2:", count)


if __name__ == "__main__":
    with open(INPUT_FILEPATH) as input:
        all_input = input.read()
        part_1(all_input)
        part_2(all_input)