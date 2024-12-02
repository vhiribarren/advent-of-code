#!/usr/bin/env python3

import os.path as op
from itertools import pairwise

INPUT_FILEPATH = op.join(op.dirname(__file__), "..", "inputs", f"{op.splitext(op.basename(__file__))[0]}.txt")


def solver(input: str, tolerate: bool = False) -> int :
    count = 0
    for line in input.splitlines():
        report_init = [int(l) for l in line.split()]
        scanner = range(0, len(report_init)) if tolerate else [len(report_init)]
        for idx in scanner:
            report_candidate = report_init[0:idx] + report_init[idx+1:]
            diffs = [l2 - l1 for l1, l2 in pairwise(report_candidate)]
            if all(1 <= abs(d) <= 3 for d in diffs)\
                  and (all(d > 0 for d in diffs)\
                  or all(d < 0 for d in diffs)):
                count += 1
                break
    return count

if __name__ == "__main__":
    with open(INPUT_FILEPATH) as input:
        all_input = input.read()
        print("Part 1", solver(all_input, tolerate=False))
        print("Part 2", solver(all_input, tolerate=True))
