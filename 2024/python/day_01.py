#!/usr/bin/env python3

import os.path as op
from collections import Counter

INPUT_FILEPATH = op.join(op.dirname(__file__), "..", "inputs", f"{op.splitext(op.basename(__file__))[0]}.txt")


def parse_input(input: str) -> tuple[list[int], list[int]]:
    list_left = []
    list_right = []
    for line in input.splitlines():
        left, right = map(int, line.split())
        list_left.append(left)
        list_right.append(right)
    return list_left, list_right

def part_1(input: str):
    all_left, all_right = parse_input(input)
    diffs = [abs(l-r) for l, r in zip(sorted(all_left), sorted(all_right))]
    print("Part 1:", sum(diffs))

def part_2(input: str):
    all_left, all_right = parse_input(input)
    right_occurences = Counter(all_right)
    similarities = [v * right_occurences[v] for v in all_left]
    print("Part 2:", sum(similarities))


if __name__ == "__main__":
    with open(INPUT_FILEPATH) as input:
        all_input = input.read()
        part_1(all_input)
        part_2(all_input)