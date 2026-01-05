#!/usr/bin/env python3

from itertools import groupby

INPUT = "1321131112"

def next_round(input_data: str) -> str:
    return ''.join(str(len(list(g)))+str(k) for k, g in groupby(input_data))

def part_1(input_data: str):
    n = input_data
    for _ in range(40):
        n = next_round(n)
    print("Part 1:", len(n))

def part_2(input_data: str):
    n = input_data
    for _ in range(50):
        n = next_round(n)
    print("Part 2:", len(n))


if __name__ == "__main__":
    part_1(INPUT)
    part_2(INPUT)
