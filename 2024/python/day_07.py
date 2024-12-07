#!/usr/bin/env python3

from pathlib import Path
from typing import Generator

INPUT_FILEPATH = (p := Path(__file__)).parent/".."/"inputs"/f"{p.stem}.txt"


def parser(input: str) -> list[tuple[int, list[int]]]:
    return [(int(t), list(map(int, v.split()))) for t, v in (line.split(":") for line in input.splitlines())]

def solver_1(target: int, next: list[int]) -> Generator[int]:
    if len(next) == 1:
        yield next[0]
    else:
        for v in [next[0] * next[1], next[0] + next[1]]:
            if v <= target:
                yield from solver_1(target, [v] + next[2:])

def solver_2(target: int, next: list[int]) -> Generator[int]:
    if len(next) == 1:
        yield next[0]
    else:
        for v in [next[0] * next[1], next[0] + next[1], int(str(next[0]) + str(next[1]))]:
            if v <= target:
                yield from solver_2(target, [v] + next[2:])


def part_1(input: str):
    results = [target if target in solver_1(target, values) else 0 for target, values in parser(input)]
    print("Part 1:", sum(results))

def part_2(input: str):
    results = [target if target in solver_2(target, values) else 0 for target, values in parser(input)]
    print("Part 2:", sum(results))


if __name__ == "__main__":
    with open(INPUT_FILEPATH) as input:
        all_input = input.read()
        part_1(all_input)
        part_2(all_input)