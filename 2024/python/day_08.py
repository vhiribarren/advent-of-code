#!/usr/bin/env python3

from pathlib import Path
from collections import defaultdict
from itertools import product, count
from typing import SupportsComplex, cast

INPUT_FILEPATH = (p := Path(__file__)).parent/".."/"inputs"/f"{p.stem}.txt"


def parser(input: str) -> tuple[dict[str, list[complex]], int, int]:
    result = defaultdict(list)
    for y, line in enumerate(input.splitlines()):
        for x, c in enumerate(line):
            if c != ".":
                result[c].append(complex(x, y))
    return result, x+1, y+1


def valid_positions(position: complex, width: int, height: int) -> bool:
    return 0 <= position.real < width and 0 <= position.imag < height

def part_1(input: str):
    antenna, width, height = parser(input)
    antinodes: set[complex] = set()
    for freq in antenna.keys():
        for l, r in product(antenna[freq], repeat=2):
            if l == r:
                continue
            dir = l - r
            for candidate in [l + dir, r - dir]:
                if valid_positions(candidate, width, height):
                    antinodes.add(candidate)
    print("Part 1:", len(antinodes))

def part_2(input: str):
    antenna, width, height = parser(input)
    antinodes: set[complex] = set()
    for freq in antenna.keys():
        for l, r in product(antenna[freq], repeat=2):
            if l == r:
                continue
            dir = l - r
            for scanner in [count(l, dir),  count(r, -dir)]:
                for candidate in scanner:
                    candidate = cast(complex, candidate)
                    if not valid_positions(candidate, width, height):
                        break
                    antinodes.add(candidate)     
    print("Part 2:", len(antinodes))


if __name__ == "__main__":
    with open(INPUT_FILEPATH) as input:
        all_input = input.read()
        part_1(all_input)
        part_2(all_input)