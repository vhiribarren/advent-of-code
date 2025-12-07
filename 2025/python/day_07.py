#!/usr/bin/env python3

from pathlib import Path
from collections import deque
from functools import cache

INPUT_FILEPATH = (p := Path(__file__)).parent/".."/"inputs"/f"{p.stem}.txt"


def parse_input(input_data: str) -> tuple[int, complex, set[complex]]:
    lines = input_data.splitlines()
    limit = len(lines) - 1
    splitters = set[complex]()
    for y, l in enumerate(lines):
        for x, c in enumerate(l):
            coord = complex(x, y)
            match c:
                case "^": splitters.add(coord)
                case "S": start = coord
    return (limit, start, splitters)

def part_1(input_data: str):
    limit, start, splitters = parse_input(input_data)
    queue = deque[complex]([start])
    found_splitters = set[complex]()
    beams = set[complex]()
    while len(queue) != 0:
        curr_beam = queue.pop()
        if curr_beam in beams:
            continue
        beams.add(curr_beam)
        next_beam = curr_beam + 1j
        if next_beam.imag > limit:
            continue
        elif next_beam in splitters:
            found_splitters.add(next_beam)
            queue.appendleft(next_beam-1)
            queue.appendleft(next_beam+1)
        else:
            queue.appendleft(next_beam)
    result = len(found_splitters)
    print("Part 1:", result)

def part_2(input_data: str):
    limit, start, splitters = parse_input(input_data)
    @cache
    def lifetimes_from_splitters(splitter_coord: complex) -> int:
        while True:
            splitter_coord += 1j
            if splitter_coord.imag > limit:
                return 1
            if splitter_coord in splitters:
                return lifetimes_from_splitters(splitter_coord -1) + lifetimes_from_splitters(splitter_coord +1)
    result = lifetimes_from_splitters(start)
    print("Part 2:", result)


if __name__ == "__main__":
    with INPUT_FILEPATH.open() as file:
        file_data = file.read()
        part_1(file_data)
        part_2(file_data)
