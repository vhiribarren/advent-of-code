#!/usr/bin/env python3

from pathlib import Path

INPUT_FILEPATH = (p := Path(__file__)).parent/".."/"inputs"/f"{p.stem}.txt"


def input_parser(input_data: str) -> tuple[list[tuple[int,int]], list[int]]:
    ranges_raw, ids_raw = input_data.split("\n\n")
    ranges = [
        (int(l), int(r))
        for l, r in (line.split("-") for line in ranges_raw.splitlines())
    ]
    ids = [int(l) for l in ids_raw.splitlines()]
    return (ranges, ids)

def merge_ranges(ranges: list[tuple[int, int]]) -> list[tuple[int, int]]:
    ranges.sort(key=lambda e: e[0])
    result = [ranges[0]]
    for r in ranges[1:]:
        prev = result[-1]
        if r[0] > prev[1]:
            result.append(r)
        elif r[1] > prev[1]:
            result[-1] = (prev[0], r[1])
    return result
        
def in_ranges(ranges: list[tuple[int, int]], id: int) -> bool:
    return any(start <= id <= end for start, end in ranges)


def part_1(input_data: str):
    ranges, ids = input_parser(input_data)
    ranges = merge_ranges(ranges)
    fresh = [id for id in ids if in_ranges(ranges, id)]
    result = len(fresh)
    print("Part 1:", result)


def part_2(input_data: str):
    ranges, ids = input_parser(input_data)
    ranges = merge_ranges(ranges)
    fresh_count = sum(end - start + 1 for start, end in ranges)
    print("Part 1:", fresh_count)


if __name__ == "__main__":
    with INPUT_FILEPATH.open() as file:
        file_data = file.read()
        part_1(file_data)
        part_2(file_data)
