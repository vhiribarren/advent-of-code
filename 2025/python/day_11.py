#!/usr/bin/env python3

from pathlib import Path
from functools import cache

INPUT_FILEPATH = (p := Path(__file__)).parent/".."/"inputs"/f"{p.stem}.txt"

START_PART1 = "you"
START_PART2 = "svr"
END = "out"

def parse_input(in_data: str) -> dict[str, list[str]]:
    return {
        p[0][:-1]: p[1:]
        for p in (l.split() for l in in_data.splitlines())
    }

def part_1(input_data: str):
    graph = parse_input(input_data)
    @cache
    def count_paths(node: str) -> int:
        if node == END:
            return 1
        return sum(count_paths(next_node) for next_node in graph[node])
    result = count_paths(START_PART1)
    print("Part 1:", result)

def part_2(input_data: str):
    graph = parse_input(input_data)
    @cache
    def count_paths(node: str, has_dac: bool, has_fft: bool) -> int:
        if node == END:
            return 1 if has_dac and has_fft else 0
        return sum(
                    count_paths(
                        next_node,
                        has_dac or next_node == "dac",
                        has_fft or next_node == "fft"
                    )
                    for next_node in graph[node]
                )
    result = count_paths(START_PART2, False, False)
    print("Part 2:", result)


if __name__ == "__main__":
    file_data = INPUT_FILEPATH.read_text()
    part_1(file_data)
    part_2(file_data)
