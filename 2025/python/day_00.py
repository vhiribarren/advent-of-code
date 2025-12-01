#!/usr/bin/env python3

"""
Template for Advent of Code.
"""

from pathlib import Path

INPUT_FILEPATH = (p := Path(__file__)).parent/".."/"inputs"/f"{p.stem}.txt"


def part_1(input_data: str):
    result = None
    print("Part 1:", result)


def part_2(input_data: str):
    result = None
    print("Part 2:", result)


if __name__ == "__main__":
    with INPUT_FILEPATH.open() as file:
        file_data = file.read()
        part_1(file_data)
        part_2(file_data)
