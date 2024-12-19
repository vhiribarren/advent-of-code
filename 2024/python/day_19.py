#!/usr/bin/env python3

from pathlib import Path
from functools import cache

INPUT_FILEPATH = (p := Path(__file__)).parent/".."/"inputs"/f"{p.stem}.txt"


def part_1(input_data: str):

    patterns_raw, designs_raw = input_data.split("\n\n")
    patterns = [p.strip() for p in patterns_raw.split(",")]
    designs = designs_raw.splitlines()

    @cache
    def check_pattern(design_part: str) -> bool:
        if len(design_part) == 0:
            return True
        for pattern in patterns:
            if design_part.startswith(pattern):
                if check_pattern(design_part[len(pattern):]):
                    return True
        return False
            
    possible_patterns = 0
    for design in designs:
        if check_pattern(design):
            possible_patterns += 1

    print("Part 1:", possible_patterns)


def part_2(input_data: str):
    ...


if __name__ == "__main__":
    with INPUT_FILEPATH.open() as file:
        file_data = file.read()
        part_1(file_data)
        part_2(file_data)
