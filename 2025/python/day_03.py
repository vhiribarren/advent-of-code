#!/usr/bin/env python3

from pathlib import Path

INPUT_FILEPATH = (p := Path(__file__)).parent/".."/"inputs"/f"{p.stem}.txt"


def max_battery(bank: list[str]) -> tuple[int, str]:
    return max(enumerate(bank), key=lambda b: int(b[1]))

def collect_battery(bank: str, count: int) -> int:
    selected = []
    first_idx = 0
    for last_idx in range(len(bank)+1-count, len(bank)+1):
        max_left = max_battery(bank[first_idx:last_idx])
        first_idx = first_idx+max_left[0]+1
        selected.append(max_left[1])
    return int(''.join(selected))

def compute_joltage(input_data: str, count: int) -> int:
    return sum([collect_battery(bank, count) for bank in input_data.splitlines()])

def part_1(input_data: str):
    print("Part 1:", compute_joltage(input_data, 2))

def part_2(input_data: str):
    print("Part 1:", compute_joltage(input_data, 12))


if __name__ == "__main__":
    with INPUT_FILEPATH.open() as file:
        file_data = file.read()
        part_1(file_data)
        part_2(file_data)
