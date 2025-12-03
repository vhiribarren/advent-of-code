#!/usr/bin/env python3

from pathlib import Path

INPUT_FILEPATH = (p := Path(__file__)).parent/".."/"inputs"/f"{p.stem}.txt"


def max_battery(bank: list[str]) -> tuple[int, str]:
    max_left = (0,"0")
    for battery in enumerate(bank):
        if int(battery[1]) > int(max_left[1]):
            max_left = battery
    return max_left

def collect_battery(bank: str, count: int) -> int:
    selected = []
    first_idx = 0
    for last_idx in range(len(bank)+1-count, len(bank)+1):
        max_left = max_battery(bank[first_idx:last_idx])
        first_idx = first_idx+max_left[0]+1
        selected.append(max_left[1])
    return int(''.join(selected))


def part_1(input_data: str):
    result = sum([collect_battery(bank, 2) for bank in input_data.splitlines()])
    print("Part 1:", result)

def part_2(input_data: str):
    result = sum([collect_battery(bank, 12) for bank in input_data.splitlines()])
    print("Part 2:", result)


if __name__ == "__main__":
    with INPUT_FILEPATH.open() as file:
        file_data = file.read()
        part_1(file_data)
        part_2(file_data)
