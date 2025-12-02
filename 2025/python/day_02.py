#!/usr/bin/env python3

from pathlib import Path

INPUT_FILEPATH = (p := Path(__file__)).parent/".."/"inputs"/f"{p.stem}.txt"


parse_input = lambda data: [ map(int, l.split("-")) for l in data.split(",") ] 

def part_1(input_data: str):
    ranges = parse_input(input_data)
    invalid_ids = []
    for [start, end] in ranges:
        for candidate in range(start, end+1):
            candidate_str = str(candidate)
            mid = len(candidate_str)//2
            if candidate_str[0:mid] == candidate_str[mid:]:
                invalid_ids.append(candidate)
    result = sum(invalid_ids)
    print("Part 1:", result)


def part_2(input_data: str):
    ranges = parse_input(input_data)
    invalid_ids = []
    for [start, end] in ranges:
        for candidate in range(start, end+1):
            candidate_str = str(candidate)
            max_window = len(candidate_str)//2
            for window in range(1, max_window+1):
                for scan in range(window, len(candidate_str), window):
                    if candidate_str[0:window] != candidate_str[scan:window+scan]:
                        break
                else:
                    invalid_ids.append(candidate)
                    break
    result = sum(invalid_ids)
    print("Part 2:", result)


if __name__ == "__main__":
    with INPUT_FILEPATH.open() as file:
        file_data = file.read()
        part_1(file_data)
        part_2(file_data)
