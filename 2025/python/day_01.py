#!/usr/bin/env python3

from pathlib import Path

INPUT_FILEPATH = (p := Path(__file__)).parent/".."/"inputs"/f"{p.stem}.txt"

parse_input = lambda input_data: [int(v[1:]) * (1 if v[0] == 'R' else -1) for v in input_data.splitlines()]

def part_1(input_data: str):
    pos = 50
    steps = [pos := (pos + x) % 100 for x in parse_input(input_data)]
    result = len([s for s in steps if s == 0])
    print("Part 1:", result)


def part_2(input_data: str):
    pos = 50
    zero_count = 0
    for inst in parse_input(input_data):
        over_count = (pos + inst) // 100
        next_pos = (pos + inst) % 100
        if over_count > 0:
            zero_count += over_count
        elif over_count == 0:
            if next_pos == 0:
                zero_count += 1
        else:
            zero_count += abs(over_count)
            if pos == 0:
                zero_count -= 1
            if next_pos == 0:
                zero_count += 1
        pos = next_pos
    result = zero_count
    print("Part 2:", result)


if __name__ == "__main__":
    with INPUT_FILEPATH.open() as file:
        file_data = file.read()
        part_1(file_data)
        part_2(file_data)
