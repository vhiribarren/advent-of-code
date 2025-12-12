#!/usr/bin/env python3

from pathlib import Path
from math import prod

INPUT_FILEPATH = (p := Path(__file__)).parent/".."/"inputs"/f"{p.stem}.txt"

AREAS = [6, 5, 7, 7, 7, 7]

def problem(input_data: str):
    result = 0
    for l in input_data.splitlines():
        if "x" not in l:
            continue
        left, right = l.split(":")
        area = prod(map(int, left.split("x")))
        quantities = list(map(int, right.split()))
        min_req_space = sum(prod(qa) for qa in zip(quantities, AREAS))
        if min_req_space <= area:
            print("remaining space:", area - min_req_space)
            result += 1
        else:
            print("not enough to fit all, missing space:", min_req_space - area)
    print("Part 1:", result)


if __name__ == "__main__":
    file_data = INPUT_FILEPATH.read_text()
    problem(file_data)
