#!/usr/bin/env python3

from pathlib import Path
import re

INPUT_FILEPATH = (p := Path(__file__)).parent/".."/"inputs"/f"{p.stem}.txt"


def part_1(input_data: str):
    parameters = re.findall(r"X\+(\d+), Y\+(\d+).*?X\+(\d+), Y\+(\d+).*?X=(\d+), Y=(\d+)", input_data, re.DOTALL)
    tokens = []
    # System of linear equations, with a known mathematical solution
    for p in parameters:
        p = list(map(int, p))
        a, b, c, d, e, f = p[0], p[2], p[1], p[3], p[4], p[5]
        D = a*d - b*c
        if D == 0:
            continue
        A = e*d-f*b
        B = a*f-e*c
        x_count = A/D
        y_count = B/D
        if x_count < 0 or y_count < 0:
            continue
        if x_count > 100 or y_count > 100:
            continue
        if not x_count.is_integer() or not y_count.is_integer():
            continue
        tokens.append(3*x_count + y_count)
    print("Part 1:", sum(tokens))

def part_2(input_data: str):
    parameters = re.findall(r"X\+(\d+), Y\+(\d+).*?X\+(\d+), Y\+(\d+).*?X=(\d+), Y=(\d+)", input_data, re.DOTALL)
    tokens = []
    for p in parameters:
        p = list(map(int, p))
        a, b, c, d, e, f = p[0], p[2], p[1], p[3], p[4]+10000000000000, p[5]+10000000000000
        D = a*d - b*c
        if D == 0:
            continue
        A = e*d-f*b
        B = a*f-e*c
        x_count = A/D
        y_count = B/D
        if x_count < 0 or y_count < 0:
            continue
        if not x_count.is_integer() or not y_count.is_integer():
            continue
        tokens.append(3*x_count + y_count)
    print("Part 2:", sum(tokens))


if __name__ == "__main__":
    with INPUT_FILEPATH.open() as file:
        file_data = file.read()
        part_1(file_data)
        part_2(file_data)
