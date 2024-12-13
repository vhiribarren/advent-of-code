#!/usr/bin/env python3

import re
from pathlib import Path
from typing import Optional

INPUT_FILEPATH = (p := Path(__file__)).parent/".."/"inputs"/f"{p.stem}.txt"

def equation_solver(a: int, b: int, c: int, d: int, e: int, f: int) -> Optional[tuple[int, int]]:
    D = a*d - b*c
    if D == 0:
        return None
    A = e*d-f*b
    B = a*f-e*c
    x_count = A/D
    y_count = B/D
    if x_count < 0 or y_count < 0:
        return None
    if not x_count.is_integer() or not y_count.is_integer():
        return None
    return (int(x_count), int(y_count))

def parser(input_data: str) -> list[list[int]]:
    parameters = re.findall(r"X\+(\d+), Y\+(\d+).*?X\+(\d+), Y\+(\d+).*?X=(\d+), Y=(\d+)", input_data, re.DOTALL)
    return [list(map(int, p)) for p in parameters]


def part_1(input_data: str):
    parameters = parser(input_data)
    tokens = []
    # System of linear equations, with a known mathematical solution
    for p in parameters:
        a, b, c, d, e, f = p[0], p[2], p[1], p[3], p[4], p[5]
        eq_result = equation_solver(a, b, c, d, e, f)
        if eq_result is None:
            continue
        x_count, y_count = eq_result
        if x_count > 100 or y_count > 100:
            continue
        tokens.append(3*x_count + y_count)
    print("Part 1:", sum(tokens))

def part_2(input_data: str):
    parameters = parser(input_data)
    tokens = []
    # System of linear equations, with a known mathematical solution
    for p in parameters:
        a, b, c, d, e, f = p[0], p[2], p[1], p[3], p[4]+10000000000000, p[5]+10000000000000
        eq_result = equation_solver(a, b, c, d, e, f)
        if eq_result is None:
            continue
        x_count, y_count = eq_result
        tokens.append(3*x_count + y_count)
    print("Part 2:", sum(tokens))


if __name__ == "__main__":
    with INPUT_FILEPATH.open() as file:
        file_data = file.read()
        part_1(file_data)
        part_2(file_data)
