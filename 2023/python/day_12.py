#!/usr/bin/env python3

import os
from functools import cache

INPUT_FILE = "day_12.txt"
INPUT_FILEPATH = os.path.join(os.path.dirname(__file__), "..", "inputs", INPUT_FILE)


def part_1(input: str):
    result = 0
    for line in input.splitlines():
        springs, records = parse_spring_line(line)
        result += compute_possibilities(springs, records)
    print("Result part 1:", result)


def part_2(input: str):
    result = 0
    for line in input.splitlines():
        springs, records = expand_and_parse_spring_line(line)
        result += compute_possibilities(springs, records)
    print("Result part 2:", result)


@cache
def compute_possibilities(springs: str, records: tuple[int]) -> int:
    if len(springs) == 0:
        if len(records) == 0 or (len(records) == 1 and records[0] == 0):
            return 1
        else:
            return 0
    if springs[0] == ".":
        if len(records) > 0 and records[0] == 0:
            records = records[1:]
        return compute_possibilities(springs[1:], records)
    if springs[0] == "#":
        if len(records) == 0 or records[0] == 0:
            return 0
        fst_record = records[0] - 1
        if fst_record > 0 and len(springs)> 1 and springs[1] != ".":
            return compute_possibilities("#"+springs[2:], tuple([fst_record, *records[1:]]))
        elif fst_record == 0:
            return compute_possibilities(springs[1:], tuple([fst_record, *records[1:]]))
        else:
            return 0
    if springs[0] == "?":
        return compute_possibilities("."+springs[1:], records) + compute_possibilities("#"+springs[1:], records)
    raise Exception("Cannot be reached")


def parse_spring_line(input: str) -> tuple[str, tuple[int]]:
    springs, records_str = input.split()
    records = tuple(int(r) for r in records_str.split(","))
    return springs, records


def expand_and_parse_spring_line(input: str) -> tuple[str, tuple[int]]:
    springs, records_str = input.split()
    records_str = ((records_str + ",") *5)[:-1]
    records = tuple(int(r) for r in records_str.split(","))
    springs = ((springs + "?") *5)[:-1] 
    return springs, records


if __name__ == "__main__":
    with open(INPUT_FILEPATH) as f:
        input = f.read()
        part_1(input)
        part_2(input)