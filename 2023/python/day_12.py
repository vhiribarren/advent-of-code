#!/usr/bin/env python3

import os
from itertools import product
from typing import Generator

INPUT_FILE = "day_12.txt"
INPUT_FILEPATH = os.path.join(os.path.dirname(__file__), "..", "inputs", INPUT_FILE)


def part_1(input: str):
    result = 0
    for line in input.splitlines():
        result += compute_spring_serie(line)
    print("Result part 1:", result)

def compute_spring_serie(line: str) -> int:
    result = 0
    springs, records_str = line.split()
    records = [int(r) for r in records_str.split(",")]
    for candidate_serie in generate_candidates(springs):
        counts = [len(group) for group in candidate_serie.split(".") if len(group) > 0]
        if counts == records:
            result += 1
    return result

def generate_candidates(input: str) -> Generator[str, None, None]:
    spring_count = input.count("?")
    for candidates in product(".#", repeat=spring_count):
        candidates = list(candidates)
        serie_attempt = ""
        for c in input:
            if c == "?":
                serie_attempt += candidates.pop()
            else:
                serie_attempt += c
        yield serie_attempt
        

if __name__ == "__main__":
    with open(INPUT_FILEPATH) as f:
        input = f.read()
        part_1(input)